use crate::common::tac_code::{Function, Operand, Statement};

use super::super::cfg::{Adjacency, CFG, dom_front::calculate_immediate_dominator_opt};

use std::collections::HashMap;

// convert conditional jumps with constant operand into unconditional jumps
pub fn convert_jumps(
    function: &mut Function,
    cfg: &mut CFG) {
    let mut remove_list = vec![];

    let mut label_to_block = HashMap::new();

    for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
        match function.statements[bb.start] {
            Statement::Label(ref label_id) => {label_to_block.insert(*label_id, bb_id); },
            _ => {},
        }
    }

    for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {

        let vals = match function.statements[bb.end-1] {
            Statement::JumpIfTrue(Operand::Boolean(val), label_id) => {
                Some((val, label_id))
            },
            Statement::JumpIfFalse(Operand::Boolean(val), label_id) => {
                Some((!val, label_id))
            }
            _ => None,
        };

        if let Some((val, label_id)) = vals {
            if val {
                function.statements[bb.end-1] = Statement::Jump(label_id);
                // remove the next block from adjacency_list, as this is
                // no longer connected to this block
                cfg.adjacency_list[bb_id].retain(|v| *v != Adjacency::Block(bb_id + 1));
            } else {
                remove_list.push(bb.end-1);
                let target = label_to_block[&label_id];
                // remove the target block, as this is no longer reachable from this block
                cfg.adjacency_list[bb_id].retain(|v| *v != Adjacency::Block(target));
            }
        }
    }

    cfg.remove_statements(function, remove_list);
    update_phi_functions(function, cfg);
}

fn update_phi_functions(
    function: &mut Function,
    cfg: &mut CFG) {

    let reaching_defs = calculate_definitions_reaching_end_of_block(
        function,
        cfg);


    for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
        let parents = cfg.get_parent_blocks(bb_id);
        for s in bb.start..bb.end {
            match function.statements[s] {
                Statement::PhiFunction(
                    Operand::SSAVariable(_, id, _),
                    ref mut operands) => {
                    remove_non_reaching_defs_from_phi(
                        operands,
                        &reaching_defs,
                        bb_id,
                        id,
                        &parents);
                },
                _ => {},

            }
        }
    }
}

// return vector of vectors, where outer vector is indexed by
// basic block id, and inner vector contains all the variable id/ssa variable id
// pairs which definition reaches the end of the block
fn calculate_definitions_reaching_end_of_block(
    function: &Function,
    cfg: &CFG) -> Vec<HashMap<u32, u32>> {

    let immediate_dominators = calculate_immediate_dominator_opt(cfg);

    let mut reaching_defs = vec![];
    reaching_defs.resize(cfg.basic_blocks.len(), HashMap::new());

    calculate_definitions(function, cfg, 0, &immediate_dominators, &mut reaching_defs);

    reaching_defs
}

fn calculate_definitions(
    function: &Function,
    cfg: &CFG,
    cur_block: usize,
    immediate_dominators: &Vec<Option<usize>>,
    reaching_defs: &mut Vec<HashMap<u32, u32>>) {

    println!("\n\nCalculating definitions for block {}", cur_block+1);
    let mut cur_hashmap = HashMap::new();

    // populate the hashmap with parent info
    let parents = cfg.get_parent_blocks(cur_block);
    for parent in parents.iter() {
            println!("Pre-populating with values from parent block {}", parent);
            for (key, value) in reaching_defs[*parent].iter() {
                cur_hashmap.insert(*key, *value);
            }
    }

    println!("After pre-population: {:?}", cur_hashmap);

    // then replace the values with local declarations
    for i in cfg.basic_blocks[cur_block].start..cfg.basic_blocks[cur_block].end {

        match function.statements[i] {
              Statement::PhiFunction(
                Operand::SSAVariable(_, id, ssa_id),
                _) |
            Statement::Assignment{ destination: Some(Operand::SSAVariable(_, id, ssa_id)), .. } => {
                println!("Found local definition {}:{} from {}", id, ssa_id, function.statements[i]);
                cur_hashmap.insert(id, ssa_id);
            },
            _ => {},
        }
    }

    println!("After update: {:?}\n\n", cur_hashmap);
    reaching_defs[cur_block] = cur_hashmap;
    // finally recursively call this with immediately dominated values
    for block in immediately_dominated_nodes(cur_block, immediate_dominators) {
        calculate_definitions(
            function,
            cfg,
            block,
            immediate_dominators,
            reaching_defs);
    }
}

fn immediately_dominated_nodes(
    block: usize,
    immediate_dominators: &Vec<Option<usize>>) -> Vec<usize> {
    let mut successor_nodes = vec![];
    for (i, opt_dominator) in immediate_dominators.iter().enumerate() {
        if let Some(dominator) = *opt_dominator {
            if block == 0 && i == 0 {
                continue;
            }
            if dominator == block {
                successor_nodes.push(i);
            }
        }
    }
    successor_nodes
}




#[allow(unreachable_code)] // ide has false positive about what's reachable and shows error, needs explicit unreachable!() to silence. This causes rustc to complain about unreachable code
fn remove_non_reaching_defs_from_phi(
    operands: &mut Vec<Operand>,
    reaching_defs: &Vec<HashMap<u32, u32>>,
    block_id: usize,
    var_id: u32,
    parents: &Vec<usize>) {

    println!("\nHandling block {} for variable {}", block_id+1, var_id);
    println!("Parents: {:?}", parents.iter().map(|v| v+1).collect::<Vec<usize>>());
    println!("Reaching defs: {:?}", reaching_defs);


    operands.retain(|v|
        if let Operand::SSAVariable(_, _, ssa_id) = *v {
            for p in parents.iter() {
                if reaching_defs[*p].get(&var_id) == Some(&ssa_id) {
                    println!("Keeping ssa id: {}", ssa_id);
                    return true;
                }
            }
            println!("Removing ssa id: {}", ssa_id);
            return false;
        } else {
            ice!("Non-SSA variable {} present in phi node", v);
            unreachable!(); // Make IDE happy
        }
    );

    println!("\n");
}

#[cfg(test)]
mod tests {

    use super::*;

    use super::super::super::cfg::basic_block::BasicBlock;
    use crate::common::{
        types::Type,
        node_info::*,
        tac_code::*,
    };

    use std::rc::Rc;

    fn create_function(statements: Vec<Statement>) -> Function {
        Function {
            function_info: FunctionInfo {
                name: Rc::new("foo".to_string()),
                parameters: vec![],
                return_type: Type::Void,
                span: Span {
                    line: 1,
                    column: 1,
                    length: 3
                },
            },
            statements,
            attributes: vec![],
        }
    }

    #[test]
    fn false_edge_is_removed_and_jump_converted_to_unconditional_if_jump_operand_is_true() {
        let statements = vec![
            // block 1
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(1)),
                left_operand: None,
                right_operand: None
            },
            Statement::JumpIfTrue(Operand::Boolean(true), 0),
            // block 2
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(2)),
                left_operand: None,
                right_operand: None},
            // block 3
            Statement::Label(0),
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(3)),
                left_operand: None,
                right_operand: None},
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
                BasicBlock{
                    start: 2,
                    end: 3,
                },
                BasicBlock{
                    start: 3,
                    end: 5,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1), Adjacency::Block(2)],
                vec![Adjacency::Block(2)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        convert_jumps(&mut f, &mut cfg);

        assert_eq!(5, f.statements.len());

        assert_eq!(
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(1)),
                left_operand: None,
                right_operand: None},
            f.statements[0]);
        assert_eq!(
            Statement::Jump(0),
            f.statements[1]);
        assert_eq!(
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(2)),
                left_operand: None,
                right_operand: None},
            f.statements[2]);
        assert_eq!(
            Statement::Label(0),
            f.statements[3]);
        assert_eq!(
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(3)),
                left_operand: None,
                right_operand: None } ,
            f.statements[4]);

        assert_eq!(3, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[2]);
    }

    #[test]
    fn true_edge_is_removed_and_jump_removed_if_jump_operand_is_true() {
        let statements = vec![
            // block 1
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(1)),
                left_operand: None,
                right_operand: None },
            Statement::JumpIfTrue(Operand::Boolean(false), 0),
            // block 2
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(2)),
                left_operand: None,
                right_operand: None },
            // block 3
            Statement::Label(0),
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(3)),
                left_operand: None,
                right_operand: None},
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
                BasicBlock{
                    start: 2,
                    end: 3,
                },
                BasicBlock{
                    start: 3,
                    end: 5,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1), Adjacency::Block(2)],
                vec![Adjacency::Block(2)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        convert_jumps(&mut f, &mut cfg);

        assert_eq!(4, f.statements.len());

        assert_eq!(
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(1)),
                left_operand: None,
                right_operand: None},
            f.statements[0]);
        assert_eq!(
            Statement::Assignment {
                operator: None,
                destination: Some(Operand::Integer(2)),
                left_operand: None,
                right_operand: None },
            f.statements[1]);
        assert_eq!(
            Statement::Label(0),
            f.statements[2]);
        assert_eq!(
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Integer(3)),
                left_operand: None,
                right_operand: None},
            f.statements[3]);

        assert_eq!(3, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(1)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[2]);
    }

    #[test]
    fn variable_is_removed_from_phi_function_if_the_edge_is_removed_and_condition_is_true() {

        let statements = vec![
            // block 1
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(
                    DeclarationInfo::new(
                        Rc::new("a".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                    0, 0)),
                left_operand: None,
                right_operand: Some(Operand::Integer(5))
            },
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                ),
                                          1, 0)),
                left_operand: None,
                right_operand: Some(Operand::Integer(6))
            },
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                ),
                                          1, 1)),
                left_operand: None,
                right_operand: Some(Operand::Integer(8))
            },
            Statement::Jump(3),
            // block 3
            Statement::Label(0),
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 4
            Statement::Label(3),
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 2),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("b".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 0),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("b".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 1)
                ]),
            Statement::Return(Some(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 2))),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 3,
                },
                BasicBlock{
                    start: 3,
                    end: 6,
                },
                BasicBlock{
                    start: 6,
                    end: 8,
                },
                BasicBlock{
                    start: 8,
                    end: 11,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(1), Adjacency::Block(3)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        convert_jumps(&mut f, &mut cfg);

        assert_eq!(4, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);

        assert_eq!(11, f.statements.len());
        assert_eq!(
            Statement::Jump(1),
            f.statements[7]);
        assert_eq!(
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 2),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("b".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 1)
                ]),
            f.statements[9]);
    }

    #[test]
    fn variable_is_removed_from_phi_function_if_the_edge_is_removed_and_condition_is_false() {

        let statements = vec![
            // block 1
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(
                    DeclarationInfo::new(
                        Rc::new("a".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                    0, 0)),
                left_operand: None,
                right_operand: Some(Operand::Integer(5))} ,
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 0)),
                left_operand: None,
                right_operand: Some(Operand::Integer(6))
            },
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 1)),
                left_operand: None,
                right_operand: Some(Operand::Integer(8))
            },
            Statement::Jump(3),
            // block 3
            Statement::Label(0),
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            // block 4
            Statement::Label(3),
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 2),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("b".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 0),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("b".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 1)
                ]),
            Statement::Return(Some(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 2))),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 3,
                },
                BasicBlock{
                    start: 3,
                    end: 6,
                },
                BasicBlock{
                    start: 6,
                    end: 8,
                },
                BasicBlock{
                    start: 8,
                    end: 11,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(1), Adjacency::Block(3)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        convert_jumps(&mut f, &mut cfg);

        assert_eq!(4, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);

        assert_eq!(10, f.statements.len());
        assert_eq!(
            Statement::Label(0),
            f.statements[6]);

        assert_eq!(
            Statement::Label(3),
            f.statements[7]);


        if let Statement::PhiFunction(ref op, ref ops) = f.statements[8] {
            println!("\nact Variable: {:?}", op);
            println!("\nact Other variables: {:?}\n", ops);
        }

        if let Statement::PhiFunction(ref op, ref ops) = Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new(
                Rc::new("b".to_string()),
                Span::new(0,0,0),
                Type::Integer,
                ),
                                 1, 2),
            vec![
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 1)
            ]) {
            println!("\nexp Variable: {:?}", op);
            println!("\nexp Other variables: {:?}\n", ops);
        }


        assert_eq!(
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("b".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 2),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("b".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 0)
                ]),
            f.statements[8]);
    }

    #[test]
    fn phi_functions_are_not_modified_if_no_edges_are_removed() {

        /*
            fn bar() : int {
                let i : int = 0;
                let j : int = 0;
                while i < 20 {
                    j = j + i;
                    i = i + 1;
                    if i > 10 {
                        j = j*2;
                    }
                }
                return j;
            }
        */

        let statements = vec![
            // block 1
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::SSAVariable(
                    DeclarationInfo::new(
                        Rc::new("i".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                    0, 0)),
                left_operand: None,
                right_operand: Some(Operand::Integer(0))
            },
            Statement::Assignment{
                operator:  None,
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 0)),
                left_operand: None,
                right_operand: Some(Operand::Integer(0))
            },
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{
                operator: Some(Operator::Plus),
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 2)),
                left_operand: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 1)),
                right_operand: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          0, 1))
            },
            Statement::Assignment{
                operator: Some(Operator::Plus),
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          0, 2)),
                left_operand: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          0, 1)),
                right_operand: Some(Operand::Integer(1))
            },
            Statement::Jump(2),
            // block 3
            Statement::Label(3),
            Statement::Assignment{
                operator: Some(Operator::Multiply),
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 3)),
                left_operand: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          1, 2)),
                right_operand: Some(Operand::Integer(2))
            },
            Statement::Jump(5),
            // block 4
            Statement::Label(2),
            Statement::Assignment{
                operator: Some(Operator::Greater),
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("tmp".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          5, 0)),
                left_operand: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          0, 2)),
                right_operand: Some(Operand::Integer(10))
            },
            Statement::JumpIfTrue(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("tmp".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     5, 0),
                3),
            // block 5
            Statement::Label(5),
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 4),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 3),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 2)
                ]),
            // block 6
            Statement::Label(0),
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 1),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 4),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 0)
                ]),
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     0, 1),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("i".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         0, 2),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("i".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         0, 0)
                ]),
            Statement::Assignment{
                operator: Some(Operator::Less),
                destination: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("tmp".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          6, 0)),
                left_operand: Some(Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                          0, 1)),
                right_operand: Some(Operand::Integer(20))
            },
            Statement::JumpIfTrue(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("tmp".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     6, 0),
                1),
            // block 7
            Statement::Return(Some(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                ),
                                     1, 1))),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 3,
                },
                BasicBlock{
                    start: 3,
                    end: 7,
                },
                BasicBlock{
                    start: 7,
                    end: 10,
                },
                BasicBlock{
                    start: 10,
                    end: 13,
                },
                BasicBlock{
                    start: 13,
                    end: 15,
                },
                BasicBlock{
                    start: 15,
                    end: 20,
                },
                BasicBlock{
                    start: 20,
                    end: 21,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(5)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(4)],
                vec![Adjacency::Block(4), Adjacency::Block(2)],
                vec![Adjacency::Block(5)],
                vec![Adjacency::Block(6), Adjacency::Block(1)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        convert_jumps(&mut f, &mut cfg);


        assert_eq!(21, f.statements.len());

        assert_eq!(7, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(5)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(4)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::Block(4), Adjacency::Block(2)], cfg.adjacency_list[3]);
        assert_eq!(vec![Adjacency::Block(5)], cfg.adjacency_list[4]);
        assert_eq!(vec![Adjacency::Block(6), Adjacency::Block(1)], cfg.adjacency_list[5]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[6]);

        assert_eq!(
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 4),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 3),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 2)
                ]),
            f.statements[14]);

        assert_eq!(
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("j".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                    ),
                                     1, 1),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 4),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("j".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         1, 0)
                ]),
            f.statements[16]);


        assert_eq!(
            Statement::PhiFunction(
                Operand::SSAVariable(DeclarationInfo::new(
                    Rc::new("i".to_string()),
                    Span::new(0,0,0),
                    Type::Integer,
                   ),
                                     0, 1),
                vec![
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("i".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                    ),
                                         0, 2),
                    Operand::SSAVariable(DeclarationInfo::new(
                        Rc::new("i".to_string()),
                        Span::new(0,0,0),
                        Type::Integer,
                        ),
                                         0, 0)
                ]),
            f.statements[17]);
    }

}