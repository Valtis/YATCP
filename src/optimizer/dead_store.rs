use crate::cfg::CFG;

use crate::tac_generator::{Function, Statement, Operand};

use std::collections::HashMap;
use std::collections::HashSet;

pub fn remove_dead_stores(
    function: &mut Function,
    cfg: &mut CFG) {
    let mut changes = true;
    while changes {
        changes = false;
        let mut reads = HashSet::new();
        let mut writes = HashMap::new();

        for (i, s) in function.statements.iter().enumerate() {

            // writes to variables and where they occur
            match *s {
                Statement::Assignment(
                    _,
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id)),
                    _,
                    _) => { writes.insert((*var_id, *ssa_id), i); },
                Statement::PhiFunction(
                     Operand::SSAVariable(_, ref var_id, ref ssa_id),
                    _) => { writes.insert((*var_id, *ssa_id), i); },
                Statement::Call(
                    _,
                    _,
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id))) => {
                        writes.insert((*var_id, *ssa_id), i);
                },
                _ => {},
            }

            // reads
            match *s {
                 Statement::Assignment(
                    _,
                    _,
                    Some(ref val),
                    Some(ref val2)) => {

                    match *val {
                        Operand::SSAVariable(_, ref var_id, ref ssa_id) => {
                            reads.insert((*var_id, *ssa_id));
                        },
                        _ => {},
                    }

                    match *val2 {
                        Operand::SSAVariable(_, ref var_id, ref ssa_id) => {
                            reads.insert((*var_id, *ssa_id));
                        },
                        _ => {},
                    }
                },
                Statement::JumpIfTrue(
                    Operand::SSAVariable(_, ref var_id, ref ssa_id),
                    _) => {

                    reads.insert((*var_id, *ssa_id));
                },
                Statement::Return(
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id))) => {

                    reads.insert((*var_id, *ssa_id));
                },
                Statement::PhiFunction(
                    _,
                    ref operands) => {

                    for o in operands.iter() {
                        match *o {
                            Operand::SSAVariable(_, ref var_id, ref ssa_id) => {
                                reads.insert((*var_id, *ssa_id));
                            },
                            _ => ice!("Invalid operand present in phi-function: {}", o),
                        }
                    }
                },
                Statement::Call(_, ref args, _) => {
                    for arg in args {
                        if let Operand::SSAVariable(_, ref var_id, ref ssa_id) = *arg {
                            reads.insert((*var_id, *ssa_id));
                        }
                    }
                }
                _ => {},
            }
        }

        for v in reads.iter() {
            writes.remove(v);
        }

        // function calls should not be removed, as they may have side effects
        // instead, remove the dead store from the operation
        remove_dead_writes_in_function_calls(function, &mut writes);


        let mut remove_list : Vec<usize> = writes.values().cloned().collect();
        remove_list.sort();

        if remove_list.len() != 0 {
            changes = true;
            cfg.remove_statements(function, remove_list);
        }


        println!("\nAfter removal:\n");
        for (i, s) in function.statements.iter().enumerate() {
            println!("{}:  {}", i, s);
        }
    }
}


fn remove_dead_writes_in_function_calls(
    function: &mut Function,
    writes: &mut HashMap<(u32, u32), usize>) {
    for line in writes.values() {
        if let Statement::Call(_, _, ref mut dest) = function.statements[*line] {
            *dest = None;
        }
    }

    *writes = writes
    .iter()
    .filter(
        |&(_, line)|
        if let Statement::Call(_, _, _) = function.statements[*line] {
            false
        } else {
            true
        })
    .map(|(k, v)| (*k, *v))
    .collect::<HashMap<(u32, u32), usize>>();

}



#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FunctionInfo, NodeInfo, DeclarationInfo};
    use crate::cfg::{Adj, basic_block::BasicBlock};


    use crate::semcheck::Type;


    use std::rc::Rc;

    fn create_function(statements: Vec<Statement>) -> Function {
        Function {
            function_info: FunctionInfo {
                name: Rc::new("foo".to_string()),
                parameters: vec![],
                return_type: Type::Void,
                node_info: NodeInfo {
                    line: 1,
                    column: 1,
                    length: 3
                },
            },
            statements,
        }
    }

    #[test]
    fn dead_store_is_removed() {
        /*
            let a : int = 2;
            a = 6;
            return a;
        */

        let decl_info = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);

        let statements = vec![
            // block 1
            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info.clone(), 0, 0)),
                                  None,
                                  Some(Operand::Integer(2))),

            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info.clone(), 0, 1)),
                                  None,
                                  Some(Operand::Integer(6))),

            Statement::Return(Some(Operand::SSAVariable(decl_info.clone(), 0, 1))),
        ];
        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 3,
                },
            ],
            adjacency_list: vec![
                vec![Adj::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        remove_dead_stores(&mut f, &mut cfg);

        assert_eq!(2, f.statements.len());

        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);


        assert_eq!(
            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info.clone(), 0, 1)),
                                  None,
                                  Some(Operand::Integer(6))),
            f.statements[0]);
        assert_eq!(
            Statement::Return(Some(Operand::SSAVariable(decl_info.clone(), 0, 1))),
            f.statements[1]);

        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adj::End], cfg.adjacency_list[0]);
    }


    #[test]
    fn store_is_not_removed_when_used_in_function_call() {
        /*
            let a : int = 2;
            foo(a);
        */

        let decl_info = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);

        let statements = vec![
            // block 1
            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info.clone(), 0, 0)),
                                  None,
                                  Some(Operand::Integer(2))),

            Statement::Call(
                Rc::new("foo".to_string()),
                vec![
                    Operand::SSAVariable(decl_info.clone(), 0, 0)],
                None),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
            ],
            adjacency_list: vec![
                vec![Adj::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        remove_dead_stores(&mut f, &mut cfg);

        assert_eq!(2, f.statements.len());

        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);


        assert_eq!(
            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info.clone(), 0, 0)),
                                  None,
                                  Some(Operand::Integer(2))),
            f.statements[0]);
        assert_eq!(
            Statement::Call(
                Rc::new("foo".to_string()),
                vec![
                    Operand::SSAVariable(decl_info.clone(), 0, 0)],
                None),
            f.statements[1]);

        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adj::End], cfg.adjacency_list[0]);
    }

    #[test]
    fn dead_return_value_is_removed_without_removing_the_call() {
        /*
            let a : int = 2;
            let b : int = foo(a);
            return a;
        */

        let decl_info_a = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);


        let decl_info_b = DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::Integer,
            0, 0, 0);


        let statements = vec![
            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info_a.clone(), 0, 0)),
                                  None,
                                  Some(Operand::Integer(2))),

            Statement::Call(
                Rc::new("foo".to_string()),
                vec![
                    Operand::SSAVariable(decl_info_a.clone(), 0, 0)],
                Some(Operand::SSAVariable(decl_info_b.clone(), 1, 0))),

            Statement::Return(Some(Operand::SSAVariable(decl_info_a.clone(), 0, 0))),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 3,
                },
            ],
            adjacency_list: vec![
                vec![Adj::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        remove_dead_stores(&mut f, &mut cfg);

        assert_eq!(3, f.statements.len());

        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);


        assert_eq!(
            Statement::Assignment(None,
                                  Some(Operand::SSAVariable(decl_info_a.clone(), 0, 0)),
                                  None,
                                  Some(Operand::Integer(2))),
            f.statements[0]);
        assert_eq!(
            Statement::Call(
                Rc::new("foo".to_string()),
                vec![
                    Operand::SSAVariable(decl_info_a.clone(), 0, 0)],
                None),
            f.statements[1]);

        assert_eq!(
            Statement::Return(Some(Operand::SSAVariable(decl_info_a.clone(), 0, 0))),
            f.statements[2]);

        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adj::End], cfg.adjacency_list[0]);
    }

    #[test]
    fn return_value_is_not_removed_when_it_is_used() {
        /*
            let a : int = foo();
            return a;
        */

        let decl_info = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);


        let statements = vec![
            Statement::Call(
                Rc::new("foo".to_string()),
                vec![],
                Some(Operand::SSAVariable(decl_info.clone(), 0, 0))),

            Statement::Return(Some(Operand::SSAVariable(decl_info.clone(), 0, 0))),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
            ],
            adjacency_list: vec![
                vec![Adj::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        remove_dead_stores(&mut f, &mut cfg);

        assert_eq!(2, f.statements.len());

        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);



        assert_eq!(
            Statement::Call(
                Rc::new("foo".to_string()),
                vec![],
                Some(Operand::SSAVariable(decl_info.clone(), 0, 0))),
            f.statements[0]);

        assert_eq!(
            Statement::Return(Some(Operand::SSAVariable(decl_info.clone(), 0, 0))),
            f.statements[1]);

        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adj::End], cfg.adjacency_list[0]);
    }
}
