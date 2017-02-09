extern crate compiler;

use compiler::ast::DeclarationInfo;
use compiler::semcheck::Type;
use compiler::tac_generator::Function;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Operand;
use compiler::tac_generator::Operator;

use compiler::cfg::Adj;
use compiler::cfg::CFG;
use compiler::cfg::basic_block::BasicBlock;
use compiler::optimizer::conditional_jump_conversion::convert_jumps;

#[test]
fn false_edge_is_removed_and_jump_converted_to_unconditional_if_jump_operand_is_true() {    
    let statements = vec![
        // block 1 
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::JumpIfTrue(Operand::Boolean(true), 0),
        // block 2
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        // block 3
        Statement::Label(0),
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(1), Adj::Block(2)],
            vec![Adj::Block(2)],
            vec![Adj::End],
        ],
        dominance_frontier: vec![],
        immediate_dominators: vec![],
    };

    convert_jumps(&mut f, &mut cfg);

    assert_eq!(5, f.statements.len());

    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None), 
        f.statements[0]);
    assert_eq!(
        Statement::Jump(0), 
        f.statements[1]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None), 
        f.statements[2]);
    assert_eq!(
        Statement::Label(0), 
        f.statements[3]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None), 
        f.statements[4]);

    assert_eq!(3, cfg.adjacency_list.len());

    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[2]);
}

#[test]
fn true_edge_is_removed_and_jump_removed_if_jump_operand_is_true() {    
    let statements = vec![
        // block 1 
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::JumpIfTrue(Operand::Boolean(false), 0),
        // block 2
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        // block 3
        Statement::Label(0),
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(1), Adj::Block(2)],
            vec![Adj::Block(2)],
            vec![Adj::End],
        ],
        dominance_frontier: vec![],
        immediate_dominators: vec![],
    };

    convert_jumps(&mut f, &mut cfg);

    assert_eq!(4, f.statements.len());

    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None), 
        f.statements[0]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None), 
        f.statements[1]);
    assert_eq!(
        Statement::Label(0), 
        f.statements[2]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None), 
        f.statements[3]);

    assert_eq!(3, cfg.adjacency_list.len());

    assert_eq!(vec![Adj::Block(1)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[2]);
}

#[test]
fn variable_is_removed_from_phi_function_if_the_edge_is_removed_and_condition_is_true() {

    let statements = vec![
        // block 1 
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(
                DeclarationInfo::new_alt(
                    "a".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 0)), 
            None, 
            Some(Operand::Integer(5))),
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0)), 
            None, 
            Some(Operand::Integer(6))),
        Statement::Jump(0),
        // block 2
        Statement::Label(1),
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)), 
            None, 
            Some(Operand::Integer(8))),
        Statement::Jump(3),        
        // block 3
        Statement::Label(0),
        Statement::JumpIfTrue(Operand::Boolean(true), 1),
        // block 4
        Statement::Label(3),
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "b".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 2),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)
            ]),
        Statement::Return(Some(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
            1, 2))),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(2)],
            vec![Adj::Block(3)],
            vec![Adj::Block(1), Adj::Block(3)],
            vec![Adj::End],
        ],
        dominance_frontier: vec![],
        immediate_dominators: vec![],
    };

    convert_jumps(&mut f, &mut cfg);

    assert_eq!(4, cfg.adjacency_list.len());

    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(3)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::Block(1)], cfg.adjacency_list[2]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[3]);

    assert_eq!(11, f.statements.len());
    assert_eq!(
        Statement::Jump(1),
        f.statements[7]);
    assert_eq!(
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "b".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 2),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)
            ]),
        f.statements[9]);
}

#[test]
fn variable_is_removed_from_phi_function_if_the_edge_is_removed_and_condition_is_false() {

    let statements = vec![
        // block 1 
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(
                DeclarationInfo::new_alt(
                    "a".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 0)), 
            None, 
            Some(Operand::Integer(5))),
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0)), 
            None, 
            Some(Operand::Integer(6))),
        Statement::Jump(0),
        // block 2
        Statement::Label(1),
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)), 
            None, 
            Some(Operand::Integer(8))),
        Statement::Jump(3),        
        // block 3
        Statement::Label(0),
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        // block 4
        Statement::Label(3),
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "b".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 2),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)
            ]),
        Statement::Return(Some(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
            1, 2))),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(2)],
            vec![Adj::Block(3)],
            vec![Adj::Block(1), Adj::Block(3)],
            vec![Adj::End],
        ],
        dominance_frontier: vec![], 
        immediate_dominators: vec![],
    };

    convert_jumps(&mut f, &mut cfg);

    assert_eq!(4, cfg.adjacency_list.len());

    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(3)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::Block(3)], cfg.adjacency_list[2]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[3]);

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
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "b".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 2),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)
            ]) {
        println!("\nexp Variable: {:?}", op);
        println!("\nexp Other variables: {:?}\n", ops);
    }


    assert_eq!(
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "b".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 2),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "b".to_string(),
                    Type::Integer,
                    0, 0, 0), 
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
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(
                DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 0)), 
            None, 
            Some(Operand::Integer(0))),
        Statement::Assignment(
            None, 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0)), 
            None, 
            Some(Operand::Integer(0))),
        Statement::Jump(0),
        // block 2
        Statement::Label(1),
        Statement::Assignment(
            Some(Operator::Plus), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 2)),             
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 1)), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 1))),
        Statement::Assignment(
            Some(Operator::Plus), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 2)), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 1)), 
            Some(Operand::Integer(1))),
        Statement::Jump(2),        
        // block 3
        Statement::Label(3),
        Statement::Assignment(
            Some(Operator::Multiply), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 3)),             
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 2)), 
            Some(Operand::Integer(2))),
        Statement::Jump(5),        
        // block 4
        Statement::Label(2),
        Statement::Assignment(
            Some(Operator::Greater), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "tmp".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                5, 0)),             
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 2)), 
            Some(Operand::Integer(10))),
        Statement::JumpIfTrue(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                    "tmp".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                5, 0), 
            3),
        // block 5
        Statement::Label(5),
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "j".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 4),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 3),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 2)
            ]),
        // block 6
        Statement::Label(0),
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "j".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 1),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 4),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0)
            ]),
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "i".to_string(),
                Type::Integer,
                0, 0, 0), 
            0, 1),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 2),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 0)
            ]),
        Statement::Assignment(
            Some(Operator::Less), 
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "tmp".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                6, 0)),             
            Some(Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 1)), 
            Some(Operand::Integer(20))),
        Statement::JumpIfTrue(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                    "tmp".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                6, 0), 
            1),
        // block 7
        Statement::Return(Some(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
            1, 1))),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(5)],
            vec![Adj::Block(3)],
            vec![Adj::Block(4)],
            vec![Adj::Block(4), Adj::Block(2)],
            vec![Adj::Block(5)],
            vec![Adj::Block(6), Adj::Block(1)],
            vec![Adj::End],
        ],
        dominance_frontier: vec![], 
        immediate_dominators: vec![],
    };

    convert_jumps(&mut f, &mut cfg);


    assert_eq!(21, f.statements.len());

    assert_eq!(7, cfg.adjacency_list.len());

    assert_eq!(vec![Adj::Block(5)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(3)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::Block(4)], cfg.adjacency_list[2]);
    assert_eq!(vec![Adj::Block(4), Adj::Block(2)], cfg.adjacency_list[3]);
    assert_eq!(vec![Adj::Block(5)], cfg.adjacency_list[4]);
    assert_eq!(vec![Adj::Block(6), Adj::Block(1)], cfg.adjacency_list[5]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[6]);

    assert_eq!(
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "j".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 4),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 3),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 2)
            ]),
        f.statements[14]);

    assert_eq!(
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "j".to_string(),
                Type::Integer,
                0, 0, 0), 
            1, 1),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 4),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "j".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                1, 0)
            ]),
        f.statements[16]);

    
    assert_eq!(
        Statement::PhiFunction(
            Operand::SSAVariable(DeclarationInfo::new_alt(
                "i".to_string(),
                Type::Integer,
                0, 0, 0), 
            0, 1),
            vec![
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 2),
                Operand::SSAVariable(DeclarationInfo::new_alt(
                    "i".to_string(),
                    Type::Integer,
                    0, 0, 0), 
                0, 0)
            ]),
        f.statements[17]);
}