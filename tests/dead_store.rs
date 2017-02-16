extern crate compiler;

use compiler::ast::DeclarationInfo;
use compiler::semcheck::Type;

use compiler::cfg::CFG;
use compiler::cfg::Adj;
use compiler::cfg::basic_block::BasicBlock;

use compiler::optimizer::dead_store::remove_dead_stores;

use compiler::tac_generator::Operand;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Function;

use std::rc::Rc;

// FIXME: Needs more test cases

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

    let mut f = Function {
        name: Rc::new("foo".to_string()),
        statements: statements,
    };

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

    let mut f = Function {
        name: Rc::new("foo".to_string()),
        statements: statements,
    };

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

    let mut f = Function {
        name: Rc::new("foo".to_string()),
        statements: statements,
    };

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

    let mut f = Function {
        name: Rc::new("foo".to_string()),
        statements: statements,
    };

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