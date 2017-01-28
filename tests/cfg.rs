extern crate compiler;


use compiler::cfg_generator::basic_block::BasicBlock;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Function;

#[test]
fn no_branching_constructs_single_bb() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(1, bb.len());
    assert_eq!(0, bb[0].start);
    assert_eq!(statements_len, bb[0].end);
}

#[test]
fn labels_start_a_new_block() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(2, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(3, bb[0].end);


    assert_eq!(3, bb[1].start);
    assert_eq!(statements_len, bb[1].end);
}

#[test]
fn jumps_end_the_block() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Jump(4),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(2, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(4, bb[0].end);

    assert_eq!(4, bb[1].start);
    assert_eq!(statements_len, bb[1].end);
}

#[test]
fn return_statement_end_the_block() {
        let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Return(None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(2, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(4, bb[0].end);

    assert_eq!(4, bb[1].start);
    assert_eq!(statements_len, bb[1].end);
}

#[test]
fn labels_followed_by_jumps_generate_correct_bb() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::Jump(1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(3, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(3, bb[0].end);

    assert_eq!(3, bb[1].start);
    assert_eq!(5, bb[1].end);

    assert_eq!(5, bb[2].start);
    assert_eq!(statements_len, bb[2].end);
}

#[test]
fn jumps_followed_by_labels_generate_correct_bb() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Jump(1),
        Statement::Label(4),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(2, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(4, bb[0].end);

    assert_eq!(4, bb[1].start);
    assert_eq!(statements_len, bb[1].end);
}

#[test]
fn return_followed_by_label_generate_correct_bb() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Return(None),
        Statement::Label(4),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(2, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(4, bb[0].end);

    assert_eq!(4, bb[1].start);
    assert_eq!(statements_len, bb[1].end);
}

#[test]
fn label_followed_by_return_generate_correct_bb() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::Jump(1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(3, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(3, bb[0].end);

    assert_eq!(3, bb[1].start);
    assert_eq!(5, bb[1].end);

    assert_eq!(5, bb[2].start);
    assert_eq!(statements_len, bb[2].end);
}

#[test]
fn return_followed_by_jump_generate_correct_bb() {
    let statements = vec![
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Return(None),
        Statement::Jump(1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];

    let statements_len = statements.len();
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let bb = BasicBlock::construct_basic_blocks(&function);

    assert_eq!(3, bb.len());

    assert_eq!(0, bb[0].start);
    assert_eq!(4, bb[0].end);

    assert_eq!(4, bb[1].start);
    assert_eq!(5, bb[1].end);

    assert_eq!(5, bb[2].start);
    assert_eq!(statements_len, bb[2].end);
}