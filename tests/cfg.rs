extern crate compiler;


use compiler::cfg::basic_block::BasicBlock;
use compiler::cfg::generate_cfg;
use compiler::cfg::CFG;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Function;
use compiler::tac_generator::Operator;
use compiler::tac_generator::Operand;

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

#[test]
fn removing_statement_from_the_beginning_of_function_updates_cfg_and_function_correctly() {
    
    let statements = vec![
        Statement::Label(1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];
    
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let mut functions = vec![function];
    println!("{:?} ", functions[0].statements);

    let mut cfgs = generate_cfg(&mut functions);
    let cfg = &mut cfgs.get_mut("foo").unwrap(); 


    let remove_list = vec![0];
    cfg.remove_statements(&mut functions[0], remove_list);

    assert_eq!(7, functions[0].statements.len());

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[0]);

    assert_eq!(
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        functions[0].statements[1]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[2]);

    assert_eq!(
        Statement::Label(4),
        functions[0].statements[3]);

    assert_eq!(
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        functions[0].statements[4]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[5]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[6]);

    assert_eq!(3, cfg.basic_blocks.len());

    assert_eq!(0, cfg.basic_blocks[0].start);
    assert_eq!(3, cfg.basic_blocks[0].end);

    assert_eq!(3, cfg.basic_blocks[1].start);
    assert_eq!(5, cfg.basic_blocks[1].end);
    
    assert_eq!(5, cfg.basic_blocks[2].start);
    assert_eq!(7, cfg.basic_blocks[2].end);
    
}

#[test]
fn removing_statements_from_multiple_basic_blocks_updates_cfg_and_function_correctly() {
    
    let statements = vec![
        Statement::Label(1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];
    
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let mut functions = vec![function];
    let mut cfgs = generate_cfg(&mut functions);
    let cfg = &mut cfgs.get_mut("foo").unwrap(); 

    let remove_list = vec![2, 5, 6];
    cfg.remove_statements(&mut functions[0], remove_list);

    assert_eq!(5, functions[0].statements.len());

    assert_eq!(
        Statement::Label(1),
        functions[0].statements[0]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[1]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[2]);

    assert_eq!(
        Statement::Label(4),
        functions[0].statements[3]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[4]);


    assert_eq!(3, cfg.basic_blocks.len());

    assert_eq!(0, cfg.basic_blocks[0].start);
    assert_eq!(3, cfg.basic_blocks[0].end);

    assert_eq!(3, cfg.basic_blocks[1].start);
    assert_eq!(4, cfg.basic_blocks[1].end);
    
    assert_eq!(4, cfg.basic_blocks[2].start);
    assert_eq!(5, cfg.basic_blocks[2].end);
}

#[test]
fn removing_statements_from_multiple_basic_blocks_boundaries_updates_cfg_and_function_correctly() {
    
    let statements = vec![
        Statement::Label(1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];
    
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let mut functions = vec![function];
    let mut cfgs = generate_cfg(&mut functions);
    let cfg = &mut cfgs.get_mut("foo").unwrap(); 

    let remove_list = vec![3, 4, 6];
    cfg.remove_statements(&mut functions[0], remove_list);

    assert_eq!(5, functions[0].statements.len());

    assert_eq!(
        Statement::Label(1),
        functions[0].statements[0]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[1]);

    assert_eq!(
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        functions[0].statements[2]);

    assert_eq!(
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        functions[0].statements[3]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[4]);

    assert_eq!(3, cfg.basic_blocks.len());

    assert_eq!(0, cfg.basic_blocks[0].start);
    assert_eq!(3, cfg.basic_blocks[0].end);

    assert_eq!(3, cfg.basic_blocks[1].start);
    assert_eq!(4, cfg.basic_blocks[1].end);
    
    assert_eq!(4, cfg.basic_blocks[2].start);
    assert_eq!(5, cfg.basic_blocks[2].end);
}

#[test]
fn removing_statements_from_block_with_size_of_one_updates_cfg_and_function_correctly() {
        let statements = vec![
        Statement::Label(1),
        Statement::Label(2),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        Statement::Assignment(None, None, None, None),
        Statement::Label(4),
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        Statement::Assignment(None, None, None, None),
        Statement::Assignment(None, None, None, None),
    ];
    
    let function = Function {
        name: "foo".to_string(),
        statements: statements,
    };

    let mut functions = vec![function];
    let mut cfgs = generate_cfg(&mut functions);
    let cfg = &mut cfgs.get_mut("foo").unwrap(); 

    let remove_list = vec![0];
    cfg.remove_statements(&mut functions[0], remove_list);

    assert_eq!(8, functions[0].statements.len());

    assert_eq!(
        Statement::Label(2),
        functions[0].statements[0]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[1]);

    assert_eq!(
        Statement::Assignment(Some(Operator::Plus), None, None, None),
        functions[0].statements[2]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[3]);

    assert_eq!(
        Statement::Label(4),
        functions[0].statements[4]);

    assert_eq!(
        Statement::JumpIfTrue(Operand::Boolean(false), 1),
        functions[0].statements[5]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[6]);

    assert_eq!(
        Statement::Assignment(None, None, None, None),
        functions[0].statements[7]);

    assert_eq!(4, cfg.basic_blocks.len());

    assert_eq!(0, cfg.basic_blocks[0].start);
    assert_eq!(0, cfg.basic_blocks[0].end);

    assert_eq!(0, cfg.basic_blocks[1].start);
    assert_eq!(4, cfg.basic_blocks[1].end);
    
    assert_eq!(4, cfg.basic_blocks[2].start);
    assert_eq!(6, cfg.basic_blocks[2].end);

    assert_eq!(6, cfg.basic_blocks[3].start);
    assert_eq!(8, cfg.basic_blocks[3].end);

}

/*
#[test]
fn removing_empty_block_updates_cfg_and_function_correctly() {
    unimplemented!();
}

#[test]
fn removing_first_block_updates_cfg_and_function_correctly() {
    unimplemented!();
}

#[test]
fn removing_last_block_updates_cfg_and_function_correctly() {
    unimplemented!();
}

#[test]
fn removing_block_from_the_middle_updates_cfg_and_function_correctly() {
    unimplemented!();
}
*/