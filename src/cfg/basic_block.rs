use crate::tac_generator::{Function, Statement};

#[derive(Debug, PartialEq, Clone)]

/*
    interval: [start, end[
*/
pub struct BasicBlock {
    pub start: usize,
    pub end: usize,
}

impl BasicBlock {
    fn new(start: usize, end: usize) -> BasicBlock {
        if start >= end {
            ice!("Invalid basic block boundaries: Start {} end {}", start, end);
        }
        BasicBlock {
            start: start,
            end: end,
        }
    }

    pub fn construct_basic_blocks(function: &Function) -> Vec<BasicBlock> {
        let mut basic_blocks = vec![];
        let mut start = 0;
        let mut end = 0;

        for s in function.statements.iter() {
            match *s {
                Statement::Label(_) => {
                    if end - start  > 0 {
                        basic_blocks.push(BasicBlock::new(start, end));
                        start = end;
                    }
                },
                Statement::Jump(_) |
                Statement::JumpIfTrue(_, _) |
                Statement::JumpIfFalse(_, _) |
                Statement::Return(_)  => {
                    // include the jump in the block
                    basic_blocks.push(BasicBlock::new(start, end + 1));
                    start = end + 1;
                },
                _ => { },
            }
            end += 1;
        }
        if start <= end && start < function.statements.len() {
            basic_blocks.push(BasicBlock::new(start, end));
        }

        basic_blocks
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{FunctionInfo, NodeInfo};
    use crate::tac_generator::{Statement, Function};
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
            attributes: vec![],
        }
    }

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

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
        let function = create_function(statements);

        let bb = BasicBlock::construct_basic_blocks(&function);

        assert_eq!(3, bb.len());

        assert_eq!(0, bb[0].start);
        assert_eq!(4, bb[0].end);

        assert_eq!(4, bb[1].start);
        assert_eq!(5, bb[1].end);

        assert_eq!(5, bb[2].start);
        assert_eq!(statements_len, bb[2].end);
    }
}