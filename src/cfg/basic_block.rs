use tac_generator::Function;
use tac_generator::Statement;

#[derive(Debug, PartialEq, Clone)]
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