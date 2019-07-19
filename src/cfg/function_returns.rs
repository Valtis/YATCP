use super::CFG;
use super::basic_block::BasicBlock;
use super::{Adj};

use crate::tac_generator::{Function, Statement};
use crate::error_reporter::{ErrorReporter, ReportKind};
use crate::semcheck::{Type};

use rayon::prelude::*;


use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;


/*
    For non-void functions, check that value is returned in all basic blocks that connect to end block,
    For void functions, insert returns if end block does not contain return
*/

pub fn check_function_returns(
    mut functions: Vec<Function>,
    cfg: &mut HashMap<Rc<String>, CFG>,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>) -> Vec<Function> {

    for f in functions.iter_mut() {
        check_function(f, cfg.get_mut(&f.function_info.name).unwrap(), error_reporter.clone());
    }

    functions
}

fn check_function(
    function: &mut Function,
    cfg: &mut CFG,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>)  {

    for (pos, adjacencies) in cfg.adjacency_list.iter().enumerate() {
        if adjacencies.contains(&Adj::End) {

            let bb = &cfg.basic_blocks[pos];
            match function.statements[bb.end-1] {
               Statement::Return(_) => (), // OK
               _ => {
                   if function.function_info.return_type != Type::Void {
                       error_reporter.borrow_mut().report_error(
                           ReportKind::DataFlowError,
                           function.function_info.node_info.line,
                           function.function_info.node_info.column,
                           function.function_info.node_info.length,
                           format!("Function '{}': Not all control flow paths return a value", function.function_info.name)
                       )
                   } else {
                       insert_return(function, &mut cfg.basic_blocks, pos);
                   }
               }
            }
        }
    }

}

fn insert_return(function: &mut Function, basic_blocks: &mut Vec<BasicBlock>, bb_pos: usize) {

    let bb = basic_blocks.get_mut(bb_pos).unwrap();
    function.statements.insert(bb.end, Statement::Return(None));

    bb.end += 1;

    let end = bb.end;
    shift_blocks(basic_blocks, end);
}

fn shift_blocks(basic_blocks: &mut Vec<BasicBlock>, after_line: usize) {
    for bb in basic_blocks.iter_mut() {
        if bb.end > after_line {
            bb.start += 1;
            bb.end += 1;
        }
    }
}
