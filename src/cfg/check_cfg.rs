use super::CFG;
use super::{Adj};

use crate::tac_generator::{Function, Statement};
use crate::error_reporter::{ErrorReporter, ReportKind};
use crate::semcheck::{Type};

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::function_attributes::FunctionAttribute;


pub fn check_cfg(
    mut functions: Vec<Function>,
    cfg: &mut HashMap<Rc<String>, CFG>,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>) -> Vec<Function> {

    for f in functions.iter_mut() {
        check_function(f, cfg.get_mut(&f.function_info.name).unwrap(), error_reporter.clone());
    }

    functions
}
/*
    For non-void functions, check that value is returned in all basic blocks that connect to end block,
    For void functions, insert returns if end block does not contain return
*/

fn check_function(
    function: &mut Function,
    cfg: &mut CFG,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>)  {

    if function.has_attribute(FunctionAttribute::External) {
        return;
    }

    let mut insert_positions = vec![];

    for (pos, adjacencies) in cfg.adjacency_list.iter().enumerate() {
        if adjacencies.contains(&Adj::End) {

            let bb = &cfg.basic_blocks[pos];
            match function.statements[bb.end-1] {
               Statement::Return(_) => (), // OK
               _ => {
                   if function.function_info.return_type != Type::Void {
                       error_reporter.borrow_mut().report_error(
                           ReportKind::DataFlowError,
                           function.function_info.node_info.clone(),
                           format!("Function '{}': Not all control flow paths return a value", function.function_info.name)
                       )
                   } else {

                       let end = cfg.basic_blocks[pos].end;
                       insert_positions.push(end);
                   }
               }
            }
        }
    }

    for pos in insert_positions.iter() {
        cfg.insert_statement(function, *pos, Statement::Return(None));
    }
}

