use crate::lexer::ReadLexer;
use crate::parser::Parser;
use crate::ast::AstNode;
use crate::semcheck::SemanticsCheck;
use crate::tac_generator::{Function, TACGenerator};
use crate::error_reporter::FileErrorReporter;

use std::fs::File;
use std::rc::Rc;
use std::cell::RefCell;


pub fn run_frontend(
    file_name: String,
    print_ast: bool,
    print_tac: bool) -> Option<Vec<Function>> {
    let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(&file_name)));

    let mut node = parse_code(&file_name,error_reporter.clone());

    if print_ast {
        node.print();
        println!();
    }

    let mut checker = SemanticsCheck::new(error_reporter.clone());
    checker.check_semantics(&mut node);

    if error_reporter.borrow().has_errors() {
        error_reporter.borrow().print_errors();
        let err = if error_reporter.borrow().errors() == 1 {
            "error"
        } else {
            "errors"
        };

        println!("Terminating compilation due to {} {}", error_reporter.borrow().errors(), err);
            return None;
    }

    generate_three_address_code(&mut node, checker, print_tac)
}


fn parse_code(file_name: &String, error_reporter: Rc<RefCell<FileErrorReporter>>) -> AstNode {
    let file = File::open(file_name)
        .unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));

    let lexer = Box::new(
        ReadLexer::new(Box::new(file),error_reporter.clone()));

    let mut parser = Parser::new(lexer, error_reporter);
    parser.parse()
}

fn generate_three_address_code(
    mut node: &mut AstNode,
    checker: SemanticsCheck,
    print_tac: bool) -> Option<Vec<Function>> {
    let tac_gen = TACGenerator::new(checker.get_current_id());
    let tac_functions = tac_gen.generate_tac_functions(&mut node);
    if print_tac {
        print_three_address_code(&tac_functions);
    }
    Some(tac_functions)
}

fn print_three_address_code(tac_functions: &Vec<Function>) {
    for f in tac_functions.iter() {
        f.print();
    }
    println!();
}
