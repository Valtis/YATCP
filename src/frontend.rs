use crate::lexer::{ReadLexer, Lexer};
use crate::lexer::token::{TokenType};
use crate::parser::Parser;
use crate::ast::AstNode;
use crate::semcheck::SemanticsCheck;
use crate::tac_generator::{Function, TACGenerator};
use crate::error_reporter::ErrorReporter;

use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use std::cell::RefCell;


pub fn run_frontend(
    file_name: String,
    print_token: bool,
    print_ast: bool,
    print_tac: bool,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>,
    ) -> Option<Vec<Function>> {

    let mut node = parse_code(&file_name,error_reporter.clone(), print_token);

    if print_ast {
        node.print();
        println!();
    }

    let current_id = check_semantics(&mut node, error_reporter.clone());

    let mut reporter = error_reporter.borrow_mut();
    if reporter.has_reports() {
        reporter.print_errors();


        if reporter.has_errors() {
            let err = if reporter.errors() == 1 {
                "error"
            } else {
                "errors"
            };

            let stderr = std::io::stderr();
            let mut handle = stderr.lock();
            writeln!(&mut handle, "Terminating compilation due to {} {}", reporter.errors(), err).unwrap();
                return None;
        }
        reporter.clear_reports();
    }

    generate_three_address_code(&mut node, current_id, print_tac)
}


fn parse_code(
    file_name: &str,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>,
    print_tokens: bool) -> AstNode {

    if print_tokens {
        let file = File::open(file_name)
            .unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));

        let mut lexer = Box::new(
            ReadLexer::new(Box::new(file),error_reporter.clone()));

        while lexer.peek_token().token_type != TokenType::Eof {
            println!("{}", lexer.next_token());
        }
        println!();
    }

    let file = File::open(file_name)
        .unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));
    let lexer = Box::new(
        ReadLexer::new(Box::new(file),error_reporter.clone()));

    let mut parser = Parser::new(lexer, error_reporter);
    parser.parse()
}

fn check_semantics(node: &mut AstNode, error_reporter: Rc<RefCell<dyn ErrorReporter>>) -> u32 {
    let mut checker = SemanticsCheck::new(error_reporter);
    checker.check_semantics(node);
    checker.get_current_id()
}

fn generate_three_address_code(
    mut node: &mut AstNode,
    current_id: u32,
    print_tac: bool) -> Option<Vec<Function>> {
    let tac_gen = TACGenerator::new(current_id);
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
