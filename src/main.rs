extern crate compiler;
extern crate argparse;

use compiler::frontend::run_frontend;
use compiler::middleend::run_middleend;
use compiler::backend::run_backend;
use compiler::error_reporter::file_reporter::FileErrorReporter;

use argparse::{ArgumentParser, StoreTrue, Store, StoreFalse};

use std::rc::Rc;
use std::cell::RefCell;

#[cfg(not(test))]
fn main() {

    let mut optimize = false;
    let mut output = "a.out".to_string();
    let mut input= "".to_string(); // one file for now
    let mut print_ast= false;
    let mut print_tac= false;
    let mut print_cfg = false;
    let mut print_bytecode = false;
    let mut generate_code = true;

    {
        let mut argparse = ArgumentParser::new();
        argparse.set_description("Yet another toy compiler project");
        argparse.refer(&mut optimize)
            .add_option(&["-O", "--optimize"], StoreTrue, "Optimize the code");
        argparse.refer(&mut output)
            .add_option(&["-o", "--output"], Store, "Output file name");

        argparse.refer(&mut input)
            .add_argument("file", Store, "Input file name")
            .required();

        argparse.refer(&mut print_ast)
            .add_option(&["--print-ast"], StoreTrue, "Print out the syntax tree");

        argparse.refer(&mut print_tac)
            .add_option(&["--print-tac"], StoreTrue, "Print out the three address code representation");

        argparse.refer(&mut print_cfg)
            .add_option(&["--print-cfg"], StoreTrue, "Print out the function control flow graphs. Requires -O");

        argparse.refer(&mut print_bytecode)
            .add_option(&["--print-bytecode"], StoreTrue, "Print out the byte code representation");

        argparse.refer(&mut generate_code)
            .add_option(&["--no-code-generation"], StoreFalse, "Skip code generation phase");
        argparse.parse_args_or_exit();
    }

    let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(&input)));
    let opt_functions = run_frontend(input, print_ast, print_tac, error_reporter.clone());

    if let Some(functions) = opt_functions {
        if let Some(functions) = run_middleend(functions, optimize, print_tac, print_cfg, error_reporter.clone()) {
            if generate_code {
                run_backend(output, functions, print_bytecode);
            }
        }
    }
}



