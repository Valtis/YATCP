extern crate compiler;
extern crate argparse;

use compiler::frontend::run_frontend;
use compiler::middleend::run_middleend;
use compiler::backend::run_backend;
use compiler::error_reporter::file_reporter::FileErrorReporter;

use argparse::{ArgumentParser, StoreTrue, Store, StoreFalse};

use took::Timer;

use std::rc::Rc;
use std::cell::RefCell;

#[cfg(not(test))]
fn main() {

    let mut optimize = false;
    let mut output = "a.out".to_string();
    let mut input= "".to_string(); // one file for now

    let mut print_token= false;
    let mut print_ast= false;
    let mut print_tac= false;
    let mut print_cfg = false;
    let mut print_bytecode = false;
    let mut print_bytecode_after_register_alloc = false;
    let mut print_stack_map = false;
    let mut generate_code = true;
    let mut print_timings = false;

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

        argparse.refer(&mut print_token)
            .add_option(&["--print-token"], StoreTrue, "Print out tokens");

        argparse.refer(&mut print_ast)
            .add_option(&["--print-ast"], StoreTrue, "Print out the syntax tree");

        argparse.refer(&mut print_tac)
            .add_option(&["--print-tac"], StoreTrue, "Print out the three address code representation");

        argparse.refer(&mut print_cfg)
            .add_option(&["--print-cfg"], StoreTrue, "Print out the function control flow graphs.");

        argparse.refer(&mut print_bytecode)
            .add_option(&["--print-bytecode"], StoreTrue, "Print out the byte code representation before register allocation");

        argparse.refer(&mut print_bytecode_after_register_alloc)
            .add_option(&["--print-allocated-bytecode"], StoreTrue, "Print out the byte code representation after register allocation is done");

        argparse.refer(&mut print_stack_map)
            .add_option(&["--print-stack-map"], StoreTrue, "Print out the stack layout for functions");

        argparse.refer(&mut print_timings)
            .add_option(&["--print-timings"], StoreTrue, "Print out how much time each component spent");

        argparse.refer(&mut generate_code)
            .add_option(&["--no-code-generation"], StoreFalse, "Skip code generation phase");
        argparse.parse_args_or_exit();
    }

    if optimize {
        eprintln!("\nOptimizations are currently broken due to changes to TAC generation. Needs to be fixed once TAC stabilizes a bit");
        std::process::exit(1);
    }

    let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(&input)));

    let timer = Timer::new();
    let opt_functions = run_frontend(input, print_token, print_ast, print_tac, print_timings, error_reporter.clone());

    if let Some(functions) = opt_functions {
        if let Some(functions) = run_middleend(functions, optimize, print_tac, print_cfg, print_timings, error_reporter.clone()) {
            if generate_code {
                run_backend(output, functions, print_bytecode, print_bytecode_after_register_alloc, print_stack_map, print_timings);
            }
        } else {
            std::process::exit(1);
        }
    } else {
        std::process::exit(1);
    }

    if print_timings {
        println!("===============================================\nCompilation took {}", timer.took());
    }
}



