extern crate compiler;
extern crate argparse;

use compiler::lexer::ReadLexer;
use compiler::parser::Parser;
use compiler::ast::AstNode;
use compiler::semcheck::SemanticsCheck;
use compiler::tac_generator::TACGenerator;
use compiler::tac_generator::Function;
use compiler::ssa_generator::convert_to_ssa;
use compiler::ssa_generator::destroy_ssa;

use compiler::byte_generator::ByteGenerator;
use compiler::code_generator::CodeGenerator;

use compiler::cfg::generate_cfg;
use compiler::cfg::CFG;

use compiler::optimizer::optimize;

use compiler::obj_generator;
use compiler::obj_generator::ObjectType;
use compiler::obj_generator::Architecture;

use compiler::error_reporter::FileErrorReporter;

use std::fs::File;
use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;

use argparse::{ArgumentParser, StoreTrue, Store };

#[cfg(not(test))]
fn main() {

    let mut optimize = false;
    let mut output = "a.out".to_string();
    let mut input : String = "".to_string(); // one file for now

    { // lifetimes
        let mut argparse = ArgumentParser::new();
        argparse.set_description("Yet another toy compiler project");
        argparse.refer(&mut optimize)
            .add_option(&["-O", "--optimize"], StoreTrue, "Optimize the code");
        argparse.refer(&mut output)
            .add_option(&["-o", "--output"], Store, "Output file name");

        argparse.refer(&mut input)
            .add_argument("Input file", Store, "Input file name")
            .required();

        argparse.parse_args_or_exit();
    }

    let opt_functions = run_frontend(input);

    if let Some(mut functions) = opt_functions {
        if optimize {
           functions = run_middleend(functions);
        }
        run_backend(output, functions);
    }
}


fn run_frontend(
    file_name: String) -> Option<Vec<Function>> {

    let error_reporter = Rc::new(
        RefCell::new(FileErrorReporter::new(&file_name)));


    let mut node = parse_code(
        &file_name,
        error_reporter.clone());

    node.print();
    println!();

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

    let tac_gen = TACGenerator::new(checker.get_current_id());
    let tac_functions = tac_gen.generate_tac_functions(&mut node);
    print_tac(&tac_functions);
    Some(tac_functions)
}

fn parse_code(
    file_name: &String,
    error_reporter: Rc<RefCell<FileErrorReporter>>) -> AstNode {

    let file = File::open(file_name).unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));

    let lexer = Box::new(
        ReadLexer::new(Box::new(file),
            error_reporter.clone()));

    let mut parser = Parser::new(lexer, error_reporter);
    parser.parse()
}

fn print_tac(tac_functions: &Vec<Function>) {
    let mut counter = 1;
    for f in tac_functions.iter() {
        println!("Function '{}'\n", f.name);
        for s in &f.statements {
            println!("    {}: {}", counter, s);
            counter += 1;
        }
        println!("");
    }
    println!("");
}

fn print_cfg(cfg: &HashMap<Rc<String>, CFG>, functions: &Vec<Function>) {

    for f in functions {
        let mut counter = 1;
        println!("Function {}", f.name);
        for bb in cfg[&f.name].basic_blocks.iter() {
            println!("<BB {}>", counter);
            for i in bb.start..bb.end {
               println!("    {}", f.statements[i])
            }
            counter += 1;
        }

        println!("adjacency:\n");

        for i in 0..cfg[&f.name].basic_blocks.len()  {
            let mut adj_str = cfg[&f.name].adjacency_list[i].
                iter().
                fold(String::new(), |acc, ref val| format!("{}, {}", val, acc));

            adj_str.pop(); adj_str.pop(); // remove last comma + space
            if adj_str.is_empty() {
                adj_str = "<None>".to_string();
            }
            println!("{}: {}", i+1, adj_str);

        }
        println!("\nDominance frontiers\n");
        for i in 0..cfg[&f.name].dominance_frontier.len() {
            println!("{}: {:?}", i+1, cfg[&f.name].dominance_frontier[i].iter().map(|v| v + 1).collect::<Vec<usize>>());
        }


        println!("\n");
    }
}

fn print_bytecode(byte_gen: &ByteGenerator) {
    println!("");
    let mut counter = 1;
    for f in byte_gen.bytecode_functions.iter() {
        println!("Function '{}'\n", f.name);
        for c in &f.code {
            println!("    {}: {:?}", counter, c);
            counter += 1;
        }
    }
    println!("");
}

fn run_middleend(mut functions: Vec<Function>) -> Vec<Function> {

    let mut cfg = generate_cfg(&mut functions);
    print_cfg(&cfg, &functions);
    convert_to_ssa(&mut functions, &mut cfg);
    println!("\nBefore optimizations\n");
    print_tac(&functions);
    optimize(&mut functions, &mut cfg);
    println!("\nAfter optimizations\n");
    print_cfg(&cfg, &functions);
    destroy_ssa(&mut functions, &mut cfg);

    println!("\nAfter SSA destruction\n");
    print_cfg(&cfg, &functions);
    functions
}

fn run_backend(output: String, functions: Vec<Function> ) {
    let mut byte_gen = ByteGenerator::new(functions.clone());
    byte_gen.generate_bytecode();
    print_bytecode(&byte_gen);

    let bytecode = byte_gen.bytecode_functions;

    let code_gen = CodeGenerator::new(bytecode);
    let asm_code = code_gen.generate_code();

    obj_generator::generate_object_file(ObjectType::Elf(Architecture::X64), output, asm_code);
}