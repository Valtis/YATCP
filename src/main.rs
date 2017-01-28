extern crate compiler;

use compiler::lexer::ReadLexer;
use compiler::parser::Parser;
use compiler::ast::AstNode;
use compiler::semcheck::SemanticsCheck;
use compiler::tac_generator::TACGenerator;
use compiler::tac_generator::Function;
use compiler::byte_generator::ByteGenerator;
use compiler::byte_generator;
use compiler::code_generator::CodeGenerator;

use compiler::cfg::generate_cfg;
use compiler::cfg::CFG;

use compiler::obj_generator;
use compiler::obj_generator::ObjectType;
use compiler::obj_generator::Architecture;

use compiler::error_reporter::FileErrorReporter;

use std::fs::File;
use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;

#[cfg(not(test))]
fn main() {

    let file_name = "file.txt";
    let opt_bytecode = run_frontend(file_name);

    if let Some(bytecode) = opt_bytecode {
     run_backend(file_name, bytecode);
}
 
}


fn run_frontend(
    file_name: &'static str) -> Option<Vec<byte_generator::Function>> {

    let error_reporter = Rc::new(
        RefCell::new(FileErrorReporter::new(file_name)));

    let mut node = parse_code(file_name, error_reporter.clone());

    node.print();
    println!("");

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

    let mut tac_gen = TACGenerator::new(checker.get_current_id());
    tac_gen.generate_tac(&mut node);
    print_tac(&tac_gen);

    let cfg = generate_cfg(tac_gen.functions());
    print_cfg(&cfg, tac_gen.functions());

    let mut byte_gen = ByteGenerator::new((*tac_gen.functions()).clone());

    byte_gen.generate_bytecode();
    print_bytecode(&byte_gen); 

    Some(byte_gen.bytecode_functions)
}

fn parse_code(
    file_name: &'static str,
    error_reporter: Rc<RefCell<FileErrorReporter>>) -> AstNode {
    
    let file = File::open(file_name).unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));

    let lexer = Box::new(ReadLexer::new(Box::new(file), error_reporter.clone()));
    let mut parser = Parser::new(lexer, error_reporter);
    parser.parse()
}

fn print_tac(tac_gen: &TACGenerator) {
    let mut counter = 1; 
    for f in tac_gen.functions() {
        println!("Function '{}'\n", f.name);
        for s in &f.statements {
            println!("    {}: {}", counter, s);
            counter += 1;
        }
        println!("");
    }
    println!("");
}

fn print_cfg(cfg: &HashMap<String, CFG>, functions: &Vec<Function>) {

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

        println!("Adjancency:\n");

        for i in 0..cfg[&f.name].basic_blocks.len()  {
            let mut adj_str = cfg[&f.name].adjancency_list[&i].
                iter().
                fold(String::new(), |acc, ref val| format!("{}, {}", val, acc));

            adj_str.pop(); adj_str.pop(); // remove last comma + space
            if adj_str.is_empty() {
                adj_str = "<None>".to_string();
            }
            println!("{}: {}", i+1, adj_str);

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

fn run_backend(file_name: &'static str, bytecode: Vec<byte_generator::Function> ) {
    let code_gen = CodeGenerator::new(bytecode);
    let asm_code = code_gen.generate_code();

    obj_generator::generate_object_file(ObjectType::Elf(Architecture::X64), file_name.to_string(), "test.o".to_owned(), asm_code);
}