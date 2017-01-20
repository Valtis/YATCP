extern crate compiler;

use compiler::lexer::ReadLexer;
use compiler::parser::Parser;
use compiler::ast::AstNode;
use compiler::semcheck::SemanticsCheck;
use compiler::tac_generator::TACGenerator;
use compiler::byte_generator::ByteGenerator;
use compiler::code_generator::CodeGenerator;
use compiler::obj_generator;
use compiler::obj_generator::ObjectType;
use compiler::obj_generator::Architecture;

use compiler::error_reporter::FileErrorReporter;

use std::fs::File;
use std::rc::Rc;
use std::cell::RefCell;

#[cfg(not(test))]
fn main() {

  run_frontend();
  run_backend();
 
}


fn run_frontend() {
  let file_name = "file.txt";
  let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(file_name)));
  
  let file = File::open(file_name).unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));

  let lexer = Box::new(ReadLexer::new(Box::new(file), error_reporter.clone()));
  let mut parser = Parser::new(lexer, error_reporter.clone());
  let mut node = parser.parse();

  node.print();
  println!("");

  let mut checker = SemanticsCheck::new(error_reporter.clone());
  checker.check_semantics(&mut node);

  if error_reporter.borrow().has_errors() {
    let err = if error_reporter.borrow().errors() == 1 {
      "error"
    } else {
      "errors"
    };

    println!("Terminating compilation due to {} {}", error_reporter.borrow().errors(), err);
    return;
  }
  /*
  let mut tac_gen = TACGenerator::new(checker.get_id_counter());
  tac_gen.generate_tac(&mut node);

  println!("");
  let mut counter = 1; 
  for f in tac_gen.functions() {
    println!("Function '{}'\n", f.name);
    for s in &f.statements {
      println!("    {}: {:?}", counter, s);
    }
    println!("");
    counter += 1;
  }

  let mut byte_gen = ByteGenerator::new((*tac_gen.functions()).clone());
  println!("");
  byte_gen.generate_bytecode();




  println!("");
  counter = 1;
  for f in &byte_gen.bytecode_functions {
    println!("Function '{}'\n", f.name);
    for c in &f.code {
      println!("    {}: {:?}", counter, c);
      counter += 1;
    }   
  }

*/
}

fn run_backend() {
 /* let code_gen = CodeGenerator::new(byte_gen.bytecode_functions);
  let asm_code = code_gen.generate_code();

  obj_generator::generate_object_file(ObjectType::Elf(Architecture::X64), file_name.to_string(), "test.o".to_owned(), asm_code);*/
}