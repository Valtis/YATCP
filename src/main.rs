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

  let file_name = "file.txt";
  let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(file_name)));
  
  let file = File::open(file_name).unwrap_or_else(|e| panic!("Failed to open file {}: {}", file_name, e));

  let lexer = Box::new(ReadLexer::new(Box::new(file), error_reporter.clone()));
  let mut parser = Parser::new(lexer);
  let mut node = match parser.parse() {
      Ok(node) => node,
      Err(e) => {
        println!("Syntax error: {}", e);
        println!("Terminating compilation");
        return;
      }
  };

  print(&node, 0);
  println!("");

  let mut checker = SemanticsCheck::new(error_reporter);
  checker.check_semantics(&mut node);
  
  if checker.errors > 0 {
    let err = if checker.errors == 1 {
      "error"
    } else {
      "errors"
    };

    println!("Terminating compilation due to {} {}", checker.errors, err);
    return;
  }
  
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

  let code_gen = CodeGenerator::new(byte_gen.bytecode_functions);
  let asm_code = code_gen.generate_code();

  obj_generator::generate_object_file(ObjectType::Elf(Architecture::X64), file_name.to_string(), "test.o".to_owned(), asm_code);
}


fn print(node: &AstNode, intendation: usize) {
   /* let int_str = std::iter::repeat(" ").take(intendation).collect::<String>();
    println!("{}{}", int_str, node);
    for child in node.get_children() {
        print(&child, intendation + 4);
    }*/
}
