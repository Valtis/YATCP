extern crate compiler;

use compiler::lexer::Lexer;
use compiler::parser::Parser;
use compiler::ast::AstNode;
use compiler::semcheck::SemanticsCheck;
use compiler::ssa_generator::SSAGenerator;
use compiler::error_reporter::FileErrorReporter;

#[cfg(not(test))]
fn main() {
  let lexer = Lexer::new("file.txt");
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

  let mut checker = SemanticsCheck::new(
    Box::new(FileErrorReporter::new("file.txt")));
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

  let mut ssa = SSAGenerator::new();
  ssa.generate_ssa(&mut node);

  let mut counter = 1; 
  for s in ssa.statements {
    println!("{}: {}", counter, s);
    counter += 1;
  }
}


fn print(node: &AstNode, intendation: usize) {
    let int_str = std::iter::repeat(" ").take(intendation).collect::<String>();
    println!("{}{}", int_str, node);
    for child in node.get_children() {
        print(&child, intendation + 4);
    }
}
