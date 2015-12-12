extern crate compiler;

use compiler::lexer::Lexer;
use compiler::parser::Parser;
use compiler::ast::AstNode;
use compiler::semcheck::SemanticsCheck;
#[cfg(not(test))]
fn main() {
  let lexer = Lexer::new("file.txt");
  let mut parser = Parser::new(lexer);
  let mut node = match parser.parse() {
      Ok(node) => node,
      Err(e) => panic!("Err: {}", e),
  };

  print(&node, 0);

  let mut checker = SemanticsCheck::new();
  checker.check_semantics(&mut node);

}


fn print(node: &AstNode, intendation: usize) {
    let int_str = std::iter::repeat(" ").take(intendation).collect::<String>();
    println!("{}{}", int_str, node);
    for child in node.get_children() {
        print(&child, intendation + 4);
    }
}
