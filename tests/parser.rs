extern crate compiler;

use compiler::lexer::Lexer;
use compiler::parser::Parser;
use compiler::ast::AstNode;
use compiler::ast::AstType;


fn parse_file(file: &str) -> Result<AstNode, String> {
  let lexer = Lexer::new(file);
  let mut parser = Parser::new(lexer);
  parser.parse()
}

fn is_plus_node(node: &AstNode) -> bool {
    match node.node_type {
        AstType::Plus(_) => true,
        _ => false,
    }
}

fn is_minus_node(node: &AstNode) -> bool {
    match node.node_type {
        AstType::Minus(_) => true,
        _ => false,
    }
}

fn is_equals_node(node: &AstNode) -> bool {
    match node.node_type {
        AstType::Equals => true,
        _ => false,
    }
}

fn is_multiply_node(node: &AstNode) -> bool {
    match node.node_type {
        AstType::Multiply(_) => true,
        _ => false,
    }
}

fn is_divide_node(node: &AstNode) -> bool {
    match node.node_type {
        AstType::Divide(_) => true,
        _ => false,
    }
}

fn is_integer_node(node: &AstNode, expected: i32) -> bool {
    match node.node_type {
        AstType::Integer(actual) => actual == expected,
        _ => false,
    }
}

fn is_identifier_node(node: &AstNode, expected: &str) -> bool {
    match node.node_type {
        AstType::Identifier(ref info) => info.name == expected,
        _ => false,
    }
}

#[test]
fn order_of_operation() {
    let ast_node = parse_file("./tests/files/parser/order_of_operation.txt").unwrap();
    // block -> function -> block -> children
    let children = ast_node.get_children()[0].get_children()[0].get_children(); 
    assert_eq!(11, children.len());
    

    check_first_node(&children[0]);
    check_second_node(&children[1]);
    check_third_node(&children[2]);
    check_fourth_node(&children[3]);
    check_fifth_node(&children[4]);
    check_sixth_node(&children[5]);
    check_seventh_node(&children[6]);
    check_eigth_node(&children[7]);
    check_ninth_node(&children[8]);
    check_tenth_node(&children[9]);
    check_eleventh_node(&children[10]);

}

fn check_first_node(node: &AstNode) {
    // assert let a : int = 4 + 2*3; produces correct ast
    /*
                +
               / \              
              4   *
                 / \
                2   3 
    */
    let children = node.get_children();

    assert!(is_plus_node(&children[0]));
    assert!(is_integer_node(&children[0].get_children()[0], 4));
    assert!(is_multiply_node(&children[0].get_children()[1]));
    assert!(is_integer_node(&children[0].get_children()[1].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[1].get_children()[1], 3));
}


fn check_second_node(node: &AstNode) {
    // assert let b : int = 4 - 2*3; produces correct ast
    /*
                -
               / \              
              4   *
                 / \
                2   3 
    */
    let children = node.get_children();

    assert!(is_minus_node(&children[0]));
    assert!(is_integer_node(&children[0].get_children()[0], 4));
    assert!(is_multiply_node(&children[0].get_children()[1]));
    assert!(is_integer_node(&children[0].get_children()[1].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[1].get_children()[1], 3));
}


fn check_third_node(node: &AstNode) {
    // assert let c : int = 4 - 2 + 3 produces correct ast
    /*
                +
               / \              
              -   3
             / \    
            4  2      
    */
    let children = node.get_children();

    assert!(is_plus_node(&children[0]));
    assert!(is_minus_node(&children[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0], 4));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[1], 2));
    assert!(is_integer_node(&children[0].get_children()[1], 3));
}


fn check_fourth_node(node: &AstNode) {
    // assert let d : int = 4 + 2 - 3 produces correct ast
    /*
                -
               / \              
              +   3
             / \    
            4  2      
    */
    let children = node.get_children();

    assert!(is_minus_node(&children[0]));
    assert!(is_plus_node(&children[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0], 4));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[1], 2));
    assert!(is_integer_node(&children[0].get_children()[1], 3));
}

fn check_fifth_node(node: &AstNode) {
    // assert let e : int = 2*3 + 4; produces correct ast
    /*
                -
               / \              
              *   4
             / \    
            2  3      
    */
    let children = node.get_children();

    assert!(is_plus_node(&children[0]));
    assert!(is_multiply_node(&children[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[1], 3));
    assert!(is_integer_node(&children[0].get_children()[1], 4));
}

fn check_sixth_node(node: &AstNode) {
    // assert let f : int = 2/3 - 4; produces correct ast
    /*
                -
               / \              
              ÷   4
             / \    
            2  3      
    */
    let children = node.get_children();

    assert!(is_minus_node(&children[0]));
    assert!(is_divide_node(&children[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[1], 3));
    assert!(is_integer_node(&children[0].get_children()[1], 4));
}

fn check_seventh_node(node: &AstNode) {
    // assert let g : int = 2*3/4; produces correct ast
    /*
                /
               / \              
              *   4
             / \    
            2  3      
    */
    let children = node.get_children();

    assert!(is_divide_node(&children[0]));
    assert!(is_multiply_node(&children[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[1], 3));
    assert!(is_integer_node(&children[0].get_children()[1], 4));
}

fn check_eigth_node(node: &AstNode) {
    // assert  let h : int = 2/3 * 4; produces correct ast
    /*
                *
               / \              
              /   4
             / \    
            2  3      
    */
    let children = node.get_children();

    assert!(is_multiply_node(&children[0]));
    assert!(is_divide_node(&children[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[1], 3));
    assert!(is_integer_node(&children[0].get_children()[1], 4));
}

fn check_ninth_node(node: &AstNode) {
    // assert let i : bool = a == b; produces correct ast
    /*
                ==
               / \              
              a   b   
    */
    let children = node.get_children();

    assert!(is_equals_node(&children[0]));
    assert!(is_identifier_node(&children[0].get_children()[0], "x"));
    assert!(is_identifier_node(&children[0].get_children()[1], "y"));    
}

fn check_tenth_node(node: &AstNode) {
    // assert let j : bool = a == b == c; produces correct ast
    /*
                ==
               / \              
              ==   z
             / \     
            a   b      
    */
    let children = node.get_children();   

    assert!(is_equals_node(&children[0]));
    assert!(is_equals_node(&children[0].get_children()[0]));
    assert!(is_identifier_node(&children[0].get_children()[0].get_children()[0], "x"));
    assert!(is_identifier_node(&children[0].get_children()[0].get_children()[1], "y"));
    assert!(is_identifier_node(&children[0].get_children()[1], "z"));
}

fn check_eleventh_node(node: &AstNode) {
    // assert let k : bool = 2*3+4 == 4-3; produces correct ast
    /*
                ==
               / \              
              +   \
             / \   \  
            *   4   \
           / \       -
          2   3     / \
                   4   3
    */    

    let children = node.get_children();  
    assert!(is_equals_node(&children[0]));
    assert!(is_plus_node(&children[0].get_children()[0]));
    assert!(is_multiply_node(&children[0].get_children()[0].get_children()[0]));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0].get_children()[0], 2));
    assert!(is_integer_node(&children[0].get_children()[0].get_children()[0].get_children()[1], 3));

    assert!(is_minus_node(&children[0].get_children()[1]));
    assert!(is_integer_node(&children[0].get_children()[1].get_children()[0], 4));
    assert!(is_integer_node(&children[0].get_children()[1].get_children()[1], 3));
}
