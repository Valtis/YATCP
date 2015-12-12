use ast::AstNode;
use ast::AstType;
use symbol_table::SymbolTable;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

macro_rules! println_stderr(
    ($($arg:tt)*) => (
        match writeln!(&mut ::std::io::stderr(), $($arg)* ) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
    )
);


#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Type {
    Integer,
    Double,
    Float,
    String,
    Boolean,
    Void,
    Uninitialized,
    Invalid, // type error occured
}

impl Display for Type {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
        Display::fmt( match *self {
          Type::Integer => "Integer",
          Type::Double => "Double",
          Type::Float => "Float",
          Type::String=> "String",
          Type::Boolean => "Boolean",
          Type::Void => "Void",
          Type::Uninitialized => "Uninitialized",
          Type::Invalid => "Invalid",
      }, formatter)
  }
}


pub struct SemanticsCheck {
    errors: bool,
    symbol_table: SymbolTable,
}


impl SemanticsCheck {
    pub fn new() -> SemanticsCheck {
        SemanticsCheck {
            errors: false,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn check_semantics(&mut self, node: &mut AstNode) -> bool {
        self.do_check(node);
        self.errors
    }

    fn do_check() {

    }

    fn do_check(&mut self, node: &mut AstNode) {
        match node.node_type {
            AstType::Block => {
                self.symbol_table.push();
                for ref mut child in node.get_mutable_children() {
                    self.do_check(child);
                }
                self.symbol_table.pop();
            },

            AstType::Function(ref i) => {
                if let Some(symbol) = self.symbol_table.find_symbol(i.name) {
                    println_stderr!("{}:{} Name error: Redefinition of function {}. {}",
                        "Previously defined at {}:{}", 
                        i.line, i.column, i.name, symbol.line, symbol.column);
                }

              //  symbol_table.add_entry()
            }
/*
            AstType::Double(_) => {
                assert!(types.len() == 0);
                Type::Double
            },
            AstType::Integer(_) => {
                assert!(types.len() == 0);
                Type::Integer
            },
            AstType::Plus(ref mut i) | AstType::Minus(ref mut i) | 
            AstType::Multiply(ref mut i) | AstType::Divide(ref mut i) => {                
                assert!(types.len() == 2);
                if !types.contains(&Type::Invalid) {
                    let t = if types[0] != types[1] {
                        SemanticsCheck::get_common_type_from(&types)
                    } else {
                        types[0]
                    };
                    i.node_type = t;
                    if t == Type::Invalid {
                        self.print_error(format!(
                            "{}:{} Type error: No conversion between {} and {}",
                            i.line, i.column, types[0], types[1]));
                    }
                    t
                } else {
                    Type::Invalid
                }

            },
            AstType::VariableDeclaration(ref i) => {
                assert!(types.len() == 1);
                if types[0] != Type::Invalid {
                    types.push(i.variable_type);
                    if SemanticsCheck::get_common_type_from(&types) != i.variable_type {
                        self.print_error(
                            format!(
                                "{}:{} Type error: Cannot assign expression with type of {} into variable {} with type of {}",
                                i.line, i.column, types[0], i.name, i.variable_type));

                    }
                    i.variable_type
                } else {
                   Type::Invalid
                }
            },
            _ => Type::Uninitialized,*/
            _ => panic!("Unimplemented: {}", node)
        }
    }

    fn get_common_type_from(types: &Vec<Type>) -> Type {
        Type::Invalid
    }


    fn print_error(&mut self, error: String) {
        self.errors = true;
        println!("{}", error);
    }
}