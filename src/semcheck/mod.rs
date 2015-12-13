use ast::AstNode;
use ast::AstType;
use ast::ArithmeticInfo;
use ast::FunctionInfo;
use ast::DeclarationInfo;


use symbol_table::SymbolTable;
use symbol_table::Symbol;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;



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

    fn do_check(&mut self, node: &mut AstNode) {
        match node.node_type {
            AstType::Block => self.handle_block_node(node),
            AstType::Function(_) =>self.handle_function_node(node),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration_node(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) =>              
                self.handle_arithmetic_node(node),
            AstType::Integer(_) | AstType::Double(_) => {/* do nothing */},
            _ => panic!("Unimplemented: {}", node)
        }
    }

    fn handle_block_node(&mut self, node: &mut AstNode) {
        self.symbol_table.push();
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }
        self.symbol_table.pop();
    }

    fn handle_function_node(&mut self, node: &mut AstNode) {
        // work around the borrow checker - otherwise the borrow will live too long
        let info = {
            match node.node_type {
                AstType::Function(ref i) => i.clone(),
                _ => panic!("Internal compiler error: Ctulhu has awoken"),
            }
        };

        if let Some(symbol) = self.symbol_table.find_symbol(&info.name) {
            let i = info.clone(); // borrow checker workaround
            self.print_error(i.line, i.column, 
                format!("Name error: Redefinition of function {}. Previously defined at {}:{}", 
                i.name, symbol.line, symbol.column));                    
        }

        self.symbol_table.add_symbol(
            Symbol::new(info.name.clone(), info.line, info.column));

        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }
    }

    fn handle_variable_declaration_node(&mut self, node: &mut AstNode) {
        let info = {
            match node.node_type {
                AstType::VariableDeclaration(ref i) => i.clone(),
                _ => panic!("Internal compiler error: Ctulhu has awoken"),
            }
        };


        let mut types = vec![];
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(SemanticsCheck::get_type(child));
        }
        assert!(types.len() == 1);

        types.push(info.variable_type);
        let shared_type = SemanticsCheck::get_widening_conversion(&types);
        // if type is invalid, errors has already been reported
        if info.variable_type != shared_type && types[0] != Type::Invalid {
            self.print_error(info.line, info.column, format!(
                "Type error: Cannot assign {} into variable '{}' with type of {}",
                types[0], info.name, info.variable_type));
        }        

    }

    fn handle_arithmetic_node(&mut self, node: &mut AstNode) {

        let mut types = vec![];
        
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(SemanticsCheck::get_type(child));
        }

        // borrow checker workarounds
        let info = {
            let mut info = {
                match node.node_type {
                    AstType::Plus(ref mut i) | AstType::Minus(ref mut i) |
                    AstType::Multiply(ref mut i) | AstType::Divide(ref mut i) => i,
                    _ => panic!("Internal compiler error: Ctulhu has awoken"),
                }
            };
            assert!(types.len() == 2);
            info.node_type = SemanticsCheck::get_widening_conversion(&types);
            info.clone() // need to end mutable borrow here, but still need the values
        };

        // if types contains Type::Invalid, error has been reported already
        if info.node_type == Type::Invalid && !types.contains(&Type::Invalid) {
            self.print_error(info.line, info.column, format!(
                "Type error: Cannot convert between {} and {}", types[0], types[1]));
        } 
    }

    fn get_type(node: &AstNode) -> Type {
        match node.node_type {
            AstType::Integer(_) => Type::Integer,
            AstType::Double(_) => Type::Double,
            AstType::Plus(ref i) | AstType::Minus(ref i) |
            AstType::Multiply(ref i) | AstType::Divide(ref i) =>i.node_type,
            _ => panic!("Internal compiler error: Invalid node type {}", node),
        }
    }

    fn get_widening_conversion(types: &Vec<Type>) -> Type {
        assert!(types.len() == 2);
        if types[0] == types[1] {
            types[0]
        } else {
            if types.contains(&Type::Double) && (types.contains(&Type::Integer) || 
                types.contains(&Type::Float)) {
                Type::Double
            } else {
                Type::Invalid   
            }
        }
    }


    fn print_error(&mut self, line:i32, column:i32, error: String) {
        self.errors = true;
        match writeln!(&mut ::std::io::stderr(), "{}:{} {}", line, column, error) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
    }
}