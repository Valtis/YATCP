use ast::AstNode;
use ast::AstType;
use ast::ArithmeticInfo;
use ast::IdentifierInfo;
use ast::FunctionInfo;
use ast::DeclarationInfo;


use symbol_table::SymbolTable;
use symbol_table::Symbol;
use symbol_table::SymbolType;

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
    errors: u32,
    symbol_table: SymbolTable,
}


impl SemanticsCheck {
    pub fn new() -> SemanticsCheck {
        SemanticsCheck {
            errors: 0,
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn check_semantics(&mut self, node: &mut AstNode) -> u32 {
        self.do_check(node);
        self.errors
    }

    fn do_check(&mut self, node: &mut AstNode) {
        match node.node_type {
            AstType::Block => self.handle_block_node(node),
            AstType::Function(_) =>self.handle_function_node(node),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration_node(node),
            AstType::VariableAssignment(_) => self.handle_variable_assignment_node(node),
            AstType::Return => self.handle_return_statement_node(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) =>
                self.handle_arithmetic_node(node),
            AstType::Integer(_) | AstType::Double(_) |
            AstType::Float(_) | AstType::Text(_) => { assert!(node.get_children().len() == 0); },
            AstType::Identifier(_) => self.handle_identifier_node(node),
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
                _ => panic!("Internal compiler error: Expected function ast node but was {}",
                        node),
            }
        };

        match self.symbol_table.find_symbol(&info.name) {
            Some(symbol) => {
                self.print_error(node.line, node.column,
                    format!("Name error: Redefinition of function {}. Previously defined at {}:{}",
                    info.name, symbol.line, symbol.column));
            },
            None => {
                self.symbol_table.add_symbol(
                    Symbol::new(info.name.clone(), node.line, node.column,
                        SymbolType::Function(info.clone())));
            },
        }

        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }
    }

    fn handle_variable_declaration_node(&mut self, node: &mut AstNode) {
        let info = {
            match node.node_type {
                AstType::VariableDeclaration(ref i) => i.clone(),
                _ => panic!("Internal compiler error: Expected variable declaration ast node but was {}",
                        node),
            }
        };

        match self.symbol_table.find_symbol(&info.name) {
            Some(symbol) => {
                let err_text = match symbol.symbol_type {
                    SymbolType::Function(_) => format!("Name error: Variable '{}' shadows function declared at {}:{}",
                        info.name, symbol.line, symbol.column),
                    SymbolType::Variable(_) => format!("Name error: Redefinition of variable {}. Previously defined at {}:{}",
                        info.name, symbol.line, symbol.column),
                };

                self.print_error(node.line, node.column, err_text);
            },
            None => {
                self.symbol_table.add_symbol(
                    Symbol::new(info.name.clone(), node.line, node.column,
                        SymbolType::Variable(info.variable_type)));
            },
        }


        let mut types = vec![];
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(self.get_type(child));
        }
        assert!(types.len() == 1);

        types.push(info.variable_type);
        let widened_type = SemanticsCheck::get_widening_conversion(&types);
        // if type is invalid, errors has already been reported
        if info.variable_type != widened_type && types[0] != Type::Invalid {
            self.print_error(node.line, node.column, format!(
                "Type error: Cannot assign {} into variable '{}' with type of {}",
                types[0], info.name, info.variable_type));
        }

    }

    fn handle_variable_assignment_node(&mut self, node: &mut AstNode) {
        
        let info = {
            match node.node_type {
                AstType::VariableAssignment(ref i) => i.clone(),
                _ => panic!("Internal compiler error: Expected variable assignment ast node but was {}",
                        node),
            }
        };

        self.check_variable_is_declared(node, &info);

        let mut types = vec![];
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(self.get_type(child));
        }

        assert!(types.len() == 1);

        // do type check only for a declared variable     
        if let Some(symbol) = self.symbol_table.find_symbol(&info.name) {
            if let SymbolType::Variable(variable_type) = symbol.symbol_type { 
                types.push(variable_type);
                let widened_type = SemanticsCheck::get_widening_conversion(&types);

                // if type is invalid, errors has already been reported
                if variable_type != widened_type && types[0] != Type::Invalid {
                    self.print_error(node.line, node.column, format!(
                        "Type error: Cannot assign {} into variable '{}' with type of {}",
                        types[0], symbol.name, variable_type));
                }
            }
        }
    }

    fn handle_return_statement_node(&mut self, node: &mut AstNode) {
        
        let mut types = vec![];

        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(self.get_type(child));
        }
        
        assert!(types.len() == 1);

        let info = self.symbol_table.get_enclosing_function_info();

        types.push(info.return_type);

        let widened_type = SemanticsCheck::get_widening_conversion(&types);
        // if type is invalid, errors has already been reported
        if info.return_type != widened_type && types[0] != Type::Invalid {
            self.print_error(node.line, node.column, format!(
                "Type error: Cannot return {} from a function '{}' with return type of {}",
                types[0], info.name, info.return_type));
        }


    }

    fn handle_arithmetic_node(&mut self, node: &mut AstNode) {

        let mut types = vec![];

        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(self.get_type(child));
        }

        // borrow checker workarounds
        let info = {
            let mut info = {
                match node.node_type {
                    AstType::Plus(ref mut i) | AstType::Minus(ref mut i) |
                    AstType::Multiply(ref mut i) | AstType::Divide(ref mut i) => i,
                    _ => panic!("Internal compiler error: Expected plus/minus/multipy/divide ast node but was {}",
                            node),
                }
            };
            assert!(types.len() == 2);
            info.node_type = SemanticsCheck::get_widening_conversion(&types);
            info.clone() // need to end mutable borrow here, but still need the values
        };

        // if types contains Type::Invalid, error has been reported already
        if info.node_type == Type::Invalid && !types.contains(&Type::Invalid) {
            self.print_error(node.line, node.column, format!(
                "Type error: Cannot convert between {} and {}", types[0], types[1]));
        }
    }

    fn handle_identifier_node(&mut self, node: &mut AstNode) {
        assert!(node.get_children().len() == 0);
        let info =
            match node.node_type {
                AstType::Identifier(ref i) => i.clone(), // clone to fix mutable\immutable borrow error
                _ => panic!("Internal compiler error: Expected identifier node but was {}",
                        node),
        };
        self.check_variable_is_declared(node, &info);
    }

    fn check_variable_is_declared(&mut self, node: &mut AstNode, info: &IdentifierInfo) {


         match self.symbol_table.find_symbol(&info.name) {
            Some(symbol) => {
                match symbol.symbol_type {
                    SymbolType::Function(_)=> self.print_error(node.line, node.column,
                        format!("Type error: Usage of function '{}' as a variable", info.name)),
                    SymbolType::Variable(_) => { /* ok - do nothing*/}
                };


            },
            None => {
                self.print_error(node.line, node.column, format!("Name error: Undeclared identifier '{}'",
                    info.name));
            },
        }
    }

    fn get_type(&self, node: &AstNode) -> Type {
        match node.node_type {
            AstType::Integer(_) => Type::Integer,
            AstType::Double(_) => Type::Double,
            AstType::Plus(ref i) | AstType::Minus(ref i) |
            AstType::Multiply(ref i) | AstType::Divide(ref i) =>i.node_type,
            AstType::Identifier(ref i) => {
                match self.symbol_table.find_symbol(&i.name) {
                    Some(symbol) => {
                        match symbol.symbol_type {
                            SymbolType::Variable(x) => x,
                            _ => Type::Invalid,
                        }
                    },
                    None => Type::Invalid,
                }
            },
            AstType::Float(_) => Type::Float,
            AstType::Text(_) => Type::String,
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
        self.errors += 1;
        match writeln!(&mut ::std::io::stderr(), "{}:{} {}", line, column, error) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
    }
}
