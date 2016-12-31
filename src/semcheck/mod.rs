use ast::AstNode;
use ast::AstType;
use ast::IdentifierInfo;

use symbol_table::SymbolTable;
use symbol_table::Symbol;
use symbol_table::SymbolType;
use symbol_table::VariableInfo;

use error_reporter::Error;
use error_reporter::ErrorReporter;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;


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
    pub errors: u32,
    symbol_table: SymbolTable,
    error_reporter: Box<ErrorReporter>,
    id_counter: u32,
}


impl SemanticsCheck {
    pub fn new(reporter: Box<ErrorReporter>) -> SemanticsCheck {
        SemanticsCheck {
            errors: 0,
            symbol_table: SymbolTable::new(),
            error_reporter: reporter,
            id_counter: 0,
        }
    }

    pub fn get_id_counter(&self) -> u32 {
        self.id_counter
    }

    pub fn check_semantics(&mut self, node: &mut AstNode) -> u32 {
        self.do_check(node);
        self.errors
    }

    fn do_check(&mut self, node: &mut AstNode) {
        match node.node_type {
            AstType::Block(_) => self.handle_block_node(node),
            AstType::Function(_) =>self.handle_function_node(node),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration_node(node),
            AstType::VariableAssignment(_) => self.handle_variable_assignment_node(node),
            AstType::Return => self.handle_return_statement_node(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) =>
                self.handle_arithmetic_node(node),
            AstType::Integer(_) | AstType::Double(_) |
            AstType::Float(_) | AstType::Text(_) => { assert!(node.get_children().len() == 0); },
            AstType::Identifier(_) => self.handle_identifier_node(node),
            AstType::Equals => self.handle_equals_node(node),
            AstType::If => self.handle_if_node(node),
            AstType::ElseIf => self.handle_else_if_node(node),
            AstType::Else => self.handle_else_node(node),
        }
    }

    fn handle_block_node(&mut self, node: &mut AstNode) {
        self.symbol_table.push_empty();
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }


        if let AstType::Block(ref mut table_entry) = node.node_type {
            *table_entry = self.symbol_table.pop();
        } else {
            panic!("Internal compiler error: Expected Block node but was {:?}", node.node_type);
        }
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
                /*self.report_error(Error::NameError, node.line, node.column,
                    format!("Redefinition of function '{}'. Previously defined at {}:{}",
                    info.name, symbol.line, symbol.column));*/
                    panic!("Not implemented");
            },
            None => {
                self.symbol_table.add_symbol(
                    Symbol::new(info.name.clone(), node.line, node.column, node.length,
                        SymbolType::Function(info.clone())));
            },
        }

        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }
    }

    fn handle_variable_declaration_node(&mut self, node: &mut AstNode) {
        // borrow checker workarounds
        let info = {
            let mut info = {
                match node.node_type {
                AstType::VariableDeclaration(ref mut i) => i,
                _ => panic!("Internal compiler error: Expected variable declaration ast node but was {}",
                        node),
                }
            };
            info.id = self.id_counter;
            self.id_counter += 1;
            info.clone() // need to end mutable borrow here, but still need the values
        };

        match self.symbol_table.find_symbol(&info.name) {
            Some(symbol) => {
                let err_text = match symbol.symbol_type {
                    SymbolType::Function(_) => format!("Variable '{}' shadows function declared at {}:{}",
                        info.name, symbol.line, symbol.column),
                    SymbolType::Variable(_) => format!("Redefinition of variable '{}'. Previously defined at {}:{}",
                        info.name, symbol.line, symbol.column),
                };

                /*self.report_error(Error::NameError, node.line, node.column, err_text); */ panic!("Not implemented");
            },
            None => {
                self.symbol_table.add_symbol(
                    Symbol::new(info.name.clone(), node.line, node.column, node.length,
                        SymbolType::Variable(VariableInfo::new(info.variable_type, info.id))));
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
            /*self.report_error(Error::TypeError, node.line, node.column, format!(
                "Expected '{}' but got '{}",
                types[0], info.variable_type));*/
                panic!("Not implemented");
        }

    }

    fn handle_variable_assignment_node(&mut self, node: &mut AstNode) {
       
        if node.get_children().len() != 1 {
            panic!("Internal compiler error: Invalid child count for assignment node: Expected 1 but was {}", node.get_children().len());
        }

        let info = {
            match node.node_type {
                AstType::VariableAssignment(ref i) => i.clone(),
                _ => panic!("Internal compiler error: Expected variable assignment ast node but was {}",
                        node),
            }
        };

        let id = self.check_variable_is_declared_and_get_id(node, &info);
        SemanticsCheck::update_identifier_id(node, id);


        let child = &mut node.get_mutable_children()[0];
        self.do_check(child);
        
        let mut types = vec![];
        types.push(self.get_type(child));
    
        // do type check only for a declared variable     
        if let Some(symbol) = self.symbol_table.find_symbol(&info.name) {
            if let SymbolType::Variable(variable_info) = symbol.symbol_type { 
                types.push(variable_info.variable_type);
                let widened_type = SemanticsCheck::get_widening_conversion(&types);

                // if type is invalid, errors has already been reported
                if variable_info.variable_type != widened_type && types[0] != Type::Invalid {
                    self.report_error(Error::TypeError, child.line, child.column, child.length, format!(
                        "Expected '{}' but got '{}'",
                         variable_info.variable_type, types[0]));

                    self.report_error(Error::Note, symbol.line, symbol.column, symbol.length, format!(
                        "Variable '{}' declared here ", info.name));
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
            /*self.report_error(Error::TypeError, node.line, node.column, format!(
                "Cannot return {} from a function '{}' with return type of {}",
                types[0], info.name, info.return_type));*/
                panic!("Not implemented");
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
            self.report_error(Error::TypeError, node.line, node.column, node.length, format!(
                        "Not a valid operation for {} and {}", types[0], types[1]));
        } else if info.node_type == Type::String {
            match node.node_type {
                AstType::Plus(_) => { /* Ok */},
                _ => /*self.report_error(Error::TypeError, node.line, node.column, 
                    format!("Operator '{}' is not a valid operator for strings", node))*/ panic!("Not implemented"),
            }
        }
    }

    fn handle_identifier_node(&mut self, node: &mut AstNode) {
        assert!(node.get_children().len() == 0);
  
        let info = {
            match node.node_type {
                AstType::Identifier(ref i) => i.clone(),
                _ => panic!("Internal compiler error: Expected identifier node but was {}",
                        node),
            }
        };
        let id = self.check_variable_is_declared_and_get_id(node, &info);
        SemanticsCheck::update_identifier_id(node, id);
    }

    fn handle_equals_node(&mut self, node: &mut AstNode) {
        let mut types = vec![];

        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
            types.push(self.get_type(child));
        }
        let widened_type = SemanticsCheck::get_widening_conversion(&types);
     
        // if types contains Type::Invalid, error has been reported already
        if widened_type == Type::Invalid && !types.contains(&Type::Invalid) {
            /*self.report_error(Error::TypeError, node.line, node.column, format!(
                "Cannot convert between {} and {}", types[0], types[1]))0;*/ panic!("Not implemented");
        } 
    }

    fn handle_if_node(&mut self, node: &mut AstNode) {
       
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }        

        let children = node.get_children();
        assert!(children.len() >= 2);
        
        let condition_expression_type = self.get_type(&children[0]);

        if condition_expression_type != Type::Boolean {
            /*self.report_error(Error::TypeError, children[0].line, children[0].column, 
                format!("{} expression expected but was {} instead", 
                    Type::Boolean, condition_expression_type));*/ panic!("Not implemented");
        }        
    }

    fn handle_else_if_node(&mut self, node: &mut AstNode) {
        self.handle_if_node(node)
    }

    fn handle_else_node(&mut self, node: &mut AstNode) {
        for ref mut child in node.get_mutable_children() {
            self.do_check(child);
        }
    }

    fn check_variable_is_declared_and_get_id(&mut self, node: &mut AstNode, info: &IdentifierInfo) -> u32 {


         match self.symbol_table.find_symbol(&info.name) {
            Some(symbol) => {
                match symbol.symbol_type {
                    SymbolType::Function(_)=>/* self.report_error(Error::TypeError, node.line, node.column,
                        format!("Usage of function '{}' as a variable", info.name))*/ panic!("Not implemented"),
                    SymbolType::Variable(symbol_info) => { 
                       return symbol_info.id;
                    }

                };
            },
            None => {
                /*self.report_error(Error::NameError, node.line, node.column, format!("Undeclared identifier '{}'",
                    info.name));*/ panic!("Not implemented");
            },
        }
        0
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
                            SymbolType::Variable(x) => x.variable_type,
                            _ => Type::Invalid,
                        }
                    },
                    None => Type::Invalid,
                }
            },
            AstType::Float(_) => Type::Float,
            AstType::Text(_) => Type::String,
            AstType::Equals => Type::Boolean,
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

    fn update_identifier_id(node: &mut AstNode, id: u32) {
        match node.node_type {
            AstType::Identifier(ref mut i) => i.id = id,
            AstType::VariableAssignment(ref mut i) => i.id = id,
            _ => panic!("Internal compiler error: Expected identifier node but was {}",
                    node),
        }
    }

    fn report_error(&mut self, error_type: Error, line:i32, column:i32, token_length : i32, error: String) {
        self.errors += 1;
        self.error_reporter.report_error(error_type, line, column, token_length, error);        
    }
}



#[test]
fn can_assign_integer() {
    
}