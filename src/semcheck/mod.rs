use ast::AstNode;
use ast::ArithmeticInfo;
use ast::FunctionInfo;
use ast::NodeInfo;
use ast::DeclarationInfo;

use symbol_table::SymbolTable;
use symbol_table::Symbol;
use symbol_table::TableEntry;

use error_reporter::Error;
use error_reporter::ErrorReporter;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;
use std::cell::RefCell;

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
    error_reporter: Rc<RefCell<ErrorReporter>>,
    id_counter: u32,
}

impl SemanticsCheck {
    pub fn new(reporter: Rc<RefCell<ErrorReporter>>) -> SemanticsCheck {
        SemanticsCheck {
            errors: 0,
            symbol_table: SymbolTable::new(),
            error_reporter: reporter,
            id_counter: 0,
        }
    }

    pub fn get_current_id(&self) -> u32 {
        self.id_counter
    }

    fn get_next_id(&mut self) -> u32 {
        let id = self.id_counter;
        self.id_counter += 1;
        id
    }

    pub fn check_semantics(&mut self, node: &mut AstNode) -> u32 {
        self.do_check(node);
        self.errors
    }

    fn do_check(&mut self, node: &mut AstNode) {
        match *node {
            AstNode::Block(ref mut children, ref mut tab_ent, ref ni) => 
                self.handle_block(children, tab_ent, ni),
            AstNode::Function(ref mut child, ref fi) => 
                self.handle_function(child, fi),
            AstNode::VariableDeclaration(ref mut child, ref vi) => 
                self.handle_variable_declaration(child, vi),
            AstNode::VariableAssignment(ref mut child, ref name, ref ni) => 
                self.handle_variable_assignment(child, name, ni), 
            AstNode::Plus(_, _, _) |
            AstNode::Minus(_, _, _) |
            AstNode::Multiply(_, _, _) |
            AstNode::Divide(_, _, _) => {
                self.handle_arithmetic_operation_with_operator_type_check(node);
            },
            AstNode::Negate(ref mut child, ref mut ai) => 
                self.handle_negation(child, ai),
            AstNode::Return(ref mut child, ref mut ai) =>
                self.handle_return(child, ai),
            AstNode::Integer(_, _) => {},
            AstNode::Float(_, _) => {},
            AstNode::Double(_, _) => {},
            AstNode::Text(_, _) => {},
            AstNode::Identifier(ref name, ref info) => 
                { self.check_identifier_is_initialized(name, info); }
            AstNode::Boolean(_, _) => {},
            AstNode::ErrorNode => {},
        }
    }

    fn handle_block(
        &mut self, children: 
        &mut Vec<AstNode>, 
        tab_ent: &mut Option<TableEntry>, 
        _node_info: &NodeInfo) {

        self.symbol_table.push_empty();
        for ref mut child in children {
            self.do_check(child);
        }
       
        *tab_ent = self.symbol_table.pop();
    }

    fn handle_function(
        &mut self, 
        child: &mut AstNode, 
        function_info: &FunctionInfo) {
    
        match self.symbol_table.find_symbol(&function_info.name) {
            Some(symbol) => {
                let (prev_line, prev_column, prev_length) = match symbol {
                    Symbol::Function(fi) => 
                        (fi.node_info.line,
                         fi.node_info.column,
                         fi.node_info.length),
                    _ => unimplemented!(),
                };

                self.report_error(
                    Error::NameError, 
                    function_info.node_info.line, 
                    function_info.node_info.column,
                    function_info.node_info.length,
                    format!("Redefinition of function '{}'",
                        function_info.name));
                    
                self.report_error(
                    Error::Note, 
                    prev_line,
                    prev_column,
                    prev_length,
                    "Previously declared here".to_string());
            },
            None => {
                self.symbol_table.add_symbol(
                    Symbol::Function(function_info.clone()));
            },
        }
        self.do_check(child);
    }

    fn handle_variable_declaration(
        &mut self, 
        child: &mut AstNode, 
        variable_info: &DeclarationInfo) {

        match self.symbol_table.find_symbol(&variable_info.name) {
            Some(symbol) => {
                let (err_text, prev_line, prev_column, prev_length) = 
                    match symbol {
                    Symbol::Function(fi) => 
                        (format!(
                            "Variable '{}' previously declared as a function", 
                            fi.name),
                         fi.node_info.line,
                         fi.node_info.column,
                         fi.node_info.length,
                        ),
                    Symbol::Variable(vi, _) => 
                        (format!("Redefinition of variable '{}'",
                            vi.name),
                         vi.node_info.line,
                         vi.node_info.column,
                         vi.node_info.length
                        ),
                };

                self.report_error(
                    Error::NameError, 
                    variable_info.node_info.line, 
                    variable_info.node_info.column,
                    variable_info.node_info.length, 
                    err_text);

                self.report_error(
                    Error::Note,
                    prev_line,
                    prev_column,
                    prev_length,
                    "Previously declared here".to_string()); 
            },
            None => {
                let id = self.get_next_id();
                self.symbol_table.add_symbol(
                    Symbol::Variable(variable_info.clone(), id));
            },
        }

        self.do_check(child);        
        let child_type = self.get_type(child);

        // if type is invalid, errors has already been reported
        if variable_info.variable_type != child_type && 
            child_type != Type::Invalid {
            self.report_type_error(variable_info, child, child_type);
        }

    }

    fn handle_variable_assignment(
        &mut self, 
        child: &mut AstNode, 
        name: &String,
        node_info: &NodeInfo
        ) {

        self.do_check(child);
        let child_type = self.get_type(child);
        
        let opt_symbol = self.check_identifier_is_initialized(name, node_info);

        let symbol = if let Some(sym) = opt_symbol {
            sym
        } else {
            // no valid variable. This has already been reported, so just return
            return;
        };
    
        // do type check only for a declared variable     
        if let Symbol::Variable(ref sym_info, _) = symbol {
            // if type is invalid, errors has already been reported
            if sym_info.variable_type != child_type && 
                child_type != Type::Invalid  {

            self.report_error(
                Error::TypeError, 
                child.line(), 
                child.column(),   
                child.length(), 
                format!(
                    "Expected '{}' but got '{}'",
                     sym_info.variable_type, child_type));

            self.report_error(
                Error::Note, 
                sym_info.node_info.line, 
                sym_info.node_info.column, 
                sym_info.node_info.length, 
                format!("Variable '{}', declared here, has type {}", 
                    name, 
                    sym_info.variable_type));
            }
        
        } else {
            // should not happen, only variables should be returned
            ice!(
                "Non-variable symbol '{:?}' returned when variable expected", 
                symbol);
        }
    }

    fn handle_return(
        &mut self, 
        opt_child: &mut Option<Box<AstNode>>,
        arith_info: &mut ArithmeticInfo) {
        
        let function_info = self.symbol_table.
            get_enclosing_function_info().
            unwrap_or_else(
                || ice!(
                    "No enclosing function found when handling return node {:?}", opt_child));

        let child = if let Some(ref mut c) = *opt_child {
            c
        } else {
            arith_info.node_type = Type::Void;
            // no return expression -> void type 
            if function_info.return_type != Type::Void {
                self.report_error(
                    Error::TypeError,
                    arith_info.node_info.line,
                    arith_info.node_info.column,
                    arith_info.node_info.length,
                    "Return statement without expression in non-void function".
                        to_string());

                self.report_error(
                    Error::Note,
                    function_info.node_info.line,
                    function_info.node_info.column,
                    function_info.node_info.length,
                    format!("Function '{}', declared here, is expected to return '{}'",
                        function_info.name,
                        function_info.return_type));
                arith_info.node_type = Type::Invalid;
            }
            return;            
        };

        self.do_check(child);
        let child_type = self.get_type(child);
       
        arith_info.node_type = child_type;
        // if type is invalid, errors has already been reported
        if function_info.return_type != child_type && child_type != Type::Invalid {
            
            let (err_str, note_str) = if function_info.return_type == Type::Void {
                (format!("Return statement has type '{}' in void function",
                    child_type), 
                format!("Function '{}', declared here, has return type '{}' and is not expected to return a value",
                    function_info.name,
                    function_info.return_type))
            }   
            else {
                (format!("Return statement has type '{}' when '{}' was expected",
                    child_type, function_info.return_type),
                format!("Function '{}', declared here, is expected to return '{}'",
                    function_info.name,
                    function_info.return_type)
                )
            };
            self.report_error(
                Error::TypeError,
                arith_info.node_info.line,
                arith_info.node_info.column,
                arith_info.node_info.length,
                err_str);

            self.report_error(
                Error::Note,
                function_info.node_info.line,
                function_info.node_info.column,
                function_info.node_info.length,
                note_str);

            arith_info.node_type == Type::Invalid;
        }
    }


    fn handle_arithmetic_operation_with_operator_type_check(
        &mut self, 
        node: &mut AstNode) {

        let (ref valid_types, ref mut ai) = match *node {
            AstNode::Plus(ref mut left, ref mut right, ref mut ai) => {
                self.handle_arithmetic_node(left, right, ai);
                (vec![
                    Type::Integer, 
                    Type::Float, 
                    Type::Double, 
                    Type::String, 
                    Type::Invalid], 
                 ai)
            },
            AstNode::Minus(ref mut left, ref mut right, ref mut ai) |
            AstNode::Multiply(ref mut left, ref mut right, ref mut ai) |
            AstNode::Divide(ref mut left, ref mut right, ref mut ai) => {
                self.handle_arithmetic_node(left, right, ai);
                (vec![
                    Type::Integer, 
                    Type::Float, 
                    Type::Double, 
                    Type::Invalid], 
                 ai)
            },
            _ => ice!(
                "Incorrect node passed to arithmetic node type checking: {}", 
                node)
        };

        if !valid_types.iter().any(|t| *t == ai.node_type) {
            self.report_error(
                Error::TypeError,
                ai.node_info.line,
                ai.node_info.column,
                ai.node_info.length,
                format!("Operands of type '{}' are not valid for this operator",
                    ai.node_type));
            ai.node_type = Type::Invalid;
        }
    }

    fn handle_arithmetic_node(
        &mut self, 
        left_child: &mut AstNode,
        right_child: &mut AstNode,
        arith_info: &mut ArithmeticInfo) {

        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);

        // if left or right type is Type::Invalid, error has been reported 
        // already. Just mark this node as invalid as well to propagate the 
        // error upwards in the tree
        if left_type == Type::Invalid || right_type == Type::Invalid {
            arith_info.node_type = Type::Invalid;    
        } else if left_type != right_type {
            arith_info.node_type = Type::Invalid;  
            self.report_error(
                Error::TypeError, 
                arith_info.node_info.line, 
                arith_info.node_info.column, 
                arith_info.node_info.length, 
                format!(
                    "Incompatible operand types '{}' and '{}' for this operation", left_type, right_type));       
        } else {
            arith_info.node_type = left_type;
        }
    }

    fn handle_negation(
        &mut self, 
        child: &mut AstNode,
        arith_info: &mut ArithmeticInfo) {

        self.do_check(child);

        let child_type = self.get_type(child);

        // if type has type invalid, do not report, just set this node to invalid
        // if type has non-arithmetic type, report it and set type to invalid
        // otherwise just set the type to the type of the child

        let valid_types = vec![Type::Integer, Type::Float, Type::Double];

        if child_type == Type::Invalid {
            arith_info.node_type = Type::Invalid;    
        } else if !valid_types.iter().any(|t| *t == child_type) {
            arith_info.node_type = Type::Invalid;  
            self.report_error(
                Error::TypeError, 
                arith_info.node_info.line, 
                arith_info.node_info.column, 
                arith_info.node_info.length, 
                format!(
                    "Cannot negate operand with type '{}'", child_type));       
        } else {
            arith_info.node_type = child_type;
        }
    }
    
    fn check_identifier_is_initialized(&mut self, name: &String, info: &NodeInfo) ->
        Option<Symbol> {
        match self.symbol_table.find_symbol(name) {
            Some(symbol) => {
                match symbol {
                    Symbol::Function(function_info) => {
                        self.report_error(
                            Error::TypeError, 
                            info.line, 
                            info.column,
                            info.length,
                            format!(
                                "Usage of function '{}' as a variable", 
                                name));

                        self.report_error(
                            Error::Note,
                            function_info.node_info.line,
                            function_info.node_info.column,
                            function_info.node_info.length,
                            "Function declared here:".to_string());
                    }
                    Symbol::Variable(_, _) => { return Some(symbol.clone()); }

                };
            },
            None => {
                self.report_error(
                    Error::NameError, 
                    info.line, 
                    info.column, 
                    info.length,
                    format!("Undeclared identifier '{}'",
                        name));
            },
        }
        None
    }

    fn get_type(&self, node: &AstNode) -> Type {
        match *node {
            AstNode::Integer(_, _) => Type::Integer,
            AstNode::Float(_, _) => Type::Float,
            AstNode::Double(_, _) => Type::Double,
            AstNode::Boolean(_, _) => Type::Boolean,
            AstNode::Plus(_, _, ref info) | 
            AstNode::Minus(_, _, ref info) |
            AstNode::Multiply(_, _,  ref info) | 
            AstNode::Divide(_, _, ref info) => info.node_type,
            AstNode::Negate(_, ref info) => info.node_type,
            AstNode::Identifier(ref name, _) => {
                match self.symbol_table.find_symbol(name) {
                    Some(symbol) => {
                        match symbol {
                            Symbol::Variable(info, _) => info.variable_type,
                            _ => Type::Invalid,
                        }
                    },
                    None => Type::Invalid,
                }
            },
            AstNode::Text(_, _) => Type::String,
            AstNode::ErrorNode => Type::Invalid,
            _ => ice!("Invalid node '{}' when resolving node type", node),
        }
    }

    fn report_error(&mut self, error_type: Error, line:i32, column:i32, token_length : i32, error: String) {
        self.errors += 1;
        self.error_reporter.borrow_mut().report_error(error_type, line, column, token_length, error);        
    }

    fn report_type_error(
        &mut self,
        variable_info: &DeclarationInfo,
        actual_node: &AstNode,
        actual_type: Type
        ) {
        self.report_error(
                Error::TypeError, 
                actual_node.line(), 
                actual_node.column(), 
                actual_node.length(),
                format!("Expected '{}' but got '{}'",
                    variable_info.variable_type, actual_type));

            self.report_error(
                Error::Note, 
                variable_info.node_info.line, 
                variable_info.node_info.column, 
                variable_info.node_info.length, 
                format!("Variable '{}', declared here, has type {}", variable_info.name, variable_info.variable_type));
    }
}