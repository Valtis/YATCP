use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, NodeInfo, DeclarationInfo};

use crate::symbol_table::{SymbolTable, Symbol, TableEntry};

use crate::error_reporter::{ReportKind, ErrorReporter};

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;


struct Width {
    start: i32,
    end: i32
}


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

impl Type {

    pub fn size_in_bytes(&self) -> u32 {
        match *self {
            Type::Integer => 4,
            Type::Double => 8,
            Type::Float => 4,
            Type::String => 8, // ptr
            Type::Boolean => 1,
            Type::Void => ice!("Reguesting size of a void type"),
            Type::Uninitialized => ice!("Requesting size of an uninitialized type"),
            Type::Invalid => ice!("Requesting size of an invalid type"),
        }
    }
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
        self.check_and_gather_functions(node);
        self.do_check(node);
        self.errors
    }

    // only process function declarations initially, so that we can call functions that appear later
    fn check_and_gather_functions(&mut self, node: &mut AstNode) {
        self.symbol_table.push_empty();
        if let AstNode::Block(ref mut children, ref mut table_entry, _) = node {

            for child in children.iter_mut() {
                match child {
                    AstNode::Function(_, ref fi) |
                    AstNode::ExternFunction(ref fi) => {

                        self.check_function_declaration(fi);

                    }
                    _ => (), // don't care right now
                }
            }
            *table_entry = self.symbol_table.top();
        } else {
            ice!("Unexpected node type when Block was expected:\n{:#?}", node);
        }


    }

    fn do_check(&mut self, node: &mut AstNode) {
        match node {
            AstNode::Block(ref mut children, ref mut tab_ent, ref ni) =>
                self.handle_block(children, tab_ent, ni),
            AstNode::Function(ref mut child,  ref fi) =>
                self.handle_function(child, fi),
            AstNode::ExternFunction(ref fi) => (), // do nothing, handled in the initial function definition pass
            AstNode::FunctionCall(ref mut args, ref name, ref ni) =>
                self.handle_function_call(args, name, ni),
            AstNode::VariableDeclaration(ref mut child, ref vi) =>
                self.handle_variable_declaration(child, vi),
            AstNode::VariableAssignment(ref mut child, ref name, ref ni) =>
                self.handle_variable_assignment(child, name, ni),
            AstNode::Plus(_, _, _) |
            AstNode::Minus(_, _, _) |
            AstNode::Multiply(_, _, _) |
            AstNode::Divide(_, _, _) =>
                self.handle_arithmetic_operation_with_operator_type_check(node),
            AstNode::Negate(ref mut child, ref mut ai) =>
                self.handle_negation(child, ai),
            AstNode::Return(ref mut child, ref mut ai) =>
                self.handle_return(child, ai),
            AstNode::While(ref mut expr, ref mut child, ref ni) =>
                self.handle_while(expr, child, ni),
            AstNode::If(
                ref mut expr,
                ref mut block,
                ref mut opt_else_blk,
                ref ni) =>
                self.handle_if(expr, block, opt_else_blk, ni),
            AstNode::Less(left, right, span) |
            AstNode::LessOrEq(left, right, span) |
            AstNode::Equals(left, right, span) |
            AstNode::NotEquals(left, right, span) |
            AstNode::GreaterOrEq(left, right, span) |
            AstNode::Greater(left, right, span) =>
                self.handle_comparison_operation(left, right, span),
            AstNode::Integer(value, info) => self.check_for_overflow(value, info),
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
        &mut self,
        children: &mut Vec<AstNode>,
        tab_ent: &mut Option<TableEntry>,
        _node_info: &NodeInfo) {

        self.symbol_table.push_empty();
        for ref mut child in children {
            self.do_check(child);
        }

        let entry = self.symbol_table.pop();
        if let None = tab_ent {
            *tab_ent = entry;
        }
    }

    fn handle_function(
        &mut self,
        child: &mut AstNode,
        function_info: &FunctionInfo) {


        /*
         we need a new symbol table level, so that the parameters
         don't get added to the previous (most likely global)
         symbol table level

         the child of this node however is very likely a AstNode::Block, which
         pushes a new symbol table level for the function body. This is a
         problem later on, as this latter level does not contain the parameter
         symbols, which will cause ICEs later on.

         what we need to do is pop the outer level and add the parameters
         to the inner symbol table to fix this

         this is arguable a hacky way to fix it, should probably refactor this
         at some point (nothing is as permanent as temporary however...)

         */
        self.symbol_table.push_empty();
        for param in function_info.parameters.iter() {
            let next_id = self.get_next_id();
            self.symbol_table.add_symbol(
                Symbol::Variable(
                    param.clone(),
                    next_id));
        }
        self.do_check(child);
        let outer_symtab_level = self.symbol_table.pop().unwrap();

        if let AstNode::Block(_, Some(ref mut inner_symtab_level), _) = *child {
            for param in function_info.parameters.iter() {
                inner_symtab_level.add_symbol(
                    outer_symtab_level.find_symbol(&param.name).unwrap());
            }
        } else {
            // as of writing this, the function child should always be a block,
            // so this should never trigger. If this is no longer true in the
            // future, this obviously must be fixed.
            unimplemented!();
        }
    }

    fn check_function_declaration(
        &mut self,
        function_info: &FunctionInfo) {

        // report redeclaration
        if let Some(symbol) = self.symbol_table.find_symbol(&function_info.name) {
            if let Symbol::Function(ref fi) = symbol {
                self.report_error(
                    ReportKind::NameError,
                    function_info.node_info.line,
                    function_info.node_info.column,
                    function_info.node_info.length,
                    format!("Redefinition of function '{}'",
                        function_info.name));

                self.report_error(
                    ReportKind::Note,
                    fi.node_info.line,
                    fi.node_info.column,
                    fi.node_info.length,
                    "Previously declared here".to_string());

            } else {
                unimplemented!("Function redeclared, but not shadowing a symbol");
            }
        } else {
            self.symbol_table.add_symbol(
                Symbol::Function(function_info.clone())
            );
        }

        let mut seen_param = HashMap::new();

        for param in function_info.parameters.iter() {
            // report function shadowing
            if let Some(symbol) = self.symbol_table.find_symbol(&param.name) {
                if let Symbol::Function(ref fi) = symbol {
                    self.report_error(
                        ReportKind::NameError,
                        param.node_info.line,
                        param.node_info.column,
                        param.node_info.length,
                        format!("Function parameter '{}' shadows function",
                            param.name));

                    self.report_error(
                        ReportKind::Note,
                        fi.node_info.line,
                        fi.node_info.column,
                        fi.node_info.length,
                        "Function declared here".to_string());
                } else {
                    unimplemented!();
                }
            }

            // report void parameter
            if param.variable_type == Type::Void {
                self.report_error(
                    ReportKind::TypeError,
                    param.node_info.line,
                    param.node_info.column,
                    param.node_info.length,
                    "Parameter may not have type 'Void'".to_string());
            }
            // report parameter name collisions
            if !seen_param.contains_key(&param.name) {
                seen_param.insert(
                    param.name.clone(),
                    param.node_info.clone());
            } else {
                self.report_error(
                    ReportKind::NameError,
                    param.node_info.line,
                    param.node_info.column,
                    param.node_info.length,
                    format!("Parameter '{}' shadows earlier parameter",
                        param.name)
                    );

                let other_info = &seen_param[&param.name];
                self.report_error(
                    ReportKind::Note,
                    other_info.line,
                    other_info.column,
                    other_info.length,
                    "Parameter with same name previously declared here".to_string());
            }
        }

    }

    fn handle_function_call(
        &mut self,
        args: &mut Vec<AstNode>,
        function_name: &Rc<String>,
        node_info: &NodeInfo) {

        for arg in args.iter_mut() {
            self.do_check(arg);
        }

        if let Some(symbol) = self.symbol_table.find_symbol(function_name) {
            match symbol {
                Symbol::Variable(ref declaration_info, _) => {
                    self.report_error(
                        ReportKind::TypeError,
                        node_info.line,
                        node_info.column,
                        node_info.length,
                        format!("Usage of variable '{}' as function",
                        function_name));

                self.report_error(
                    ReportKind::Note,
                    declaration_info.node_info.line,
                    declaration_info.node_info.column,
                    declaration_info.node_info.length,
                    "Variable declared here".to_string());
                }
                Symbol::Function(ref function_info) => {
                    if args.len() != function_info.parameters.len() {
                        self.report_error(
                            ReportKind::TypeError,
                            node_info.line,
                            node_info.column,
                            node_info.length,
                            format!("{} arguments expected but {} provided",
                                function_info.parameters.len(),
                                args.len()));

                        self.report_error(
                            ReportKind::Note,
                            function_info.node_info.line,
                            function_info.node_info.column,
                            function_info.node_info.length,
                            "Function declared here".to_string());
                    } else {
                        for (param, arg) in function_info.parameters
                        .iter()
                        .zip(args.iter()) {
                            let arg_type = self.get_type(arg);
                            if arg_type != param.variable_type &&
                                arg_type != Type::Invalid &&
                                param.variable_type != Type::Void {
                                self.report_error(
                                    ReportKind::TypeError,
                                    arg.line(),
                                    arg.column(),
                                    arg.length(),
                                    format!("Got argument of type '{}' when '{}' was expected",
                                        arg_type,
                                        param.variable_type,
                                        ));

                                self.report_error(
                                    ReportKind::Note,
                                    param.node_info.line,
                                    param.node_info.column,
                                    param.node_info.length,
                                    "Corresponding parameter declared here"
                                        .to_string());
                            }
                        }

                    }
                },
            }
        } else {

            self.report_error(
                ReportKind::NameError,
                node_info.line,
                node_info.column,
                node_info.length,
                format!("Function '{}' has not been declared",
                    function_name));
            return;
        }
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
                    ReportKind::NameError,
                    variable_info.node_info.line,
                    variable_info.node_info.column,
                    variable_info.node_info.length,
                    err_text);

                self.report_error(
                    ReportKind::Note,
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

        if variable_info.variable_type == Type::Void {
            self.report_error(
                ReportKind::TypeError,
                variable_info.node_info.line,
                variable_info.node_info.column,
                variable_info.node_info.length,
                "Variable may not have type 'Void'".to_string());
        } else if variable_info.variable_type != child_type &&
            child_type != Type::Invalid {
            // if type is invalid, errors has already been reported

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
                ReportKind::TypeError,
                child.line(),
                child.column(),
                child.length(),
                format!(
                    "Expected '{}' but got '{}'",
                     sym_info.variable_type, child_type));

            self.report_error(
                ReportKind::Note,
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
                    ReportKind::TypeError,
                    arith_info.node_info.line,
                    arith_info.node_info.column,
                    arith_info.node_info.length,
                    "Return statement without expression in non-void function".
                        to_string());

                self.report_error(
                    ReportKind::Note,
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
                ReportKind::TypeError,
                arith_info.node_info.line,
                arith_info.node_info.column,
                arith_info.node_info.length,
                err_str);

            self.report_error(
                ReportKind::Note,
                function_info.node_info.line,
                function_info.node_info.column,
                function_info.node_info.length,
                note_str);

            arith_info.node_type = Type::Invalid;
        }
    }

    fn handle_while(
        &mut self,
        expr: &mut AstNode,
        body: &mut AstNode,
        _info: &NodeInfo) {

       self.do_check(expr);
       let expr_type = self.get_type(expr);

       if expr_type != Type::Invalid && expr_type != Type::Boolean {
            self.report_error(
                ReportKind::TypeError,
                expr.line(),
                expr.column(),
                expr.length(),
                format!("Expected '{}' for loop expression but was '{}'",
                    Type::Boolean, expr_type));
       }

       self.do_check(body);
    }

    fn handle_if(
        &mut self,
        expr: &mut AstNode,
        if_blk: &mut AstNode,
        opt_else_blk: &mut Option<Box<AstNode>>,
        _info: &NodeInfo) {

        self.do_check(expr);
        let expr_type = self.get_type(expr);

        if expr_type != Type::Invalid && expr_type != Type::Boolean {
            self.report_error(
                ReportKind::TypeError,
                expr.line(),
                expr.column(),
                expr.length(),
                format!("Expected '{}' for if expression but was '{}'",
                    Type::Boolean, expr_type));
        }

        self.do_check(if_blk);
        if let Some(ref mut else_blk) = *opt_else_blk {
            self.do_check(else_blk);
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

                if let AstNode::Integer(ref value, _) = **right {
                   if let AstInteger::Int(0) = value  {
                       self.report_error(
                           ReportKind::Warning,
                           ai.node_info.line,
                           ai.node_info.column,
                           ai.node_info.length,
                           "Division by zero".to_owned(),
                       )
                   }
                };

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
                ReportKind::TypeError,
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
                ReportKind::TypeError,
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
                ReportKind::TypeError,
                arith_info.node_info.line,
                arith_info.node_info.column,
                arith_info.node_info.length,
                format!(
                    "Cannot negate operand with type '{}'", child_type));
        } else {
            arith_info.node_type = child_type;
        }
    }

    fn handle_comparison_operation(&mut self, left_child: &mut AstNode, right_child: &mut AstNode, span: &NodeInfo) {

        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);

        if left_type != right_type &&
            left_type != Type::Invalid && right_type != Type::Invalid
        {
            self.report_error(
                ReportKind::TypeError,
                span.line,
                span.column,
                span.length,
                format!(
                    "Incompatible operand types '{}' and '{}' for this operation", left_type, right_type));
        }
    }

    fn check_for_overflow(&mut self, integer: &AstInteger, info: &NodeInfo) {
        match integer {
            AstInteger::Int(_) => (), // OK
            AstInteger::IntMaxPlusOne | AstInteger::Invalid(_) => {
                self.report_error(
                  ReportKind::TokenError,
                    info.line,
                    info.column,
                    info.length,
                    "Integer overflow".to_owned(),
                );
            },

        }
    }

    fn check_identifier_is_initialized(&mut self, name: &String, info: &NodeInfo) ->
        Option<Symbol> {
        match self.symbol_table.find_symbol(name) {
            Some(symbol) => {
                match symbol {
                    Symbol::Function(function_info) => {
                        self.report_error(
                            ReportKind::TypeError,
                            info.line,
                            info.column,
                            info.length,
                            format!(
                                "Usage of function '{}' as a variable",
                                name));

                        self.report_error(
                            ReportKind::Note,
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
                    ReportKind::NameError,
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
            AstNode::Less(_, _, _) |
            AstNode::LessOrEq(_, _, _) |
            AstNode::Equals(_, _, _) |
            AstNode::NotEquals(_, _, _) |
            AstNode::GreaterOrEq(_, _, _) |
            AstNode::Greater(_, _, _) => Type::Boolean,
            AstNode::Plus(_, _, ref info) |
            AstNode::Minus(_, _, ref info) |
            AstNode::Multiply(_, _,  ref info) |
            AstNode::Divide(_, _, ref info) => info.node_type,
            AstNode::Negate(_, ref info) => info.node_type,
            AstNode::Identifier(ref name, _) => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    info.variable_type
                } else {
                    Type::Invalid
                }
            },
            AstNode::FunctionCall(_, ref name, _) => {
                if let Some(Symbol::Function(ref info)) = self.symbol_table.find_symbol(name) {
                    info.return_type
                } else {
                    Type::Invalid
                }
            },
            AstNode::Text(_, _) => Type::String,
            AstNode::ErrorNode => Type::Invalid,
            _ => ice!("Invalid node '{}' when resolving node type", node),
        }
    }

    fn get_subtree_width(&self, node: &AstNode) -> Option<Width> {
        return match *node {
            AstNode::Integer(_, ref ni) |
            AstNode::Text(_, ref ni) |
            AstNode::Float(_, ref ni) |
            AstNode::Double(_, ref ni) |
            AstNode::Boolean(_, ref ni) |
            AstNode::Identifier(_, ref ni) => Some(Width { start: ni.column, end: ni.column + ni.length }),

            AstNode::Plus(ref left, ref right, ref ai) |
            AstNode::Minus(ref left, ref right, ref ai) |
            AstNode::Multiply(ref left, ref right, ref ai) |
            AstNode::Divide(ref left, ref right, ref ai) => {
                let left_tree = self.get_subtree_width(left);
                let right_tree = self.get_subtree_width(right);

                let mut start = ai.node_info.column;
                let mut end = ai.node_info.column + ai.node_info.length;

                if let Some(x) = left_tree {
                    start = std::cmp::min(start, x.start);
                    end = std::cmp::max(end, x.end);
                }

                if let Some(x) = right_tree {
                    start = std::cmp::min(start, x.start);
                    end = std::cmp::max(end, x.end);
                }

                Some(Width {
                    start: start,
                    end: end,
                })
            },

            _ => None
        };

    }

    fn report_error(&mut self, error_type: ReportKind, line:i32, column:i32, token_length : i32, error: String) {
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
            ReportKind::TypeError,
            actual_node.line(),
            actual_node.column(),
            actual_node.length(),
            format!("Expected '{}' but got '{}'",
                    variable_info.variable_type, actual_type));

            self.report_error(
                ReportKind::Note,
                variable_info.node_info.line,
                variable_info.node_info.column,
                variable_info.node_info.length,
                format!("Variable '{}', declared here, has type {}", variable_info.name, variable_info.variable_type));
    }
}