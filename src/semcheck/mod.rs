use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, Span as Span, DeclarationInfo, ExtraDeclarationInfo};

use crate::symbol_table::{SymbolTable, Symbol, TableEntry};

use crate::error_reporter::{ReportKind, ErrorReporter};

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;



pub const ARRAY_LENGTH_PROPERTY: &'static str = "length";

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Integer,
    Double,
    Float,
    String,
    Boolean,
    Void,
    IntegerArray,
    BooleanArray,
    Uninitialized,
    Reference(Box<Type>),
    Invalid, // type error occurred
}

impl Type {
    pub fn size_in_bytes(&self) -> u32 {
        match *self {
            Type::Integer => 4,
            Type::Double => 8,
            Type::Float => 4,
            Type::String => unimplemented!(),
            Type::Boolean => 1,
            Type::Void => ice!("Reguesting size of a void type"),
            Type::IntegerArray => unimplemented!(), // TODO define semantics
            Type::BooleanArray=> unimplemented!(), // TODO define semantics
            Type::Reference(_) => 8,
            Type::Uninitialized => ice!("Requesting size of an uninitialized type"),
            Type::Invalid => ice!("Requesting size of an invalid type"),
        }
    }

    pub fn is_array(&self) -> bool {
        match *self {
            Type::Integer => false,
            Type::Double => false,
            Type::Float => false,
            Type::String => false,
            Type::Boolean => false,
            Type::Void => false,
            Type::IntegerArray => true,
            Type::BooleanArray => true,
            Type::Uninitialized => false,
            Type::Reference(ref x) => x.is_array(),
            Type::Invalid => false,
        }
    }

    pub fn is_reference(&self) -> bool {
        match *self {
            Type::Reference(_) => true,
            _ => false,
        }
    }

    pub fn get_array_basic_type(&self) -> Type {
        match *self {
            Type::IntegerArray => Type::Integer,
            Type::BooleanArray => Type::Boolean,
            Type::Reference(ref x) if x.is_array() => x.get_array_basic_type(),
            _ => ice!("{} is not an array type but requested basic type anyway", self),
        }
    }

    pub fn get_array_type_from_basic_type(&self) -> Type {
        match *self {
            Type::Integer => Type::IntegerArray,
            Type::Boolean => Type::BooleanArray,
            _ => ice!("{} is not valid basic type for arrays, but requested array type anyway", self),
        }
    }
}

impl Display for Type {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
        Display::fmt( &match *self {
            Type::Integer => "Integer".to_owned(),
            Type::Double => "Double".to_owned(),
            Type::Float => "Float".to_owned(),
            Type::String=> "String".to_owned(),
            Type::Boolean => "Boolean".to_owned(),
            Type::Void => "Void".to_owned(),
            Type::IntegerArray => "Integer array".to_owned(),
            Type::BooleanArray=> "Boolean array".to_owned(),
            Type::Reference(ref x) => format!("Reference to {}", x),
            Type::Uninitialized => "Uninitialized".to_owned(),
            Type::Invalid => "Invalid".to_owned(),
      }, formatter)
  }
}

pub struct SemanticsCheck {
    pub errors: u32,
    symbol_table: SymbolTable,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>,
    id_counter: u32,
    enclosing_function_stack: Vec<FunctionInfo>,
}

impl SemanticsCheck {
    pub fn new(reporter: Rc<RefCell<dyn ErrorReporter>>) -> SemanticsCheck {
        SemanticsCheck {
            errors: 0,
            symbol_table: SymbolTable::new(),
            error_reporter: reporter,
            id_counter: 0,
            enclosing_function_stack: vec![],
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

        ice_if!(
            !self.enclosing_function_stack.is_empty(),
            "Enclosing function stack not empty after semantic checks");
        self.errors
    }

    // only process function declarations initially, so that we can call functions that appear later
    fn check_and_gather_functions(&mut self, node: &mut AstNode) {
        self.symbol_table.push_empty();
        if let AstNode::Block{ref mut statements, ref mut block_symbol_table_entry , ..} = node {

            for statement in statements.iter_mut() {
                match statement {
                    AstNode::Function(_, ref fi) |
                    AstNode::ExternFunction(ref fi) => {
                        self.check_function_declaration(fi);

                    }
                    _ => (), // don't care right now
                }
            }
            // check parameters after all function decalrations, so that parameters shadowing a function that appears later
            // are reported correctly
            for statement in statements.iter_mut() {
                match statement {
                    AstNode::Function(_, ref fi) |
                    AstNode::ExternFunction(ref fi) =>
                        self.check_function_parameter_list(fi),
                    _ => (),
                }
            }

            *block_symbol_table_entry = self.symbol_table.top();

        } else {
            ice!("Unexpected node type when Block was expected:\n{:#?}", node);
        }
    }

    fn do_check(&mut self, node: &mut AstNode) {
        match node {
            AstNode::Block{ref mut statements, ref mut block_symbol_table_entry, ref span} =>
                self.handle_block(statements, block_symbol_table_entry, span),
            AstNode::Function(ref mut child,  ref fi) =>
                self.handle_function(child, fi),
            AstNode::ExternFunction(_) => (), // do nothing, handled in the initial function definition pass
            AstNode::FunctionCall(ref mut args, ref name, ref span) =>
                self.handle_function_call(args, name, span),
            AstNode::VariableDeclaration(ref mut child, ref vi) =>
                self.handle_variable_declaration(child, vi),
            AstNode::VariableAssignment(ref mut child, ref name, ref span) =>
                self.handle_variable_assignment(child, name, span),
            AstNode::ArrayAssignment {
                index_expression,
                assignment_expression,
                variable_name,
                span,
            } => self.handle_array_assignment(index_expression, assignment_expression, variable_name, span),
            AstNode::MemberAccess {
                object,
                member,
                span,
            } => self.handle_member_access(object, member, span),
            AstNode::Plus(_, _, _) |
            AstNode::Minus(_, _, _) |
            AstNode::Multiply(_, _, _) |
            AstNode::Divide(_, _, _) |
            AstNode::Modulo(_, _, _) =>
                self.handle_arithmetic_operation_with_operator_type_check(node),
            AstNode::Negate(ref mut child, ref mut ai) =>
                self.handle_negation(child, ai),
            AstNode::Return(ref mut child, ref mut ai) =>
                self.handle_return(child, ai),
            AstNode::While(ref mut expr, ref mut child, ref span) =>
                self.handle_while(expr, child, span),
            AstNode::If(
                ref mut expr,
                ref mut block,
                ref mut opt_else_blk,
                ref span) =>
                self.handle_if(expr, block, opt_else_blk, span),
            AstNode::Less(left, right, span) |
            AstNode::LessOrEq(left, right, span) |
            AstNode::Equals(left, right, span) |
            AstNode::NotEquals(left, right, span) |
            AstNode::GreaterOrEq(left, right, span) |
            AstNode::Greater(left, right, span) =>
                self.handle_comparison_operation(left, right, span),
            AstNode::BooleanAnd(left, right, span) |
            AstNode::BooleanOr(left, right, span) => self.check_boolean_and_or(left, right, span),
            AstNode::Not(child, span) => self.check_boolean_not(child, span),
            AstNode::Integer(value, info) => self.check_for_overflow(value, info),
            AstNode::Float(_, _) => {},
            AstNode::Double(_, _) => {},
            AstNode::Text(_, _) => {},
            AstNode::Identifier(ref name, ref info) => {
                self.check_identifier_is_declared(name, info);
            }
            AstNode::ArrayAccess{ index_expression, indexable_expression } => {
                 self.handle_array_access(index_expression, indexable_expression);
            },
            AstNode::Boolean(_, _) => {},
            AstNode::ErrorNode => {},
        }
    }

    fn handle_block(
        &mut self,
        children: &mut Vec<AstNode>,
        tab_ent: &mut Option<TableEntry>,
        _span: &Span) {

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

        self.enclosing_function_stack.push(function_info.clone());
        self.do_check(child);
        self.enclosing_function_stack.pop();
        let outer_symtab_level = self.symbol_table.pop().unwrap();

        if let AstNode::Block{ block_symbol_table_entry: Some(ref mut inner_symtab_level), ..} = *child {
            for param in function_info.parameters.iter() {
                inner_symtab_level.add_symbol(
                    outer_symtab_level.find_symbol(&param.name).unwrap());
            }
        } else {
            ice!("Block node expected but got {}", child);
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
                    function_info.span.clone(),
                    format!("Redefinition of function '{}'",
                        function_info.name));

                self.report_error(
                    ReportKind::Note,
                    fi.span.clone(),
                    "Previously declared here".to_string());

            } else {
                ice!("Function redeclared, but not shadowing a symbol");
            }
        } else {
            self.symbol_table.add_symbol(
                Symbol::Function(function_info.clone())
            );
        }
    }

    fn check_function_parameter_list(&mut self, function_info: &FunctionInfo) {

        let mut seen_param = HashMap::new();

        for param in function_info.parameters.iter() {
            // report function shadowing
            if let Some(symbol) = self.symbol_table.find_symbol(&param.name) {
                if let Symbol::Function(ref fi) = symbol {
                    self.report_error(
                        ReportKind::NameError,
                        param.span.clone(),
                        format!("Function parameter '{}' shadows function",
                                param.name));

                    self.report_error(
                        ReportKind::Note,
                        fi.span.clone(),
                        "Function declared here".to_string());
                } else {
                    unimplemented!();
                }
            }

            // report void parameter
            if param.variable_type == Type::Void {
                self.report_error(
                    ReportKind::TypeError,
                    param.span.clone(),
                    "Parameter may not have type 'Void'".to_string());
            }
            // report parameter name collisions
            if !seen_param.contains_key(&param.name) {
                seen_param.insert(
                    param.name.clone(),
                    param.span.clone());
            } else {
                self.report_error(
                    ReportKind::NameError,
                    param.span.clone(),
                    format!("Parameter '{}' shadows earlier parameter",
                            param.name)
                );

                let other_info = &seen_param[&param.name];
                self.report_error(
                    ReportKind::Note,
                    other_info.clone(),
                    "Parameter with same name previously declared here".to_string());
            }
        }

    }

    fn handle_function_call(
        &mut self,
        args: &mut Vec<AstNode>,
        function_name: &Rc<String>,
        span: &Span) {

        for arg in args.iter_mut() {
            self.do_check(arg);
        }

        if let Some(symbol) = self.symbol_table.find_symbol(function_name) {
            match symbol {
                Symbol::Variable(ref declaration_info, _) => {
                    self.report_error(
                        ReportKind::TypeError,
                        span.clone(),
                        format!("Usage of variable '{}' as function",
                        function_name));

                    self.report_error(
                        ReportKind::Note,
                        declaration_info.span.clone(),
                        "Variable declared here".to_string());
                }
                Symbol::Function(ref function_info) => {
                    if args.len() != function_info.parameters.len() {
                        self.report_error(
                            ReportKind::TypeError,
                            span.clone(),
                            format!("{} arguments expected but {} provided",
                                function_info.parameters.len(),
                                args.len()));

                        self.report_error(
                            ReportKind::Note,
                            function_info.span.clone(),
                            "Function declared here".to_string());
                    } else {
                        for (param, arg) in function_info.parameters
                        .iter()
                        .zip(args.iter_mut()) {
                            let arg_type = self.get_type(arg);

                            // automatically use ref to array, if array is used as a param to function expecting array ref
                            if let Type::Reference(ref x) = param.variable_type {
                                if x.is_array() && arg_type.is_array() &&
                                    x.get_array_basic_type() == arg_type.get_array_basic_type() {
                                    return;
                                }
                            }

                            if arg_type != param.variable_type &&
                                arg_type != Type::Invalid &&
                                param.variable_type != Type::Invalid {
                                self.report_error(
                                    ReportKind::TypeError,
                                    arg.span(),
                                    format!("Got argument of type '{}' when '{}' was expected",
                                        arg_type,
                                        param.variable_type,
                                        ));

                                self.report_error(
                                    ReportKind::Note,
                                    param.span.clone(),
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
                span.clone(),
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
                            "Variable '{}' shadows a function",
                            fi.name),
                         fi.span.line,
                         fi.span.column,
                         fi.span.length,
                        ),
                    Symbol::Variable(vi, _) =>
                        (format!("Redefinition of variable '{}'",
                            vi.name),
                         vi.span.line,
                         vi.span.column,
                         vi.span.length
                        ),
                };

                self.report_error(
                    ReportKind::NameError,
                    variable_info.span.clone(),
                    err_text);

                self.report_error(
                    ReportKind::Note,
                    Span::new(prev_line, prev_column, prev_length),
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
                variable_info.span.clone(),
                "Variable may not have type 'Void'".to_string());
        } else if variable_info.variable_type.is_array() {

            if variable_info.variable_type.get_array_basic_type() != child_type && child_type != Type::Invalid {
                self.report_error(
                    ReportKind::TypeError,
                    child.span(),
                    format!("Expected '{}' but got '{}' instead", variable_info.variable_type.get_array_basic_type(), child_type)
                );
            }


            let dims = if let Some(ExtraDeclarationInfo::ArrayDimension(ref x)) = variable_info.extra_info {
                x
            } else {
                ice!("Variable {} has type {}, which is an array type, but array dimension info is not set", variable_info.name, variable_info.variable_type);
            };


            let mut report_dimension_error = || {
                self.report_error(
                    ReportKind::TypeError,
                    variable_info.span.clone(),
                    "Array has invalid dimensions".to_owned(),
                )
            };


            for dim in dims {
                match dim {
                    AstInteger::Int(x) if *x <= 0 => {
                        report_dimension_error();
                        break;
                    },
                    AstInteger::IntMaxPlusOne |
                    AstInteger::Invalid(_) => {
                        report_dimension_error();
                        break;
                    },
                    _ => (), // OK
                }
            }


        } else if variable_info.variable_type != child_type &&
            child_type != Type::Invalid && variable_info.variable_type != Type::Invalid {
            self.report_type_error(variable_info, child, child_type);
        }
    }


    fn handle_variable_assignment(
        &mut self,
        child: &mut AstNode,
        name: &str,
        span: &Span
        ) {

        self.do_check(child);
        let child_type = self.get_type(child);

        let opt_symbol = self.check_identifier_is_declared(name, span);

        let symbol = if let Some(sym) = opt_symbol {
            sym
        } else {
            // no valid variable. This has already been reported, so just return
            return;
        };

        // do type check only for a declared variable
        if let Symbol::Variable(ref sym_info, _) = symbol {
            if sym_info.variable_type.is_array() {
                self.report_error(
                    ReportKind::TypeError,
                    span.clone(),
                    "Cannot assign into an array".to_owned());

                self.report_error(
                    ReportKind::Note,
                    sym_info.span.clone(),
                    format!("Variable '{}', declared here, has type '{}'",
                            name,
                            sym_info.variable_type));
                // if type is invalid, errors has already been reported
            } else if sym_info.variable_type != child_type &&
                child_type != Type::Invalid && sym_info.variable_type != Type::Invalid  {

            self.report_error(
                ReportKind::TypeError,
                child.span(),
                format!(
                    "Expected '{}' but got '{}'",
                     sym_info.variable_type, child_type));

            self.report_error(
                ReportKind::Note,
                sym_info.span.clone(),
                format!("Variable '{}', declared here, has type '{}'",
                    name,
                    sym_info.variable_type));
            }

        } else {
            // should not happen, only variables should be returned
            ice!(
                "Non-variable symbol '{:#?}' returned when variable expected",
                symbol);
        }
    }

    fn handle_array_assignment(
        &mut self,
        index_expression: &mut AstNode,
        assignment_expression: &mut AstNode,
        name: &str,
        span: &Span
    ) {

        let opt_symbol = self.check_identifier_is_declared(name, span);

        self.do_check(index_expression);
        self.do_check(assignment_expression);


        let symbol = if let Some(sym) = opt_symbol {
            sym
        } else {
            // identifier has not been declared. Above method reports this, bail out
            return;
        };

        if let Symbol::Variable(ref sym_info, _) = symbol {

            if !sym_info.variable_type.is_array() {
                self.report_error(
                    ReportKind::TypeError,
                    span.clone(),
                    format!("Variable '{}' is not an array", name));

                self.report_error(
                    ReportKind::Note,
                    sym_info.span.clone(),
                    format!("Variable '{}', declared here, has type '{}'",
                            name,
                            sym_info.variable_type));
                return;
            }

            let index_type = self.get_type(index_expression);
            let assignment_type = self.get_type(assignment_expression);

            if index_type != Type::Invalid && index_type != Type::Integer {
                self.report_error(
                    ReportKind::TypeError,
                    index_expression.span(),
                    format!(
                        "Array index must be an '{}', but got '{}' instead",
                        Type::Integer, index_type));
            }

            if sym_info.variable_type.get_array_basic_type() != assignment_type && assignment_type != Type::Invalid {
                self.report_error(
                    ReportKind::TypeError,
                    assignment_expression.span(),
                    format!(
                        "Expected '{}' but got '{}'",
                        sym_info.variable_type.get_array_basic_type(), assignment_type));

                self.report_error(
                    ReportKind::Note,
                    sym_info.span.clone(),
                    format!("Variable '{}', declared here, has type '{}'",
                            name,
                            sym_info.variable_type));
            }
        } else {
            ice!("Non-variable symbol '{:#?}' returned when variable expected", symbol);
        }
    }

    fn handle_array_access(
        &mut self,
        index_expression: &mut AstNode,
        indexable_expression: &mut AstNode,
    ) {

        self.do_check(index_expression);
        self.do_check(indexable_expression);

        let indexable_type = self.get_type(indexable_expression);

        if !indexable_type.is_array() && indexable_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                indexable_expression.span(),
                format!("Cannot index expression of type '{}', array type expected", indexable_type));

            return;
        }

        let index_type = self.get_type(index_expression);

        if index_type != Type::Invalid && index_type != Type::Integer {
            self.report_error(
                ReportKind::TypeError,
                index_expression.span(),
                format!(
                    "Array index must be an '{}', but got '{}' instead",
                    Type::Integer, index_type));
        }
    }

    fn handle_member_access(
        &mut self,
        object: &mut AstNode,
        member: &mut AstNode,
        _span: &Span,
    ) {
        self.do_check(object);

        let name = if let AstNode::Identifier(ref name, _) = member {
            name.clone()
        } else {
            ice!("Unexpected AST node '{:?}' for property", member)
        };

        match member {
            AstNode::Identifier(ref name, _) if self.get_type(object).is_array() => {
                if **name != ARRAY_LENGTH_PROPERTY {
                    self.report_error(
                        ReportKind::TypeError,
                        member.span(),
                        format!("Invalid property '{}' for an array", name, ));
                }
            },
            _ if self.get_type(object) != Type::Invalid => {
                self.report_error(
                    ReportKind::TypeError,
                    member.span(),
                    format!("Invalid property '{}' for expression of type '{}'", name, self.get_type(object)));
            },
            _ if self.get_type(object) == Type::Invalid => (),
            _ => ice!("Unexpected member type {:?}", member),
        }
    }

    fn handle_return(
        &mut self,
        opt_child: &mut Option<Box<AstNode>>,
        arith_info: &mut ArithmeticInfo) {

        let function_info = self.get_enclosing_function_info();

        let child = if let Some(ref mut c) = *opt_child {
            c
        } else {
            arith_info.node_type = Type::Void;
            // no return expression -> void type
            if function_info.return_type != Type::Void {
                self.report_error(
                    ReportKind::TypeError,
                    arith_info.span.clone(),
                    "Return statement without expression in non-void function".
                        to_string());

                self.report_error(
                    ReportKind::Note,
                    function_info.span.clone(),
                    format!("Function '{}', declared here, is expected to return '{}'",
                        function_info.name,
                        function_info.return_type));
                arith_info.node_type = Type::Invalid;
            }
            return;
        };

        self.do_check(child);
        let child_type = self.get_type(child);


        arith_info.node_type = child_type.clone();
        // if type is invalid, errors has already been reported
        if (function_info.return_type == Type::Void || function_info.return_type != child_type) && child_type != Type::Invalid {


            let (err_str, note_str) = if function_info.return_type == Type::Void {
                ("Return statement with expression in a void function".to_owned(),
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
                child.span(),
                err_str);

            self.report_error(
                ReportKind::Note,
                function_info.span.clone(),
                note_str);

            arith_info.node_type = Type::Invalid;
        }
    }

    fn handle_while(
        &mut self,
        expr: &mut AstNode,
        body: &mut AstNode,
        _span: &Span) {

       self.do_check(expr);
       let expr_type = self.get_type(expr);

       if expr_type != Type::Invalid && expr_type != Type::Boolean {
            self.report_error(
                ReportKind::TypeError,
                expr.span(),
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
        _span: &Span) {

        self.do_check(expr);
        let expr_type = self.get_type(expr);

        if expr_type != Type::Invalid && expr_type != Type::Boolean {
            self.report_error(
                ReportKind::TypeError,
                expr.span(),
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
                           ai.span.clone(),
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
            AstNode::Modulo(ref mut left, ref mut right, ref mut ai) => {

                self.handle_arithmetic_node(left, right, ai);

                if let AstNode::Integer(ref value, _) = **right {
                    if let AstInteger::Int(0) = value  {
                        self.report_error(
                            ReportKind::Warning,
                            ai.span.clone(),
                            "Division by zero".to_owned(),
                        )
                    }
                };
                (vec![
                    Type::Integer,
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
                ai.span.clone(),
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
                arith_info.span.clone(),
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
                arith_info.span.clone(),
                format!(
                    "Cannot negate operand of type '{}'", child_type));
        } else {
            arith_info.node_type = child_type;
        }
    }

    fn handle_comparison_operation(&mut self, left_child: &mut AstNode, right_child: &mut AstNode, span: &Span) {

        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);

        if left_type != right_type &&
            left_type != Type::Invalid && right_type != Type::Invalid
        {
            self.report_error(
                ReportKind::TypeError,
                span.clone(),
                format!(
                    "Incompatible operand types '{}' and '{}' for this operation", left_type, right_type));
        }
    }

    fn check_boolean_and_or(&mut self, left_child: &mut AstNode, right_child: &mut AstNode, span: &Span) {
        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);


        if left_type == right_type && left_type != Type::Invalid && left_type != Type::Boolean {
            self.report_error(
                ReportKind::TypeError,
                span.clone(),
                format!("Operands of type '{}' are not valid for this operator",
                        left_type));
        } else {
            if left_type != Type::Boolean &&
                left_type != Type::Invalid {
                self.report_error(
                    ReportKind::TypeError,
                    left_child.span(),
                    format!(
                        "Operand must be '{}', got '{}' instead", Type::Boolean, left_type));
            }

            if right_type != Type::Boolean &&
                right_type != Type::Invalid {
                self.report_error(
                    ReportKind::TypeError,
                    right_child.span(),
                    format!(
                        "Operand must be '{}', got '{}' instead", Type::Boolean, right_type));
            }
        }

    }

    fn check_boolean_not(&mut self, child: &mut AstNode, _span: &Span) {
        self.do_check(child);
        let child_type = self.get_type(child);

        if child_type != Type::Boolean &&
            child_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                child.span(),
                format!(
                    "Operand must be '{}', got '{}' instead", Type::Boolean, child_type));
        }
    }

    fn check_for_overflow(&mut self, integer: &AstInteger, span: &Span) {
        match integer {
            AstInteger::Int(_) => (), // OK
            AstInteger::IntMaxPlusOne | AstInteger::Invalid(_) => {
                self.report_error(
                  ReportKind::TokenError,
                    span.clone(),
                    "Number does not fit inside 32 bit signed integer".to_owned(),
                );
            },

        }
    }

    fn check_identifier_is_declared(&mut self, name: &str, span: &Span) ->
        Option<Symbol> {
        match self.symbol_table.find_symbol(name) {
            Some(symbol) => {
                match symbol {
                    Symbol::Function(function_info) => {
                        self.report_error(
                            ReportKind::TypeError,
                            span.clone(),
                            format!(
                                "Usage of function '{}' as a variable",
                                name));

                        self.report_error(
                            ReportKind::Note,
                            function_info.span.clone(),
                            "Function declared here:".to_string());
                    }
                    Symbol::Variable(_, _) => { return Some(symbol.clone()); }

                };
            },
            None => {
                self.report_error(
                    ReportKind::NameError,
                    span.clone(),
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
            AstNode::Divide(_, _, ref info) |
            AstNode::Modulo(_, _, ref info)=> info.node_type.clone(),
            AstNode::Negate(_, ref info) => info.node_type.clone(),
            AstNode::Identifier(ref name, _) => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    info.variable_type.clone()
                } else {
                    Type::Invalid
                }
            },
            AstNode::FunctionCall(_, ref name, _) => {
                if let Some(Symbol::Function(ref info)) = self.symbol_table.find_symbol(name) {
                    info.return_type.clone()
                } else {
                    Type::Invalid
                }
            },
            AstNode::BooleanAnd(_, _, _) |
            AstNode::BooleanOr(_, _, _) |
            AstNode::Not(_, _) => Type::Boolean,
            AstNode::Text(_, _) => Type::String,
            AstNode::ArrayAccess {
                index_expression: _,
                ref indexable_expression,
            } => {
                match **indexable_expression {
                    AstNode::Identifier(ref name, _) => {
                        if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                            if info.variable_type.is_array() {
                                info.variable_type.get_array_basic_type()
                            } else {
                                Type::Invalid
                            }
                        } else {
                            Type::Invalid
                        }
                    },
                    AstNode::FunctionCall(_, _, _) => todo!(),
                    _ => Type::Invalid,
                }
            },
            AstNode::MemberAccess{ ref object, ref member, span: _ } => {
                if self.get_type(object).is_array() {
                    if let AstNode::Identifier(ref name, _) = **member {
                        if **name == ARRAY_LENGTH_PROPERTY {
                            return Type::Integer;
                        }
                        return Type::Invalid;
                    } else {
                        return Type::Invalid;
                    }
                }
                return Type::Invalid;
            }
            AstNode::ErrorNode => Type::Invalid,
            _ => ice!("Invalid node '{}' when resolving node type", node),
        }
    }

    fn get_enclosing_function_info(&self) -> FunctionInfo {
        ice_if!(self.enclosing_function_stack.is_empty(),
            "No enclosing function found");

        return self.enclosing_function_stack[self.enclosing_function_stack.len()-1].clone()
    }

    fn report_error(&mut self, error_type: ReportKind, span: Span, error: String) {
        self.errors += 1;
        self.error_reporter.borrow_mut().report_error(error_type, span,error);
    }

    fn report_type_error(
        &mut self,
        variable_info: &DeclarationInfo,
        actual_node: &AstNode,
        actual_type: Type
        ) {
        self.report_error(
            ReportKind::TypeError,
            actual_node.span(),
            format!("Expected '{}' but got '{}'",
                    variable_info.variable_type, actual_type));
        self.report_error(
            ReportKind::Note,
            variable_info.span.clone(),
            format!("Variable '{}', declared here, has type {}", variable_info.name, variable_info.variable_type));
    }
}
