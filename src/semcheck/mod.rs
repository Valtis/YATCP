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
                    function_info.span.clone(),
                    format!("Redefinition of function '{}'",
                        function_info.name));

                self.report_error(
                    ReportKind::Note,
                    fi.span.clone(),
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
                            "Variable '{}' previously declared as a function",
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

            if variable_info.variable_type.get_array_basic_type() != child_type {
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
                    "Cannot negate operand with type '{}'", child_type));
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

    fn check_boolean_and_or(&mut self, left_child: &mut AstNode, right_child: &mut AstNode, _span: &Span) {
        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);

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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::{AstNode, AstInteger, ArithmeticInfo, Span as Span, FunctionInfo, DeclarationInfo };
    use crate::error_reporter::{ReportKind, Message };
    use crate::error_reporter::null_reporter::NullReporter;

    use std::rc::Rc;
    use std::cell::RefCell;

    fn create_sem_checker() -> (Rc<RefCell<NullReporter>>, SemanticsCheck) {
        let reporter = Rc::new(RefCell::new(NullReporter::new()));

        (reporter.clone(), SemanticsCheck::new(reporter))
    }

    #[test]
    fn assigning_boolean_is_allowed() {
       let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = true;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Boolean(
                                true,
                                Span::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn concatenation_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : string = "hello " + "world";
            }
        */
        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Plus(
                                Box::new(AstNode::Text(
                                    Rc::new("hello ".to_string()),
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Text(
                                    Rc::new("world".to_string()),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn artihmetic_operation_with_integers_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int = 3.2 + 14;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(3),
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(14),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn artihmetic_operation_with_floats_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : float = 3.2f + 14f;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Float(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Float(
                                    14f32,
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Float,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn artihmetic_operation_with_doubles_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : double = 3.2 + 14d;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Double(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Double(
                                    14f64,
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Double,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn negation_of_number_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : double = -3.2;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Negate(
                                Box::new(AstNode::Double(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Double,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn expression_using_variables_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();
         /*
            fn foo() {
                let a : double = -3.2;
                let b : double = a * 4.2;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Negate(
                                Box::new(AstNode::Double(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Double,
                                1, 1, 1),
                            ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    Span::new(9, 7, 4)
                                )),
                                Box::new(AstNode::Double(
                                    4.2,
                                    Span::new(9, 7, 4)
                                )),
                                ArithmeticInfo::new_alt(4, 5, 6)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                2, 2, 2),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn undeclared_variable_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
         /*
            fn foo() {
                let a : double = c
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Identifier(
                                    Rc::new("c".to_string()),
                                    Span::new(9, 7, 4)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Double,
                                1, 1, 1),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(9,7,4),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn redeclaration_of_variable_is_allowed_if_scopes_do_not_overlap() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() {
            {
            let a : int = 4;
            }
            let a : float = 6.2f;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Block(vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(4),
                                        Span::new(8, 6, 3)
                                    )),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        1, 2, 3),
                                ),
                                ],
                                None,
                                Span::new(0, 0, 0),
                            ),
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Float(
                                    6.2,
                                    Span::new(12, 65, 4)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Float,
                                    2, 3, 4),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn return_without_expression_in_void_function_is_allowed() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : void {
            return;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(None, ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn return_with_correct_constant_type_is_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            return 5;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(7, 23, 212)
                                    ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn return_with_correct_variable_type_is_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            let a : int = 24;
            return a;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(24),
                                    Span::new(99, 88, 77)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_owned()),
                                    Type::Integer,
                                    55, 44, 33),
                            ),
                            AstNode::Return(
                                Some(Box::new(AstNode::Identifier(
                                    Rc::new("a".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn return_with_correct_function_type_is_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
          return bar();
        }
        fn bar() : int {
            return 4;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(7, 23, 212)
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                        None,
                        Span::new(150, 160, 170))
                    ),
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::Integer, 9, 9, 9),
                )],
               None,
               Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn return_with_correct_extern_function_type_is_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        extern fn bar() : int;
        fn foo() : int {
            return bar();
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::ExternFunction(
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::Integer, 9, 9, 9),
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                ),
                ],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn correct_while_loop_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            while 1 >= 23 {
                let a : int = 1;
            } else {
                let b : double = 1.23;
            }
        }
        */
        let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::While(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn if_statement_without_else_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            if 1 >= 23 {
                let a : int = 1;
            }
        }
        */
        let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        None,
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn if_statement_with_else_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            if 1 >= 23 {
                let a : int = 1;
            } else {
                let b : double = 1.23;
            }
        }
        */
        let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Double(
                                    1.23,
                                    Span::new(7, 6, 5),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                41, 51, 61)),
                            ],
                            None,
                            Span::new(0, 0, 0)
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_function_call_with_arguments_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                foo(4, "hello");
            }

        }
        */
        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            Span::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    Span::new(0,0,0)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn using_function_parameters_in_function_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : int) : int {
                return a + b;
            }

        }
        */
        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::Integer,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::Plus(
                                    Box::new(AstNode::Identifier(
                                        Rc::new("a".to_string()),
                                        Span::new(9, 7, 4)
                                    )),
                                    Box::new(AstNode::Identifier(
                                        Rc::new("b".to_string()),
                                        Span::new(9, 7, 4)
                                    )),
                                    ArithmeticInfo::new_alt(4, 5, 6)
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn using_function_in_expression_is_accepted() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : int {

            }

            fn bar() : void {
                let a : int = foo(4, "hello");
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                            AstNode::Text(
                                                Rc::new("hello".to_string()),
                                                Span::new(0,0,0)),
                                        ],
                                        Rc::new("foo".to_string()),
                                        Span::new(0,0,0))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        1, 2, 3),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn calling_extern_function_is_accepted() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            extern fn foo(a : int, b : string);
            fn bar() : void {
                foo(4, "hello");
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                    AstNode::ExternFunction(foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                            AstNode::Text(
                                                Rc::new("hello".to_string()),
                                                Span::new(0,0,0)),
                                        ],
                                        Rc::new("foo".to_string()),
                                        Span::new(0,0,0))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        1, 2, 3),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn function_parameters_are_added_to_the_symbol_table_level_of_function_block() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : int {

            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);

        if let AstNode::Block(child_nodes, _, _) = node  {
            if let AstNode::Function(boxed_val, _) = child_nodes[0].clone() {
                if let AstNode::Block(_, Some(sym_tab), _) = *boxed_val {
                    if sym_tab.find_symbol(&"a".to_string()) == None {
                        panic!("Failed to find symbol 'a' in symbol table");
                    }

                    if sym_tab.find_symbol(&"b".to_string()) == None {
                        panic!("Failed to find symbol 'b' in symbol table");
                    }

                    return;
                }
            }
        }
        panic!("Invalid node present");

    }

    #[test]
    fn redeclaration_of_variable_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() {
            let a : int = 4;
            let a : float = 6.2f;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(8, 6, 3)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Float(
                                    6.2,
                                    Span::new(12, 65, 4)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Float,
                                    2, 3, 4),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(2,3,4),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,2,3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn declaration_of_variable_which_shares_name_with_function_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn a() : void {

        }

        fn foo() : void {
            let a : int = 4;
        }
        */
        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0)
                        )
                    ),
                    FunctionInfo::new_alt(Rc::new("a".to_string()), Type::Void, 4, 5, 6)
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(8, 6, 3)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(1,2,3),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(4,5,6),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn redefinition_of_a_function_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() : void {

            }

            fn foo : void() {

            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                    ),
                AstNode::Function(
                    Box::new(AstNode::Block(vec![],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 2, 3, 4)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(2,3,4),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,2,3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn using_function_as_variable_in_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() : void {

            }

            fn bar : void() {
                let a : int = foo;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                    ),
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Identifier(
                                    Rc::new("foo".to_string()),
                                    Span::new(8, 6, 3)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("bar".to_string()), Type::Void, 2, 3, 4)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8,6,3),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,2,3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn assigning_into_function_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() : void {

            }

            fn bar : void() {
                foo = 4;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                    ),
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                            AstNode::VariableAssignment(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(8, 6, 3)
                                )),
                                Rc::new("foo".to_string()),
                                Span::new(9, 10, 11)
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("bar".to_string()), Type::Void, 2, 3, 4)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,10,11),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,2,3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn type_error_when_variable_is_declared_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() : void {
                let a : int = 3.2f;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Float(
                                3.2,
                                Span::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 1, 1),
                        )],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8,6,3),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,1,1),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn type_error_in_plus_expression_is_reported() {
           let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int = 3.2f + 14;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Plus(
                                Box::new(AstNode::Float(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(14),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_in_minus_expression_is_reported() {
           let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int = 3.2f - 14;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Minus(
                                Box::new(AstNode::Float(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(14),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_in_multiplication_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int = 3.2f * 14;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Float(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(14),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_in_division_expression_is_reported() {
           let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int = 3.2f / 14;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Divide(
                                Box::new(AstNode::Float(
                                    3.2,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(14),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn arithmetic_operation_on_booleans_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : bool = true * false;
            }
        */
        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Boolean(
                                    true,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Boolean(
                                    false,
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn non_concatenation_arithmetic_operation_on_text_is_reported() {
            let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : string = "hello " - "world";
            }
        */
        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Minus(
                                Box::new(AstNode::Text(
                                    Rc::new("hello ".to_string()),
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Text(
                                    Rc::new("world".to_string()),
                                    Span::new(9, 7, 2)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }


    #[test]
    fn negation_of_string_is_reported() {
       let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : string = -"hello";
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Negate(
                                Box::new(AstNode::Text(
                                    Rc::new("hello".to_string()),
                                    Span::new(8, 6, 3)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn negation_of_string_variable_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : string = "hello";
                a = -a;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Text(
                                Rc::new("hello".to_string()),
                                Span::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 1, 1),
                            ),
                        AstNode::VariableAssignment(
                            Box::new(AstNode::Negate(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    Span::new(11, 112, 1112),
                                    )),
                                ArithmeticInfo::new_alt(21, 22, 23)
                            )),
                            Rc::new("a".to_string()),
                            Span::new(2, 6, 34),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(21,22,23),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn negation_of_boolean_is_reported() {
       let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = -false;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Negate(
                                Box::new(AstNode::Boolean(
                                    false,
                                    Span::new(8, 6, 3)
                                )),
                                ArithmeticInfo::new_alt(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                1, 1, 1),
                            )
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3,4,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn negation_of_boolean_variable_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = true;
                a = -a;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Boolean(
                                true,
                                Span::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                1, 1, 1),
                            ),
                        AstNode::VariableAssignment(
                            Box::new(AstNode::Negate(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    Span::new(11, 112, 1112),
                                    )),
                                ArithmeticInfo::new_alt(21, 22, 23)
                            )),
                            Rc::new("a".to_string()),
                            Span::new(2, 6, 34),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(21,22,23),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_involving_variables_in_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : string = "hello";
                a = a / 4;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Text(
                                Rc::new("hello".to_string()),
                                Span::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 4, 8),
                            ),
                        AstNode::VariableAssignment(
                            Box::new(AstNode::Divide(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    Span::new(33, 34, 35),
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(11, 112, 1112),
                                )),
                                ArithmeticInfo::new_alt(14, 41, 342)
                            )),
                            Rc::new("a".to_string()),
                            Span::new(2, 6, 34),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(14,41,342),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_when_assigning_into_variable_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : string = "hello";
                a = 23;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Text(
                                Rc::new("hello".to_string()),
                                Span::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 4, 8),
                            ),
                        AstNode::VariableAssignment(
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(11, 112, 1112),
                            )),
                            Rc::new("a".to_string()),
                            Span::new(2, 6, 34),
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(11,112,1112),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,4,8),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn syntax_error_node_is_handled_correctly() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : string = <syntax error node>;
                <syntax error node>
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::ErrorNode),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                1, 4, 8),
                            ),
                        AstNode::ErrorNode,
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn missing_expression_in_return_is_reported_if_function_has_non_void_return_type() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            return;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(None, ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 6, 5, 4)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(5,6, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(6,5,4),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn return_with_incorrect_constant_type_is_not_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : string {
            return 5;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(7, 23, 212)
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::String, 6, 12, 53)
                )],
                None,
                Span::new(2, 12, 23),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(7,23, 212),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(6,12,53),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn return_with_incorrect_variable_type_is_not_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            let a : float = 24f;
            return a;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Float(
                                    234.2f32,
                                    Span::new(99, 88, 77)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_owned()),
                                    Type::Float,
                                    55, 44, 33),
                            ),
                            AstNode::Return(
                                Some(Box::new(AstNode::Identifier(
                                    Rc::new("a".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 4, 8, 12)
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(4,8,12),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn return_with_incorrect_function_type_is_not_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
          return bar();
        }
        fn bar() : string {
            return "string";
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 7, 14, 21)
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::Text(
                                    Rc::new("foobar".to_owned()),
                                    Span::new(7, 23, 212)
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(150, 160, 170))
                    ),
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::String, 9, 9, 9),
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(7,14,21),
                "".to_owned()),
            messages[1]);


    }

    #[test]
    fn return_with_function_calling_void_function_is_not_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : void {
          return bar();
        }
        fn bar() : string {
            return string;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 7, 14, 21)
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::Text(
                                    Rc::new("foobar".to_owned()),
                                    Span::new(7, 23, 212)
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(150, 160, 170))
                    ),
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::String, 9, 9, 9),
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(7,14,21),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn returning_void_function_from_void_function_is_not_allowed(){
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : void {
          return bar();
        }
        fn bar() : void {
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 7, 14, 21)
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                        ],
                                       None,
                                       Span::new(150, 160, 170))
                    ),
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::Void, 9, 9, 9),
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(7,14,21),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn return_with_incorrect_extern_function_type_is_not_allowed_in_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        extern fn bar() : double;
        fn foo() : int {
            return bar();
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::ExternFunction(
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::Double, 9, 9, 9),
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 9, 18, 27)
                ),
            ],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(9,18,27),
                "".to_owned()),
            messages[1]);

    }

    #[test]
    fn return_with_incorrect_extern_function_type_is_not_allowed_in_void_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        extern fn bar() : double;
        fn foo() : void {
            return bar();
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::ExternFunction(
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::Double, 9, 9, 9),
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                ),
            ],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1,2,3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn return_with_void_extern_function_is_not_allowed_in_void_function() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        extern fn bar() : void;
        fn foo() : void {
            return bar();
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::ExternFunction(
                    FunctionInfo::new_alt(Rc::new("bar".to_owned()), Type::Void, 9, 9, 9),
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    Span::new(9, 8, 7),
                                ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                                       None,
                                       Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 14, 28, 42)
                ),
            ],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(14,28,42),
                "".to_owned()),
            messages[1]);
    }




    #[test]
    fn returning_value_from_void_function_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : void {
            return 5;
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::Return(
                                Some(Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(7, 23, 212)
                                    ))),
                                ArithmeticInfo::new_alt(5, 6, 7)),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 6, 5, 4)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(7,23, 212),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(6,5,4),
                "".to_owned()),
            messages[1]);
    }


    #[test]
    fn while_loop_with_non_boolean_expression_is_reported() {
       let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : void {
            while 4 {

            }
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::While(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(7, 23, 212)
                                    )),
                                Box::new(AstNode::Block(vec![
                                    ],
                                    None,
                                    Span::new(0, 0, 0))),
                                Span::new(89, 54, 12)
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 6, 5, 4)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(7,23, 212),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn error_in_while_loop_body_is_handled() {
       let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : void {
            while true {
                4 + hello;
            }
        }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::While(
                                Box::new(AstNode::Boolean(
                                    true,
                                    Span::new(7, 23, 212)
                                    )),
                                Box::new(AstNode::Block(vec![
                                    AstNode::Plus(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(4),
                                            Span::new(1, 2, 3))
                                        ),
                                        Box::new(AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            Span::new(4, 3, 1))
                                        ),
                                        ArithmeticInfo::new_alt(9, 7, 6))
                                    ],
                                    None,
                                    Span::new(0, 0, 0))),
                                Span::new(89, 54, 12)
                            ),
                        ],
                        None,
                        Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 6, 5, 4)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9,7,6),
                "".to_owned()),
            messages[0]);
    }


    #[test]
    fn if_statement_with_non_boolean_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
            /*
        fn foo() : int {
            if 1 {
                let a : string = "foo";
            } else {
                let b : double = 3.14159;
            }
        }*/

       let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            Span::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Text(
                                    Rc::new("foo".to_string()),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Double(
                                    3.14159,
                                    Span::new(7, 6, 5),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                41, 51, 61)),
                            ],
                            None,
                            Span::new(0, 0, 0)
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(11,22,33),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn if_statement_with_non_boolean_expression_in_else_if_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            if 1 > 23 {
                let a : string = "foo";
            } else if 4 {
                let b : double = 3.14159;
            }
        }*/

       let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Text(
                                    Rc::new("foo".to_string()),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::String,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::If(
                            Box::new(AstNode::Integer(
                                AstInteger::from(4),
                                Span::new(99, 88, 77),
                            )),
                            Box::new(AstNode::Block(vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Double(
                                        3.14159,
                                        Span::new(7, 6, 5),
                                    )),
                                    DeclarationInfo::new_alt(
                                    Rc::new("b".to_string()),
                                    Type::Double,
                                    41, 51, 61)),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                            None,
                            Span::new(0, 0, 0),
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(99,88,77),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn error_in_if_statement_true_block_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            if 1 > 23 {
                let a : int = "foo";
            } else {
                let b : double = 3.14159;
            }
        }*/

       let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Text(
                                    Rc::new("foo".to_string()),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Double(
                                    3.14159,
                                    Span::new(7, 6, 5),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                41, 51, 61)),
                            ],
                            None,
                            Span::new(0, 0, 0)
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(4, 5, 6),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn error_in_else_block_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
        fn foo() : int {
            if 1 > 23 {
                let a : int = 1;
            } else {
                let b : double = 3;
            }
        }*/
        let mut node = AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(3),
                                    Span::new(7, 6, 5),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                41, 51, 61)),
                            ],
                            None,
                            Span::new(0, 0, 0)
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0));

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(7, 6, 5),
                "".to_owned()),
            messages[0]);

         assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(41, 51, 61),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn calling_nonexistent_function_is_reported() {
       let (reporter, mut checker) = create_sem_checker();
        /*
            fn bar() : void {
                foo();
            }

        }
        */

        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("foo".to_string()),
                                    Span::new(7,8,9)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(7, 8, 9),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn using_variable_as_function_is_reported() {
       let (reporter, mut checker) = create_sem_checker();
        /*
            fn bar() : void {
                let foo : int = 2;
                foo();
            }

        }
        */

        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(4),
                                        Span::new(0,0,0))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("foo".to_string()),
                                        Type::Integer,
                                        1, 2, 3)),
                                AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("foo".to_string()),
                                    Span::new(7,8,9)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(7, 8, 9),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn wrong_number_of_function_arguments_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                foo(4);
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            4, 7, 9);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    Span::new(23,24,25)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(23, 24, 25),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(4, 7, 9),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn wrong_number_of_extern_function_arguments_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            extern fn foo(a : int, b : string) : void;

            fn bar() : void {
                foo(4);
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            4, 7, 9);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::ExternFunction(foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    Span::new(23,24,25)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(23, 24, 25),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(4, 7, 9),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn wrong_type_in_function_argument_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                foo(4, 6);
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            4, 7, 9);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                4, 5, 6));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                        AstNode::Integer(AstInteger::from(6), Span::new(9,8,7)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    Span::new(23,24,25)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9, 8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(4, 5, 6),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn redefinition_of_function_parameter_in_function_body_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {
                let a : int = 0;
            }

            fn bar() : void {
                foo(4, 6);
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            4, 7, 9);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                5, 4, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                4, 5, 6));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(0),
                                        Span::new(0,0,0))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        1, 2, 3),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(5, 4, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn type_mismatch_with_function_parameter_usage_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {
                let a : int = 0;
            }

            fn bar() : void {
                foo(4, 6);
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            4, 7, 9);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                5, 4, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                4, 5, 6));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Identifier(
                                        Rc::new("a".to_string()),
                                        Span::new(9,8,7))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("c".to_string()),
                                        Type::Float,
                                        1, 2, 3),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9, 8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn using_void_funtion_in_expression_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                let a : int = foo(4, "hello");
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                            AstNode::Text(
                                                Rc::new("hello".to_string()),
                                                Span::new(0,0,0)),
                                        ],
                                        Rc::new("foo".to_string()),
                                        Span::new(23,24,25))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        1, 2, 3),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(23, 24, 25),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn using_non_void_function_with_wrong_type_in_expression_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                let a : int = foo(4, "hello");
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::String,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                            AstNode::Text(
                                                Rc::new("hello".to_string()),
                                                Span::new(0,0,0)),
                                        ],
                                        Rc::new("foo".to_string()),
                                        Span::new(23,24,25))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        1, 2, 3),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(23, 24, 25),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn error_in_function_argument_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                foo(4-"abc", "hello");
            }

        }
        */
        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![
                                        AstNode::Minus(
                                            Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(4),
                                                    Span::new(0,0,0))),
                                            Box::new(AstNode::Text(
                                                Rc::new("abc".to_string()),
                                                Span::new(0,0,0))),
                                            ArithmeticInfo::new_alt(8, 4, 2),
                                        ),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            Span::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    Span::new(0,0,0)),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 4, 2),
                "".to_owned()),
            messages[0]);
    }


    #[test]
    fn function_parameter_shadowing_function_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {

            }

            extern fn bar(foo : int);
        */
        let mut func_info = FunctionInfo::new_alt(
                Rc::new("bar".to_string()),
                Type::String,
                7, 8 ,9);

        func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            8,12,14));

        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(
                            Rc::new("foo".to_string()),
                            Type::Void,
                            1, 2 ,3)),
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                        func_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(8, 12, 14),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn function_parameter_name_collision_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*

            fn bar(a : int, foo : int, a : int) {

            }
        */
        let mut func_info = FunctionInfo::new_alt(
                Rc::new("bar".to_string()),
                Type::String,
                7, 8 ,9);

         func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1,2,3));

        func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            8,12,14));

         func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            9,99,34));

        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                        func_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(9, 99, 34),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn extern_function_parameter_shadowing_function_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {

            }

            extern fn bar(foo : int);
        */
        let mut func_info = FunctionInfo::new_alt(
                Rc::new("bar".to_string()),
                Type::String,
                7, 8 ,9);

        func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            8,12,14));

        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(
                            Rc::new("foo".to_string()),
                            Type::Void,
                            1, 2 ,3)),
                    AstNode::ExternFunction(
                        func_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(8, 12, 14),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn extern_function_parameter_name_collision_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*

            extern fn bar(a : int, foo : int, a : int);
        */
        let mut func_info = FunctionInfo::new_alt(
                Rc::new("bar".to_string()),
                Type::String,
                7, 8 ,9);

         func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1,2,3));

        func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            8,12,14));

         func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            9,99,34));

        let mut node =
            AstNode::Block(vec![
                    AstNode::ExternFunction(
                        func_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(9, 99, 34),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn extern_function_redefinition_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() : void {

            }

            extern fn foo() : string;
        */
        let mut node =
            AstNode::Block(vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(
                            Rc::new("foo".to_string()),
                            Type::Void,
                            1, 2 ,3)),
                    AstNode::ExternFunction(
                        FunctionInfo::new_alt(
                            Rc::new("foo".to_string()),
                            Type::String,
                            7, 8 ,9)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(7, 8, 9),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 2, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn declaring_void_variable_is_reported() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo(a : int, b : string) : void {

            }

            fn bar() : void {
                let a : void = foo(4, "hello");
            }
        }
        */

        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Integer,
            0, 0 ,0);

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("a".to_string()),
                Type::Integer,
                1, 2, 3));

        foo_info.parameters.push(
            DeclarationInfo::new_alt(
                Rc::new("b".to_string()),
                Type::String,
                1, 2, 3));

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            Span::new(0, 0, 0))),
                    foo_info),
                    AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Integer(AstInteger::from(4), Span::new(0,0,0)),
                                            AstNode::Text(
                                                Rc::new("hello".to_string()),
                                                Span::new(0,0,0)),
                                        ],
                                        Rc::new("foo".to_string()),
                                        Span::new(0,0,0))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Void,
                                        99, 88, 77),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("bar".to_string()),
                        Type::Void,
                        0, 0 ,0)),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(99, 88, 77),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn void_function_parameter_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*

            extern fn bar(a : int, foo : void);

        */
        let mut func_info = FunctionInfo::new_alt(
                Rc::new("bar".to_string()),
                Type::String,
                7, 8 ,9);

         func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1,2,3));

        func_info.parameters.push(DeclarationInfo::new_alt(
            Rc::new("foo".to_string()),
            Type::Void,
            8,12,14));

        let mut node =
            AstNode::Block(vec![
                    AstNode::ExternFunction(
                        func_info),
                ],
                None,
                Span::new(0, 0,0)
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 12, 14),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn int_max_plus_one_is_reported() {

        let (reporter, mut checker) = create_sem_checker();

         let mut node = AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableDeclaration(
                                        Box::new(
                                            AstNode::Integer(
                                                AstInteger::IntMaxPlusOne,
                                                Span::new(9, 10, 11)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(9, 10, 11),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn integer_larger_than_i32_max_plus_one_generates_correct_ast() {

        let (reporter, mut checker) = create_sem_checker();

        let mut node = AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableDeclaration(
                                        Box::new(
                                            AstNode::Integer(
                                                AstInteger::Invalid(2147483649),
                                                Span::new(2, 3, 4)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );
        checker.check_semantics(&mut node);

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(2, 3, 4),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn binary_and_with_boolean_values_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = false;
                let b : bool = true && a;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Boolean(
                                false,
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::BooleanAnd(
                                Box::new(AstNode::Boolean(
                                    true,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_owned()),
                                    Span::new(9, 7, 2)
                                )),
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
               None,
               Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn binary_and_with_non_boolean_values_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = false;
                let b : bool = 5 && a;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Boolean(
                                false,
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::BooleanAnd(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(5),
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_owned()),
                                    Span::new(9, 7, 2)
                                )),
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
               None,
               Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 6, 3),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn binary_or_with_boolean_values_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = false;
                let b : bool = true || a;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Boolean(
                                false,
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::BooleanOr(
                                Box::new(AstNode::Boolean(
                                    true,
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_owned()),
                                    Span::new(9, 7, 2)
                                )),
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        )
                    ],
                                            None,
                                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn binary_or_with_non_boolean_values_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : bool = false;
                let b : bool = 5 && a;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Boolean(
                                false,
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::BooleanOr(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(5),
                                    Span::new(8, 6, 3)
                                )),
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_owned()),
                                    Span::new(9, 7, 2)
                                )),
                                Span::new(3, 4, 1)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 6, 3),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn binary_not_with_boolean_value_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let b : bool = !true;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Not(
                                Box::new(AstNode::Boolean(
                                    true,
                                    Span::new(8, 6, 3)
                                )),
                                Span::new(3, 4, 1),
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        )
                    ],
                                            None,
                                            Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 0);
    }

    #[test]
    fn binary_not_with_non_boolean_value_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let b : bool = !5;
            }
        */

        let mut node =
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Not(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(5),
                                    Span::new(8, 6, 3)
                                )),
                                Span::new(3, 4, 1),
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Boolean,
                                2, 2, 2),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0))),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )],
               None,
               Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 6, 3),
                "".to_owned()),
            messages[0]);
    }


    #[test]
    fn valid_array_declaration_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 0;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 0);
    }

    #[test]
    fn array_declaration_with_invalid_initialization_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 2.34f;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Float(
                                            2.34f32,
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 6, 3),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_declaration_with_negative_dimension_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[-1] = 0;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(-1)]))
                                    }
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(6, 5, 4),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_declaration_with_zero_dimension_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[0] = 0;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(0)]))
                                    }
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(6, 5, 4),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn non_indexed_array_assignment_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[2] = 0;
                a = a;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(2)]))
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::Identifier(
                                            Rc::new("a".to_string()),
                                            Span::new(33, 34, 35),
                                        )
                                    ),
                                    Rc::new("a".to_string()),
                                    Span::new(34, 67, 32),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(34, 67, 32),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(6, 5, 4),
                "".to_owned()),
            messages[1]);
    }


    #[test]
    fn valid_integer_array_assignment_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 0;
                a[0] = 4;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(4),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 0);
    }

    #[test]
    fn valid_boolean_array_assignment_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = true;
                a[0] = false;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Boolean(
                                           true,
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::BooleanArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Boolean(
                                            false,
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 0);
    }



    #[test]
    fn array_assignment_with_undeclared_array_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                a[0] = 4;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(4),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(9, 8, 7),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_assignment_using_function_as_array_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                foo[0] = 4;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(4),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("foo".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 7, 8, 6)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9, 8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(7, 8, 6),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn array_assignment_using_regular_variable_as_array_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int = 0;
                a[0] = 4;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::Integer,
                                        span: Span::new(6, 5, 4),
                                        extra_info: None,
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(4),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(9, 8, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(6, 5, 4),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn type_error_in_array_assignment_index_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 0;
                a[0+"test"] = 4;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Plus(
                                            Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(0),
                                                    Span::new(8, 6, 3)
                                                )
                                        ),
                                        Box::new(
                                            AstNode::Text(
                                                Rc::new("test".to_owned()),
                                                Span::new(6,5,4),
                                            )
                                        ),
                                        ArithmeticInfo::new_alt(1, 3, 4),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(4),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(1, 3, 4),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_in_array_assignment_assignment_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 0;
                a[0] = 4 + "test";
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(0),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Plus(
                                            Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(4),
                                                    Span::new(8, 6, 3)
                                                )
                                            ),
                                            Box::new(
                                                AstNode::Text(
                                                    Rc::new("test".to_owned()),
                                                    Span::new(6,5,4),
                                                )
                                            ),
                                            ArithmeticInfo::new_alt(5, 3, 1),
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(5, 3, 1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn non_integer_array_index_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 0;
                a[12.3f] = 4;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Float(
                                            12.3f32,
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(4),
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(98, 87, 76),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_in_array_assignment_is_reported_if_array_type_and_assignment_type_are_not_correct() {
        let (reporter, mut checker) = create_sem_checker();

        /*
            fn foo() {
                let a : int[4] = 0;
                a[3] = 4.2;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::ArrayAssignment {
                                    index_expression: Box::new(
                                        AstNode::Integer(
                                            AstInteger::Int(3),
                                            Span::new(98, 87, 76),
                                        )
                                    ),
                                    assignment_expression: Box::new(
                                        AstNode::Double(
                                            4.2f64,
                                            Span::new(12, 23, 34)
                                        )
                                    ),
                                    variable_name: Rc::new("a".to_owned()),
                                    span: Span::new(9, 8, 7),
                                },
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(12, 23, 34),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(6, 5, 4),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn valid_array_access_is_accepted() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = a[0];
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::ArrayAccess {
                                            index_expression: Box::new(
                                              AstNode::Integer(
                                                  AstInteger::from(0),
                                                  Span::new(8, 1, 3),
                                              )
                                            ),
                                            indexable_expression: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("a".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            )
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 0);
    }

    #[test]
    fn array_access_on_undeclared_identifier_is_rejected() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = abc[0];
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::ArrayAccess {
                                            index_expression: Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(0),
                                                    Span::new(8, 1, 3),
                                                )
                                            ),
                                            indexable_expression: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("abc".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::NameError,
                Span::new(3, 8, 2),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_access_with_method_identifier_is_rejected() {

        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = foo[0];
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::ArrayAccess {
                                            index_expression: Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(0),
                                                    Span::new(8, 1, 3),
                                                )
                                            ),
                                            indexable_expression: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("foo".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 54, 3)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3, 8, 2),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 54, 3),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn array_access_with_regular_variable_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = b[0];
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::ArrayAccess {
                                            index_expression: Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(0),
                                                    Span::new(8, 1, 3),
                                                )
                                            ),
                                            indexable_expression: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("b".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 54, 3)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3, 8, 2),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_access_with_non_integer_index_expression_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = a[34.2];
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::ArrayAccess {
                                            index_expression: Box::new(
                                                AstNode::Float(
                                                    34.2f32,
                                                    Span::new(8, 1, 3),
                                                )
                                            ),
                                            indexable_expression: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("a".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);


        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 1, 3),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_access_in_expression_with_incompatible_type_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : float = 2.32f;
                b = a[3];
            }
        */
        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Float(
                                            2.3f32,
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Float,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::ArrayAccess {
                                            index_expression: Box::new(
                                                AstNode::Integer(
                                                    AstInteger::from(3),
                                                    Span::new(8, 1, 3),
                                                )
                                            ),
                                            indexable_expression: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("a".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3, 8, 2),
                "".to_owned()),
            messages[0]);
        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(1, 5, 9),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn using_array_length_property_is_accepted() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = a.length;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::MemberAccess {
                                            object: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("a".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                            member: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("length".to_owned()),
                                                    Span::new(8, 7, 1),
                                                )
                                            ),
                                            span: Span::new(6, 4, 1),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 0);
    }

    #[test]
    fn array_length_property_on_non_array_variable_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = b.length;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::MemberAccess {
                                            object: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("b".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                            member: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("length".to_owned()),
                                                    Span::new(8, 7, 1),
                                                )
                                            ),
                                            span: Span::new(6, 4, 1),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 7, 1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_length_property_on_function_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = foo.length;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::MemberAccess {
                                            object: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("foo".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                            member: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("length".to_owned()),
                                                    Span::new(8, 7, 1),
                                                )
                                            ),
                                            span: Span::new(6, 4, 1),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 8, 4, 7)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 2);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(3, 8, 2),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(8, 4, 7),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn invalid_property_on_array_is_rejected() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = a.bar;
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::MemberAccess {
                                            object: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("a".to_owned()),
                                                    Span::new(3, 8, 2)
                                                )
                                            ),
                                            member: Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("bar".to_owned()),
                                                    Span::new(8, 7, 1),
                                                )
                                            ),
                                            span: Span::new(6, 4, 1),
                                        }
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 7, 1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn type_error_when_using_array_length_property_in_expression_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
        /*
            fn foo() {
                let a : int[4] = 0;
                let b : int = 4;
                b = a.length + "foo";
            }
        */

        let mut node =
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(8, 6, 3)
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("a".to_string()),
                                        variable_type: Type::IntegerArray,
                                        span: Span::new(6, 5, 4),
                                        extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::Int(4)]))
                                    }
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(0),
                                            Span::new(6, 2, 8),
                                        )
                                    ),
                                    DeclarationInfo {
                                        name: Rc::new("b".to_owned()),
                                        variable_type: Type::Integer,
                                        span: Span::new(1, 5, 9),
                                        extra_info: None
                                    }
                                ),
                                AstNode::VariableAssignment(
                                    Box::new(
                                        AstNode::Plus(
                                            Box::new(
                                                AstNode::MemberAccess {
                                                    object: Box::new(
                                                        AstNode::Identifier(
                                                            Rc::new("a".to_owned()),
                                                            Span::new(3, 8, 2)
                                                        )
                                                    ),
                                                    member: Box::new(
                                                        AstNode::Identifier(
                                                            Rc::new("bar".to_owned()),
                                                            Span::new(8, 7, 1),
                                                        )
                                                    ),
                                                    span: Span::new(6, 4, 1),
                                                }
                                            ),
                                            Box::new(
                                                AstNode::Text(
                                                    Rc::new("foo".to_owned()),
                                                    Span::new(5, 6, 7),
                                                ),
                                            ),
                                            ArithmeticInfo::new_alt(3, 6, 1),
                                        )
                                    ),
                                    Rc::new("b".to_owned()),
                                    Span::new(3, 1, 8),
                                )
                            ],
                            None,
                            Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            );

        checker.check_semantics(&mut node);
        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.reports(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(8, 7, 1),
                "".to_owned()),
            messages[0]);
    }
}
