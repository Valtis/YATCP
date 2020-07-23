use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, Span as Span, DeclarationInfo, ExtraDeclarationInfo};

use crate::symbol_table::{SymbolTable, Symbol, TableEntry};

use crate::error_reporter::{ReportKind, ErrorReporter};

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;
use std::cell::RefCell;

use std::collections::HashMap;
use crate::variable_attributes::VariableAttribute;


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
    constant_initializer_stack: Vec<HashMap<String, AstNode>>,
}

impl SemanticsCheck {
    pub fn new(reporter: Rc<RefCell<dyn ErrorReporter>>) -> SemanticsCheck {
        SemanticsCheck {
            errors: 0,
            symbol_table: SymbolTable::new(),
            error_reporter: reporter,
            id_counter: 0,
            enclosing_function_stack: vec![],
            constant_initializer_stack: vec![],
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
        self.check_and_gather_top_level_symbols(node);
        self.do_check(node);

        ice_if!(
            !self.enclosing_function_stack.is_empty(),
            "Enclosing function stack is not empty after semantic checks");

        ice_if!(
            !self.constant_initializer_stack.is_empty(),
            "Constant initializer stack is not empty after semantic checks");
        self.errors
    }

    // only process function declarations initially, so that we can call functions that appear later
    fn check_and_gather_top_level_symbols(&mut self, node: &mut AstNode) {
        self.symbol_table.push_empty();
        if let AstNode::Block{ref mut statements, ref mut block_symbol_table_entry , ..} = node {

            for statement in statements.iter_mut() {
                match statement {
                    AstNode::Function{ ref function_info, ..} |
                    AstNode::ExternFunction{ ref function_info } => {
                        self.check_function_declaration(function_info);

                    }
                    _ => (), // don't care right now
                }
            }
            // check parameters after all function decalrations, so that parameters shadowing a function that appears later
            // are reported correctly
            for statement in statements.iter_mut() {
                match statement {
                    AstNode::Function{ ref function_info, .. }|
                    AstNode::ExternFunction{ ref function_info}=>
                        self.check_function_parameter_list(function_info),
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
            AstNode::Block{ statements, block_symbol_table_entry, span} =>
                self.handle_block(statements, block_symbol_table_entry, span),
            AstNode::Function{block,  function_info } =>
                self.handle_function(block, function_info),
            AstNode::ExternFunction{..} => (), // do nothing, handled in the initial function definition pass
            AstNode::FunctionCall{arguments, function_name, span} =>
                self.handle_function_call(arguments, function_name, span),
            AstNode::VariableDeclaration{initialization_expression, declaration_info} =>
                self.handle_variable_declaration(initialization_expression, declaration_info),
            AstNode::VariableAssignment{expression, name, span} =>
                self.handle_variable_assignment(expression, name, span),
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
            AstNode::Plus{ .. } |
            AstNode::Minus { .. } |
            AstNode::Multiply { .. } |
            AstNode::Divide { .. } |
            AstNode::Modulo { .. } =>
                self.handle_arithmetic_operation_with_operator_type_check(node),
            AstNode::Negate{ expression, arithmetic_info } =>
                self.handle_negation(expression, arithmetic_info),
            AstNode::Return{ return_value, arithmetic_info } =>
                self.handle_return(return_value, arithmetic_info),
            AstNode::While{ condition_expression, block, span} =>
                self.handle_while(condition_expression, block, span),
            AstNode::If{ condition_expression, main_block, else_block, span } =>
                self.handle_if(condition_expression, main_block, else_block, span),
            AstNode::Less{ left_expression, right_expression, span} |
            AstNode::LessOrEq{ left_expression, right_expression, span} |
            AstNode::Equals{ left_expression, right_expression, span} |
            AstNode::NotEquals{ left_expression, right_expression, span} |
            AstNode::GreaterOrEq{ left_expression, right_expression, span} |
            AstNode::Greater{ left_expression, right_expression, span} =>
                self.handle_comparison_operation(left_expression, right_expression, span),
            AstNode::BooleanAnd{ left_expression, right_expression, span} |
            AstNode::BooleanOr{left_expression, right_expression, span} =>
                self.check_boolean_and_or(left_expression, right_expression, span),
            AstNode::BooleanNot{expression, span} =>
                self.check_boolean_not(expression, span),
            AstNode::Integer{ value, span } => self.check_for_overflow(value, span),
            AstNode::Float{ .. } => (),
            AstNode::Double{ .. }  => (),
            AstNode::Text{ .. } => (),
            AstNode::Boolean{ .. } => (),
            AstNode::Identifier{ name, span } => {
                self.check_identifier_is_declared(name,  span);
            }
            AstNode::ArrayAccess{ index_expression, indexable_expression } => {
                 self.handle_array_access(index_expression, indexable_expression);
            },
            AstNode::ErrorNode => {},
        }
    }

    fn handle_block(
        &mut self,
        children: &mut Vec<AstNode>,
        tab_ent: &mut Option<TableEntry>,
        _span: &Span) {

        self.symbol_table.push_empty();
        self.constant_initializer_stack.push(HashMap::new());
        for ref mut child in children {
            self.do_check(child);
        }

        let entry = self.symbol_table.pop();
        self.constant_initializer_stack.pop();
        if let Some(ref mut main_entries) = tab_ent {
            if let Some(entries) = entry {
                for symbol in entries.symbols() {
                    if let None = main_entries.find_symbol(symbol.name()) {
                        main_entries.add_symbol(symbol.clone());
                    }
                }
            }

        } else {
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
        variable_info: &mut DeclarationInfo) {

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

                self.check_constness_init(child, variable_info);

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


            let dims = if let Some(ExtraDeclarationInfo::ArrayDimension(ref mut x)) = variable_info.extra_info {
                x
            } else {
                ice!("Variable {} has type {}, which is an array type, but array dimension info is not set", variable_info.name, variable_info.variable_type);
            };

            let dimension_span = variable_info.span.clone();

            for dim in dims.iter_mut() {
                if self.is_constant(dim) {

                    if self.get_type(dim) != Type::Integer {
                        self.report_error(
                            ReportKind::TypeError,
                            dim.span(),
                            "Array dimension must be an integer constant".to_owned()
                        );
                        continue;
                    }

                    let expr = self.get_constant_initializer_expression(dim);
                    ice_if!(!self.is_constant(&expr), "Inconsistent constant value tracking");

                    if let AstNode::Integer { ref value, .. } = expr {
                        match value {
                            AstInteger::IntMaxPlusOne |
                            AstInteger::Invalid(_) => {

                                self.report_error(
                                    ReportKind::TypeError,
                                    dimension_span.clone(),
                                    "Array has invalid dimensions".to_owned(),
                                );

                            }
                            AstInteger::Int(x) if *x <= 0 => {

                                self.report_error(
                                    ReportKind::TypeError,
                                    dimension_span.clone(),
                                    "Array has invalid dimensions".to_owned(),
                                );
                                break;
                            }
                            _ => *dim = expr,
                        }
                    } else {
                        // this can happen if constant is initialized with bad expression. Ignore
                    }

                } else {
                    self.report_error(
                        ReportKind::TypeError,
                        dim.span(),
                        "Array dimension must be a constant".to_owned()
                    );
                }
            }

        } else if variable_info.variable_type != child_type &&
            child_type != Type::Invalid && variable_info.variable_type != Type::Invalid {
            self.report_type_error(variable_info, child, child_type);
        }
    }

    fn check_constness_init(
        &mut self,
        init_expression: &mut AstNode,
        declaration_info: &DeclarationInfo) {

        if !declaration_info.attributes.contains(&VariableAttribute::Const) {
            return;
        }

        if self.is_constant(init_expression) {
            ice_if!(self.constant_initializer_stack.len() == 0, "Empty constant initializer stack");
            let last = self.constant_initializer_stack.len() - 1;
            self.constant_initializer_stack[last]
                .insert(declaration_info.name.to_string(), init_expression.clone());
        } else {
            self.report_error(
               ReportKind::TypeError,
               declaration_info.span,
               "Cannot initialize constant variable with non-constant initializer".to_owned()
           );
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
            } else {
                self.check_constness(&span, sym_info);
            }

        } else {
            // should not happen, only variables should be returned
            ice!(
                "Non-variable symbol '{:#?}' returned when variable expected",
                symbol);
        }
    }

    fn check_constness(
        &mut self,
        assignment_span: &Span,
        declaration_info: &DeclarationInfo) {
       if declaration_info.attributes.contains(&VariableAttribute::ReadOnly) ||
           declaration_info.attributes .contains(&VariableAttribute::Const) {
           self.report_error(
               ReportKind::TypeError,
               *assignment_span,
               "Cannot assign to read only variable".to_owned()
           );

           self.report_error(
               ReportKind::Note,
               declaration_info.span,
               format!("Variable '{}', declared here, has been declared as read only", declaration_info.name)
           );
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

        let name = if let AstNode::Identifier{ref name, ..} = member {
            name.clone()
        } else {
            ice!("Unexpected AST node '{:?}' for property", member)
        };

        match member {
            AstNode::Identifier{ref name, ..} if self.get_type(object).is_array() => {
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

    let (ref valid_types, ref mut arithmetic_info) = match *node {
            AstNode::Plus{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } => {
                self.handle_arithmetic_node(left_expression, right_expression, arithmetic_info);
                (vec![
                    Type::Integer,
                    Type::Float,
                    Type::Double,
                    Type::String,
                    Type::Invalid],
                 arithmetic_info)
            },
            AstNode::Minus{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } |
            AstNode::Multiply{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } |
            AstNode::Divide{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } => {
                self.handle_arithmetic_node(left_expression, right_expression, arithmetic_info);
                if let AstNode::Integer{value: AstInteger::Int(0), ..} = **right_expression {
                       self.report_error(
                           ReportKind::Warning,
                           arithmetic_info.span.clone(),
                           "Division by zero".to_owned(),
                       )
                };

                (vec![
                    Type::Integer,
                    Type::Float,
                    Type::Double,
                    Type::Invalid],
                    arithmetic_info)
            },
            AstNode::Modulo{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } => {

                self.handle_arithmetic_node(left_expression, right_expression, arithmetic_info);

                if let AstNode::Integer{ value: AstInteger::Int(0), .. } = **right_expression {
                        self.report_error(
                            ReportKind::Warning,
                            arithmetic_info.span.clone(),
                            "Division by zero".to_owned(),
                        )
                };
                (vec![
                    Type::Integer,
                    Type::Invalid],
                 arithmetic_info)
            },
            _ => ice!(
                "Incorrect node passed to arithmetic node type checking: {}",
                node)
        };

        if !valid_types.iter().any(|t| *t == arithmetic_info.node_type) {
            self.report_error(
                ReportKind::TypeError,
                arithmetic_info.span.clone(),
                format!("Operands of type '{}' are not valid for this operator",
                        arithmetic_info.node_type));
            arithmetic_info.node_type = Type::Invalid;
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
        match node {
            AstNode::Integer{ .. } => Type::Integer,
            AstNode::Float{ .. } => Type::Float,
            AstNode::Double{ .. } => Type::Double,
            AstNode::Boolean{ .. } => Type::Boolean,
            AstNode::Text{ .. } => Type::String,
            AstNode::Less{ .. } |
            AstNode::LessOrEq{ .. } |
            AstNode::Equals{ .. } |
            AstNode::NotEquals{ .. } |
            AstNode::GreaterOrEq{ .. } |
            AstNode::Greater{ .. } => Type::Boolean,
            AstNode::Plus{ arithmetic_info, .. } |
            AstNode::Minus{  arithmetic_info, .. } |
            AstNode::Multiply{  arithmetic_info, .. } |
            AstNode::Divide{  arithmetic_info, .. } |
            AstNode::Modulo{  arithmetic_info, .. } => arithmetic_info.node_type.clone(),
            AstNode::Negate{  arithmetic_info, .. } => arithmetic_info.node_type.clone(),
            AstNode::Identifier{ name, ..} => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    info.variable_type.clone()
                } else {
                    Type::Invalid
                }
            },
            AstNode::FunctionCall{ function_name, ..} => {
                if let Some(Symbol::Function(ref info)) = self.symbol_table.find_symbol(function_name) {
                    info.return_type.clone()
                } else {
                    Type::Invalid
                }
            },
            AstNode::BooleanAnd { .. } |
            AstNode::BooleanOr { .. } |
            AstNode::BooleanNot { .. } => Type::Boolean,
            AstNode::ArrayAccess {
                index_expression: _,
                ref indexable_expression,
            } => {
                match **indexable_expression {
                    AstNode::Identifier{ref name, ..} => {
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
                    AstNode::FunctionCall{ .. } => todo!("Indexing function return value is not yet implemented"),
                    _ => Type::Invalid,
                }
            },
            AstNode::MemberAccess{ object, member, span: _ } => {
                if self.get_type(object).is_array() {
                    if let AstNode::Identifier{ref name, ..} = **member {
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

    fn is_constant(&self, node: &AstNode) -> bool {
        match node {
            // TODO - If constant folding is always applied, could also consider arithmetic expressions
            AstNode::Integer{ .. } => true,
            AstNode::Float{ .. } => true ,
            AstNode::Double{ .. } => true ,
            AstNode::Boolean{ .. } => true,
            AstNode::Text{ .. } => true,
            AstNode::Negate { expression, .. } => self.is_constant(expression),
            AstNode::Identifier { name, .. } => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                  info.attributes.contains(&VariableAttribute::Const)
                } else {
                    true // we pretend that undeclared symbols are constant ones, to suppress any extra errors we may see
                }
            },
            AstNode::BooleanNot { expression , ..} => self.is_constant(expression),
            AstNode::ArrayAccess { index_expression, indexable_expression }
                => self.is_constant(index_expression) && self.is_constant(indexable_expression),
            _ => false,
        }
    }

    fn get_constant_initializer_expression(&self, node: &AstNode) -> AstNode {
        ice_if!(!self.is_constant(node), "Constant initializer requested for non-constant expression {}", node);
        match node {
            AstNode::Integer{ .. } => node.clone(),
            AstNode::Float{ .. } => node.clone() ,
            AstNode::Double{ .. } => node.clone() ,
            AstNode::Boolean{ .. } => node.clone(),
            AstNode::Text{ .. } => node.clone(),
            AstNode::Identifier{ name, ..} => {
                let node = self.get_constant_initializer_stack_entry(name);
                self.get_constant_initializer_expression(&node)
            },
            _ => ice!("Non-constant node {}", node),
        }
    }

    fn get_constant_initializer_stack_entry(&self, name: &String) -> AstNode {
        for level in (0..self.constant_initializer_stack.len()).rev() {
            match self.constant_initializer_stack[level].get(name) {
                Some(x) => return x.clone(),
                None => (),
            }
        }

        ice!("No entry {} in constant initializer stack", name)
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
