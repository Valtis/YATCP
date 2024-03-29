use super::ast::{AstNode, AstByte, AstShort, AstInteger, AstLong};

use crate::common::{
    variable_attributes::VariableAttribute,
    types::Type,
    node_info::*,
    error_reporter::{ErrorReporter, ReportKind},
    symbol_table::{SymbolTable, Symbol, TableEntry},
};

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

pub const ARRAY_LENGTH_PROPERTY: &'static str = "length";

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
            // check parameters after all function declarations, so that parameters shadowing a function that appears later
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
            AstNode::VariableDeclaration{initialization_expression, declaration_info} => {
                self.handle_variable_declaration(initialization_expression, declaration_info);
                // Compile time constant - remove from tree
                if declaration_info.attributes.contains(&VariableAttribute::Const) {
                    *node = AstNode::EmptyNode;
                }
            },
            AstNode::VariableAssignment{expression, name, span} =>
                self.handle_variable_assignment(expression, name, span),
            AstNode::ArrayDeclaration { initialization_expression, dimensions, declaration_info } => {
                self.handle_array_variable_declaration(initialization_expression, dimensions, declaration_info);
                if declaration_info.attributes.contains(&VariableAttribute::Const) {
                    *node = AstNode::EmptyNode;
                }
            },
            AstNode::ArrayAssignment {
                index_expression,
                assignment_expression,
                variable_name,
                span,
            } => self.handle_array_assignment(index_expression, assignment_expression, variable_name, span),
            AstNode::InitializerList { values, list_type, span } =>
                self.handle_initializer_list(values, list_type, span),
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
            AstNode::Loop { condition_expression, post_body_statements, block, span} =>
                self.handle_loop(condition_expression, post_body_statements, block, span),
            AstNode::If{ condition_expression, main_block, else_block, span } =>
                self.handle_if(condition_expression, main_block, else_block, span),
            AstNode::Less{ left_expression, right_expression, span} |
            AstNode::LessOrEq{ left_expression, right_expression, span} |
            AstNode::NotEquals{ left_expression, right_expression, span} |
            AstNode::GreaterOrEq{ left_expression, right_expression, span} |
            AstNode::Greater{ left_expression, right_expression, span} =>
                self.handle_comparison_operation(left_expression, right_expression, span, false),
            AstNode::Equals{ left_expression, right_expression, span} =>
                self.handle_comparison_operation(left_expression, right_expression, span, true),
            AstNode::BooleanAnd{ left_expression, right_expression, span} |
            AstNode::BooleanOr{left_expression, right_expression, span} =>
                self.check_boolean_and_or(left_expression, right_expression, span),
            AstNode::BooleanNot{expression, span} =>
                self.check_boolean_not(expression, span),
            AstNode::BitwiseAnd{ left_expression, right_expression, arithmetic_info } |
            AstNode::BitwiseOr { left_expression, right_expression, arithmetic_info } |
            AstNode::BitwiseXor { left_expression, right_expression, arithmetic_info } =>
                self.check_bitwise_and_or_xor(left_expression, right_expression, arithmetic_info),
            AstNode::BitwiseNot { expression, arithmetic_info } =>
                self.check_bitwise_not(expression, arithmetic_info),
            AstNode::ArithmeticShiftRight{  value, shift_count, arithmetic_info } |
            AstNode::LogicalShiftLeft { value, shift_count, arithmetic_info  } |
            AstNode::LogicalShiftRight { value, shift_count, arithmetic_info } =>
                self.handle_shift_expression(value, shift_count, arithmetic_info),
            AstNode::Long{ value, span } => self.check_long_for_overflow(value, span),
            AstNode::Integer{ value, span } => self.check_integer_for_overflow(value, span),
            AstNode::Short{ value, span } => self.check_short_for_overflow(value, span),
            AstNode::Byte{ value, span } => self.check_byte_for_overflow(value, span),
            AstNode::Float{ .. } => (),
            AstNode::Double{ .. }  => (),
            AstNode::Text{ .. } => (),
            AstNode::Boolean{ .. } => (),
            AstNode::Identifier{ name, span } => {
                self.check_identifier_is_declared(name, span);
            },
            AstNode::ArrayAccess{ index_expression, indexable_expression } =>
                 self.handle_array_access(index_expression, indexable_expression),
            AstNode::ArraySlice{ start_expression, end_expression, array_expression } =>
                self.handle_array_slice(start_expression, end_expression, array_expression),
            AstNode::Cast{ expression, target_type, span } => {
                self.handle_cast(expression, target_type, span);
            },
            AstNode::Break(_) |
            AstNode::Continue(_) |
            AstNode::IntegralNumber { .. } |
            AstNode::EmptyNode |
            AstNode::ErrorNode => (),
        }

        if let Some(replacement_node) = self.propagate_constants(node) {
            let replacement_node = match (&replacement_node, &node) {
                (AstNode::InitializerList { values, .. },
                    AstNode::ArrayAccess { index_expression, ..}) => {

                    ice_if!(!self.is_constant(index_expression), "Index expression must be constant when propagating constants");

                    let index = match &**index_expression {
                        AstNode::Integer { value, .. } => {
                            match value {
                                AstInteger::Int(value) => *value,
                                AstInteger::Invalid(_) => return,
                            }
                        },
                        _ => ice!("Bad expression: {}", index_expression),
                    };

                    if index < 0 || index >= values.len() as i32 {
                        self.report_error(
                            ReportKind::TypeError,
                            index_expression.span(),
                            "Index expression is out of bounds".to_owned());
                        return;
                    }
                    values[index as usize].clone()
                },
                (_, _) => replacement_node,
            };

            let old_span = node.span();
            *node = replacement_node;
            if let Some(span) = node.span_ref_mut() {
                *span = old_span;
            }
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

        for arg in args.into_iter() {
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

                                    // constness checks
                                    match arg {
                                        AstNode::Identifier { ref name, .. } => {
                                            if let Some(Symbol::Variable(decl_info, _)) = self.symbol_table.find_symbol(name) {

                                                // unconditionally error on const arrays.
                                                //
                                                // This is primarily due to aggressive constant folding,
                                                // const values do not actually exist and their values get folded into
                                                // other expressions.
                                                if decl_info.attributes.contains(&VariableAttribute::Const) {
                                                    self.report_error(
                                                        ReportKind::TypeError,
                                                        arg.span(),
                                                        "Cannot use const array as function argument".to_owned()
                                                    );

                                                    self.report_error(
                                                        ReportKind::Note,
                                                        decl_info.span,
                                                        format!("Array '{}', declared here, is const array", name)
                                                    );
                                                }


                                                // report readonly array use when method parameter is not readonly

                                                if decl_info.attributes.contains(&VariableAttribute::ReadOnly) &&
                                                    !param.attributes.contains(&VariableAttribute::ReadOnly) {
                                                    self.report_error(
                                                        ReportKind::TypeError,
                                                        arg.span(),
                                                        "Cannot use readonly array here, mutability differs".to_owned()
                                                    );

                                                    self.report_error(
                                                        ReportKind::Note,
                                                        param.span,
                                                        format!("Corresponding parameter '{}', declared here, is mutable", param.name)
                                                    );
                                                     self.report_error(
                                                        ReportKind::Note,
                                                        decl_info.span,
                                                        format!("Array '{}', declared here, is immutable", decl_info.name)
                                                    );
                                                }
                                            }
                                        },
                                        AstNode::ArraySlice {
                                            array_expression,
                                            ..
                                        } => {

                                            if let AstNode::Identifier { ref name, .. } = **array_expression {
                                                if let Some(Symbol::Variable(decl_info, _)) = self.symbol_table.find_symbol(name) {
                                                    // report error if underlying array we are slicing from is readonly but
                                                    // corresponding parameter is not
                                                    if decl_info.attributes.contains(&VariableAttribute::ReadOnly) &&
                                                        !param.attributes.contains(&VariableAttribute::ReadOnly) {
                                                        self.report_error(
                                                            ReportKind::TypeError,
                                                            arg.span(),
                                                            "Cannot use readonly array slice here, mutability differs".to_owned()
                                                        );

                                                        self.report_error(
                                                            ReportKind::Note,
                                                            param.span,
                                                            format!("Corresponding parameter '{}', declared here, is mutable", param.name)
                                                        );
                                                        self.report_error(
                                                            ReportKind::Note,
                                                            decl_info.span,
                                                            format!("Array '{}', declared here, is immutable", decl_info.name)
                                                        );
                                                    }
                                                }
                                            }
                                        },

                                        _ => ice!("Unexpectedly failed to match the correct AST node"),
                                    }

                                    continue;
                                }
                            }

                            if arg_type != param.variable_type &&
                                arg_type != Type::Invalid &&
                                param.variable_type != Type::Invalid {

                                let conversion_result = self.perform_type_conversion(&param.variable_type, arg);

                                if conversion_result != ConversionResult::Converted {
                                    self.report_error(
                                        ReportKind::TypeError,
                                        arg.span(),
                                        format!("Got argument of type '{}' when '{}' was expected",
                                                arg_type.clone(),
                                                param.variable_type,
                                        ));

                                    self.report_error(
                                        ReportKind::Note,
                                        param.span.clone(),
                                        "Corresponding parameter declared here"
                                            .to_string());

                                    if conversion_result == ConversionResult::CastRequired {
                                        self.report_conversion_note(
                                            &param.variable_type,
                                            &arg_type,
                                            &arg.span()
                                        );
                                    }
                                }
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
        declaration_info: &mut DeclarationInfo) {

        self.do_check(child);
        let child_type = self.get_type(child);

        if declaration_info.variable_type == Type::Uninitialized {
            ice_if!(child_type == Type::Uninitialized, "Uninitialized child type when performing type checks");

            if child_type.is_invalid() {
                declaration_info.variable_type = Type::Invalid;
            } else {
                if child_type.is_array() {
                    self.report_error(
                        ReportKind::TypeError,
                        child.span(),
                        format!("'{}' is not a legal expression type for type inference", child_type));
                    return;
                }


                if child_type == Type::IntegralNumber {
                    declaration_info.variable_type = Type::Integer;
                } else {
                    if let Type::InitializerList(_) = child_type {
                        ice!("Unexpected array initializer in variable declaration");
                    } else {
                        declaration_info.variable_type = child_type.clone();
                    }
                }
            }
        }

        self.check_declaration_validity(child, declaration_info);


        ice_if!(declaration_info.variable_type.is_array(), "Array present in non-array declaration");

        if declaration_info.variable_type == Type::Void {
            self.report_error(
                ReportKind::TypeError,
                declaration_info.span.clone(),
                "Variable may not have type 'Void'".to_string());
        } else if declaration_info.variable_type != child_type &&
            child_type != Type::Invalid && declaration_info.variable_type != Type::Invalid {

            let conversion_result =  self.perform_type_conversion(&declaration_info.variable_type, child);
            if conversion_result == ConversionResult::Converted {
                return;
            }

            self.report_type_error(declaration_info, child, child_type.clone());

            if conversion_result == ConversionResult::CastRequired {
                self.report_conversion_note(
                    &declaration_info.variable_type,
                    &child_type,
                    &child.span());
            }
        }
    }


    fn handle_array_variable_declaration(
        &mut self,
        child: &mut AstNode,
        dimensions: &mut Vec<AstNode>,
        declaration_info: &mut DeclarationInfo) {

        self.do_check(child);

        let (mut child_type, original_is_initializer) = if let AstNode::InitializerList { list_type: Type::InitializerList(inner), ..} = child {
            (*inner.clone(), true)
        } else {
            (self.get_type(child), false)
        };

        // array declaration using the initializer list
        if declaration_info.variable_type == Type::Uninitialized {
             ice_if!(child_type == Type::Uninitialized, "Uninitialized child type when performing type checks!");

            if child_type == Type::IntegralNumber {
                declaration_info.variable_type = Type::Array(Box::new(Type::Integer), vec![]);
            } else {
                if let Type::InitializerList(_) = child_type {
                    ice!("Unexpected array initializer, expected to be unwrapped already");
                } else {

                    declaration_info.variable_type = Type::Array(Box::new(child_type.clone()), vec![]);
                }
            }
        }

        // Syntactic sugar: If assigning string into byte array, convert string into byte initializer list
        if declaration_info.variable_type.get_array_basic_type() == Type::Byte && child_type == Type::String && !original_is_initializer {
            let text = if let AstNode::Text { value, .. } = child {
                value.clone()
            } else {
                ice!("Unexpected node '{}' when Text node was expected", child);
            };

            let values = text.bytes().map(|byte| AstNode::Byte {
                value: AstByte::Byte(byte as i8),
                span: Span::new(0, 0, 0),
            }).collect::<Vec<_>>();

            *child = AstNode::InitializerList {
                values,
                list_type: Type::InitializerList(Box::new(Type::Byte)),
                span: child.span(),
            };

            child_type = Type::Byte;
        }


        if declaration_info.variable_type == Type::Invalid {
            return;
        }

        ice_if!(!declaration_info.variable_type.is_array(), "Non-array type '{}' in array declaration", declaration_info.variable_type);

        if declaration_info.variable_type == Type::Void {
            self.report_error(
                ReportKind::TypeError,
                declaration_info.span.clone(),
                "Array may not have type 'Void'".to_string());
            return;
        }



        if declaration_info.variable_type.get_array_basic_type() != child_type && child_type != Type::Invalid {
            let conversion_result = self.perform_type_conversion(
                &declaration_info.variable_type.get_array_basic_type(),  child);

            if conversion_result != ConversionResult::Converted {
                self.report_error(
                    ReportKind::TypeError,
                    child.span(),
                    format!("Expected '{}' but got '{}' instead", declaration_info.variable_type.get_array_basic_type(), child_type.clone())
                );

                if conversion_result == ConversionResult::CastRequired {
                    self.report_conversion_note(
                        &declaration_info.variable_type.get_array_basic_type(), &child_type, &child.span());
                }
            }
        }

        let dimension_span = declaration_info.span.clone();
        let mut dimension_constants = vec![];
        for dim in dimensions.into_iter() {
            if self.is_constant(dim) {

                self.do_check(dim);

                let dimension_type = self.get_type(dim);
                if dimension_type != Type::IntegralNumber && dimension_type != Type::Integer {
                    self.report_error(
                        ReportKind::TypeError,
                        dim.span(),
                        format!("Array dimension must be an integer constant (now {})", self.get_type(dim))
                    );
                    continue;
                }

                if dimension_type == Type::IntegralNumber {
                    let conversion_result = self.perform_type_conversion(&Type::Integer, dim);
                    ice_if!(conversion_result != ConversionResult::Converted, "Unexpected conversion failure");
                }

                let expr = self.get_constant_initializer_expression(dim);
                ice_if!(!self.is_constant(&expr), "Inconsistent constant value tracking");

                if let AstNode::Integer { ref value, .. } = expr {
                    match value {
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
                                format!("Array index negative or zero ({})", x)
                            );
                            continue;
                        }
                        AstInteger::Int(x) => {
                            dimension_constants.push(*x);
                            *dim = expr;
                        }
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


        if let AstNode::InitializerList {ref values, span, .. } = child {
            // TODO: Support multiple dimensions
            if dimension_constants.is_empty() {
                dimensions.push(AstNode::Integer {
                    value: AstInteger::Int(values.len() as i32),
                    span: Span::new(0, 0, 0)
                });
                dimension_constants.push(values.len() as i32);
            } else if dimension_constants.len() == 1 {
                if dimension_constants[0] != values.len() as i32 {
                    self.report_error(
                        ReportKind::TypeError,
                        span.clone(),
                        format!(
                            "Initializer list length does not match array length: Array of length {} but list has {} elements",
                            dimension_constants[0],
                            values.len()
                        )
                    );
                }
            } else {
                todo!("Mutiple dimensions not supported")
            }

        } else {
                if dimensions.is_empty() {
                self.report_error(
                    ReportKind::TypeError,
                    declaration_info.span,
                    "Array length must be specified if shorthand initialization syntax is used".to_owned()
                )
            }
        }
        declaration_info.variable_type = Type::Array(Box::new(declaration_info.variable_type.get_array_basic_type()), dimension_constants);
        self.check_declaration_validity(child, declaration_info);
    }


    fn check_declaration_validity(
        &mut self,
        child: &mut AstNode,
        declaration_info: &DeclarationInfo) {
        match self.symbol_table.find_symbol(&declaration_info.name) {
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
                    declaration_info.span.clone(),
                    err_text);

                self.report_error(
                    ReportKind::Note,
                    Span::new(prev_line, prev_column, prev_length),
                    "Previously declared here".to_string());
            },
            None => {

                self.check_constness_init(child, declaration_info);

                let id = self.get_next_id();
                self.symbol_table.add_symbol(
                    Symbol::Variable(declaration_info.clone(), id));
            },
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
        } else if self.get_type(init_expression) == Type::Invalid {
            ice_if!(self.constant_initializer_stack.len() == 0, "Empty constant initializer stack");
            let last = self.constant_initializer_stack.len() - 1;
            self.constant_initializer_stack[last]
                .insert(declaration_info.name.to_string(), AstNode::ErrorNode);
        } else {
            self.report_error(
               ReportKind::TypeError,
               init_expression.span(),
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

                return;
                // if type is invalid, errors has already been reported
            } else if sym_info.variable_type != child_type &&
                child_type != Type::Invalid && sym_info.variable_type != Type::Invalid {
                let conversion_result = self.perform_type_conversion(&sym_info.variable_type, child);
                if conversion_result != ConversionResult::Converted {
                    self.report_type_error(sym_info, child, child_type.clone());

                    if conversion_result == ConversionResult::CastRequired {
                        self.report_conversion_note(
                            &sym_info.variable_type,
                            &child_type,
                            &child.span());
                    }
                    return;
                }
            }
            self.report_if_read_only(&span, sym_info);

        } else {
            // should not happen, only variables should be returned
            ice!(
                "Non-variable symbol '{:#?}' returned when variable expected",
                symbol);
        }
    }

    fn report_if_read_only(
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

            self.report_if_read_only(span, sym_info);

            let index_type = self.get_type(index_expression);
            let assignment_type = self.get_type(assignment_expression);

            if index_type != Type::Invalid && index_type != Type::Integer && index_type != Type::IntegralNumber {
                self.report_error(
                    ReportKind::TypeError,
                    index_expression.span(),
                    format!(
                        "Array index must be an '{}', but got '{}' instead",
                        Type::Integer, index_type));
            }

            if index_type == Type::IntegralNumber {
                let conversion_result = self.perform_type_conversion(&Type::Integer, index_expression);
                ice_if!(conversion_result != ConversionResult::Converted, "Unexpected conversion failure");
            }

            if sym_info.variable_type.get_array_basic_type() != assignment_type && assignment_type != Type::Invalid {

                let conversion_result = self.perform_type_conversion(
                    &sym_info.variable_type.get_array_basic_type(),  assignment_expression);

                if conversion_result == ConversionResult::Converted {
                    return;
                }

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

                if conversion_result == ConversionResult::CastRequired {

                    self.report_conversion_note(
                        &sym_info.variable_type.get_array_basic_type(),
                        &assignment_type,
                        &assignment_expression.span());
                }
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

        let indexable_span = indexable_expression.span();
        self.do_check(index_expression);
        self.do_check(indexable_expression);

        let indexable_type = self.get_type(indexable_expression);

        if !indexable_type.is_array() && indexable_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                indexable_span,
                format!("Cannot index expression of type '{}', array type expected", indexable_type));

            return;
        }

        let index_type = self.get_type(index_expression);

        if index_type != Type::Invalid && index_type != Type::Integer && index_type != Type::IntegralNumber {
            self.report_error(
                ReportKind::TypeError,
                index_expression.span(),
                format!(
                    "Array index must be an '{}', but got '{}' instead",
                    Type::Integer, index_type));
        }

        if !self.is_constant(index_expression) {
            if let AstNode::Identifier { name, .. } = indexable_expression {
                if let Some(declaration_info) = self.symbol_table.get_declaration_info(&name) {
                    if declaration_info.attributes.contains(&VariableAttribute::Const) {
                        self.report_error(
                            ReportKind::TypeError,
                            index_expression.span(),
                            "Index expression must be constant when indexing constant array".to_owned(),
                        );

                        if let AstNode::Identifier { name, .. } = index_expression {
                            if let Some(declaration_info) = self.symbol_table.get_declaration_info(name) {
                                self.report_error(
                                     ReportKind::Note,
                                    declaration_info.span,
                                     format!("Variable '{}', declared here, is not compile time constant", name)
                                );
                            }
                        }
                    }
                }

            }
        }

        if index_type == Type::IntegralNumber {
            let conversion_result = self.perform_type_conversion(&Type::Integer, index_expression);
            ice_if!(conversion_result != ConversionResult::Converted, "Unexpected conversion failure");
        }
    }

    fn handle_array_slice(
        &mut self,
        start_expression: &mut AstNode,
        end_expression: &mut AstNode,
        array_expression: &mut AstNode,
    ) {

        let array_span = array_expression.span();
        self.do_check(start_expression);
        self.do_check(end_expression);
        self.do_check(array_expression);

        let array_type  = self.get_type(array_expression);

        if !array_type.is_array() && array_type!= Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                array_span,
                format!("Cannot slice expression of type '{}', array type expected", array_type));
            return;
        }

        let start_type = self.get_type(start_expression);
        let end_type = self.get_type(end_expression);

        if start_type != Type::Invalid && start_type != Type::Integer && start_type != Type::IntegralNumber {
            self.report_error(
                ReportKind::TypeError,
                start_expression.span(),
                format!(
                    "Array slice start index must be an '{}', but got '{}' instead",
                    Type::Integer, start_type));
        }

        if end_type != Type::Invalid && end_type != Type::Integer && end_type != Type::IntegralNumber {
            self.report_error(
                ReportKind::TypeError,
                end_expression.span(),
                format!(
                    "Array slice end index must be an '{}', but got '{}' instead",
                    Type::Integer, end_type));
        }

        if let AstNode::Identifier { name, .. } = array_expression {
            if let Some(declaration_info) = self.symbol_table.get_declaration_info(&name) {
                if declaration_info.attributes.contains(&VariableAttribute::Const) {
                    self.report_error(
                        ReportKind::TypeError,
                        array_expression.span(),
                        "Cannot slice constant array".to_owned(),
                    );
                }
            }
        }

        if start_type == Type::IntegralNumber {
            let conversion_result = self.perform_type_conversion(&Type::Integer, start_expression);
            ice_if!(conversion_result != ConversionResult::Converted, "Unexpected conversion failure");
        }

        if end_type == Type::IntegralNumber {
            let conversion_result = self.perform_type_conversion(&Type::Integer, end_expression);
            ice_if!(conversion_result != ConversionResult::Converted, "Unexpected conversion failure");
        }
    }


    fn handle_initializer_list(
        &mut self,
        values: &mut Vec<AstNode>,
        list_type: &mut Type,
        span: &Span
    ) {
        for value in values.into_iter() {
            self.do_check(value);
        }

        if values.is_empty() {
            self.report_error(
                ReportKind::TypeError,
                *span,
                "Empty initializer list".to_owned(),
            );
            *list_type = Type::Invalid;
            return;
        }

        *list_type = self.get_type(&values[0]);

        let mut is_invalid = false;
        for value in values.into_iter().skip(1) {
            let value_type = self.get_type(value);
            if *list_type != value_type {
                if *list_type == Type::IntegralNumber && value_type.is_integral() {
                    continue;
                }

                self.report_error(
                    ReportKind::TypeError,
                    value.span(),
                    format!("Invalid initializer list member type, expected '{}' but got '{}'", list_type, self.get_type(value)),
                );
                is_invalid = true;
            }
        }

        if is_invalid {
            *list_type = Type::Invalid;
        }

        if *list_type == Type::IntegralNumber {
            values.into_iter().for_each(|value| {
                let conversion_result = self.perform_type_conversion(&Type::Integer, value);
                ice_if!(conversion_result != ConversionResult::Converted, "Unexpected conversion failure");
            });
            *list_type = Type::Integer;
        }


        *list_type = Type::InitializerList(Box::new(list_type.clone()));
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

                if self.perform_type_conversion(&function_info.return_type, child) == ConversionResult::Converted {
                    return;
                }

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

    fn handle_loop(
        &mut self,
        expr: &mut AstNode,
        post_body_statements: &mut Option<Vec<AstNode>>,
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
        if let Some(statements ) = post_body_statements {
            for s in statements.into_iter() {
                self.do_check(s);
            }
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
                let mut acceptable_types = Type::get_numeric_types().to_vec();
                acceptable_types.push(Type::Invalid);
                acceptable_types.push(Type::String);

                (acceptable_types, arithmetic_info)
            },
            AstNode::Minus{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } |
            AstNode::Multiply{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } => {
                self.handle_arithmetic_node(left_expression, right_expression, arithmetic_info);

                let mut acceptable_types = Type::get_numeric_types().to_vec();
                acceptable_types.push(Type::Invalid);

                (acceptable_types, arithmetic_info)
            },
            AstNode::Divide{ ref mut left_expression, ref mut right_expression, ref mut arithmetic_info } => {
                self.handle_arithmetic_node(left_expression, right_expression, arithmetic_info);

                match **right_expression {
                    AstNode::IntegralNumber{value: 0, ..} |
                    AstNode::Long{value: AstLong::Long(0), ..} |
                    AstNode::Integer{value: AstInteger::Int(0), ..} |
                    AstNode::Short {value: AstShort::Short(0), .. } |
                    AstNode::Byte { value: AstByte::Byte(0), .. } =>
                       self.report_error(
                           ReportKind::Warning,
                           arithmetic_info.span.clone(),
                           "Division by zero".to_owned(),
                       ),
                    _ => (),
                };

                let mut acceptable_types = Type::get_numeric_types().to_vec();
                acceptable_types.push(Type::Invalid);

                (acceptable_types, arithmetic_info)

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

                let mut acceptable_types = Type::get_integral_types().to_vec();
                acceptable_types.push(Type::Invalid);
                (acceptable_types, arithmetic_info)
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
            self.check_binary_operand_casting(left_child, right_child, arith_info, left_type, right_type);
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

        const VALID_TYPES: [Type; 7] = Type::get_numeric_types();

        if child_type == Type::Invalid {
            arith_info.node_type = Type::Invalid;
        } else if !VALID_TYPES.contains(&child_type) {
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

    fn handle_comparison_operation(&mut self, left_child: &mut AstNode, right_child: &mut AstNode, span: &Span, equality_comparison: bool) {

        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);

        let mut accepted_types = Type::get_numeric_types().to_vec();

        if equality_comparison {
            accepted_types.push(Type::Boolean);
        }

        if left_type != right_type &&
            left_type != Type::Invalid && right_type != Type::Invalid
        {

            let (target_type, mut origin_node) = self.get_binary_node_types_for_conversion(left_child, right_child);
            let conversion_result =  self.perform_type_conversion(&target_type, &mut origin_node);

            if conversion_result != ConversionResult::Converted {
                self.report_error(
                    ReportKind::TypeError,
                    span.clone(),
                    format!(
                        "Incompatible operand types '{}' and '{}' for this operation", left_type, right_type));

                if conversion_result == ConversionResult::CastRequired {
                    self.report_conversion_note(
                        &target_type,
                        &self.get_type(&origin_node),
                        span
                    );
                }

                return;
            }
        }

        if !accepted_types.contains(&left_type) {
            self.report_error(
                ReportKind::TypeError,
                span.clone(),
                format!("Cannot compare operands of type '{}'",
                        left_type));
        }

        if left_type == Type::IntegralNumber {
            ice_if!(
                self.perform_type_conversion(&Type::Integer, left_child) != ConversionResult::Converted,
                "Unexpected conversion failure");

            ice_if!(
                self.perform_type_conversion(&Type::Integer, right_child) != ConversionResult::Converted,
                "Unexpected conversion failure");
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

    fn check_bitwise_and_or_xor(&mut self, left_child: &mut AstNode, right_child: &mut AstNode, arithmetic_info: &mut ArithmeticInfo) {
        self.do_check(left_child);
        self.do_check(right_child);

        let left_type = self.get_type(left_child);
        let right_type = self.get_type(right_child);


        const VALID_TYPES: [Type; 5] = Type::get_integral_types();

        let mut bad_type = false;
        if !VALID_TYPES.contains(&left_type) && left_type != Type::Invalid {
             self.report_error(
                ReportKind::TypeError,
                left_child.span(),
                format!("Operand of type '{}' is not valid for this operator",
                        left_type));
            bad_type = true;
        }

        if !VALID_TYPES.contains(&right_type) && right_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                right_child.span(),
                format!("Operand of type '{}' is not valid for this operator",
                        right_type));
            bad_type = true;
        }

        if left_type == Type::Invalid || right_type == Type::Invalid || bad_type {
            arithmetic_info.node_type = Type::Invalid;
        } else if left_type != right_type {
            self.check_binary_operand_casting(left_child, right_child, arithmetic_info, left_type, right_type)
        } else {
            arithmetic_info.node_type = left_type;
        }
    }

    fn check_bitwise_not(&mut self, child: &mut AstNode, arithmetic_info: &mut ArithmeticInfo) {
        self.do_check(child);
        let child_type = self.get_type(child);

        if !child_type.is_integral() && child_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                child.span(),
                format!(
                    "Operand must be an integral value, got '{}' instead", child_type));
        }


        arithmetic_info.node_type = child_type;
    }

    fn check_binary_operand_casting(&mut self,
                                    left_child: &mut AstNode,
                                    right_child: &mut AstNode,
                                    arithmetic_info: &mut ArithmeticInfo,
                                    left_type: Type,
                                    right_type: Type) {
        let (target_type, mut origin_node) = self.get_binary_node_types_for_conversion(left_child, right_child);
        let conversion_result = self.perform_type_conversion(&target_type, &mut origin_node);
        if conversion_result == ConversionResult::Converted {
            arithmetic_info.node_type = target_type;
            return;
        }

        arithmetic_info.node_type = Type::Invalid;
        self.report_error(
            ReportKind::TypeError,
            arithmetic_info.span.clone(),
            format!(
                "Incompatible operand types '{}' and '{}' for this operation", left_type, right_type));

        if conversion_result == ConversionResult::CastRequired {
            self.report_conversion_note(
                &target_type,
                &self.get_type(&origin_node),
                &arithmetic_info.span
            );
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

    fn handle_shift_expression(&mut self, value: &mut AstNode, shift_count: &mut AstNode, arithmetic_info: &mut ArithmeticInfo) {

        self.do_check(value);
        self.do_check(shift_count);

        let value_type = self.get_type(value);
        let shift_type = self.get_type(shift_count);

        const ACCEPTED_SHIFT_TYPES: [Type; 5] = Type::get_integral_types();

        if !ACCEPTED_SHIFT_TYPES.contains(&value_type) && value_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                value.span(),
                format!("Cannot shift expression of type '{}', must be integral value", self.get_type(value))
            );
            arithmetic_info.node_type = Type::Invalid;
        } else {
            let value_type = if value_type == Type::IntegralNumber {
                ice_if!(self.perform_type_conversion(&Type::Integer, value) != ConversionResult::Converted, "Unexpected conversion failure");
                Type::Integer
            } else {
                value_type
            };

            arithmetic_info.node_type = value_type;
        }

        if !ACCEPTED_SHIFT_TYPES.contains(&shift_type)  && shift_type != Type::Invalid {
            self.report_error(
                ReportKind::TypeError,
                shift_count.span(),
                format!("Shift count must be an integral value, got '{}' instead", shift_type),
            );
        } else if self.is_constant(shift_count) {
            if shift_type == Type::IntegralNumber {
                ice_if!(self.perform_type_conversion(&Type::Integer, shift_count) != ConversionResult::Converted, "Unexpected conversion failure");
            }
            if let AstNode::Integer { value: AstInteger::Int(count), .. } = *shift_count {
                if count < 0 {
                    self.report_error(
                        ReportKind::TypeError,
                        shift_count.span(),
                        "Shift count cannot be negative".to_owned(),
                    );
                } else if arithmetic_info.node_type != Type::Invalid && count as u32 >= arithmetic_info.node_type.size_in_bytes()*8 {
                    self.report_error(
                        ReportKind::TypeError,
                        shift_count.span(),
                        format!("Shift count too large for type '{}'", arithmetic_info.node_type),
                    );
                }
            }
        }
    }

    fn handle_cast(&mut self, expression: &mut AstNode, target_type: &mut Type, span: &Span) {
        // special case handling for integers:
        // we want to support casts like OVERFLOWING_INTEGER_CONSTANT as int, or ARITHMETIC_EXPRESSION_RESULTING_IN_OVERFLOWING_INTEGER_CONSTANT as int
        // right now the basic do_check reports the overflow, before we apply the ast.
        self.do_check(expression);

        // clone is important here, we do not want to actually cast the expression, merely check if it would be required
        let conversion_result = self.perform_type_conversion_with_overflow_definition(
            target_type, expression, true);

        if self.get_type(expression) != Type::Invalid && conversion_result == ConversionResult::NotPossible {
            self.report_error(
                ReportKind::TypeError,
                span.clone(),
                format!("Cannot cast type '{}' into type '{}'", self.get_type(expression), target_type)
            );
            *target_type = Type::Invalid;
        }
    }

    fn check_long_for_overflow(&mut self, long: &AstLong, span: &Span) {
        let value = match long{
            AstLong::Long(_) => return, // OK
            AstLong::Invalid(value) => {
              *value
            },
        };

        self.report_error(
            ReportKind::TokenError,
            span.clone(),
            format!("Type '{}' cannot represent value '{}'", Type::Long, value),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            format!("Value '{}' would be stored as '{}'", value, value as i64),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            "Use explicit 'as long' cast if this is wanted".to_owned(),
        );
    }

    fn check_integer_for_overflow(&mut self, integer: &AstInteger, span: &Span) {
        let value = match integer {
            AstInteger::Int(_) => return, // OK
            AstInteger::Invalid(value) => {
              *value
            },
        };

        self.report_error(
            ReportKind::TokenError,
            span.clone(),
            format!("Type '{}' cannot represent value '{}'", Type::Integer, value),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            format!("Value '{}' would be stored as '{}'", value, value as i32),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            "Use explicit 'as int' cast if this is wanted".to_owned(),
        );
    }

    fn check_short_for_overflow(&mut self, short: &AstShort, span: &Span) {
        let value = match short {
            AstShort::Short(_) => return, // OK
            AstShort::Invalid(value) => {
              *value
            },
        };

        self.report_error(
            ReportKind::TokenError,
            span.clone(),
            format!("Type '{}' cannot represent value '{}'", Type::Short, value),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            format!("Value '{}' would be stored as '{}'", value, value as i16),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            "Use explicit 'as short' cast if this is wanted".to_owned(),
        );
    }

    fn check_byte_for_overflow(&mut self, byte: &AstByte, span: &Span) {
        let value = match byte {
            AstByte::Byte(_) => return, // OK
            AstByte::Invalid(value) => {
               *value
            },
        };

        self.report_error(
            ReportKind::TokenError,
            span.clone(),
            format!("Type '{}' cannot represent value '{}'", Type::Byte, value),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            format!("Value '{}' would be stored as '{}'", value, value as i8),
        );
        self.report_error(
            ReportKind::Note,
            span.clone(),
            "Use explicit 'as byte' cast if this is wanted".to_owned(),
        );
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
            AstNode::IntegralNumber { .. } => Type::IntegralNumber,
            AstNode::Long { .. } => Type::Long,
            AstNode::Integer{ .. } => Type::Integer,
            AstNode::Short{ .. } => Type::Short,
            AstNode::Byte{ .. } => Type::Byte,
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
            AstNode::ArithmeticShiftRight { arithmetic_info, ..} |
            AstNode::LogicalShiftRight { arithmetic_info, ..} |
            AstNode::LogicalShiftLeft { arithmetic_info, ..} |
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
            AstNode::BitwiseAnd {arithmetic_info, ..} |
            AstNode::BitwiseOr { arithmetic_info, .. } |
            AstNode::BitwiseXor { arithmetic_info, .. } |
            AstNode::BitwiseNot { arithmetic_info, .. } =>  arithmetic_info.node_type.clone(),
            AstNode::ArrayAccess {
                ref indexable_expression,
                ..
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
            AstNode::ArraySlice {
                ref array_expression,
                ..
            } => {
                match **array_expression {
                    AstNode::Identifier{ref name, ..} => {
                        if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                            if info.variable_type.is_array() {
                                Type::Reference(Box::new(info.variable_type.clone()))
                            } else {
                                Type::Invalid
                            }
                        } else {
                            Type::Invalid
                        }
                    },
                    AstNode::FunctionCall{ .. } => todo!("Slicing function return value is not yet implemented"),
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
            },
            AstNode::Cast{ target_type, ..  } => target_type.clone(),
            AstNode::InitializerList { list_type, .. } => list_type.clone(),
            AstNode::ErrorNode => Type::Invalid,
            _ => ice!("Invalid node '{}' when resolving node type", node),
        }
    }

    fn is_constant(&self, node: &AstNode) -> bool {
        match node {
            AstNode::IntegralNumber { .. } => true,
            AstNode::Long { .. } => true,
            AstNode::Integer { .. } => true,
            AstNode::Short{ .. } => true,
            AstNode::Byte{ .. } => true,
            AstNode::Float { .. } => true,
            AstNode::Double { .. } => true,
            AstNode::Boolean { .. } => true,
            AstNode::Text { .. } => true,
            AstNode::BooleanNot { expression, .. } |
            AstNode::BitwiseNot{ expression, .. } |
            AstNode::Negate { expression, .. } => self.is_constant(expression),
            AstNode::Identifier { name, .. } => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    info.attributes.contains(&VariableAttribute::Const)
                } else {
                    false
                }
            },
            AstNode::ArrayAccess {
                index_expression, indexable_expression
            } => self.is_constant(index_expression) && self.is_constant(indexable_expression),

            AstNode::BooleanAnd { left_expression, right_expression, .. } |
            AstNode::BooleanOr { left_expression, right_expression, .. } |
            AstNode::BitwiseAnd {  left_expression, right_expression, .. } |
            AstNode::BitwiseOr { left_expression, right_expression, .. } |
            AstNode::BitwiseXor{ left_expression, right_expression, .. } |
            AstNode::Plus {
                left_expression,
                right_expression,
                ..
            } |
            AstNode::Minus {
                left_expression,
                right_expression,
                ..
            } |
            AstNode::Multiply {
                left_expression,
                right_expression,
                ..
            } |
            AstNode::Divide {
                left_expression,
                right_expression,
                ..
            } |
            AstNode::Modulo {
                left_expression,
                right_expression,
                ..
            } => {
                self.is_constant(left_expression) && self.is_constant(right_expression)
            },
            AstNode::LogicalShiftRight { value, shift_count, .. } |
            AstNode::LogicalShiftLeft { value, shift_count, .. } |
            AstNode::ArithmeticShiftRight { value, shift_count, .. } => {
              self.is_constant(value) && self.is_constant(shift_count)
            },
            AstNode::Cast { expression, target_type, ..  } => {
                // don't propagate invalid cast results, can lead to bad error reporting
                if *target_type == Type::Invalid {
                    false
                } else {
                    self.is_constant(expression)
                }
            },
            AstNode::InitializerList { values, ..} =>
                values.iter().all(|value| self.is_constant(value) ),
            AstNode::ErrorNode => true,
            _ => false,
        }
    }

    fn get_constant_initializer_expression(&self, node: &AstNode) -> AstNode {
        ice_if!(!self.is_constant(node), "Constant initializer requested for non-constant expression {}", node);
        match node {
            AstNode::IntegralNumber { .. } => node.clone(),
            AstNode::Long { .. } => node.clone(),
            AstNode::Integer{ .. } => node.clone(),
            AstNode::Short{ .. } => node.clone(),
            AstNode::Byte{ .. } => node.clone(),
            AstNode::Float{ .. } => node.clone() ,
            AstNode::Double{ .. } => node.clone() ,
            AstNode::Boolean{ .. } => node.clone(),
            AstNode::Text{ .. } => node.clone(),
            AstNode::Identifier{ name, ..} => {

                // if identifier is array, don't fold - handled separately
                // if we do fold this, const array identifier gets folded in ArrayAccess type checking phase,
                // and we get const_array[some_index] --> const_array_value[some_index], which is ill-formed
                if let Some(Symbol::Variable(decl_info, _)) = self.symbol_table.find_symbol(&name) {
                   if decl_info.variable_type.is_array() {
                       return node.clone();
                   }
                }


                let node = self.get_constant_initializer_stack_entry(name);
                // in case of:
                //
                //   const a: int = 4;
                //   const b: int = a;
                //
                // above would return "a" for entry of b. We need to recurse until we get the constant
                self.get_constant_initializer_expression(&node)
            },
            AstNode::ArrayAccess { indexable_expression, .. } => {

                let node = self.get_constant_initializer_expression(indexable_expression);
                match node {
                    AstNode::Identifier{name, .. } => {
                        let node = self.get_constant_initializer_stack_entry(&name);
                        self.get_constant_initializer_expression(&node)
                    },
                    _ => self.get_constant_initializer_expression(&node),
                }
            },
            AstNode::BitwiseAnd {
                left_expression,
                right_expression,
                arithmetic_info,
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) =>
                        AstNode::IntegralNumber { value: i1 & i2, span: arithmetic_info.span },
                    (AstNode::Long{ value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. } ) =>
                        AstNode::Long { value: AstLong::Long(i1 & i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. } ) =>
                        AstNode::Integer { value: AstInteger::Int(i1 & i2), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. } ) =>
                        AstNode::Short{ value: AstShort::Short(i1 & i2), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) =>
                        AstNode::Byte { value: AstByte::Byte(i1 & i2), span: arithmetic_info.span },
                    _ => node.clone()
                }
            },
            AstNode::BitwiseOr {
                left_expression,
                right_expression,
                arithmetic_info,
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) =>
                        AstNode::IntegralNumber { value: i1 | i2, span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. } ) =>
                        AstNode::Long { value: AstLong::Long(i1 | i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. } ) =>
                        AstNode::Integer { value: AstInteger::Int(i1 | i2), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. } ) =>
                        AstNode::Short{ value: AstShort::Short(i1 | i2), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) =>
                        AstNode::Byte { value: AstByte::Byte(i1 | i2), span: arithmetic_info.span },
                    _ => node.clone()
                }
            },
            AstNode::BitwiseXor {
                left_expression,
                right_expression,
                arithmetic_info,
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) =>
                        AstNode::IntegralNumber { value: i1 ^ i2, span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. } ) =>
                        AstNode::Long { value: AstLong::Long(i1 ^ i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. } ) =>
                        AstNode::Integer { value: AstInteger::Int(i1 ^ i2), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. } ) =>
                        AstNode::Short{ value: AstShort::Short(i1 ^ i2), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) =>
                        AstNode::Byte { value: AstByte::Byte(i1 ^ i2), span: arithmetic_info.span },
                    _ => node.clone()
                }
            },
            AstNode::Plus {
                left_expression,
                right_expression,
                arithmetic_info// use operator span, so that type errors show correct place
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) =>
                        AstNode::IntegralNumber { value: i1.wrapping_add(i2), span: arithmetic_info.span },
                    (AstNode::Long{ value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. } ) =>
                        AstNode::Long { value: AstLong::Long(i1.wrapping_add(i2)), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. } ) =>
                        AstNode::Integer { value: AstInteger::Int(i1.wrapping_add(i2)), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. } ) =>
                        AstNode::Short{ value: AstShort::Short(i1.wrapping_add(i2)), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) =>
                        AstNode::Byte { value: AstByte::Byte(i1.wrapping_add(i2)), span: arithmetic_info.span },
                    (AstNode::Float{ value: f1, .. } , AstNode::Float { value: f2, .. } ) =>
                        AstNode::Float { value: f1 + f2 , span: arithmetic_info.span },
                    (AstNode::Double{ value: f1, .. } , AstNode::Double{ value: f2, .. } ) =>
                        AstNode::Double { value: f1 + f2, span: arithmetic_info.span },
                    (AstNode::Text{ value: str1, .. } , AstNode::Text{ value: str2, .. } ) =>
                        AstNode::Text { value: Rc::new(format!("{}{}", str1, str2)) , span: arithmetic_info.span },
                    _ => node.clone()
                }
            },
            AstNode::Minus {
                left_expression,
                right_expression,
                arithmetic_info
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) =>
                        AstNode::IntegralNumber { value: i1.wrapping_sub(i2), span: arithmetic_info.span },
                    (AstNode::Long{ value: AstLong::Long(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. } ) =>
                        AstNode::Long { value: AstLong::Long(i1.wrapping_sub(i2)), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. } ) =>
                        AstNode::Integer { value: AstInteger::Int(i1.wrapping_sub(i2)), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. } ) =>
                        AstNode::Short{ value: AstShort::Short(i1.wrapping_sub(i2)), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) =>
                        AstNode::Byte { value: AstByte::Byte(i1.wrapping_sub(i2)), span: arithmetic_info.span },
                    (AstNode::Float{ value: f1, .. } , AstNode::Float { value: f2, .. } ) =>
                        AstNode::Float { value: f1 - f2 , span: arithmetic_info.span },
                    (AstNode::Double{ value: f1, .. } , AstNode::Double{ value: f2, .. } ) =>
                        AstNode::Double { value: f1 - f2, span: arithmetic_info.span },
                    _ => node.clone()
                }
            }
            AstNode::Multiply {
                left_expression,
                right_expression,
                arithmetic_info
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {

                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) =>
                        AstNode::IntegralNumber { value: i1.wrapping_mul(i2), span: arithmetic_info.span },
                    (AstNode::Long{ value: AstLong::Long(i1), .. }, AstNode::Long{ value: AstLong::Long(i2), .. }) =>
                        AstNode::Long { value: AstLong::Long(i1.wrapping_mul(i2)), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. }, AstNode::Integer { value: AstInteger::Int(i2), .. }) =>
                        AstNode::Integer { value: AstInteger::Int(i1.wrapping_mul(i2)), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. } ) =>
                        AstNode::Short{ value: AstShort::Short(i1.wrapping_mul(i2)), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) =>
                        AstNode::Byte { value: AstByte::Byte(i1.wrapping_mul(i2)), span: arithmetic_info.span },
                    (AstNode::Float{ value: f1, .. } , AstNode::Float { value: f2, .. } ) =>
                        AstNode::Float { value: f1 * f2 , span: arithmetic_info.span },
                    (AstNode::Double{ value: f1, .. } , AstNode::Double{ value: f2, .. } ) =>
                        AstNode::Double { value: f1 * f2, span: arithmetic_info.span },
                    _ => node.clone()
                }
            },
            AstNode::Divide {
                left_expression,
                right_expression,
                arithmetic_info
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) if i2 != 0 =>
                        AstNode::IntegralNumber { value: i1.wrapping_div(i2), span: arithmetic_info.span },
                    (AstNode::Long{ value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 != 0  =>
                        AstNode::Long{ value: AstLong::Long(i1.wrapping_div(i2)), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. }) if i2 != 0  =>
                        AstNode::Integer { value: AstInteger::Int(i1.wrapping_div(i2)), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. } ) if i2 != 0 =>
                        AstNode::Short { value: AstShort::Short(i1.wrapping_div(i2)), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) if i2 != 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1.wrapping_div(i2)), span: arithmetic_info.span },
                    (AstNode::Float{ value: f1, .. } , AstNode::Float { value: f2, .. } ) =>
                        AstNode::Float { value: f1 / f2 , span: arithmetic_info.span },
                    (AstNode::Double{ value: f1, .. } , AstNode::Double{ value: f2, .. } ) =>
                        AstNode::Double { value: f1 / f2, span: arithmetic_info.span },

                    _ => node.clone()
                }
            },
            AstNode::Modulo{
                left_expression,
                right_expression,
                arithmetic_info
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::IntegralNumber{ value: i1, .. } , AstNode::IntegralNumber{ value: i2, .. } ) if i2 != 0 =>
                        AstNode::IntegralNumber { value: i1 % i2, span: arithmetic_info.span },
                    (AstNode::Long{ value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. } ) if i2 != 0 =>
                        AstNode::Long { value: AstLong::Long(i1 % i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. } ) if i2 != 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 % i2), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. } ) if i2 != 0 =>
                        AstNode::Short { value: AstShort::Short(i1 % i2), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. } ) if i2 != 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 % i2), span: arithmetic_info.span },

                    _ => node.clone()
                }
            },
            AstNode::Negate{
                expression,
                arithmetic_info
            } => {
                match self.get_constant_initializer_expression(expression) {
                    AstNode::IntegralNumber{ value, .. }  =>
                        AstNode::IntegralNumber { value: value.wrapping_neg(), span: arithmetic_info.span },
                    AstNode::Long { value: AstLong::Long(i1), .. }  =>
                        AstNode::Long { value: AstLong::Long(i1.wrapping_neg()), span: arithmetic_info.span },
                    AstNode::Integer { value: AstInteger::Int(i1), .. }  =>
                        AstNode::Integer { value: AstInteger::Int(i1.wrapping_neg()), span: arithmetic_info.span },
                    AstNode::Short{ value: AstShort::Short(i1), .. }  =>
                        AstNode::Short{ value: AstShort::Short(i1.wrapping_neg()), span: arithmetic_info.span },
                    AstNode::Byte { value: AstByte::Byte(i1), .. }  =>
                        AstNode::Byte { value: AstByte::Byte(i1.wrapping_neg()), span: arithmetic_info.span },

                    _ => node.clone()
                }
            },
            AstNode::BooleanNot{
                expression,
                span
            } => {
                match self.get_constant_initializer_expression(expression) {
                    AstNode::Boolean { value: boolean, .. }  =>
                        AstNode::Boolean { value: !boolean, span: *span },

                    _ => node.clone()
                }
            },
            AstNode::BitwiseNot {
                expression,
                arithmetic_info
            } => {
                match self.get_constant_initializer_expression(expression) {
                    AstNode::IntegralNumber{ value, .. }  =>
                        AstNode::IntegralNumber { value: !value, span: arithmetic_info.span },
                    AstNode::Long { value: AstLong::Long(i1), .. }  =>
                        AstNode::Long { value: AstLong::Long(!i1), span: arithmetic_info.span },
                    AstNode::Integer { value: AstInteger::Int(i1), .. }  =>
                        AstNode::Integer { value: AstInteger::Int(!i1), span: arithmetic_info.span },
                    AstNode::Short{ value: AstShort::Short(i1), .. }  =>
                        AstNode::Short { value: AstShort::Short(!i1), span: arithmetic_info.span },
                    AstNode::Byte { value: AstByte::Byte(i1), .. }  =>
                        AstNode::Byte { value: AstByte::Byte(!i1), span: arithmetic_info.span },

                    _ => node.clone()
                }
            },
            AstNode::BooleanAnd{
                left_expression,
                right_expression,
                span
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::Boolean { value: boolean1, .. } , AstNode::Boolean { value: boolean2 , .. } ) =>
                        AstNode::Boolean { value: boolean1 && boolean2, span: *span },

                    _ => node.clone()
                }
            },
            AstNode::BooleanOr {
                left_expression,
                right_expression,
                span
            } => {
                match (
                    self.get_constant_initializer_expression(left_expression),
                    self.get_constant_initializer_expression(right_expression),
                ) {
                    (AstNode::Boolean { value: boolean1, .. } , AstNode::Boolean { value: boolean2 , .. } ) =>
                        AstNode::Boolean { value: boolean1 || boolean2, span: *span },

                    _ => node.clone()
                }
            },
            AstNode::ArithmeticShiftRight { value, shift_count, arithmetic_info } => {
                match (
                    self.get_constant_initializer_expression(value),
                    self.get_constant_initializer_expression(shift_count),
                ) {
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 >> i2), span: arithmetic_info.span },

                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 >> i2), span: arithmetic_info.span },

                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short(i1 >> i2), span: arithmetic_info.span },
                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short(i1 >> i2), span: arithmetic_info.span },


                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 >> i2 as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 >> i2 as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 >> i2 as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 >> i2), span: arithmetic_info.span },
                    _ => node.clone(),
                }
            }
            AstNode::LogicalShiftRight { value, shift_count, arithmetic_info }  => {
                match (
                    self.get_constant_initializer_expression(value),
                    self.get_constant_initializer_expression(shift_count),
                ) {
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long((i1 as u64 >> i2) as i64), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long((i1 as u64 >> i2) as i64), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long((i1 as u64 >> i2) as i64), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long((i1 as u64 >> i2) as i64), span: arithmetic_info.span },

                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int((i1 as u32 >> i2) as i32), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int((i1 as u32 >> i2) as i32), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int((i1 as u32 >> i2) as i32), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 32 && i2 > 0 =>
                        AstNode::Integer { value: AstInteger::Int((i1 as u32 >> i2) as i32), span: arithmetic_info.span },

                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Long { value: AstLong::Long(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short((i1 as u16 >> i2) as i16), span: arithmetic_info.span },
                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short((i1 as u16 >> i2) as i16), span: arithmetic_info.span },
                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short((i1 as u16 >> i2) as i16), span: arithmetic_info.span },
                    (AstNode::Short { value: AstShort::Short(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short { value: AstShort::Short((i1 as u16 >> i2) as i16), span: arithmetic_info.span },


                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(((i1 as u8) >> i2) as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(((i1 as u8) >> i2) as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(((i1 as u8) >> i2) as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(((i1 as u8) >> i2) as i8), span: arithmetic_info.span },
                    _ => node.clone(),
                }
            },
            AstNode::LogicalShiftLeft { value, shift_count, arithmetic_info } => {
                match (
                    self.get_constant_initializer_expression(value),
                    self.get_constant_initializer_expression(shift_count),
                ) {
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Long { value: AstLong::Long(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 64 && i2 >= 0 =>
                        AstNode::Long { value: AstLong::Long(i1 << i2), span: arithmetic_info.span },

                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 32 && i2 >= 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 << i2), span: arithmetic_info.span },
                   (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Integer { value: AstInteger::Int(i2), .. }) if i2 < 32 && i2 >= 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Short { value: AstShort::Short(i2), .. }) if i2 < 32 && i2 >= 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Integer { value: AstInteger::Int(i1), .. } , AstNode::Byte { value: AstByte::Byte(i2), .. }) if i2 < 32 && i2 >= 0 =>
                        AstNode::Integer { value: AstInteger::Int(i1 << i2), span: arithmetic_info.span },

                   (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short{ value: AstShort::Short(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short{ value: AstShort::Short(i1 << i2 as i8), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short{ value: AstShort::Short(i1 << i2 as i8), span: arithmetic_info.span },
                    (AstNode::Short{ value: AstShort::Short(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 16 && i2 > 0 =>
                        AstNode::Short{ value: AstShort::Short(i1 << i2), span: arithmetic_info.span },

                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Long{ value: AstLong::Long(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 << i2), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Integer{ value: AstInteger::Int(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 << i2 as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Short{ value: AstShort::Short(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 << i2 as i8), span: arithmetic_info.span },
                    (AstNode::Byte{ value: AstByte::Byte(i1), .. } , AstNode::Byte{ value: AstByte::Byte(i2), .. }) if i2 < 8 && i2 > 0 =>
                        AstNode::Byte { value: AstByte::Byte(i1 << i2), span: arithmetic_info.span },
                    _ => node.clone(),
                }
            },
            AstNode::Cast{ expression, .. } => {
                *expression.clone()
            },
            node @ AstNode::InitializerList { .. }  => node.clone(),
            AstNode::ErrorNode => AstNode::ErrorNode,
            _ => ice!("Non-constant node {}", node),
        }
    }

    fn get_constant_initializer_stack_entry(&self, name: &str) -> AstNode {
        for level in self.constant_initializer_stack.iter().rev() {
            match level.get(name) {
                Some(x) => return x.clone(),
                None => (),
            }
        }

        ice!("No entry {} in constant initializer stack", name)
    }

    fn propagate_constants(&self, node: &AstNode) -> Option<AstNode> {
        if !self.is_constant(node) {
            return None;
        }

        Some(self.get_constant_initializer_expression(node))
    }


    /*
        Perform implicit conversions from original node to decl-info type, if possible

        Return true if conversion succeeded

        Return false if conversion failed
    */



    fn perform_type_conversion(
        &mut self,
        target_type: &Type,
        origin_node: &mut AstNode,
    ) -> ConversionResult {
        self.perform_type_conversion_with_overflow_definition(
            target_type,
            origin_node,
            false,
        )
    }

    fn perform_type_conversion_with_overflow_definition(
        &mut self,
        target_type: &Type,
        origin_node: &mut AstNode,
        allow_constant_overflow: bool,
    ) -> ConversionResult {

        let origin_type = self.get_type(origin_node);
        let origin_is_constant = self.is_constant(origin_node);

        // report narrowing conversions requiring manual cast, if non-constant. Try casting constant first
        // report any cast that would be possible with explicit cast
        // reject otherwise
        match (origin_type, target_type) {
            (Type::IntegralNumber, &Type::Long) => {
               if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::IntegralNumber { value, span}   => {

                            *origin_node = if allow_constant_overflow {
                                AstNode::Long { value: AstLong::from(value as i64), span: span.clone() }
                            } else {
                                AstNode::Long { value: AstLong::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow check

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                   ice!("Integral number type should not appear as non-constant");
                }
            },
            (Type::IntegralNumber, &Type::Integer) => {
               if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::IntegralNumber { value, span}   => {

                            *origin_node = if allow_constant_overflow {
                                AstNode::Integer { value: AstInteger::from(value as i32), span: span.clone() }
                            } else {
                                AstNode::Integer { value: AstInteger::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow check

                            ConversionResult::Converted
                        },
                        // special case handling for division with zero: this will not get folded, for obvious reasons
                        // convert the integral nubmers inside to integers
                        AstNode::Divide{
                            mut left_expression,
                            mut right_expression,
                            arithmetic_info } => {

                            //if let (AstNode::IntegralNumber{ value: _, span: _}, AstNode::IntegralNumber{ value: 0, span: _}) = (*left_expression, *right_expression) {
                            let lresult = self.perform_type_conversion(&Type::Integer, &mut left_expression);
                            let rresult = self.perform_type_conversion(&Type::Integer, &mut right_expression);

                            if lresult == ConversionResult::Converted && rresult == ConversionResult::Converted {
                                *origin_node = AstNode::Divide{
                                    left_expression,
                                    right_expression,
                                    arithmetic_info
                                };
                                ConversionResult::Converted
                            } else {
                                ConversionResult::NotPossible
                            }

                        },
                        _ => ConversionResult::NotPossible,
                    }
                } else {
                   ice!("Integral number type should not appear as non-constant");
                }
            },
            (Type::IntegralNumber, &Type::Short) => {
               if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::IntegralNumber { value, span}   => {
                            *origin_node = if allow_constant_overflow {
                                AstNode::Short { value: AstShort::from(value as i16), span: span.clone() }
                            } else {
                                AstNode::Short { value: AstShort::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow check

                            ConversionResult::Converted
                        },
                        _ => ConversionResult::NotPossible,
                    }
                } else {
                   ice!("Integral number type should not appear as non-constant");
                }
            },
            (Type::IntegralNumber, &Type::Byte) => {
               if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::IntegralNumber { value, span}   => {
                            *origin_node = if allow_constant_overflow {
                                AstNode::Byte { value: AstByte::from(value as i8), span: span.clone() }
                            } else {
                                AstNode::Byte { value: AstByte::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow check

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                   ice!("Integral number type should not appear as non-constant");
                }
            },
            (Type::Long, &Type::Integer) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Long { value, span}   => {

                            *origin_node = if allow_constant_overflow {

                                match value {
                                    AstLong::Long(value) => {
                                        AstNode::Integer { value: AstInteger::Int(value as i32), span: span.clone() }
                                    }
                                    AstLong::Invalid(value) => {
                                        AstNode:: Integer{  value: AstInteger::Int(value as i32), span: span.clone() }
                                    }
                                }
                            } else {
                                AstNode::Integer{ value: AstInteger::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow checks

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Long, &Type::Short) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Long { value, span}   => {

                            *origin_node = if allow_constant_overflow {

                                match value {
                                    AstLong::Long(value) => {
                                        AstNode::Short { value: AstShort::Short(value as i16), span: span.clone() }
                                    }
                                    AstLong::Invalid(value) => {
                                        AstNode::Short{  value: AstShort::Short(value as i16), span: span.clone() }
                                    }
                                }
                            } else {
                                AstNode::Short{ value: AstShort::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow checks

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Long, &Type::Byte) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Long { value, span}   => {

                            *origin_node = if allow_constant_overflow {

                                match value {
                                    AstLong::Long(value) => {
                                        AstNode::Byte { value: AstByte::Byte(value as i8), span: span.clone() }
                                    }
                                    AstLong::Invalid(value) => {
                                        AstNode::Byte { value: AstByte::Byte(value as i8), span: span.clone() }
                                    }
                                }
                            } else {
                                AstNode::Byte { value: AstByte::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow checks

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Integer, &Type::Long) => {
                 if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Integer{ value, span}   => {
                            *origin_node = AstNode::Long{ value: AstLong::from(value), span: span.clone() };

                            self.do_check(origin_node); // overflow check. Should not trigger unless there's a bug somewhere
                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                     ConversionResult::CastRequired
                }
            },
            (Type::Integer, &Type::Short) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Integer { value, span}   => {

                            *origin_node = if allow_constant_overflow {

                                match value {
                                    AstInteger::Int(value) => {
                                        AstNode::Short { value: AstShort::Short(value as i16), span: span.clone() }
                                    }
                                    AstInteger::Invalid(value) => {
                                        AstNode::Short { value: AstShort::Short(value as i16), span: span.clone() }
                                    }
                                }
                            } else {
                                AstNode::Short { value: AstShort::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow checks

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Integer, &Type::Byte) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Integer { value, span}   => {

                            *origin_node = if allow_constant_overflow {

                                match value {
                                    AstInteger::Int(value) => {
                                        AstNode::Byte { value: AstByte::Byte(value as i8), span: span.clone() }
                                    }
                                    AstInteger::Invalid(value) => {
                                        AstNode::Byte { value: AstByte::Byte(value as i8), span: span.clone() }
                                    }
                                }
                            } else {
                                AstNode::Byte { value: AstByte::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow checks

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Short, &Type::Long) => {
                 if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Short{ value, span}   => {
                            *origin_node = AstNode::Long{ value: AstLong::from(value), span: span.clone() };

                            self.do_check(origin_node); // overflow check. Should not trigger unless there's a bug somewhere
                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                     ConversionResult::CastRequired
                }
            },
            (Type::Short, &Type::Integer) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Short{ value, span}   => {
                            *origin_node = AstNode::Integer{ value: AstInteger::from(value), span: span.clone() };

                            self.do_check(origin_node); // overflow check. Should not trigger unless there's a bug somewhere
                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Short, &Type::Byte) => {
                if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Short { value, span}   => {

                            *origin_node = if allow_constant_overflow {
                                match value {
                                    AstShort::Short(value) => {
                                        AstNode::Byte { value: AstByte::Byte(value as i8), span: span.clone() }
                                    }
                                    AstShort::Invalid(value) => {
                                        AstNode::Byte { value: AstByte::Byte(value as i8), span: span.clone() }
                                    }
                                }
                            } else {
                                AstNode::Byte { value: AstByte::from(value), span: span.clone() }
                            };
                            self.do_check(origin_node); // overflow checks

                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                    ConversionResult::CastRequired
                }
            },
            (Type::Byte, &Type::Long) => {
                 if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Byte{ value, span}   => {
                            *origin_node = AstNode::Long{ value: AstLong::from(value), span: span.clone() };

                            self.do_check(origin_node); // overflow check. Should not trigger unless there's a bug somewhere
                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                     ConversionResult::CastRequired
                }
            },
            (Type::Byte, &Type::Integer) => {
                 if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Byte{ value, span}   => {
                            *origin_node = AstNode::Integer{ value: AstInteger::from(value), span: span.clone() };

                            self.do_check(origin_node); // overflow check. Should not trigger unless there's a bug somewhere
                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                     ConversionResult::CastRequired
                }
            },
            (Type::Byte, &Type::Short) => {
                 if origin_is_constant {
                    match origin_node.clone() {
                        AstNode::Byte{ value, span}   => {
                            *origin_node = AstNode::Short{ value: AstShort::from(value), span: span.clone() };

                            self.do_check(origin_node); // overflow check. Should not trigger unless there's a bug somewhere
                            ConversionResult::Converted
                        },
                        _ => return ConversionResult::NotPossible,
                    }
                } else {
                     ConversionResult::CastRequired
                }
            },
            (Type::Float, &Type::Double) => {
                ConversionResult::CastRequired
            },
            (Type::Double, &Type::Float) => {
                ConversionResult::CastRequired
            },
            // self casts
            (x, y) if &x == y => ConversionResult::Converted,

            _ => ConversionResult::NotPossible
        }
    }

    /*
        given two nodes:
            * if one is integral number and another isn't:
                * if both constants, convert to integral number, let higher level handle
                * else convert to non-integral
            * if both are variables or both are constants:
                * return wider type and narrower node
            * if one is constant and one is variable:
                * return variable type and const node
                    * e.g. expressions of type "foo + 123" try to coerce constant to variable type
     */
    fn get_binary_node_types_for_conversion<'a>(
        &mut self,
        left_node: &'a mut AstNode,
        right_node: &'a mut AstNode,
    ) -> (Type, &'a mut AstNode) {
        let left_type = self.get_type(left_node);
        let right_type = self.get_type(right_node);

        let left_is_constant = self.is_constant(left_node);
        let right_is_constant = self.is_constant(right_node);

        if left_is_constant == right_is_constant {
            match (&left_type, &right_type) {
                (Type::Byte, Type::Integer) |
                (Type::Float, Type::Double) => (right_type.clone(), left_node),
                (_, Type::IntegralNumber) => {
                    (Type::IntegralNumber, left_node)
                },
                (Type::IntegralNumber, _) => {
                    (Type::IntegralNumber, right_node)
                }
                _ => (left_type.clone(), right_node)
            }
        } else {
            match (left_is_constant, right_is_constant) {
                (true, false) => {
                    (right_type, left_node)
                },
                (false, true) => {
                    (left_type, right_node)
                }
                _ => ice!("Should not be reached!")
            }
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
            format!("Variable '{}', declared here, has type '{}'", variable_info.name, variable_info.variable_type));
    }

    fn report_conversion_note(
        &mut self,
        target_type: &Type,
        origin_type: &Type,
        span: &Span) {
        self.report_error(
                    ReportKind::Note,
                    *span,
                    format!("Explicit cast is required to convert '{}' to '{}'", origin_type, target_type)
                );
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
enum ConversionResult {
    NotPossible,
    Converted,
    CastRequired,
}
