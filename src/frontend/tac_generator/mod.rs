mod peephole_optimizations;
use peephole_optimizations::optimize;

use super::ast::*;
use super::semcheck::{ARRAY_LENGTH_PROPERTY};

use crate::common::{
    function_attributes::FunctionAttribute,
    variable_attributes::VariableAttribute,
    types::Type,
    tac_code::*,
    node_info::*,
    symbol_table::{TableEntry, SymbolTable, Symbol},
};

use std::rc::Rc;
use std::collections::HashMap;

pub const ARRAY_LENGTH_ID_OFFSET: u32 = 1000000;

pub struct TACGenerator {
    functions: Vec<Function>,
    function_stack: Vec<Function>,
    loop_label_stack: Vec<(u32, u32)>,
    operands: Vec<Operand>,
    id_counter: u32,
    label_counter: u32,
    symbol_table: SymbolTable,
    tmp_name: Rc<String>,
    array_length_param_data: HashMap<Rc<String>, (u32, DeclarationInfo)>

}

impl TACGenerator {
    pub fn new(start_id: u32) -> TACGenerator {
        TACGenerator {
            functions: vec![],
            function_stack: vec![],
            loop_label_stack: vec![],
            operands: vec![],
            id_counter: start_id,
            label_counter: 0,
            symbol_table: SymbolTable::new(),
            tmp_name: Rc::new(".tmp".to_string()),
            array_length_param_data: HashMap::new(),
        }
    }

    pub fn generate_tac_functions(mut self, node: &AstNode) -> Vec<Function> {
        self.generate_tac(node);

        for function in self.functions.iter_mut() {
            if function.statements.is_empty() {
               function.statements.push(Statement::Empty); // Makes CFG generation easier, if we don't have completely empty function bodies
            }
        }

        optimize(&mut self.functions);
        self.functions
    }

    fn generate_tac(&mut self, node: &AstNode) {
        match node {
            AstNode::Block{ref statements, ref block_symbol_table_entry, ref span} =>
                self.handle_block(statements, block_symbol_table_entry, span),
            AstNode::Function{ref block, ref function_info } =>
                self.handle_function(block, function_info),
           AstNode::ExternFunction{ref function_info} =>
                self.handle_extern_function(function_info),
            AstNode::FunctionCall{ ref arguments, ref function_name, ref span} =>
                self.handle_function_call(function_name, arguments, span),
            AstNode::VariableDeclaration{ref initialization_expression, ref declaration_info} =>
                self.handle_variable_declaration(initialization_expression, declaration_info),
            AstNode::VariableAssignment{ref expression, ref name, ..} =>
                self.handle_variable_assignment(expression, name),
            AstNode::ArrayDeclaration{ref initialization_expression, dimensions: _, ref declaration_info} =>
                self.handle_array_declaration(initialization_expression, declaration_info),
            AstNode::ArrayAccess { index_expression, indexable_expression} => {
                self.handle_array_access(indexable_expression, index_expression);
            },
            AstNode::ArraySlice { start_expression, end_expression, array_expression}=> {
                self.handle_array_slice(start_expression, end_expression, array_expression);
            },
            AstNode::ArrayAssignment { assignment_expression, index_expression, variable_name, span: _, } =>
                self.handle_array_assignment(variable_name, index_expression, assignment_expression),
            AstNode::MemberAccess {
                object,
                member,
                span: _,
            } => self.handle_member_access(object, member),
            AstNode::Plus { .. } |
            AstNode::Minus { .. } |
            AstNode::Multiply { .. } |
            AstNode::Divide { .. } |
            AstNode::Modulo { .. } =>
                self.handle_arithmetic_node(node),
            AstNode::Long { .. } |
            AstNode::Integer{ .. } |
            AstNode::Short{ .. } |
            AstNode::Byte{ .. } |
            AstNode::Boolean{ .. } => self.handle_constant(node),
            AstNode::Identifier { name, ..} =>
                self.handle_identifier(name),
            AstNode::Return { return_value, .. } => self.handle_return(return_value),
            AstNode::Loop {condition_expression, post_body_statements, block, ..} =>
                self.handle_loop(condition_expression, post_body_statements, block),
            AstNode::If { condition_expression, main_block, else_block, ..} =>
                self.handle_if(condition_expression, main_block, else_block),
            AstNode::Less{ .. } |
            AstNode::LessOrEq{ .. } |
            AstNode::Equals{ .. } |
            AstNode::NotEquals{ .. } |
            AstNode::GreaterOrEq{ .. } |
            AstNode::Greater{ .. } =>
                self.handle_comparison(node),
            AstNode::Negate{ expression, arithmetic_info } => self.handle_negate(expression, arithmetic_info),
            AstNode::BooleanAnd{ left_expression, right_expression, span} => {
                self.handle_boolean_and(left_expression, right_expression, span);
            },
            AstNode::BooleanOr{ left_expression, right_expression, span} => {
                self.handle_boolean_or(left_expression, right_expression, span);
            },
            AstNode::BooleanNot{ expression, span} => {
                self.handle_boolean_not(expression, span);
            },
            AstNode::BitwiseAnd { ..} |
            AstNode::BitwiseOr { ..} |
            AstNode::BitwiseXor { ..} =>
                self.handle_bitwise_node(node),
            AstNode::BitwiseNot { expression, ..} =>
                self.handle_bitwise_not(expression),
            AstNode::ArithmeticShiftRight { .. } |
            AstNode::LogicalShiftRight { .. } |
            AstNode::LogicalShiftLeft { .. } =>
                self.handle_shift(node),
            AstNode::Cast{ expression, target_type, span } =>
                self.handle_cast(expression, target_type, span),
            AstNode::InitializerList {values, .. } =>
                self.handle_initializer_list(values),
            AstNode::EmptyNode => (),
            AstNode::ErrorNode => ice!("Error node present in three-address-code generation"),
            AstNode::Continue(_) =>
                self.handle_continue(),
            AstNode::Break(_) =>
                self.handle_break(),

            x @ AstNode::IntegralNumber { .. } => ice!("Unexpected unspecified integral number {:?} in TAC generation", x),
            x => todo!("Three-address code generation not implemented for '{}'", x),
        }
    }

    fn handle_block(
        &mut self,
        children: &Vec<AstNode>,
        table_entry: &Option<TableEntry>,
        _span: &Span) {

        if let Some(ref entry) = *table_entry {
           self.symbol_table.push(entry.clone());
        }
        else {
            ice!("No symbol table information attached to AST block node");
        }

        for ref child in children {
            self.generate_tac(child);
        }

        self.symbol_table.pop();
    }

    fn handle_function(&mut self, child: &AstNode, info: &FunctionInfo) {

        let function = Function::new(info.clone());
        self.function_stack.push(function);
        self.array_length_param_data.clear();

        // add mock initializations for function parameters, as later stages
        // always assume that variables are first assigned into before usage

        let level = if let AstNode::Block{ block_symbol_table_entry: Some(ref entry), ..} = *child {
            entry
        } else {
            ice!("Failed to find symbol table entry for function {}",
                info.name);
        };

        for param in info.parameters.iter() {
            let id = if let Some(Symbol::Variable(_, var_id)) =
                level.find_symbol(&param.name) {
                var_id
            } else {
                ice!(
                 "Failed to find symbol '{}' when handling function parameters",
                 param.name);
            };

            self.current_function().statements.push(
                Statement::Assignment{
                    operator: None,
                    destination: Some(Operand::Variable(
                        param.clone(),
                        id)),
                    left_operand: None,
                    right_operand: Some(Operand::Initialized(param.variable_type.clone()))
                });

            // handle array length passing - push array length after ref to array
            if param.variable_type.is_array() {
                // FIXME: HANDLES ONE DIMENSIONAL ARRAYS ONLY!
                // Move length variables into tmp

                let len_id = id + ARRAY_LENGTH_ID_OFFSET;
                let decl_info = DeclarationInfo::new(
                    Rc::new(format!("arraylength_of_{}", param.name)),
                    Span::new(0, 0, 0),
                    Type::Integer);

                self.current_function().statements.push(
                    Statement::Assignment{
                        operator: None,
                        destination: Some(Operand::Variable(
                            decl_info.clone(),
                            len_id)),
                        left_operand: None,
                        right_operand: Some(Operand::Initialized(Type::Integer))
                    });
                self.array_length_param_data.insert(Rc::new(format!("{}_{}", param.name.clone(), id)), (len_id, decl_info));
            }
        }

        self.generate_tac(child);
        self.operands.clear();

        self.functions.push(
            self.function_stack.pop().unwrap_or_else(
                || ice!("Function stack empty when generating function 3AC")));
    }

    fn handle_extern_function(&mut self, info: &FunctionInfo) {
        let mut function = Function::new(info.clone());
        function.attributes.push(FunctionAttribute::External);
        self.functions.push(function);
    }

    fn handle_function_call(
        &mut self,
        name: &Rc<String>,
        args: &Vec<AstNode>,
        _info: &Span) {

        let (dest, params) = if let Some(sym) = self.symbol_table.find_symbol(name) {
            if let Symbol::Function(fi) = sym {
                if fi.return_type == Type::Void {
                    (None, fi.parameters)
                } else {
                    let tmp = self.get_temporary(fi.return_type.clone());
                    self.operands.push(tmp.clone());
                    (Some(tmp), fi.parameters)
                }
            } else {
                ice!(
                    "Invalid symbol {:?} in symbol table when function expected", sym)
            }
        } else {
            ice!("Failed to find function '{}' from symbol table", name);
        };

        let mut function_operands = vec![];
        for (i, arg) in args.iter().enumerate() {

            let mut operand = self.get_operand(arg);


            // handle array length passing
            if let Operand::Variable(info, id ) = operand.clone() {

                if info.variable_type.is_array() {
                    if  info.variable_type.is_reference() {
                        // already a function param, find value variable
                        // FIXME ONLY HANDLES ONE DIMENSIONAL ARRAY
                        let (id, decl_info) = self.array_length_param_data
                            .get(&format!("{}_{}", info.name, id))
                            .unwrap_or_else(|| ice!("No array length information stored for array '{}'", info.name));
                        function_operands.push(operand);
                        operand = Operand::Variable(decl_info.clone(), *id);

                    } else {
                        // declared in this function, use known constants
                        let dims = info.variable_type.get_array_dimensions();
                        self.convert_array_to_ref(&params[i], &mut operand, info, id);
                        function_operands.push(operand);
                        // push N - 1 lengths into param list, leave last for the base push at the bottom of this loop
                        for size in dims.iter().take(dims.len() - 1) {
                            function_operands.push(Operand::Integer(*size));
                        }
                        operand = Operand::Integer(*dims.last().unwrap_or_else(|| ice!("Array '{:?}'has no dimension data available", arg)));
                    }
                }
            }


            // if we use array, then push length(s)

            function_operands.push(operand);
        }

        self.current_function().statements.push(
            Statement::Call(
                name.clone(),
                function_operands,
                dest));
    }

    fn convert_array_to_ref(&mut self, decl_info: &DeclarationInfo, operand: &mut Operand, info: DeclarationInfo, id: u32) {
        if let Type::Reference(ref param) = decl_info.variable_type {
            if param.is_array() {
                // arg is array, param is ref to array - generate address move code
                *operand = self.get_temporary(decl_info.variable_type.clone());
                self.current_function().statements.push(
                    Statement::Assignment {
                        operator: None,
                        destination: Some(operand.clone()),
                        left_operand: None,
                        right_operand: Some(Operand::AddressOf { variable_info: info, id })
                    });
            }
        }
    }

    fn handle_variable_declaration(
        &mut self,
        child: &AstNode,
        info: &DeclarationInfo) {

        if info.attributes.contains(&VariableAttribute::Const) {
            ice!("Const variable present in three-address-code generation - expected to be inlined");
        }

        self.declaration_assignment_common(child, &info.name);
    }

    fn handle_array_declaration(
        &mut self,
        child: &AstNode,
        info: &DeclarationInfo
    ) {

        if info.attributes.contains(&VariableAttribute::Const) {
            ice!("Const variable present in three-address-code generation - expected to be inlined");
        }

        self.generate_tac(child);
        let (_, id) = self.get_variable_info_and_id(&info.name);

        let length = info.variable_type.get_array_dimensions().iter().fold(0, |acc, dim| acc + dim);

        self.allocate_space_for_array(id,
                                      length,
                                      (length as u32)*info.variable_type.get_array_basic_type().size_in_bytes());

        if let AstNode::InitializerList { .. } = child {
            self.emit_array_initialization_using_initializer_list(id, info, length);
        }  else {
            let operand = self.operands.pop().unwrap_or_else(|| ice!("No initialization value provided for array"));
            self.emit_array_initialization_using_shorthand(id, info, length, operand);
        }
    }

    fn allocate_space_for_array(&mut self,
                                id: u32,
                                length: i32,
                                size_in_bytes: u32) {
        self.current_function().statements.push(Statement::Array{ id, length, size_in_bytes });
    }

    fn emit_array_initialization_using_initializer_list(
        &mut self,
        id: u32,
        variable_info: &DeclarationInfo,
        size: i32,
        ) {

        // FIXME: Constant array should be a pointer to readonly data section, skip initialization in code

        for i in (0..size).rev() {

            let operand = self.operands.pop().unwrap_or_else(|| ice!("Missing array initializer value"));
            self.current_function().statements.push(
                Statement::Assignment{
                    operator: None,
                    destination: Some(Operand::ArrayIndex {
                        id,
                        variable_info: variable_info.clone(),
                        index_operand: Box::new(Operand::Integer(i)),
                    }),
                    left_operand: None, right_operand: Some(operand),
                }
            );
        }

    }

    fn emit_array_initialization_using_shorthand(
        &mut self,
        id: u32,
        variable_info: &DeclarationInfo,
        size: i32,
        operand: Operand) {

        // FIXME: Right now there is no runtime, so initialing the array is done like this. Replacing this with a call to memset would likely make more sense
        // FIXME: Constant array should be a pointer to readonly data section, skip initialization in code
        /*
            i = 0;
            START:
            i <= ARRAY_SIZE
            JUMP_IF_FALSE END
            array[i] = OPERAND
            i = i + 1
            JUMP START
            END:

        */


        let start_label_id= self.get_label_id();
        let end_label_id = self.get_label_id();
        let index_var = self.get_temporary(Type::Integer);
        let cmp_result = self.get_temporary(Type::Boolean);


        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(index_var.clone()),
                left_operand: None,
                right_operand: Some(Operand::Integer(0))
            });

        self.current_function().statements.push(
            Statement::Label(start_label_id));
        self.current_function().statements.push(
            Statement::Assignment{
                operator: Some(Operator::Less),
                destination: Some(cmp_result.clone()),
                left_operand: Some(index_var.clone()),
                right_operand: Some(Operand::Integer(size))
            });

        self.current_function().statements.push(
            Statement::JumpIfFalse(cmp_result, end_label_id));

        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::ArrayIndex {
                    id,
                    variable_info: variable_info.clone(),
                    index_operand: Box::new(index_var.clone()),
                }),
                left_operand: None,
                right_operand: Some(operand)
            });

        self.current_function().statements.push(
            Statement::Assignment{
                operator: Some(Operator::Plus),
                destination: Some(index_var.clone()),
                left_operand: Some(index_var.clone()),
                right_operand: Some(Operand::Integer(1))
            });

        self.current_function().statements.push(
            Statement::Jump(start_label_id));
        self.current_function().statements.push(
            Statement::Label(end_label_id));

    }

    fn handle_variable_assignment(
        &mut self,
        child: &AstNode,
        name: &String) {

        let (declaration_info, _) = self.get_variable_info_and_id(name);

        ice_if!(declaration_info.attributes.contains(&VariableAttribute::Const)
            || declaration_info.attributes.contains(&VariableAttribute::ReadOnly),
            "Generating TAC assignment to constant or read-only array {}", name);

        self.declaration_assignment_common(child, name);
    }

    fn declaration_assignment_common(
        &mut self,
        child: &AstNode,
        name: &String) {

        self.generate_tac(child);

        let (variable_info, id) = self.get_variable_info_and_id(name);

        let operand = self.operands.pop();
        self.generate_assignment(
            variable_info,
            id,
            operand)
    }

    fn generate_assignment(
        &mut self,
        var_info: DeclarationInfo,
        id: u32,
        operand: Option<Operand>) {
        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(
                    Operand::Variable(
                        var_info, id)),
                left_operand: None,
                right_operand: operand
            });
    }

    fn handle_array_access(&mut self,
        indexable_expression: &AstNode,
        index_expression: &AstNode,
    ) {

        let index = self.get_operand(index_expression);

        let (array_type, name) = match indexable_expression {
            AstNode::Identifier{ name, ..} => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    if info.variable_type.is_array() {
                        (info.variable_type.get_array_basic_type(), name.clone())
                    } else {
                        ice!("Non-array type '{}' variable '{}'", info.variable_type, name)
                    }
                } else {
                    ice!("Invalid symbol '{:?}'", self.symbol_table.find_symbol(name));
                }
            },
            _ => ice!("Invalid AST node '{:?}'", indexable_expression),
        };

        let dst = self.get_temporary(array_type);

        let (var_info, id) = self.get_variable_info_and_id(&name);

        self.current_function().statements.push(
            Statement::Assignment {
                operator: None,
                destination: Some(dst.clone()),
                left_operand: None,
                right_operand: Some(Operand::ArrayIndex {
                    index_operand: Box::new(index),
                    variable_info: var_info,
                    id
                })
            });

        self.operands.push(dst);
    }

    fn handle_array_slice(
        &mut self,
        start_expression: &AstNode,
        end_expression: &AstNode,
        array_expression: &AstNode,
    ) {

        let start_operand = self.get_operand(start_expression);
        let end_operand = self.get_operand(end_expression);

        let (array_type, name) = match array_expression {
            AstNode::Identifier{ name, ..} => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    if info.variable_type.is_array() {
                        (Type::Reference(Box::new(info.variable_type.clone())), name.clone())
                    } else {
                        ice!("Non-array type '{}' variable '{}'", info.variable_type, name)
                    }
                } else {
                    ice!("Invalid symbol '{:?}'", self.symbol_table.find_symbol(name));
                }
            },
            _ => ice!("Invalid AST node '{:?}'", array_expression),
        };

        let dst = self.get_temporary(array_type);

        let (var_info, id) = self.get_variable_info_and_id(&name);

        let length_dst = self.get_temporary(Type::Integer);

        self.current_function().statements.push(
            Statement::Assignment {
                operator: Some(Operator::Minus),
                destination: Some(length_dst.clone()),
                left_operand: Some(end_operand.clone()),
                right_operand: Some(start_operand.clone()),
            });


        match (&dst, &length_dst) {
            (
                Operand::Variable(dst_decl_info, dst_id),
                Operand::Variable(length_dst_decl_info, length_dst_id),
            )  => {
                self.array_length_param_data.insert(
                    Rc::new(format!("{}_{}", dst_decl_info.name, dst_id)),
                    (*length_dst_id, length_dst_decl_info.clone()),
                );
            }
            _ => ice!("Unexpected operands {:?} and {:?}", dst, length_dst)
        }


        self.current_function().statements.push(
            Statement::Assignment {
                operator: None,
                destination: Some(dst.clone()),
                left_operand: None,
                right_operand: Some(Operand::ArraySlice {
                    start_operand: Box::new(start_operand),
                    end_operand: Box::new(end_operand),
                    variable_info: var_info,
                    id
                })
            });

        self.operands.push(dst);
    }

    fn handle_array_assignment(
        &mut self,
        variable_name: &str,
        index_expression: &AstNode,
        assignment_expression: &AstNode) {

        let index_operand = self.get_operand(index_expression);
        let assignment_operand = self.get_operand(assignment_expression);

        let (var_info, id) = self.get_variable_info_and_id(variable_name);


        ice_if!(var_info.attributes.contains(&VariableAttribute::Const)
            || var_info.attributes.contains(&VariableAttribute::ReadOnly),
            "Generating TAC assignment to constant or read-only array {}", variable_name);

        self.current_function().statements.push(
            Statement::Assignment {
                operator: None,
                destination: Some(Operand::ArrayIndex {
                    index_operand: Box::new(index_operand),
                    variable_info: var_info,
                    id,
                }),
                left_operand: None,
                right_operand: Some(assignment_operand),
            }
        );
    }

    fn handle_member_access(
        &mut self,
        object: &AstNode,
        member: &AstNode) {

        if let AstNode::Identifier{name, ..} = object {
            let (decl_info, array_id) = self.get_variable_info_and_id(name);
            ice_if!(!decl_info.variable_type.is_array(), "Access not implemented for non-arrays");

            if let AstNode::Identifier{ name: prop, ..} = member {
                ice_if!(**prop != ARRAY_LENGTH_PROPERTY, "Not implemented for non-length properties: {:?}", member);

                let operand =  if decl_info.variable_type.is_reference() {
                    let (length_id, declaration_info) = self.array_length_param_data
                        .get(&format!("{}_{}", name, array_id))
                        .unwrap_or_else(|| ice!("No array length information stored for array '{}'", name));
                    Operand::Variable(declaration_info.clone(), *length_id)
                } else {
                    Operand::Integer(decl_info.variable_type.get_array_dimensions()[0])
                };

                self.operands.push(operand);
            } else {
                ice!("Member access TAC generation not implemented for member: {:?}", member)
            }

        } else {
            todo!("Member access TAC generation not implemented for: {:?}", object)
        }
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {

       let (operator, left_child, right_child) = match *node {
            AstNode::Plus{ref left_expression, ref right_expression, ..} =>
                (Operator::Plus, left_expression, right_expression),
            AstNode::Minus{ref left_expression, ref right_expression, ..} =>
                (Operator::Minus, left_expression, right_expression),
            AstNode::Multiply{ref left_expression, ref right_expression, ..} =>
                (Operator::Multiply, left_expression, right_expression),
            AstNode::Divide{ref left_expression, ref right_expression, ..} =>
                (Operator::Divide, left_expression, right_expression),
           AstNode::Modulo{ref left_expression, ref right_expression, ..} =>
               (Operator::Modulo, left_expression, right_expression),
           _ => ice!("Invalid node '{}' passed when arithmetic node expected", node),
        };

        let left_op = self.get_operand(left_child);
        let right_op = self.get_operand(right_child);


        if self.get_type(&left_op) != self.get_type(&right_op) {
            ice!(
                "Left and right operand have differing types: '{}' vs '{}'",
                self.get_type(&left_op),
                self.get_type(&right_op));
        }

        let tmp_type = self.get_type(&left_op);
        let temp = self.get_temporary(tmp_type);

        self.current_function().statements.push(Statement::Assignment {
            operator: Some(operator),
            destination: Some(temp.clone()),
            left_operand: Some(left_op),
            right_operand: Some(right_op),
        });

        self.operands.push(temp)
    }

    fn handle_identifier(&mut self, name: &String) {

        let (variable_info, id) = self.get_variable_info_and_id(name);


        // const values are expected to be folded - if we see them, something has gone wrong
        ice_if!(
            variable_info.attributes.contains(&VariableAttribute::Const),
            "Const value seen when it should have been folded at earlier stage");
        self.operands.push(Operand::Variable(variable_info, id));
    }

    fn handle_constant(&mut self, node : &AstNode) {
        let operand = match node {
            AstNode::Long { value, ..} => {
                match value {
                    AstLong::Long(val) => Operand::Long(*val),
                    _ => ice!("Invalid integer type in three-address code generation: {}", value),
                }
            }
            AstNode::Integer{ value, ..} => {
                match value {
                    AstInteger::Int(val) => Operand::Integer(*val),
                    _ => ice!("Invalid integer type in three-address code generation: {}", value),
                }
            }
            AstNode::Short{ value, ..} => {
                match value {
                    AstShort::Short(val) => Operand::Short(*val),
                    _ => ice!("Invalid integer type in three-address code generation: {}", value),
                }
            }
            AstNode::Boolean{ value, ..} => Operand::Boolean(*value),
            AstNode::Byte { value, .. } => {
                if let AstByte::Byte(b) = value {
                   Operand::Byte(*b)
                } else {
                    ice!("Invalid byte type in three-adress code generation: {}", value);
                }
            }
            _ => ice!("Unexpected node '{:?}' encountered when constant was expected during TAC generation", node),
        };

        self.operands.push(operand);
    }

    fn handle_return(&mut self, child: &Option<Box<AstNode>>) {
        let operand = if let Some(ref node) = *child {
            Some(self.get_operand(node))
        } else {
            None
        };

        self.current_function().statements.push(Statement::Return(operand));
    }

    fn handle_loop(&mut self, expr: &AstNode, post_body_statements: &Option<Vec<AstNode>>, block: &AstNode) {
        let comparison_label_id = self.get_label_id();
        let post_body_label = self.get_label_id();
        let block_label_id = self.get_label_id();
        let end_label_id = self.get_label_id();

        self.loop_label_stack.push((post_body_label, end_label_id));

        self.current_function().statements.push(
            Statement::Jump(comparison_label_id));
        self.current_function().statements.push(
            Statement::Label(block_label_id));
        self.generate_tac(block);


        self.current_function().statements.push(
            Statement::Label(post_body_label));
        if let Some(statements) = post_body_statements {
            for s in statements.into_iter() {
                self.generate_tac(s);
            }
        }

        self.current_function().statements.push(
            Statement::Label(comparison_label_id));
        let operand = self.get_operand(expr);
        self.current_function().statements.push(
            Statement::JumpIfTrue(operand, block_label_id));

        self.current_function().statements.push(
            Statement::Label(end_label_id)
        );

        self.loop_label_stack.pop();
    }


    fn handle_if(
        &mut self,
        condition: &AstNode,
        if_blk: &AstNode,
        opt_else_blk: &Option<Box<AstNode>>) {

        /*
            COMPARE
            JUMP TO ELSE_LABEL IF COMPARISON FALSE
                TRUE BLOCK
                OPTIONAL JUMP TO OUT_LABEL IF ELSE BLOCK PRESENT
            ELSE_LABEL
                OPTIONAL ELSE BLOCK
            OPTIONAL OUT_LABEL
        */
        let skip_true_block = self.get_label_id();
        let out_label = self.get_label_id();

        let operand = self.get_operand(&condition);
        self.current_function().statements.push(
            Statement::JumpIfFalse(operand, skip_true_block));

        self.generate_tac(if_blk);

        if let Some(_) = opt_else_blk {
            self.current_function().statements.push(
                Statement::Jump(out_label));
        }

        self.current_function().statements.push(
            Statement::Label(skip_true_block));

        if let Some(else_block) = opt_else_blk {
            self.generate_tac(else_block);
            self.current_function().statements.push(
                Statement::Label(out_label));
        }
    }
    fn handle_comparison(&mut self, node: &AstNode) {
        let (operator, left, right) = match node {
            AstNode::Less{ left_expression, right_expression, ..} =>
                (Operator::Less, left_expression, right_expression),
            AstNode::LessOrEq{ left_expression, right_expression, ..} =>
                (Operator::LessOrEq, left_expression, right_expression),
            AstNode::Equals{ left_expression, right_expression, ..} =>
                (Operator::Equals, left_expression, right_expression),
            AstNode::NotEquals{ left_expression, right_expression, ..} =>
                (Operator::NotEquals, left_expression, right_expression),
            AstNode::GreaterOrEq{ left_expression, right_expression, ..} =>
                (Operator::GreaterOrEq, left_expression, right_expression),
            AstNode::Greater{ left_expression, right_expression, ..} =>
                (Operator::Greater, left_expression, right_expression),
            _ => ice!("Invalid AstNode '{}' passed for comparison handling", node)
        };

        let left_op = self.get_operand(left);
        let right_op = self.get_operand(right);

        let temp = self.get_temporary(Type::Boolean);

        self.current_function().statements.push(Statement::Assignment {
            operator: Some(operator),
            destination: Some(temp.clone()),
            left_operand: Some(left_op),
            right_operand: Some(right_op),
        });

        self.operands.push(temp);
    }

    fn handle_negate(&mut self, child_node: &AstNode, arith_info: &ArithmeticInfo) {
        let op = self.get_operand(child_node);

        let temp = self.get_temporary(arith_info.node_type.clone());
        self.current_function().statements.push(Statement::Assignment {
            operator: Some(Operator::Minus),
            destination: Some(temp.clone()),
            left_operand: None,
            right_operand: Some(op)
        });

        self.operands.push(temp);
    }

    fn handle_boolean_and(&mut self, left: &AstNode, right: &AstNode, _span: &Span) {
       /*

            GET FIRST EXPRESSION RESULT
            JUMP TO out_lbl IF FALSE
            GET SECOND EXPRESSION RESULT
            out_lbl:
       */

        let out_label = self.get_label_id();
        let tmp = self.get_temporary(Type::Boolean);


        let operand = self.get_operand(left);
        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(tmp.clone()),
                left_operand: None,
                right_operand: Some(operand.clone())
            });
        self.current_function().statements.push(
            Statement::JumpIfFalse(operand, out_label));
        let operand = self.get_operand(right);
        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(tmp.clone()),
                left_operand: None,
                right_operand: Some(operand)});
        self.operands.push(tmp);
        self.current_function().statements.push(
            Statement::Label(out_label));
    }

    fn handle_boolean_or(&mut self, left: &AstNode, right: &AstNode, _span: &Span) {
        /*
            GET FIRST EXPRESSION RESULT
            JUMP TO out_lbl IF TRUE
            GET SECOND EXPRESSION RESULT
            out_lbl:
        */

        let out_label = self.get_label_id();
        let tmp = self.get_temporary(Type::Boolean);

        let operand = self.get_operand(left);
        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(tmp.clone()),
                left_operand: None,
                right_operand: Some(operand.clone())
            });
        self.current_function().statements.push(
            Statement::JumpIfTrue(operand, out_label));
        let operand = self.get_operand(right);
        self.current_function().statements.push(
            Statement::Assignment{
                operator: None,
                destination: Some(tmp.clone()),
                left_operand: None,
                right_operand: Some(operand)
            });
        self.operands.push(tmp);
        self.current_function().statements.push(
            Statement::Label(out_label));
    }

    fn handle_boolean_not(&mut self, node: &AstNode, _span: &Span) {
        let operand = self.get_operand(node);
        let tmp = self.get_temporary(Type::Boolean);
        self.current_function().statements.push(
            Statement::Assignment{
                operator: Some(Operator::BitwiseXor),
                destination: Some(tmp.clone()),
                left_operand: Some(operand),
                right_operand: Some(Operand::Byte(1))
           });

        self.operands.push(tmp);
    }

    fn handle_bitwise_node(&mut self, node: &AstNode) {

        let (operator, left_child, right_child) = match *node {
            AstNode::BitwiseAnd{ref left_expression, ref right_expression, ..} =>
                (Operator::BitwiseAnd, left_expression, right_expression),
            AstNode::BitwiseOr{ref left_expression, ref right_expression, ..} =>
                (Operator::BitwiseOr, left_expression, right_expression),
            AstNode::BitwiseXor{ref left_expression, ref right_expression, ..} =>
                (Operator::BitwiseXor, left_expression, right_expression),
            _ => ice!("Invalid node '{}' passed when arithmetic node expected", node),
        };

        let left_op = self.get_operand(left_child);
        let right_op = self.get_operand(right_child);


        if self.get_type(&left_op) != self.get_type(&right_op) {
            ice!(
                "Left and right operand have differing types: '{}' vs '{}'",
                self.get_type(&left_op),
                self.get_type(&right_op));
        }

        let tmp_type = self.get_type(&left_op);
        let temp = self.get_temporary(tmp_type);

        self.current_function().statements.push(Statement::Assignment {
            operator: Some(operator),
            destination: Some(temp.clone()),
            left_operand: Some(left_op),
            right_operand: Some(right_op),
        });

        self.operands.push(temp)
    }


    fn handle_bitwise_not(&mut self, node: &AstNode) {

        let operand = self.get_operand(node);
        let tmp = self.get_temporary(self.get_type(&operand));
        self.current_function().statements.push(
            Statement::Assignment{
                operator: Some(Operator::BitwiseNot),
                destination: Some(tmp.clone()),
                left_operand: None,
                right_operand: Some(operand),
           });

        self.operands.push(tmp);
    }

    fn handle_shift(&mut self, node: &AstNode) {

        let (shift_operator, value, shift_count, arithmetic_info) = match node {
            AstNode::ArithmeticShiftRight { value, shift_count, arithmetic_info } => {
                (Operator::ArithmeticShiftRight, value, shift_count, arithmetic_info)
            },
            AstNode::LogicalShiftRight { value, shift_count, arithmetic_info } => {
                (Operator::LogicalShiftRight, value, shift_count, arithmetic_info)
            },
            AstNode::LogicalShiftLeft{  value, shift_count, arithmetic_info } => {
                (Operator::LogicalShiftLeft, value, shift_count, arithmetic_info)
            },
            _ => ice!("Invalid node {:?}, expected shift node", node),
        };

        let value = self.get_operand(value);
        // treat constant byte and long values as integer values for shift count - simplifies later stages
        let shift_count = match self.get_operand(shift_count) {
            Operand::Byte(value) => Operand::Integer(value as i32),
            Operand::Long(value) => Operand::Integer(value as i32),
            other => other,
        };

        let destination = self.get_temporary(arithmetic_info.node_type.clone());

        self.current_function().statements.push(
            Statement::Assignment {
                destination: Some(destination.clone()),
                left_operand: Some(value),
                right_operand: Some(shift_count),
                operator: Some(shift_operator.clone()),
            }
        );

        self.operands.push(destination);
    }

    fn handle_cast(&mut self, expression: &AstNode, target_type: &Type, _span: &Span) {
        let destination = self.get_temporary(target_type.clone());

        let operand = self.get_operand(expression);
        let operand_type = self.get_type(&operand);
        self.current_function().statements.push(
            Statement::Assignment {
                destination: Some(destination.clone()),
                left_operand: None,
                right_operand: Some(operand),
                operator: Some(Operator::Cast{
                    from: operand_type,
                    to: target_type.clone()}),
            }
        );

        self.operands.push(destination);
    }

    fn handle_continue(&mut self) {
        let (start_label, _) = *self.loop_label_stack.last().unwrap_or_else(|| ice!("Loop label stack is empty"));
        self.current_function().statements.push(
                Statement::Jump(start_label)

        );
    }

    fn handle_break(&mut self) {
        let (_, end_label) = *self.loop_label_stack.last().unwrap_or_else(|| ice!("Loop label stack is empty"));
        self.current_function().statements.push(
            Statement::Jump(end_label)
        );
    }


    fn handle_initializer_list(&mut self, values: &Vec<AstNode>) {
        values.iter().for_each(|node| self.generate_tac(node));
    }

    fn get_type(&self, operand: &Operand) -> Type {
        match *operand {
            Operand::Variable(ref info, _) => info.variable_type.clone(),
            Operand::AddressOf { ref variable_info, id: _ } => {
              Type::Reference(Box::new(variable_info.variable_type.clone()))
            },
            Operand::Long(_) => Type::Long,
            Operand::Integer(_) => Type::Integer,
            Operand::Short(_) => Type::Short,
            Operand::Byte(_) => Type::Byte,
            Operand::Float(_) => Type::Float,
            Operand::Double(_) => Type::Double,
            Operand::Boolean(_) => Type::Boolean,
            Operand::Initialized(ref t) => t.clone(),
            Operand::ArrayIndex {index_operand: _, id: _, variable_info: ref var_info } => {
                var_info.variable_type.get_array_basic_type()
            },
            Operand::ArraySlice{ ref variable_info, .. } => {
                Type::Reference(Box::new(variable_info.variable_type.clone()))
            },
            Operand::SSAVariable(_, _, _) =>
                ice!("Unexpected SSA variable during TAC generation"),
        }
    }

    fn get_operand(&mut self, node: &AstNode) -> Operand {
        self.generate_tac(node);
        self.operands.pop().unwrap_or_else(|| ice!("Operand stack empty"))
    }

    fn get_temporary(&mut self, var_type: Type) -> Operand {
        let id = self.get_next_id();

        let mut info = DeclarationInfo::new(
            self.tmp_name.clone(),
            Span::new(0,0,0),
            var_type);
        info.attributes.insert(VariableAttribute::Synthetic);
        Operand::Variable(
            info,
            id)
    }

    fn get_next_id(&mut self) -> u32 {
        let ret = self.id_counter;
        self.id_counter += 1;
        ret
    }

    fn get_label_id(&mut self) -> u32 {
        let ret = self.label_counter;
        self.label_counter += 1;
        ret
    }

    fn get_variable_info_and_id(&self, name: &str) -> (DeclarationInfo, u32) {
        let symbol = self.symbol_table.find_symbol(name).unwrap_or_else(
                || ice!("No symbol '{}' found during TAC construction", name));

        match symbol {
            Symbol::Variable(variable_info, id) => (variable_info, id),
            _ => ice!("Invalid symbol found during TAC construction - expected variable but got '{:?}' instead", symbol),
        }
    }

    fn current_function(&mut self) -> &mut Function {
        self.function_stack.last_mut().unwrap_or_else(|| ice!("Internal compiler error: Function stack empty"))
    }
}