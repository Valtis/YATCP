pub mod byte_code;

use byte_code::*;

use crate::common::node_info::DeclarationInfo;

use std::collections::HashMap;

use crate::common::{
    function_attributes::FunctionAttribute,
    types::Type,
    tac_code::{Statement, Operator, Operand, Function as TACFunction},
    constants::ARRAY_LENGTH_SLOT_SIZE,
};

pub struct ByteGenerator {
    pub bytecode_functions: Vec<Function>,
    tac_functions: Vec<TACFunction>,
    next_register: u32,
    variable_id_to_register: HashMap<u32, VirtualRegisterData>,
}

impl ByteGenerator {
    pub fn new(functions: Vec<TACFunction>) -> ByteGenerator {
        ByteGenerator {
            bytecode_functions: vec![],
            tac_functions: functions,
            next_register: 0,
            variable_id_to_register: HashMap::new(),
        }
    }

    pub fn print_bytecode(&self) {
        println!();
        for f in self.bytecode_functions.iter() {
            f.print_bytecode();
        }

    }

    pub fn generate_bytecode(&mut self) {
        // clone to fix borrow issue. TODO: Figure out how to avoid an unnecessary copy.
        let functions = self.tac_functions.clone();

        for f in functions {


            self.bytecode_functions.push(Function {
                name: (*f.function_info.name).clone(),
                code: vec![],
                parameter_count: 0,
                attributes: f.attributes,
            });
            self.next_register = 0;

            let cmp = vec![Operator::Less, Operator::LessOrEq, Operator::Equals, Operator::NotEquals, Operator::GreaterOrEq, Operator::Greater];

            for s in f.statements {
                match s {
                    Statement::Assignment{
                        operator: None,
                        destination: Some(ref dest),
                        left_operand: None,
                        right_operand: Some(ref src @ Operand::AddressOf { variable_info: _, id: _ })} => self.emit_lea(src, dest),
                    Statement::Assignment{
                        operator: Some(ref op @ Operator::Plus),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)} |
                    Statement::Assignment{
                        operator: Some(ref op @ Operator::Minus),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)} |
                    Statement::Assignment{
                        operator: Some(ref op @ Operator::Multiply),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)} |
                    Statement::Assignment{
                        operator: Some(ref op @ Operator::Divide),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)} |
                    Statement::Assignment{
                        operator:  Some(ref op @ Operator::Modulo),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)} |
                    Statement::Assignment {
                        operator: Some(ref op @ Operator::ArithmeticShiftRight),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2 )
                    } |
                    Statement::Assignment {
                        operator: Some(ref op @ Operator::LogicalShiftRight),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)
                    } |
                    Statement::Assignment {
                        operator: Some(ref op @ Operator::LogicalShiftLeft),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1 ),
                        right_operand: Some(ref op2)
                    } |
                    Statement::Assignment{
                        operator: Some(ref op @ Operator::Xor),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1 ),
                        right_operand: Some(ref op2 )} => self.emit_binary_op(op.clone(), op1, op2, dest),
                    Statement::Assignment{
                        operator: None,
                        destination: Some(ref dest),
                        left_operand: None,
                        right_operand: Some(ref src)} => self.emit_move(src, dest),
                    Statement::Assignment {
                        operator: Some(Operator::Cast{ ref from, ref to }),
                        destination: Some(ref dest),
                        left_operand: None,
                        right_operand: Some(ref src),
                    } => self.emit_move_with_casting(src, dest, from, to),
                    Statement::Assignment{
                        operator: Some(Operator::Minus),
                        destination: Some(ref dest),
                        left_operand: None,
                        right_operand: Some(ref src)}=> self.emit_negate(dest, src),
                    Statement::Assignment{
                        operator: Some(ref x),
                        destination: Some(ref dest),
                        left_operand: Some(ref op1),
                        right_operand: Some(ref op2)} if cmp.contains(x) =>
                        self.emit_comparison(x, op1, op2, dest),
                    Statement::Array{id, length, size_in_bytes} => {
                        self.emit_array_init(id, length, size_in_bytes);
                    },
                    Statement::Return(val) => self.emit_return(val),
                    Statement::Label(id) => self.emit_label(id),
                    Statement::Jump(id) => self.emit_jump(id),
                    Statement::JumpIfTrue(ref operand, id) => self.emit_conditional_equals_jump(operand, id),
                    Statement::JumpIfFalse(ref operand, id) => self.emit_conditional_not_equals_jump(operand, id),
                    Statement::Call(ref callee, ref args, ref retval) => self.emit_function_call(callee, args, retval),
                    Statement::Empty => (),
                   _ => panic!("Not implemented: {:?}", s),
                }
            }

            ice_if!(
                self.current_function().code.is_empty() && !self.current_function().has_attribute(FunctionAttribute::External),
                "No bytecode was generated for function {}", f.function_info.name);
        }
    }

    fn emit_return(&mut self, retval: Option<Operand>) {
        let ret_val = if let Some(op) = retval {
             Some(self.get_source(&op))
        } else {
            None
        };

        self.current_function().code.push(ByteCode::Ret(ret_val));
    }

    fn emit_negate(&mut self, dest: &Operand, src: &Operand) {
        let data = self.form_unary_operation(src, dest);
        self.current_function().code.push(ByteCode::Negate(data));
    }

    fn emit_move(&mut self, src: &Operand, dest: &Operand) {
        let data = self.form_unary_operation(src, dest);
        self.current_function().code.push(ByteCode::Mov(data));
    }

    fn emit_move_with_casting(
        &mut self,
        src: &Operand,
        dest: &Operand,
        from: &Type,
        to: &Type) {
        let mut data = self.form_unary_operation(src, dest);

         match data.src {
             Value::VirtualRegister(VirtualRegisterData { ref mut size, ..}) => {
                 *size = to.size_in_bytes();
             },
             _ => ice!("Unexpected value {:?}", data.src)
         }

        let op = if from.size_in_bytes() >= to.size_in_bytes() {
            ByteCode::Mov(data)
        } else {
            ByteCode::Movsx(data)
        };

        self.current_function().code.push(op);
    }

    fn emit_lea(&mut self, src: &Operand, dest: &Operand) {
        let data = self.form_unary_operation(src, dest);
        self.current_function().code.push(ByteCode::Lea(data));
    }

    fn emit_array_init(&mut self, id: u32, _length: i32, size_in_bytes: u32) {
        self.current_function().code.push(ByteCode::PseudoArrayInit { size_in_bytes, id });
    }

    fn emit_comparison(&mut self,
        operator: &Operator,
        op1: &Operand,
        op2: &Operand,
        dest: &Operand) {

        let comparison_type = match *operator {
            Operator::Less => ComparisonType::Less,
            Operator::LessOrEq => ComparisonType::LessOrEq,
            Operator::Equals => ComparisonType::Equals,
            Operator::NotEquals => ComparisonType::NotEquals,
            Operator::GreaterOrEq => ComparisonType::GreaterOrEq,
            Operator::Greater => ComparisonType::Greater,
            _ => ice!("Invalid operator '{}' for comparison", operator),
        };

        let src1 = self.get_source(op1);
        let src2 = self.get_source(op2);

        self.current_function().code.push(
            ByteCode::Compare(
                ComparisonOperation{
                    src1,
                    src2,
                }));

        if let Operand::Variable(declaration_info, id) = dest {
            let reg = self.get_register_for(declaration_info,*id);
            self.current_function().code.push(
                ByteCode::Mov(UnaryOperation{
                    src: Value::ComparisonResult(comparison_type.clone()),
                    dest: reg,
                })
            );
        } else {
            ice!("Non-variable destination for comparison results");
        }

    }

    fn emit_label(&mut self, id: u32) {
        self.current_function().code.push(ByteCode::Label(id));
    }

    fn emit_jump(&mut self, id: u32) {
        self.current_function().code.push(ByteCode::Jump(id));
    }

    fn emit_conditional_equals_jump(&mut self, op: &Operand, id: u32) {
        self.emit_conditional_jump(op, id, false);
    }

    fn emit_conditional_not_equals_jump(&mut self, op: &Operand, id: u32) {
        self.emit_conditional_jump(op, id, true);
    }

    fn emit_conditional_jump(&mut self, op: &Operand, id: u32, reverse_comparison: bool) {

        let src = self.get_source(op);

        match src {
            Value::VirtualRegister(vregdata) => {
                ice_if!(
                    vregdata.size != 1,
                    "Non-boolean register value used for conditional jump:\n{:#?}",
                    vregdata);

                self.current_function().code.push(
                    ByteCode::Compare(ComparisonOperation {
                        src1: Value::VirtualRegister(vregdata.clone()),
                        src2: Value::ByteConstant(1),
                    })
                );

                let cmp = if reverse_comparison {
                    ComparisonType::NotEquals
                } else {
                    ComparisonType::Equals
                };
                self.current_function().code.push(
                    ByteCode::JumpConditional(id, cmp));
            },
            _ => ice!("Invalid operand type in conditional jump: {:#?}", src),
        }
    }

    fn emit_function_call(&mut self, callee: &str, args: &Vec<Operand>, retval: &Option<Operand>) {

        let mut arguments = vec![];
        for  arg in args.iter() {
            let value = self.get_source(arg);
            arguments.push(value);
        }

        if !arguments.is_empty() {
            self.current_function().code.push(
                ByteCode::FunctionArguments(arguments)
            );
        }

        self.current_function().code.push(
            ByteCode::Call(callee.to_owned() )
        );

        if let Some(dest) = retval {
            let dest = self.get_source(dest);
            self.current_function().code.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: Value::ReturnValue,
                        dest,
                    }
            ));
        }

    }

    fn emit_binary_op(&mut self, operator: Operator, op1: &Operand, op2: &Operand, dest: &Operand) {
        let data = self.form_binary_operation(op1, op2, dest);
        let code = self.form_binary_code(operator, data);
        self.current_function().code.push(code);
    }

    fn form_unary_operation(&mut self, op: &Operand, dest: &Operand) -> UnaryOperation {
        let src = self.get_source(op);
        let dest = self.get_source(dest);
        UnaryOperation { src, dest }
    }

    fn form_binary_operation(&mut self, op1: &Operand, op2: &Operand, dest: &Operand) -> BinaryOperation {
        let src1 = self.get_source(op1);
        let src2 = self.get_source(op2);
        let op_dest = self.get_source(dest);

        BinaryOperation { src1: src1, src2: src2, dest: op_dest }
    }

    fn get_source(&mut self, op: &Operand) -> Value {
        match op {
            Operand::Variable(declaration_info, id) => {
                self.get_register_for(declaration_info, *id)
            },
            Operand::AddressOf {variable_info, id} => {
                if variable_info.variable_type.is_array() {
                    Value::ArrayPtr {
                        id: *id,
                    }
                } else {
                    ice!("Not implemented for non-array-types, got {}: ", variable_info.variable_type);
                }
            }
            Operand::Integer(val) => Value::IntegerConstant(*val),
            Operand::Byte(val) => Value::ByteConstant(*val),
            Operand::Boolean(val) => Value::ByteConstant(if *val { 1 } else { 0 }),
            Operand::Initialized(value_type) => {
                let pos = self.current_function().parameter_count;
                self.current_function().parameter_count += 1;
                Value::FunctionParameter( value_type.clone(), pos as usize)
            },
            Operand::ArrayIndex {
                index_operand,
                id,
                variable_info
            } => {
                match &variable_info.variable_type {
                    Type::Reference(x) if x.is_array() => {
                        let base_reg = self.get_register_for(variable_info, *id);

                        let size = variable_info.variable_type.get_array_basic_type().size_in_bytes();
                        Value::IndirectAddress {
                            base: Box::new(base_reg),
                            index: Some(Box::new(self.get_source(index_operand))),
                            offset: None,
                            size: size,
                        }
                    },
                    x if x.is_array() => {
                        let size = variable_info.variable_type.get_array_basic_type().size_in_bytes();
                        Value::DynamicStackOffset {
                            id: *id,
                            index: Box::new(self.get_source(index_operand)),
                            offset: ARRAY_LENGTH_SLOT_SIZE,
                            size,
                        }
                    }
                    _ => ice!("Unexpected type for array indexing: {}", variable_info.variable_type),

                }

            },
            Operand::ArrayLength{ id, variable_info }=> {
                match &variable_info.variable_type {
                    Type::Reference(ref x) if x.is_array() => {
                        let base_reg = self.get_register_for(variable_info, *id);

                        let size = ARRAY_LENGTH_SLOT_SIZE;
                        Value::IndirectAddress {
                            base: Box::new(base_reg),
                            index: None,
                            offset: Some(ARRAY_LENGTH_SLOT_SIZE),
                            size: size,
                        }
                    }
                    x if x.is_array() => {
                        let size = ARRAY_LENGTH_SLOT_SIZE;
                        Value::DynamicStackOffset {
                            id: *id,
                            index: Box::new(Value::IntegerConstant(-1)),
                            offset: 0,
                            size,
                        }
                    },
                    _ => ice!("Unexpected variable type {} for array length", variable_info.variable_type),
                }
            }
            x => panic!("Not implemented yet for {}", x),
        }
    }

    fn get_register_for(&mut self, declaration_info: &DeclarationInfo, variable: u32) -> Value {
        if self.variable_id_to_register.contains_key(&variable) {
            let reg  = self.variable_id_to_register[&variable].clone();
            Value::VirtualRegister(reg)
        } else {
            let reg = self.next_register;
            self.next_register += 1;

            let vregdata = VirtualRegisterData {
                id: reg,
                size: declaration_info.variable_type.size_in_bytes(),
            };

            self.variable_id_to_register.insert(variable, vregdata.clone());
            Value::VirtualRegister(vregdata)
        }
    }

    fn form_binary_code(&mut self, operator: Operator, data: BinaryOperation) -> ByteCode {
        match operator {
            Operator::Plus => ByteCode::Add(data),
            Operator::Minus => ByteCode::Sub(data),
            Operator::Multiply => ByteCode::Mul(data),
            Operator::Divide => ByteCode::Div(data),
            Operator::Modulo => ByteCode::Mod(data),
            Operator::Xor => ByteCode::Xor(data),
            Operator::ArithmeticShiftRight => ByteCode::Sar(data),
            Operator::LogicalShiftRight => ByteCode::Shr(data),
            Operator::LogicalShiftLeft => ByteCode::Shl(data),
            _ => ice!("Invalid operator '{}'", operator),
        }
    }

    fn current_function(&mut self) -> &mut Function {
        self.bytecode_functions.last_mut().unwrap_or_else(|| panic!("Internal compiler error: Empty function array"))
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use super::Value::*;

    use crate::common::{
        tac_code::{Operand, Function},
        node_info::{FunctionInfo, Span},
        types::Type,
    };

    use std::collections::HashSet;
    use std::rc::Rc;

    fn create_function(statements: Vec<Statement>) -> Function {
        Function {
            function_info: FunctionInfo {
                name: Rc::new("foo".to_string()),
                parameters: vec![],
                return_type: Type::Void,
                span: Span {
                    line: 1,
                    column: 1,
                    length: 3
                },
            },
            statements,
            attributes: vec![],
        }
    }

    fn create_byte_generator(statements: Vec<Statement>) -> ByteGenerator {
        ByteGenerator::new(
            vec![create_function(statements)]
        )

    }

    #[test]
    fn should_generate_byte_code_for_return_without_value() {
        let statements = vec![
            Statement::Return(None)
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].code.len());

        assert_eq!(ByteCode::Ret(None), functions[0].code[0]);
    }

    #[test]
    fn should_generate_byte_code_for_return_with_integer_value() {
        let statements = vec![
            Statement::Return(Some(Operand::Integer(32)))
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].code.len());

        assert_eq!(ByteCode::Ret(Some(Value::IntegerConstant(32))), functions[0].code[0]);
    }

    #[test]
    fn should_generate_byte_code_for_boolean_true_constant() {
        let statements = vec![
            Statement::Assignment {
                operator: None,
                destination: Some(Operand::Variable(
                    DeclarationInfo {
                        name: Rc::new("Foo".to_owned()),
                        variable_type: Type::Boolean,
                        span: Span {
                            line: 1,
                            column: 1,
                            length: 1,
                        },
                        extra_info: None,
                        attributes: HashSet::new(),
                    },
                    3)
                ),
                left_operand: None,
                right_operand: Some(Operand::Boolean(true)),
            }
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].code.len());

        assert_eq!(ByteCode::Mov(
            UnaryOperation {
                src: ByteConstant(1),
                dest: VirtualRegister (
                    VirtualRegisterData {
                        id: 0,
                        size: 1,
                    }),
            }),
            functions[0].code[0]);

    }

    #[test]
    fn should_generate_byte_code_for_boolean_false_constant() {
        let statements = vec![
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Variable(
                    DeclarationInfo {
                        name: Rc::new("Foo".to_owned()),
                        variable_type: Type::Boolean,
                        span: Span {
                            line: 1,
                            column: 1,
                            length: 1,
                        },
                        extra_info: None,
                        attributes: HashSet::new(),
                    },
                    3)
                ),
                left_operand: None,
                right_operand: Some(Operand::Boolean(false))
            },
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].code.len());

        assert_eq!(ByteCode::Mov(
            UnaryOperation {
                src: ByteConstant(0),
                dest: VirtualRegister (
                    VirtualRegisterData {
                        id: 0,
                        size: 1,
                    }),
            }),
                   functions[0].code[0]);
    }

    #[test]
    fn should_generate_byte_code_for_conditional_jump_using_boolean_variable() {
        let statements = vec![
            Statement::JumpIfTrue(
                Operand::Variable(
                    DeclarationInfo {
                        name: Rc::new("foo".to_owned()),
                        variable_type: Type::Boolean,
                        span: Span {
                            line: 1,
                            column: 1,
                            length: 1,
                        },
                        extra_info: None,
                        attributes: HashSet::new(),
                    },
                    1),
                3),
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(2, functions[0].code.len());

        assert_eq!(
            ByteCode::Compare(
                ComparisonOperation{
                    src1: VirtualRegister(VirtualRegisterData {
                       size: 1,
                       id: 0,
                    }),
                    src2: ByteConstant(1),
                }
            ),
            functions[0].code[0]
        );

    }
}