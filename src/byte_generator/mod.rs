#![allow(dead_code)] // primarily for the unused variant lints

use crate::tac_generator::{Statement, Operator, Operand};
use crate::tac_generator;
use crate::ast::DeclarationInfo;
use crate::code_generator::x64::X64Register;
use crate::semcheck::Type;

use std::collections::HashMap;
use crate::byte_generator::Value::{VirtualRegister, ComparisonResult};
use crate::function_attributes::FunctionAttribute;


#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub src: Value,
    pub dest: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub src1: Value,
    pub src2: Value,
    pub dest: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonOperation {
    pub src1: Value,
    pub src2: Value,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonType {
    Less,
    LessOrEq,
    Equals,
    NotEquals,
    GreaterOrEq,
    Greater,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VirtualRegisterData {
    pub id: u32,
    pub size: u32
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    VirtualRegister(VirtualRegisterData),
    PhysicalRegister(X64Register), // FIXME Not arch agnostic, assumes X64
    IntegerConstant(i32),
    BooleanConstant(bool),
    ComparisonResult(ComparisonType),
    StackOffset{offset: u32, size: u32},
    ReturnValue,
    FunctionParameter(Type, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    Nop,
    Add(BinaryOperation),
    Sub(BinaryOperation),
    Mul(BinaryOperation),
    Div(BinaryOperation),
    Negate(UnaryOperation),
    Xor(BinaryOperation),
    Mov(UnaryOperation),
    SignExtend(UnaryOperation),
    Compare(ComparisonOperation),
    Label(u32),
    Jump(u32),
    JumpConditional(u32, ComparisonType),
    FunctionArguments(Vec<Value>),
    Push(Value),
    Pop(Value),
    Call(String),
    Ret(Option<Value>),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub code: Vec<ByteCode>,
    pub parameter_count: u32,
    pub attributes: Vec<FunctionAttribute>
}

impl Function {
    pub fn has_attribute(&self, attribute: FunctionAttribute) -> bool {
        self.attributes.contains(&attribute)
    }
}

pub struct ByteGenerator {
    pub bytecode_functions: Vec<Function>,
    tac_functions: Vec<tac_generator::Function>,
    next_register: u32,
    variable_id_to_register: HashMap<u32, VirtualRegisterData>,
}

impl ByteGenerator {
    pub fn new(functions: Vec<tac_generator::Function>) -> ByteGenerator {
        ByteGenerator {
            bytecode_functions: vec![],
            tac_functions: functions,
            next_register: 0,
            variable_id_to_register: HashMap::new(),
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
                    Statement::Assignment(Some(Operator::Plus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Plus, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Minus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Minus, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Multiply), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Multiply, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Divide), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Divide, op1, op2, dest),
                    Statement::Assignment(None, Some(ref dest), None, Some(ref op)) => self.emit_move(op, dest),
                    Statement::Assignment(Some(Operator::Minus), Some(ref dest), None, Some(ref src)) => self.emit_negate(dest, src),
                    Statement::Assignment(Some(Operator::Xor), Some(ref dest), Some(ref src1), Some(ref src2)) => self.emit_xor(dest, src1, src2),
                    Statement::Assignment(
                        Some(ref x),
                        Some(ref dest),
                        Some(ref op1),
                        Some(ref op2)) if cmp.contains(x) =>
                        self.emit_comparison(x, op1, op2, dest),

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

    fn emit_xor(&mut self, dest: &Operand, src1: &Operand, src2: &Operand) {
        let data = self.form_binary_operation(src1, src2, dest);
        self.current_function().code.push(ByteCode::Xor(data));
    }

    fn emit_move(&mut self, op: &Operand, dest: &Operand) {
        let data = self.form_unary_operation(op, dest);
        self.current_function().code.push(ByteCode::Mov(data));
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
                    src: ComparisonResult(comparison_type.clone()),
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
                        src1: VirtualRegister(vregdata.clone()),
                        src2: Value::BooleanConstant(true),
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

    fn reverse_comparison(&mut self, cmp_type: ComparisonType) -> ComparisonType {
        match cmp_type {
            ComparisonType::Equals => ComparisonType::NotEquals,
            ComparisonType::NotEquals => ComparisonType::Equals,
            ComparisonType::Greater => ComparisonType::LessOrEq,
            ComparisonType::GreaterOrEq => ComparisonType::Less,
            ComparisonType::LessOrEq => ComparisonType::Greater,
            ComparisonType::Less => ComparisonType::GreaterOrEq
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
            Operand::Integer(val) => Value::IntegerConstant(*val),
            Operand::Boolean(val) => Value::BooleanConstant(*val),
            Operand::Initialized(value_type) => {
                let pos = self.current_function().parameter_count;
                self.current_function().parameter_count += 1;
                Value::FunctionParameter( *value_type, pos as usize)
            },
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

    use crate::tac_generator::{Operand, Function};
    use crate::ast::{FunctionInfo, NodeInfo};
    use crate::semcheck::Type;

    use std::rc::Rc;

    fn create_function(statements: Vec<Statement>) -> Function {
        Function {
            function_info: FunctionInfo {
                name: Rc::new("foo".to_string()),
                parameters: vec![],
                return_type: Type::Void,
                node_info: NodeInfo {
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
            Statement::Assignment(
                None,
                Some(Operand::Variable(
                    DeclarationInfo {
                        name: Rc::new("Foo".to_owned()),
                        variable_type: Type::Boolean,
                        node_info: NodeInfo {
                            line: 1,
                            column: 1,
                            length: 1,
                        },
                        extra_info: None
                    },
                    3)
                ),
                None,
                Some(Operand::Boolean(true)),
            )
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].code.len());

        assert_eq!(ByteCode::Mov(
            UnaryOperation {
                src: BooleanConstant(true),
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
            Statement::Assignment(
                None,
                Some(Operand::Variable(
                    DeclarationInfo {
                        name: Rc::new("Foo".to_owned()),
                        variable_type: Type::Boolean,
                        node_info: NodeInfo {
                            line: 1,
                            column: 1,
                            length: 1,
                        },
                        extra_info: None,

                    },
                    3)
                ),
                None,
                Some(Operand::Boolean(false)),
            )
        ];

        let mut generator = create_byte_generator(statements);
        generator.generate_bytecode();
        let functions = generator.bytecode_functions;

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].code.len());

        assert_eq!(ByteCode::Mov(
            UnaryOperation {
                src: BooleanConstant(false),
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
                Operand::Variable(DeclarationInfo {
                        name: Rc::new("foo".to_owned()),
                        variable_type: Type::Boolean,
                        node_info: NodeInfo {
                            line: 1,
                            column: 1,
                            length: 1,
                        },
                        extra_info: None, },
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
                    src2: BooleanConstant(true),
                }
            ),
            functions[0].code[0]
        );

    }
}