#![allow(dead_code)] // primarily for the unused variant lints

use crate::tac_generator::{Statement, Operator, Operand, ARRAY_LENGTH_SLOT_SIZE};
use crate::tac_generator;
use crate::ast::DeclarationInfo;
use crate::code_generator::x64::x64_register::X64Register;
use crate::semcheck::Type;

use std::collections::HashMap;
use crate::byte_generator::Value::{VirtualRegister, ComparisonResult};
use crate::function_attributes::FunctionAttribute;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub src: Value,
    pub dest: Value,
}

impl Display for UnaryOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", format!("{}, {}", self.dest, self.src))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub src1: Value,
    pub src2: Value,
    pub dest: Value,
}

impl Display for BinaryOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", format!("{}, {}, {}", self.dest, self.src1, self.src2))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonOperation {
    pub src1: Value,
    pub src2: Value,
}

impl Display for ComparisonOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", format!("{}, {}", self.src1, self.src2))
    }
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
impl Display for ComparisonType{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", match *self {
            ComparisonType::Less =>  "l",
            ComparisonType::LessOrEq => "le",
            ComparisonType::Equals => "eq",
            ComparisonType::NotEquals => "ne",
            ComparisonType::GreaterOrEq => "ge",
            ComparisonType::Greater => "g",
        })

    }
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
    DynamicStackOffset {index: Box<Value>, offset: u32, size: u32, id: u32 },
    IndirectAddress { base: Box<Value>, index: Option<Box<Value>>, offset: Option<u32>, size: u32, },
    ArrayPtr{ id: u32 },
    ReturnValue,
    FunctionParameter(Type, usize),
}

impl Display for Value {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", match self {
            Value::ReturnValue => "<ARCHITECTURE/CALLING CONVENTION SPECIFIC RETURN VALUE LOCATION>".to_owned(),
            Value::FunctionParameter(_param_type, id) => format!("PARAMETER {}", id),
            Value::IntegerConstant(i) => format!("0x{:x}", i),
            Value::BooleanConstant(b) => format!("{}", b),
            Value::PhysicalRegister(reg) => format!("{:?}", reg),
            Value::VirtualRegister(vregdata) => format!("VR{}", vregdata.id),
            Value::ComparisonResult(res) => format!("<{}>", res),
            Value::StackOffset {
                offset,
                size: _,
            } => format!("[stack - 0x{:x}]", offset),

            Value::DynamicStackOffset {
                index, offset, size, id
            } => format!("[stack + {}*{} - (0x{:x}+array_{}_offset)]", index, size, offset, id),
            Value::IndirectAddress {
                base, index, offset, size
            } => format!("[{} + {}*{} - 0x{:x}]",
                         base,
                         if let Some(x) = index { x.clone() } else { Box::new(Value::IntegerConstant(0)) },
                         size,
                         if let Some(x) = offset { *x } else { 0 }),
            Value::ArrayPtr { id } => format!("Ptr to array {}", id),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ByteCode {
    Nop,
    PseudoArrayInit{size_in_bytes: u32, id: u32}, // for adjusting stack & dynamic offsets
    Add(BinaryOperation),
    Sub(BinaryOperation),
    Mul(BinaryOperation),
    Div(BinaryOperation),
    Mod(BinaryOperation),
    Negate(UnaryOperation),
    Xor(BinaryOperation),
    Mov(UnaryOperation),
    Lea(UnaryOperation),
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

impl Display for ByteCode {

    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", match *self {
            ByteCode::Nop => "NOP".to_owned(),
            ByteCode::PseudoArrayInit {size_in_bytes, id} => format!("Reserve {} bytes stack for array {}", size_in_bytes, id),
            ByteCode::Mov(UnaryOperation {
                ref dest,
                src: ComparisonResult(ref comp_result), }) =>
                format!("set{} {}", comp_result, dest),
            ByteCode::Mov(ref unary_op) => format!("mov {}", unary_op),
            ByteCode::Negate(ref unary_op) => format!("neg {}", unary_op),
            ByteCode::SignExtend(ref unary_op) => format!("sxt {}", unary_op),
            ByteCode::Add(ref binary_op) => format!("add {}", binary_op),
            ByteCode::Sub(ref binary_op) => format!("sub {}", binary_op),
            ByteCode::Mul(ref binary_op) => format!("mul {}", binary_op),
            ByteCode::Div(ref binary_op) => format!("div {}", binary_op),
            ByteCode::Mod(ref binary_op) => format!("mod {}", binary_op),
            ByteCode::Xor(ref binary_op) => format!("xor {}", binary_op),
            ByteCode::Lea(ref binary_op) => format!("lea {}", binary_op),
            ByteCode::Compare(ref comp_op) => format!("cmp {}", comp_op),
            ByteCode::Jump(label_id) => format!("jmp LABEL {}", label_id),
            ByteCode::JumpConditional(label_id, ref comp_type) => format!("j{} LABEL {}", comp_type, label_id),
            ByteCode::Label(label_id) => format!("LABEL {}", label_id),
            ByteCode::FunctionArguments(ref args) => {
                let mut fmt_str;
                if args.is_empty() {
                    fmt_str = "<NO FUNCTION ARGUMENTS>".to_owned();
                } else {
                    fmt_str = "Function arguments".to_owned();
                    let mut i = 1;
                    let indentation = "           ";
                    for arg in args.iter() {
                        fmt_str = format!("{}\n{} Argument {}: {}", fmt_str, indentation, i, arg);
                        i += 1;
                    }
                }
                fmt_str
            },
            ByteCode::Call(ref name) => format!("call '{}'", name),
            ByteCode::Ret(None) => "ret".to_owned(),
            ByteCode::Ret(Some(ref op)) => format!("ret {}", op),
            ByteCode::Push(ref value) => format!("push {}", value),
            ByteCode::Pop(ref value) => format!("pop {}", value),
        })
    }
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

    pub fn print_bytecode(&self) {
        let mut counter = 1;
        println!("Function '{}'", self.name);
        for c in self.code.iter() {
            println!("    {}: {}", counter, c);
            counter += 1;
        }
        println!();
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
                    Statement::Assignment(
                        None,
                        Some(ref dest),
                        None,
                        Some(ref src @ Operand::AddressOf { variable_info: _, id: _ })) =>
                        self.emit_lea(src, dest),
                    Statement::Assignment(Some(Operator::Plus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Plus, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Minus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Minus, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Multiply), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Multiply, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Divide), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Divide, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Modulo), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Modulo, op1, op2, dest),
                    Statement::Assignment(None, Some(ref dest), None, Some(ref op)) => self.emit_move(op, dest),
                    Statement::Assignment(Some(Operator::Minus), Some(ref dest), None, Some(ref src)) => self.emit_negate(dest, src),
                    Statement::Assignment(Some(Operator::Xor), Some(ref dest), Some(ref src1), Some(ref src2)) => self.emit_xor(dest, src1, src2),
                    Statement::Assignment(
                        Some(ref x),
                        Some(ref dest),
                        Some(ref op1),
                        Some(ref op2)) if cmp.contains(x) =>
                        self.emit_comparison(x, op1, op2, dest),
                    Statement::Array{id, length, size_in_bytes} => {
                        self.emit_array_init(id, length, size_in_bytes);
                    }
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
            Operand::Boolean(val) => Value::BooleanConstant(*val),
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

                        let size = ARRAY_LENGTH_SLOT_SIZE;
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
    use crate::ast::{FunctionInfo, Span};
    use crate::semcheck::Type;

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
            Statement::Assignment(
                None,
                Some(Operand::Variable(
                    DeclarationInfo {
                        name: Rc::new("Foo".to_owned()),
                        variable_type: Type::Boolean,
                        span: Span {
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
                        span: Span {
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
                        span: Span {
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