use super::super::code_generator::x64::x64_register::X64Register;

use crate::common::{function_attributes::FunctionAttribute, types::Type};

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;


#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub code: Vec<ByteCode>,
    pub parameter_count: u32,
    pub attributes: Vec<FunctionAttribute>
}

#[allow(dead_code)]
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
    Sar(BinaryOperation),
    Shr(BinaryOperation),
    Shl(BinaryOperation),
    Mov(UnaryOperation),
    Movzx(UnaryOperation), // zero extending
    Movsx(UnaryOperation), // sign-extending
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
pub enum Value {
    VirtualRegister(VirtualRegisterData),
    PhysicalRegister(X64Register), // FIXME Not arch agnostic, assumes X64
    IntegerConstant(i32),
    ByteConstant(i8),
    ComparisonResult(ComparisonType),
    StackOffset{offset: u32, size: u32},
    DynamicStackOffset {index: Box<Value>, offset: u32, size: u32, id: u32 },
    IndirectAddress { base: Box<Value>, index: Option<Box<Value>>, offset: Option<u32>, size: u32, },
    ArrayPtr{ id: u32 },
    ReturnValue,
    FunctionParameter(Type, usize),
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

impl Display for UnaryOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", format!("{}, {}", self.dest, self.src))
    }
}

impl Display for BinaryOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", format!("{}, {}, {}", self.dest, self.src1, self.src2))
    }
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

impl Display for ByteCode {

    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", match *self {
            ByteCode::Nop => "NOP".to_owned(),
            ByteCode::PseudoArrayInit {size_in_bytes, id} => format!("Reserve {} bytes stack for array {}", size_in_bytes, id),
            ByteCode::Mov(UnaryOperation {
                ref dest,
                src: Value::ComparisonResult(ref comp_result), }) =>
                format!("set{} {}", comp_result, dest),
            ByteCode::Mov(ref unary_op) => format!("mov {}", unary_op),
            ByteCode::Movsx(ref unary_op) => format!("movsx {}", unary_op),
            ByteCode::Movzx(ref unary_op) => format!("movzx {}", unary_op),
            ByteCode::Negate(ref unary_op) => format!("neg {}", unary_op),
            ByteCode::SignExtend(ref unary_op) => format!("sxt {}", unary_op),
            ByteCode::Add(ref binary_op) => format!("add {}", binary_op),
            ByteCode::Sub(ref binary_op) => format!("sub {}", binary_op),
            ByteCode::Mul(ref binary_op) => format!("mul {}", binary_op),
            ByteCode::Div(ref binary_op) => format!("div {}", binary_op),
            ByteCode::Mod(ref binary_op) => format!("mod {}", binary_op),
            ByteCode::Xor(ref binary_op) => format!("xor {}", binary_op),
            ByteCode::Lea(ref binary_op) => format!("lea {}", binary_op),
            ByteCode::Sar(ref unary_op) => format!("sar {}", unary_op),
            ByteCode::Shr(ref unary_op) => format!("shr {}", unary_op),
            ByteCode::Shl(ref unary_op) => format!("shl {}", unary_op),
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

impl Display for Value {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", match self {
            Value::ReturnValue => "<ARCHITECTURE/CALLING CONVENTION SPECIFIC RETURN VALUE LOCATION>".to_owned(),
            Value::FunctionParameter(_param_type, id) => format!("PARAMETER {}", id),
            Value::IntegerConstant(i) => format!("0x{:X}", i),
            Value::ByteConstant(b) => format!("0x{:X}", b),
            Value::PhysicalRegister(reg) => format!("{:?}", reg),
            Value::VirtualRegister(vregdata) => format!("VR{}", vregdata.id),
            Value::ComparisonResult(res) => format!("<{}>", res),
            Value::StackOffset {
                offset,
                size,
            } => format!("{} PTR [stack - 0x{:x}]",
                         match *size {
                             1 => "BYTE",
                             2 => "WORD",
                             4 => "DWORD",
                             8 => "QWORD",
                             _ => "<INVALID SIZE>",
                         },
                         offset),

            Value::DynamicStackOffset {
                index, offset, size, id
            } => format!("{} PTR [stack + {} - (0x{:x}+array_{}_offset)]",
                         match *size {
                             1 => "BYTE",
                             2 => "WORD",
                             4 => "DWORD",
                             8 => "QWORD",
                             _ => "<INVALID SIZE>",
                         },
                         index,
                         offset,
                         id),
            Value::IndirectAddress {
                base, index, offset, size
            } => format!("{}[{} + {} - 0x{:x}]",
                         if let Some(_) = index {
                             match size {
                                 1 => "BYTE PTR ",
                                 2 => "WORD PTR ",
                                 4 => "DWORD PTR ",
                                 8 => "QWORD PTR ",
                                 _ => "<INVALID SIZE>",
                             }
                         } else {
                             ""
                         },
                         base,
                         if let Some(x) = index { x.clone() } else { Box::new(Value::IntegerConstant(0)) },
                         if let Some(x) = offset { *x } else { 0 }),
            Value::ArrayPtr { id } => format!("Ptr to array {}", id),
        })
    }
}

impl Display for ComparisonOperation {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        write!(formatter, "{}", format!("{}, {}", self.src1, self.src2))
    }
}
