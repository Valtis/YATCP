use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use crate::ast::{ DeclarationInfo, FunctionInfo };
use crate::function_attributes::FunctionAttribute;

use crate::semcheck::Type;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment{ operator: Option<Operator>, destination: Option<Operand>, left_operand: Option<Operand>, right_operand: Option<Operand> },
    Array{ id: u32, length: i32, size_in_bytes: u32 },
    Call(Rc<String>, Vec<Operand>, Option<Operand>),
    Label(u32),
    Jump(u32),
    JumpIfTrue(Operand, u32),
    JumpIfFalse(Operand, u32),
    Return(Option<Operand>),
    Empty,
    PhiFunction(Operand, Vec<Operand>)
}

// TODO replace tuples with structs for better readability
#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Variable(DeclarationInfo, u32),
    AddressOf{ variable_info: DeclarationInfo, id: u32, },
    ArrayIndex{
        id: u32,
        index_operand: Box<Operand>,
        variable_info: DeclarationInfo,
    },
    ArrayLength {
        id: u32,
        variable_info: DeclarationInfo,
    },
    SSAVariable(DeclarationInfo, u32, u32),
    Integer(i32),
    Byte(i8),
    Float(f32),
    Double(f64),
    Boolean(bool),
    // special operand to represent initialized, but unknown, value
    // used for things like function parameters
    Initialized(Type),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Less,
    LessOrEq,
    Equals,
    NotEquals,
    GreaterOrEq,
    Greater,
    Xor,
    ArithmeticShiftRight,
    LogicalShiftRight,
    LogicalShiftLeft,
    Cast{ from: Type, to: Type},
}

#[derive(Clone, Debug)]
pub struct Function {
    pub statements: Vec<Statement>,
    pub function_info: FunctionInfo,
    pub attributes: Vec<FunctionAttribute>,
}

impl Function {
    pub fn new(function_info: FunctionInfo) -> Function {
        Function {
            statements: vec![],
            function_info,
            attributes: vec![],
        }
    }

    pub fn print(&self) {
        let mut counter = 0;
        println!("Function '{}'\n", self.function_info.name);
        for s in &self.statements {
            println!("    {}: {}", counter, s);
            counter += 1;
        }
        println!();
    }


    pub fn has_attribute(&self, attribute: FunctionAttribute) -> bool {
        self.attributes.contains(&attribute)
    }
}

impl Display for Statement {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Statement::Assignment{ref operator, ref destination, ref left_operand, ref right_operand} =>
                format!("{}= {}{}{}",
                        opt_to_str(destination),
                        opt_to_str(left_operand),
                        opt_to_str(operator),
                        opt_to_str(right_operand)),
            Statement::Array{ id: _, length, size_in_bytes} => format!("Init<Array[{}], {} bytes>", length, size_in_bytes),
            Statement::Call(ref name, ref operands, ref dest) => {
                let op_str = operands.iter()
                    .fold(
                        String::new(),
                        |acc, v| format!("{}, {}", acc, v))
                    .chars()
                    .skip(1)
                    .collect::<String>();

                let dest_str = if let Some(ref op) = *dest {
                    format!("{} = ", op)
                } else {
                    String::new()
                };

                format!("{}{}({})",
                        dest_str,
                        name,
                        op_str)
            },
            Statement::Return(ref v0) =>
                format!("return {}", opt_to_str(v0)),
            Statement::Label(id) => format!("Label {}", id),
            Statement::Jump(id) => format!("Jump {}", id),
            Statement::JumpIfTrue(ref op, id) =>
                format!("Jump {} if {}", id, op),
            Statement::JumpIfFalse(ref op, id) =>
                format!("Jump {} if not {}", id, op),
            Statement::PhiFunction(ref dest, ref operands) => {
                let mut op_str = operands.
                    iter().
                    fold(
                        String::new(),
                        |acc, ref t| format!("'{}', {}", t, acc));

                op_str.pop(); op_str.pop();
                format!("{} = phi<{}>", dest, op_str)
            },
            Statement::Empty => "<Empty statement>".to_owned()
        })
    }
}



impl Display for Operand {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operand::ArrayIndex { id, ref index_operand, ref variable_info} => {
                format!("{}_{}[{}]", variable_info.name, id, index_operand)
            },
            Operand::ArrayLength { id, ref variable_info } => format!("{}_{}.length", variable_info.name, id),
            Operand::Variable(ref info, id) => format!("{}_{}", info.name, id),
            Operand::AddressOf { ref variable_info, ref id } => format!("&{}_{}", variable_info.name, id),
            Operand::SSAVariable(ref info, id, ssa_id) =>
                format!("{}_{}:{}", info.name, id, ssa_id),
            Operand::Integer(v) => format!("{}i", v),
            Operand::Byte(v) => format!("{}b", v),
            Operand::Float(v) => format!("{}f", v),
            Operand::Double(v) => format!("{}d", v),
            Operand::Boolean(v) => v.to_string(),
            Operand::Initialized(ref t) => format!(
                "<initialized {} value>", t),
        })
    }
}

impl Display for Operator {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operator::Plus => "+".to_string(),
            Operator::Minus => "-".to_string(),
            Operator::Multiply => "*".to_string(),
            Operator::Divide => "/".to_string(),
            Operator::Modulo=> "%".to_string(),
            Operator::Less => "<".to_string(),
            Operator::LessOrEq => "<=".to_string(),
            Operator::Equals => "==".to_string(),
            Operator::NotEquals => "!=".to_string(),
            Operator::GreaterOrEq => ">=".to_string(),
            Operator::Greater => ">".to_string(),
            Operator::Xor => "^".to_string(),
            Operator::ArithmeticShiftRight => ">>".to_string(),
            Operator::LogicalShiftRight => ">>>".to_string(),
            Operator::LogicalShiftLeft => "<<".to_string(),
            Operator::Cast{ref from, ref to}=> format!("Cast({} to {})", from, to),
        })
    }
}

fn opt_to_str<T: Display>(op: &Option<T>) -> String {
    match *op {
        Some(ref val) => format!("{} ", val),
        None => "".to_owned(),
    }
}