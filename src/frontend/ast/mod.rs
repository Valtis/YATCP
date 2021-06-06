
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::fmt::Debug;
use std::iter;

use std::rc::Rc;
use crate::common::{
    types::Type,
    node_info::*,
    symbol_table,
};

/*** AstLong ***/

#[derive(Clone, Debug, PartialEq)]
pub enum AstLong {
    Long(i64),
    Invalid(i128),
}

impl From<i128> for AstLong {
    fn from(val: i128) -> AstLong {
        if val <= i64::MAX as i128 && val >= i64::MIN as i128 {
            AstLong::Long(val as i64)
        } else {
            AstLong::Invalid(val)
        }
    }
}

impl From<u64> for AstLong {
    fn from(val: u64) -> AstLong {
        AstLong::Long(val as i64)
    }
}


impl From<i64> for AstLong {
    fn from(val: i64) -> AstLong {
        AstLong::Long(val)
    }
}


impl From<i32> for AstLong {
    fn from(val: i32) -> AstLong {
        AstLong::Long(val as i64)
    }
}

impl From<i16> for AstLong {
    fn from(val: i16) -> AstLong {
        AstLong::Long(val as i64)
    }
}

impl From<AstByte> for AstLong {
    fn from(value: AstByte) -> AstLong {
        match value {
            AstByte::Byte(value) => AstLong::Long(value as i64),
            AstByte::Invalid(value ) => {
                if value > i64::MAX as i128 || value < i64::MIN as i128 {
                    AstLong::Invalid(value)
                } else {
                    AstLong::Long(value as i64)
                }
            }
        }
    }
}

impl From<AstShort> for AstLong {
    fn from(value: AstShort) -> AstLong {
        match value {
            AstShort::Short(value) => AstLong::Long(value as i64),
            AstShort::Invalid(value ) => {
                if value > i64::MAX as i128 || value < i64::MIN as i128 {
                    AstLong::Invalid(value)
                } else {
                    AstLong::Long(value as i64)
                }
            }
        }
    }
}

impl From<AstInteger> for AstLong {
    fn from(value: AstInteger) -> AstLong {
        match value {
            AstInteger::Int(value) => AstLong::Long(value as i64),
            AstInteger::Invalid(value ) => {
                if value > i64::MAX as i128 || value < i64::MIN as i128 {
                    AstLong::Invalid(value)
                } else {
                    AstLong::Long(value as i64)
                }
            }
        }
    }
}

impl Display for AstLong {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}",
               match self {
                   AstLong::Invalid(val) => format!("(Overflow, {} does not fit in 'long')", val),
                   AstLong::Long(val) => format!("{}", val),
               })
    }
}





/*** AstInteger ***/

#[derive(Clone, Debug, PartialEq)]
pub enum AstInteger {
    Int(i32),
    Invalid(i128),
}

impl From<i128> for AstInteger {
    fn from(val: i128) -> AstInteger {
        if val <= i32::MAX as i128 && val >= i32::MIN as i128 {
            AstInteger::Int(val as i32)
        } else {
            AstInteger::Invalid(val)
        }
    }
}

impl From<u64> for AstInteger {
    fn from(val: u64) -> AstInteger {
        if val <= i32::MAX as u64 {
            AstInteger::Int(val as i32)
        } else {
            AstInteger::Invalid(val as i128)
        }
    }
}

impl From<i32> for AstInteger {
    fn from(val: i32) -> AstInteger {
        AstInteger::Int(val)
    }
}

impl From<i16> for AstInteger {
    fn from(val: i16) -> AstInteger {
        AstInteger::Int(val as i32)
    }
}

impl From<AstByte> for AstInteger {
    fn from(value: AstByte) -> AstInteger {
        match value {
            AstByte::Byte(value) => AstInteger::Int(value as i32),
            AstByte::Invalid(value ) => {
                if value > i32::MAX as i128 || value < i32::MIN as i128 {
                    AstInteger::Invalid(value)
                } else {
                    AstInteger::Int(value as i32)
                }
            }
        }
    }
}

impl From<AstShort> for AstInteger {
    fn from(value: AstShort) -> AstInteger {
        match value {
            AstShort::Short(value) => AstInteger::Int(value as i32),
            AstShort::Invalid(value ) => {
                if value > i32::MAX as i128 || value < i32::MIN as i128 {
                    AstInteger::Invalid(value)
                } else {
                    AstInteger::Int(value as i32)
                }
            }
        }
    }
}

impl From<AstLong> for AstInteger {
    fn from(value: AstLong ) -> AstInteger {
        match value {
            AstLong::Long(value) => {
                if value > i32::MAX as i64 || value < i32::MIN as i64 {
                    AstInteger::Invalid(value as i128)
                } else {
                    AstInteger::Int(value as i32)
                }

            },
            AstLong::Invalid(x) => AstInteger::Invalid(x)
        }
    }
}

impl Display for AstInteger {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}",
               match self {
                   AstInteger::Invalid(val) => format!("(Overflow, {} does not fit in 'int'", val),
                   AstInteger::Int(val) => format!("{}", val),
               })
    }
}






/*** AstShort ***/

#[derive(Clone, Debug, PartialEq)]
pub enum AstShort {
    Short(i16),
    Invalid(i128),
}

impl From<i128> for AstShort {
    fn from(val: i128) -> AstShort {
        if val <= i16::MAX as i128 && val >= i16::MIN as i128 {
            AstShort::Short(val as i16)
        } else {
            AstShort::Invalid(val)
        }
    }
}

impl From<u64> for AstShort {
    fn from(val: u64) -> AstShort {
        if val <= i16::MAX as u64 {
            AstShort::Short(val as i16)
        } else {
            AstShort::Invalid(val as i128)
        }
    }
}

impl From<i32> for AstShort {
    fn from(val: i32) -> AstShort {
        if val <= i16::MAX as i32 && val >= i16::MIN as i32 {
            AstShort::Short(val as i16)
        } else {
            AstShort::Invalid(val as i128)
        }
    }
}

impl From<i16> for AstShort {
    fn from(val: i16) -> AstShort {
        AstShort::Short(val)
    }
}

impl From<i8> for AstShort {
    fn from(val: i8) -> AstShort {
        AstShort::Short(val as i16)
    }
}

impl From<AstByte> for AstShort {
    fn from(value: AstByte) -> AstShort {
        match value {
            AstByte::Byte(value) => AstShort::Short(value as i16),
            AstByte::Invalid(value ) => {
                if value > i16::MAX as i128 || value < i16::MIN as i128 {
                    AstShort::Invalid(value)
                } else {
                    AstShort::Short(value as i16)
                }
            }
        }
    }
}


impl From<AstInteger> for AstShort {
    fn from(value: AstInteger ) -> AstShort {
        match value {
            AstInteger::Int(value) => {
                if value > i16::MAX as i32 || value < i16::MIN as i32 {
                    AstShort::Invalid(value as i128)
                } else {
                    AstShort::Short(value as i16)
                }
            },
            AstInteger::Invalid(x) => AstShort::Invalid(x)
        }
    }
}

impl From<AstLong> for AstShort {
    fn from(value: AstLong ) -> AstShort {
        match value {
            AstLong::Long(value) => {
                if value > i16::MAX as i64 || value < i16::MIN as i64 {
                    AstShort::Invalid(value as i128)
                } else {
                    AstShort::Short(value as i16)
                }

            },
            AstLong::Invalid(x) => AstShort::Invalid(x)
        }
    }
}

impl Display for AstShort {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}",
               match self {
                   AstShort::Invalid(val) => format!("(Overflow, {} does not fit in 'short')", val),
                   AstShort::Short(val) => format!("{}", val),
               })
    }
}






/***  AstByte  ***/


#[derive(Clone, Debug, PartialEq)]
pub enum AstByte {
    Byte(i8),
    Invalid(i128),
}

impl From<i128> for AstByte {
    fn from(val: i128) -> AstByte {
        if val <= i8::MAX as i128 && val >= i8::MIN as i128  {
            AstByte::Byte(val as i8)
        } else {
            AstByte::Invalid(val)
        }
    }
}

impl From<u64> for AstByte {
    fn from(val: u64) -> AstByte {
        if val <= i8::MAX as u64 {
            AstByte::Byte(val as i8)
        } else {
            AstByte::Invalid(val as i128)
        }
    }
}


impl From<i16> for AstByte {
    fn from(val: i16) -> AstByte {
        if val <= i8::MAX as i16 && val >= i8::MIN as i16  {
            AstByte::Byte(val as i8)
        } else {
            AstByte::Invalid(val as i128)
        }
    }
}

impl From<i8> for AstByte {
    fn from(val: i8) -> AstByte {
        AstByte::Byte(val)
    }
}

impl From<AstShort> for AstByte {
    fn from(value: AstShort) -> AstByte {
        match value {
            AstShort::Short(value) => {
                if value > i8::MAX as i16 || value < i8::MIN as i16 {
                    AstByte::Invalid(value as i128)
                } else {
                    AstByte::Byte(value as i8)
                }
            },
            AstShort::Invalid(x) => AstByte::Invalid(x)
        }
    }
}


impl From<AstInteger> for AstByte {
    fn from(value: AstInteger) -> AstByte {
        match value {
            AstInteger::Int(value) => {
                if value > i8::MAX as i32 || value < i8::MIN as i32 {
                    AstByte::Invalid(value as i128)
                } else {
                    AstByte::Byte(value as i8)
                }

            },
            AstInteger::Invalid(x) => AstByte::Invalid(x)
        }
    }
}

impl From<AstLong> for AstByte {
    fn from(value: AstLong ) -> AstByte {
        match value {
            AstLong::Long(value) => {
                if value > i8::MAX as i64 || value < i8::MIN as i64 {
                    AstByte::Invalid(value as i128)
                } else {
                    AstByte::Byte(value as i8)
                }

            },
            AstLong::Invalid(x) => AstByte::Invalid(x)
        }
    }
}

impl Display for AstByte {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}",
               match self {
                   AstByte::Invalid(val) => format!("(Overflow, {} does not fit in 'byte')", val),
                   AstByte::Byte(val) => format!("{}", val),
               })
    }
}


#[derive(Clone)]
pub enum AstNode {
    Block{ statements: Vec<AstNode>, block_symbol_table_entry: Option<symbol_table::TableEntry>, span: Span },
    Function{ block: Box<AstNode>, function_info: FunctionInfo },
    ExternFunction{ function_info: FunctionInfo },
    FunctionCall{ arguments: Vec<AstNode>, function_name: Rc<String>, span: Span },
    VariableDeclaration{ initialization_expression: Box<AstNode>, declaration_info: DeclarationInfo },
    ArrayDeclaration{ initialization_expression: Box<AstNode>, dimensions: Vec<AstNode>, declaration_info: DeclarationInfo },
    VariableAssignment{ expression: Box<AstNode>, name: Rc<String>, span: Span },
    ArrayAssignment{index_expression: Box<AstNode>, assignment_expression: Box<AstNode>, variable_name: Rc<String>, span: Span },
    InitializerList { values: Vec<AstNode>, list_type: Type, span: Span },

    MemberAccess { object: Box<AstNode>, member: Box<AstNode>, span: Span },
    Plus{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Minus{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Multiply{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Divide{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Modulo{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Negate{ expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },

    ArithmeticShiftRight { value: Box<AstNode>, shift_count: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    LogicalShiftRight { value: Box<AstNode>, shift_count: Box<AstNode>, arithmetic_info: ArithmeticInfo  },
    LogicalShiftLeft { value: Box<AstNode>, shift_count: Box<AstNode>, arithmetic_info: ArithmeticInfo, },

    BooleanAnd{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    BooleanOr{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    BooleanNot{ expression: Box<AstNode>, span: Span },

    BitwiseAnd { left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    BitwiseOr { left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    BitwiseXor { left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    BitwiseNot { expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },

    Return{ return_value: Option<Box<AstNode>>, arithmetic_info: ArithmeticInfo },

    Loop { condition_expression: Box<AstNode>, post_body_statements: Option<Vec<AstNode>>, block: Box<AstNode>, span: Span },
    If { condition_expression: Box<AstNode>, main_block: Box<AstNode>, else_block: Option<Box<AstNode>>, span: Span },

    Less { left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span } ,
    LessOrEq { left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span} ,
    Equals { left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    NotEquals { left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    GreaterOrEq{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    Greater { left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },

    Cast { expression: Box<AstNode>, target_type: Type, span: Span},

    IntegralNumber{ value: i128, span: Span},
    // Signed long/integer/byte, but we may need to store INT_MAX +1 while negation is still unresolved
    Byte { value: AstByte, span: Span },
    Short { value: AstShort, span: Span },
    Integer{ value: AstInteger, span: Span },
    Long{ value: AstLong, span: Span },
    Float { value: f32, span: Span },
    Double{ value: f64, span: Span },
    Text { value: Rc<String>, span: Span } ,
    Boolean { value: bool, span: Span},
    Identifier{ name: Rc<String>, span: Span },
    ArrayAccess{ index_expression: Box<AstNode>, indexable_expression: Box<AstNode> },
    ArraySlice { start_expression: Box<AstNode>, end_expression: Box<AstNode>, array_expression: Box<AstNode> },
    ErrorNode,
    EmptyNode, // No-operation in later stages,
    Break(Span),
    Continue(Span),
}

impl Display for AstNode {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match self {
            AstNode::Block{ .. } => "Block".to_string(),
            AstNode::Function{ function_info, .. } => {
                let param_str = function_info.parameters.iter().fold(
                    String::new(),
                    |acc, ref t| format!("{}, {} : {}", acc, t.name, t.variable_type))
                    .chars()
                    .skip(2).collect::<String>();

                format!("Function {}({}) -> {}",
                        function_info.name,
                        param_str,
                        function_info.return_type)
            },
            AstNode::ExternFunction{ function_info} => {
                let param_str = function_info.parameters.iter().fold(
                    String::new(),
                    |acc, ref t| format!("{}, {} : {}", acc, t.name, t.variable_type))
                    .chars()
                    .skip(2).collect::<String>();

                format!("Extern Function {}({}) -> {}",
                        function_info.name,
                        param_str,
                        function_info.return_type)
            },
            AstNode::FunctionCall{ref function_name, ..} =>
                format!("Call function {}", function_name),
            AstNode::VariableDeclaration{ref declaration_info, ..} =>
                format!("Variable declaration '{}' : {}", declaration_info.name, declaration_info.variable_type),
            AstNode::ArrayDeclaration{ref declaration_info, ..} => {
                    let mut dim_str = "".to_owned();
                    for dim in declaration_info.variable_type.get_array_dimensions().iter() {
                        dim_str = format!("{}[{}]", dim_str, dim);
                    }
                    format!("Variable declaration '{}' : {}{}", declaration_info.name, declaration_info.variable_type, dim_str)
            }
            AstNode::VariableAssignment{ ref name, .. } =>
                format!("Variable assignment '{}'", name),
            AstNode::ArrayAssignment {
                index_expression: _,
                assignment_expression: _,
                variable_name: name,
                span: _
            } => format!("Array assignment '{}'", name),
            AstNode::InitializerList{..} => "Initializer list".to_owned(),
            AstNode::MemberAccess { .. } => "Member access".to_owned(),
            AstNode::IntegralNumber{value, ..} => format!("Integral number: {}", value),
            AstNode::Long{ value, .. } => format!("Long: {}", value),
            AstNode::Integer{ value, .. } => format!("Integer: {}", value),
            AstNode::Short{ value, .. } => format!("Short: {}", value),
            AstNode::Byte{ value, .. } => format!("Byte: {}", value),
            AstNode::Float{value , .. } => format!("Float: {}", value),
            AstNode::Double{ value, .. } => format!("Double: {}", value),
            AstNode::Boolean{value, .. } => format!("Boolean: {}", value),
            AstNode::Text{ value, .. } => format!("Text: {}", value ),
            AstNode::Identifier{ name, .. } => format!("Identifier: {}", name),
            AstNode::ArrayAccess{ .. }=> format!("Array access"),
            AstNode::ArraySlice{ .. }=> format!("Array slice"),
            AstNode::Plus{ .. } => "Plus".to_string(),
            AstNode::Minus{ .. } => "Minus".to_string(),
            AstNode::Multiply {.. } => "Multiply".to_string(),
            AstNode::Divide { .. } => "Divide".to_string(),
            AstNode::Modulo { .. }  => "Modulo".to_string(),
            AstNode::ArithmeticShiftRight { .. } => "Arithmetic Shift Right".to_string(),
            AstNode::LogicalShiftRight{ .. } => "Logical Shift Right".to_string(),
            AstNode::LogicalShiftLeft { .. } => "Logical Shift Left".to_string(),
            AstNode::BooleanAnd { .. } => "And".to_string(),
            AstNode::BooleanOr { .. } => "Or".to_string(),
            AstNode::BooleanNot{ .. } => "Not".to_string(),
            AstNode::BitwiseAnd{ .. } => "Bitwise And".to_string(),
            AstNode::BitwiseOr { .. } => "Bitwise Or".to_string(),
            AstNode::BitwiseXor { .. } => "Bitwise Xor".to_string(),
            AstNode::BitwiseNot{ .. } => "Bitwise Not".to_string(),
            AstNode::Negate { .. }  => "Negate".to_string(),
            AstNode::Return{ .. } => "Return".to_string(),
            AstNode::Loop { .. }=> "While".to_string(),
            AstNode::If { .. }=> "If".to_string(),
            AstNode::Less { .. }=> "Less".to_string(),
            AstNode::LessOrEq { .. } => "LessOrEq".to_string(),
            AstNode::Equals { ..}  => "Equals".to_string(),
            AstNode::NotEquals { ..} => "NotEquals".to_string(),
            AstNode::GreaterOrEq { .. } => "GreaterOrEq".to_string(),
            AstNode::Greater { .. } => "Greater".to_string(),
            AstNode::Cast { .. }  => "As".to_string(),
            AstNode::ErrorNode => "<syntax error>".to_string(),
            AstNode::EmptyNode => "<empty node>".to_string(),
            AstNode::Break(_) => "break".to_string(),
            AstNode::Continue(_) => "continue".to_string(),
        })
    }
}

// primarily implemented for tests so that assert_eq! pretty prints the tree
// when test fails
impl Debug for AstNode {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "\n{}", self.print_impl(0))
    }
}

impl AstNode {
    pub fn print(&self) {
        println!("{}", self.print_impl(0));
    }

    fn print_impl(&self, indentation: usize) -> String {
        let int_str = iter::repeat(" ").take(indentation).collect::<String>();
        let mut string = format!("{}{}\n", int_str, self);

        let next_int = indentation + 2;
        match *self {
            AstNode::Block{ ref statements, ..} => {
                for c in statements {
                    string = format!("{}{}", string, c.print_impl(next_int));
                }
            },
            AstNode::Function{ ref block, .. } => string = format!("{}{}", string, block.print_impl(next_int)),
            AstNode::ExternFunction{ .. } => { /* do nothing, no children */},
            AstNode::FunctionCall{ ref arguments, .. } => {
                for arg in  arguments {
                    string = format!("{}{}", string, arg.print_impl(next_int));
                }
            },
            AstNode::VariableDeclaration{ref initialization_expression, ..} =>
                string = format!("{}{}", string, initialization_expression.print_impl(next_int)),
            AstNode::VariableAssignment{ref expression, ..} =>
                string = format!("{}{}", string, expression.print_impl(next_int)),
            AstNode::ArrayDeclaration{ref initialization_expression, ..} =>
                string = format!("{}{}", string, initialization_expression.print_impl(next_int)),
            AstNode::ArrayAssignment {
                index_expression: ref index,
                assignment_expression: ref assign,
                variable_name: _,
                span: _,
            } => {
                string = format!("{}{}", string, index.print_impl(next_int));
                string = format!("{}{}", string, assign.print_impl(next_int));
            },
            AstNode::InitializerList { ref values, .. } => {
                let mut list = vec![];
                for value in values.iter() {
                    list.push(value.print_impl(next_int).trim_end().to_owned());
                }

                let list = list.
                    iter().
                    skip(0).
                    fold(String::new(), |acc, t| format!("{}{}\n", acc, t));

                string = format!("{}{}", string, list);

            }
            AstNode::MemberAccess {
                object: ref member_access,
                ref member,
                span: _,
            } => {
                string = format!("{}{}", string, member_access.print_impl(next_int));
                string = format!("{}{}", string, member.print_impl(next_int));
            }
            AstNode::IntegralNumber{ .. } |
            AstNode::Long{ .. } |
            AstNode::Integer{ .. } |
            AstNode::Short{ .. } |
            AstNode::Byte{ .. } |
            AstNode::Float{ .. }  |
            AstNode::Double{ .. }  => {},
            AstNode::Text{ .. } => {},
            AstNode::Boolean{ .. }  => {},
            AstNode::Identifier{ .. }  => (), // no child elements
            AstNode::ArrayAccess{ref index_expression,  ref indexable_expression} => {
                string = format!("{}{}", string, indexable_expression.print_impl(next_int));
                string = format!("{}{}", string, index_expression.print_impl(next_int));
            },
            AstNode::ArraySlice{ref start_expression, ref end_expression,  ref array_expression} => {
                string = format!("{}{}", string, array_expression.print_impl(next_int));
                string = format!("{}{}", string, start_expression.print_impl(next_int));
                string = format!("{}{}", string, end_expression.print_impl(next_int));
            },
            AstNode::ArithmeticShiftRight { ref value, shift_count: ref shift_amount, .. } |
            AstNode::LogicalShiftRight { ref value, shift_count: ref shift_amount, .. } |
            AstNode::LogicalShiftLeft {  ref value, shift_count: ref shift_amount, .. } => {
                string = format!("{}{}", string, value.print_impl(next_int));
                string = format!("{}{}", string, shift_amount.print_impl(next_int));
            },
            AstNode::BooleanAnd{ ref left_expression, ref right_expression, .. } |
            AstNode::BooleanOr{ ref left_expression, ref right_expression, .. } |
            AstNode::BitwiseAnd{ ref left_expression, ref right_expression, .. } |
            AstNode::BitwiseOr{ ref left_expression, ref right_expression, .. } |
            AstNode::BitwiseXor { ref left_expression, ref right_expression, .. } |
            AstNode::Plus{ ref left_expression, ref right_expression, .. } |
            AstNode::Minus{ ref left_expression, ref right_expression, .. } |
            AstNode::Multiply{ ref left_expression, ref right_expression, .. } |
            AstNode::Divide{ ref left_expression, ref right_expression, .. } |
            AstNode::Modulo{ ref left_expression, ref right_expression, .. } => {
                string = format!("{}{}", string, left_expression.print_impl(next_int));
                string = format!("{}{}", string, right_expression.print_impl(next_int));
            },
            AstNode::Negate{ref expression, .. } => {
                string = format!("{}{}", string, expression.print_impl(next_int));
            },
            AstNode::Return{ ref return_value, .. } => {
                if let Some(ref child) = *return_value {
                    string = format!("{}{}", string, child.print_impl(next_int))
                }
            },
            AstNode::Loop { ref condition_expression, ref block, .. } => {
                string = format!("{}{}", string, condition_expression.print_impl(next_int));
                string = format!("{}{}", string, block.print_impl(next_int));
            },
            AstNode::If{ ref condition_expression, ref main_block, ref else_block, .. } => {
                string = format!("{}{}", string, condition_expression.print_impl(next_int));
                string = format!("{}{}", string, main_block.print_impl(next_int));
                if let Some(ref else_blk) = *else_block {
                    string = format!("{}{}", string, else_blk.print_impl(next_int));
                }
            },
            AstNode::Less{ ref left_expression, ref right_expression, .. } |
            AstNode::LessOrEq{ ref left_expression, ref right_expression, .. } |
            AstNode::Equals{ ref left_expression, ref right_expression, .. } |
            AstNode::NotEquals{ ref left_expression, ref right_expression, .. } |
            AstNode::GreaterOrEq{ ref left_expression, ref right_expression, .. } |
            AstNode::Greater{ ref left_expression, ref right_expression, .. } => {
                string = format!("{}{}", string, left_expression.print_impl(next_int));
                string = format!("{}{}", string, right_expression.print_impl(next_int));
            }
            AstNode::BooleanNot{ ref expression, .. } |
            AstNode::BitwiseNot{ ref expression, .. }
            => {
                string = format!("{}{}", string, expression.print_impl(next_int));
            },
            AstNode::Cast{ ref expression, .. } => {
                string = format!("{}{}", string, expression.print_impl(next_int));
            },
            AstNode::Break(_) |
            AstNode::Continue(_) |
            AstNode::EmptyNode |
            AstNode::ErrorNode => (),
        }

        string
    }

    pub fn span(&self) -> Span {
        let empty = Span::new(0, 0, 0);
        let x = match self {
            AstNode::Block{ span, ..} => *span,
            AstNode::Function{ function_info, .. } => function_info.span,
            AstNode::ExternFunction{ function_info} => function_info.span,
            AstNode::FunctionCall{span, ..} => *span,
            AstNode::VariableDeclaration{declaration_info, .. } => declaration_info.span,
            AstNode::VariableAssignment{ span, .. } => *span,
            AstNode::ArrayDeclaration{declaration_info, .. } => declaration_info.span,
            AstNode::ArrayAssignment{ span, .. } => *span,
            AstNode::InitializerList{ span, .. } => *span,
            AstNode::MemberAccess { span, .. } => *span,
            AstNode::BooleanAnd { span, ..} |
            AstNode::BooleanOr { span, ..} => *span,
            AstNode::BitwiseAnd { arithmetic_info, ..} |
            AstNode::BitwiseOr { arithmetic_info, ..} |
            AstNode::BitwiseXor { arithmetic_info, ..} |
            AstNode::Plus { arithmetic_info, .. } |
            AstNode::Minus { arithmetic_info, .. } |
            AstNode::Multiply { arithmetic_info, .. } |
            AstNode::Divide { arithmetic_info, .. } |
            AstNode::ArithmeticShiftRight { arithmetic_info, .. } |
            AstNode::LogicalShiftRight { arithmetic_info, .. } |
            AstNode::LogicalShiftLeft { arithmetic_info, .. } |
            AstNode::Modulo { arithmetic_info, .. } => arithmetic_info.span,
            AstNode::Negate{ arithmetic_info, ..} => arithmetic_info.span,
            AstNode::Return{ arithmetic_info, ..} => arithmetic_info.span,
            AstNode::Loop { span, .. } => *span,
            AstNode::If{ span, .. } => *span,
            AstNode::Less { span, .. } |
            AstNode::LessOrEq { span, .. } |
            AstNode::Equals { span, .. } |
            AstNode::NotEquals { span, .. } |
            AstNode::GreaterOrEq { span, .. } |
            AstNode::Greater { span, .. } => *span,
            AstNode::IntegralNumber{ span, .. } => *span,
            AstNode::Long{ span, .. } => *span,
            AstNode::Integer{ span, .. } => *span,
            AstNode::Short{ span, .. } => *span,
            AstNode::Byte{ span, .. } => *span,
            AstNode::Float{ span, .. } => *span,
            AstNode::Double{  span, .. } => *span,
            AstNode::Boolean{ span, .. } => *span,
            AstNode::Text{ span, .. } => *span,
            AstNode::Identifier{ span, ..} => *span,
            AstNode::ArrayAccess { indexable_expression, ..} => indexable_expression.span(),
            AstNode::ArraySlice { array_expression, ..} => array_expression.span(),
            AstNode::BooleanNot { span, .. } => *span,
            AstNode::BitwiseNot {  arithmetic_info, .. } => arithmetic_info.span,
            AstNode::Cast{ span, .. } => *span,
            AstNode::Break(span) |
            AstNode::Continue(span) => *span,
            AstNode::EmptyNode |
            AstNode::ErrorNode => empty,
        };
        x
    }

    pub fn span_ref_mut(&mut self) -> Option<&mut Span> {
        let x = match self {
            AstNode::Block { ref mut span, .. } => span,
            AstNode::Function { function_info, .. } => &mut function_info.span,
            AstNode::ExternFunction { function_info } => &mut function_info.span,
            AstNode::FunctionCall { span, .. } => span,
            AstNode::VariableDeclaration { declaration_info, .. } => &mut declaration_info.span,
            AstNode::VariableAssignment { span, .. } => span,
            AstNode::ArrayDeclaration { declaration_info, .. } => &mut declaration_info.span,
            AstNode::ArrayAssignment { span, .. } => span,
            AstNode::InitializerList { span, .. } => span,
            AstNode::MemberAccess { span, .. } => span,
            AstNode::BooleanAnd { span, .. } |
            AstNode::BooleanOr { span, .. } => span,
            AstNode::BitwiseAnd { arithmetic_info, .. } |
            AstNode::BitwiseOr { arithmetic_info, .. } |
            AstNode::BitwiseXor { arithmetic_info, .. } |
            AstNode::Plus { arithmetic_info, .. } |
            AstNode::Minus { arithmetic_info, .. } |
            AstNode::Multiply { arithmetic_info, .. } |
            AstNode::Divide { arithmetic_info, .. } |
            AstNode::ArithmeticShiftRight { arithmetic_info, .. } |
            AstNode::LogicalShiftRight { arithmetic_info, .. } |
            AstNode::LogicalShiftLeft { arithmetic_info, .. } |
            AstNode::Modulo { arithmetic_info, .. } => &mut arithmetic_info.span,
            AstNode::Negate { arithmetic_info, .. } => &mut arithmetic_info.span,
            AstNode::Return { arithmetic_info, .. } => &mut arithmetic_info.span,
            AstNode::Loop { span, .. } => span,
            AstNode::If { span, .. } => span,
            AstNode::Less { span, .. } |
            AstNode::LessOrEq { span, .. } |
            AstNode::Equals { span, .. } |
            AstNode::NotEquals { span, .. } |
            AstNode::GreaterOrEq { span, .. } |
            AstNode::Greater { span, .. } => span,
            AstNode::IntegralNumber { span, .. } => span,
            AstNode::Long{ span, .. } => span,
            AstNode::Integer { span, .. } => span,
            AstNode::Short{ span, .. } => span,
            AstNode::Byte { span, .. } => span,
            AstNode::Float { span, .. } => span,
            AstNode::Double { span, .. } => span,
            AstNode::Boolean { span, .. } => span,
            AstNode::Text { span, .. } => span,
            AstNode::Identifier { span, .. } => span,
            AstNode::ArrayAccess { indexable_expression, .. } => return indexable_expression.span_ref_mut(),
            AstNode::ArraySlice { array_expression, ..} => return array_expression.span_ref_mut(),
            AstNode::BooleanNot { span, .. } => span,
            AstNode::BitwiseNot { arithmetic_info, .. } => &mut arithmetic_info.span,
            AstNode::Cast { span, .. } => span,
            AstNode::Break(span) |
            AstNode::Continue(span) => span,
            AstNode::EmptyNode |
            AstNode::ErrorNode => return None,
        };
        Some(x)
    }
}
