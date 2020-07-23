use crate::lexer::token::{Token, TokenSubType};
use crate::semcheck::Type;
use crate::symbol_table;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::fmt::Debug;
use std::iter;

use std::rc::Rc;

fn get_text_from_identifier(identifier: &Token) -> Rc<String> {
    match identifier.token_subtype {
        TokenSubType::Identifier(ref text) => text.clone(),
        _ => ice!("Expected identifier but was '{}' instead", identifier),
    }
}

fn get_type_from_type_token(variable_type: &Token) -> Type {
  match variable_type.token_subtype {
    TokenSubType::IntegerType => Type::Integer,
    TokenSubType::StringType => Type::String,
    TokenSubType::FloatType => Type::Float,
    TokenSubType::DoubleType => Type::Double,
    TokenSubType::BooleanType => Type::Boolean,
    TokenSubType::VoidType => Type::Void,
    _ => ice!("Expected type but was '{}' instead", variable_type),
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstInteger {
    Int(i32),
    IntMaxPlusOne,
    Invalid(u64),
}

impl From<u64> for AstInteger {
    fn from(val: u64) -> AstInteger {
        if val <= i32::max_value() as u64 {
            AstInteger::Int(val as i32)
        } else if val == i32::max_value() as u64 + 1 {
            AstInteger::IntMaxPlusOne
        } else {
            AstInteger::Invalid(val)
        }
    }
}

impl From<i32> for AstInteger {
    fn from(val: i32) -> AstInteger {
        AstInteger::Int(val)
    }
}



impl Display for AstInteger {
     fn fmt(&self, formatter: &mut Formatter) -> Result {
         write!(formatter, "{}",
                match self {
                    AstInteger::Invalid(val) => format!("(Overflow, {} does not fit in i32)", val),
                    AstInteger::Int(val) => format!("{}", val),
                    AstInteger::IntMaxPlusOne => "Int max plus one (2147483648".to_owned(),
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
    VariableAssignment{ expression: Box<AstNode>, name: Rc<String>, span: Span },
    ArrayAssignment{index_expression: Box<AstNode>, assignment_expression: Box<AstNode>, variable_name: Rc<String>, span: Span },

    MemberAccess { object: Box<AstNode>, member: Box<AstNode>, span: Span },
    Plus{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Minus{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Multiply{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Divide{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Modulo{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },
    Negate{ expression: Box<AstNode>, arithmetic_info: ArithmeticInfo },

    BooleanAnd{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    BooleanOr{ left_expression: Box<AstNode>, right_expression: Box<AstNode>, span: Span },
    BooleanNot{ expression: Box<AstNode>, span: Span },

    Return(Option<Box<AstNode>>, ArithmeticInfo),

    While(Box<AstNode>, Box<AstNode>, Span),
    If(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>, Span),

    Less(Box<AstNode>, Box<AstNode>, Span),
    LessOrEq(Box<AstNode>, Box<AstNode>, Span),
    Equals(Box<AstNode>, Box<AstNode>, Span),
    NotEquals(Box<AstNode>, Box<AstNode>, Span),
    GreaterOrEq(Box<AstNode>, Box<AstNode>, Span),
    Greater(Box<AstNode>, Box<AstNode>, Span),

    // Signed integer, but we may need to store INT_MAX +1 while negation is still unresolved
    Integer(AstInteger, Span),
    Float(f32, Span),
    Double(f64, Span),
    Text(Rc<String>, Span),
    Boolean(bool, Span),
    Identifier(Rc<String>, Span),
    ArrayAccess{index_expression: Box<AstNode>, indexable_expression: Box<AstNode>},

    ErrorNode,
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
            AstNode::VariableDeclaration{ref declaration_info, ..} => {
                match declaration_info.variable_type {
                    _ if declaration_info.variable_type.is_array() => {
                        let mut dim_str = "".to_owned();

                        if let Some(ExtraDeclarationInfo::ArrayDimension(ref dims)) = declaration_info.extra_info {
                            for dim in dims.iter() {
                                match dim {
                                    AstInteger::Int(val) => dim_str = format!("{}[{}]", dim_str, val),
                                    AstInteger::IntMaxPlusOne => dim_str = format!("{}[{}]", dim_str, (std::i32::MAX as u64) + 1),
                                    AstInteger::Invalid(val) => dim_str = format!("{}[{}<invalid>]", dim_str, val),
                                }
                            }
                        } else {
                            ice!("Non-array dimension information on array declaration");
                        }

                        format!("Variable declaration '{}' : {}{}", declaration_info.name, declaration_info.variable_type, dim_str)
                    },
                    _ => format!("Variable declaration '{}' : {}", declaration_info.name, declaration_info.variable_type),
                }
            }
            AstNode::VariableAssignment{ ref name, .. } =>
                format!("Variable assignment '{}'", name),
            AstNode::ArrayAssignment {
                index_expression: _,
                assignment_expression: _,
                variable_name: name,
                span: _
            } => format!("Array assignment '{}'", name),
            AstNode::MemberAccess { .. } => format!("Member access"),
            AstNode::Integer(val, _) => format!("Integer: {}", val),
            AstNode::Float(val, _) => format!("Float: {}", val),
            AstNode::Double(val, _) => format!("Double: {}", val),
            AstNode::Text(ref text, _) => format!("Text: {}", text),
            AstNode::Identifier(ref name, _) => format!("Identifier: {}", name),
            AstNode::ArrayAccess{ .. }=> format!("Array access"),
            AstNode::Boolean(ref value, _) => format!("Boolean: {}", value),
            AstNode::Plus{ .. } => "Plus".to_string(),
            AstNode::Minus{ .. } => "Minus".to_string(),
            AstNode::Multiply {.. } => "Multiply".to_string(),
            AstNode::Divide { .. } => "Divide".to_string(),
            AstNode::Modulo { .. }  => "Modulo".to_string(),
            AstNode::BooleanAnd { .. } => "And".to_string(),
            AstNode::BooleanOr { .. } => "Or".to_string(),
            AstNode::Negate { .. }  => "Negate".to_string(),
            AstNode::Return(_, _) => "Return".to_string(),
            AstNode::While(_, _, _) => "While".to_string(),
            AstNode::If(_, _, _, _) => "If".to_string(),
            AstNode::Less(_, _, _) => "Less".to_string(),
            AstNode::LessOrEq(_, _, _) => "LessOrEq".to_string(),
            AstNode::Equals(_, _, _) => "Equals".to_string(),
            AstNode::NotEquals(_, _, _) => "NotEquals".to_string(),
            AstNode::GreaterOrEq(_, _, _) => "GreaterOrEq".to_string(),
            AstNode::Greater(_, _, _) => "Greater".to_string(),
            AstNode::BooleanNot{ .. } => "Not".to_string(),
            AstNode::ErrorNode => "<syntax error>".to_string(),
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

    fn print_impl(&self, intendation: usize) -> String {
        let int_str = iter::repeat(" ").take(intendation).collect::<String>();
        let mut string = format!("{}{}\n", int_str, self);

        let next_int = intendation + 2;
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
            AstNode::ArrayAssignment {
                index_expression: ref index,
                assignment_expression: ref assign,
                variable_name: _,
                span: _,
            } => {
                string = format!("{}{}", string, index.print_impl(next_int));
                string = format!("{}{}", string, assign.print_impl(next_int));
            },
            AstNode::MemberAccess {
                object: ref member_access,
                ref member,
                span: _,
            } => {
                string = format!("{}{}", string, member_access.print_impl(next_int));
                string = format!("{}{}", string, member.print_impl(next_int));
            }
            AstNode::Integer(_, _) |
            AstNode::Float(_, _) |
            AstNode::Double(_, _) => {},
            AstNode::Text(_, _) => {},
            AstNode::Identifier(_, _) => {},
            AstNode::ArrayAccess{ref index_expression,  ref indexable_expression} => {
                string = format!("{}{}", string, indexable_expression.print_impl(next_int));
                string = format!("{}{}", string, index_expression.print_impl(next_int));
            },
            AstNode::Boolean(_, _) => {},
            AstNode::BooleanAnd{ ref left_expression, ref right_expression, .. } |
            AstNode::BooleanOr{ ref left_expression, ref right_expression, .. } |
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
            AstNode::Return(ref opt_child, _) => {
                if let Some(ref child) = *opt_child {
                    string = format!("{}{}", string, child.print_impl(next_int))
                }
            },
            AstNode::While(ref expr, ref child, _) => {
                string = format!("{}{}", string, expr.print_impl(next_int));
                string = format!("{}{}", string, child.print_impl(next_int));
            },
            AstNode::If(ref expr, ref child, ref opt_else_blk, _) => {
                string = format!("{}{}", string, expr.print_impl(next_int));
                string = format!("{}{}", string, child.print_impl(next_int));
                if let Some(ref else_blk) = *opt_else_blk {
                    string = format!("{}{}", string, else_blk.print_impl(next_int));
                }
            },
            AstNode::Less(ref left, ref right, _) |
            AstNode::LessOrEq(ref left, ref right, _) |
            AstNode::Equals(ref left, ref right, _) |
            AstNode::NotEquals(ref left, ref right, _) |
            AstNode::GreaterOrEq(ref left, ref right, _) |
            AstNode::Greater(ref left, ref right, _) => {
                string = format!("{}{}", string, left.print_impl(next_int));
                string = format!("{}{}", string, right.print_impl(next_int));
            }
            AstNode::BooleanNot{ ref expression, .. } => {
                string = format!("{}{}", string, expression.print_impl(next_int));
            },
            AstNode::ErrorNode => {}
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
            AstNode::ArrayAssignment{ span, .. } => *span,
            AstNode::MemberAccess { span, .. } => *span,
            AstNode::BooleanAnd{ span, ..} |
            AstNode::BooleanOr{ span, ..} => *span,
            AstNode::Plus { arithmetic_info, .. } |
            AstNode::Minus { arithmetic_info, .. } |
            AstNode::Multiply { arithmetic_info, .. } |
            AstNode::Divide { arithmetic_info, .. } |
            AstNode::Modulo { arithmetic_info, .. } => arithmetic_info.span,
            AstNode::Negate{ arithmetic_info, ..} => arithmetic_info.span,
            AstNode::Return(_, info) => info.span,
            AstNode::While(_, _, span) => *span,
            AstNode::If(_, _, _, span) => *span,
            AstNode::Less(_, _, span) |
            AstNode::LessOrEq(_, _, span) |
            AstNode::Equals(_, _, span) |
            AstNode::NotEquals(_, _, span) |
            AstNode::GreaterOrEq(_, _, span) |
            AstNode::Greater(_, _, span) => *span,
            AstNode::Integer(_, span) => *span,
            AstNode::Float(_, span) => *span,
            AstNode::Double(_, span) => *span,
            AstNode::Text(_, span) => *span,
            AstNode::Identifier(_, span) => *span,
            AstNode::ArrayAccess { indexable_expression, ..} => indexable_expression.span(),
            AstNode::Boolean(_, span) => *span,
            AstNode::BooleanNot{ span, .. } => *span,
            AstNode::ErrorNode => empty,
        };
        x
    }
}


#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub line: i32,
    pub column: i32,
    pub length: i32,
}

impl Span {
    pub fn new(line: i32, column: i32, length: i32) -> Span {
        Span {
            line,
            column,
            length,
        }
    }
}

impl From<Token> for Span {
    fn from(val: Token) -> Span {
        Span {
            line: val.line,
            column: val.column,
            length: val.length,
        }
    }
}

impl From<&Token> for Span {
    fn from(val: &Token) -> Span {
        Span {
            line: val.line,
            column: val.column,
            length: val.length,
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: Rc<String>,
    pub parameters: Vec<DeclarationInfo>,
    pub return_type: Type,
    pub span: Span,
}

impl FunctionInfo {
    pub fn new(
        identifier: &Token,
        return_type: &Token) -> FunctionInfo {

        FunctionInfo::new_alt(
            get_text_from_identifier(identifier),
            get_type_from_type_token(return_type),
            identifier.line,
            identifier.column,
            identifier.length)
    }

    pub fn new_alt(
        name: Rc<String>,
        return_type: Type,
        line: i32,
        column: i32,
        length: i32,
        ) -> FunctionInfo {
        FunctionInfo {
            name: name,
            parameters: vec![],
            return_type: return_type,
            span: Span::new(
                line, column, length),
        }

    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationInfo {
    pub name: Rc<String>,
    pub variable_type: Type,
    pub span: Span,
    pub extra_info: Option<ExtraDeclarationInfo>,
}

impl DeclarationInfo {
    pub fn new(identifier: &Token, variable_type: &Token) -> DeclarationInfo {
        DeclarationInfo::new_alt(
            get_text_from_identifier(identifier),
            get_type_from_type_token(variable_type),
            identifier.line,
            identifier.column,
            identifier.length)
    }

    pub fn new_alt(
        name: Rc<String>,
        var_type: Type,
        line: i32,
        column: i32,
        length: i32) -> DeclarationInfo {

        DeclarationInfo {
            name: name,
            variable_type: var_type,
            span: Span::new(line, column, length),
            extra_info: None,
        }
    }

    pub fn new_with_extra_info(
        identifier: &Token,
        variable_type: &Token,
        extra_info: Option<ExtraDeclarationInfo>) -> DeclarationInfo {
        DeclarationInfo {
            name: get_text_from_identifier(identifier),
            variable_type: get_type_from_type_token(variable_type),
            span: Span::new(identifier.line, identifier.column, identifier.length),
            extra_info,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExtraDeclarationInfo {
    ArrayDimension(Vec<AstInteger>), // left-to-right, in declaration order
}



#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticInfo {
    pub node_type: Type,
    pub span: Span
}

impl ArithmeticInfo {
    pub fn new(token: &Token) -> ArithmeticInfo {
        ArithmeticInfo::new_alt(token.line, token.column, token.length)
    }

    pub fn new_alt(
        line: i32,
        column: i32,
        length: i32) -> ArithmeticInfo {

        ArithmeticInfo {
            node_type: Type::Uninitialized,
            span: Span::new(line, column, length)
        }
    }
}
