use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::rc::Rc;

use crate::common::{
    node_info::Span,
    types::Type,
};

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum TokenType {
    Exclamation,
    NumberConstant,
    BooleanConstant,
    Text,
    Identifier,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Dot,
    SemiColon,
    Colon,
    If,
    Else,
    While,
    For,
    Let,
    Const,
    Val,
    Fn,
    Return,
    New,
    Class,
    Struct,
    Public,
    Protected,
    Private,
    Extern,
    Float,
    Double,
    Byte,
    Short,
    Integer,
    Long,
    Boolean,
    Void,
    String,
    Plus,
    Minus,
    Star,
    ForwardSlash,
    Percentage,
    Ampersand,
    DoubleAmpersand,
    Pipe,
    DoublePipe,
    Caret,
    Tilde,
    DoubleArrowLeft,
    DoubleArrowRight,
    TripleArrowRight,
    As,
    Eof,
    Equals,
    DoubleEquals,
    ExclamationEquals,
    ArrowLeft,
    ArrowLeftEquals,
    ArrowRight,
    ArrowRightEquals,
    PlusEquals,
    MinusEquals,
    StarEquals,
    ForwardSlashEquals,
    PercentageEquals,
    DoubleArrowLeftEquals,
    DoubleArrowRightEquals,
    TripleArrowRightEquals,
    AmpersandEquals,
    PipeEquals,
    CaretEquals,
    Break,
    Continue,
}

impl TokenType {
    pub fn type_tokens() -> Vec<TokenType> {
        vec![
            TokenType::Byte,
            TokenType::Short,
            TokenType::Integer,
            TokenType::Long,
            TokenType::Boolean,
            TokenType::Void,
            TokenType::String,
            TokenType::Float,
            TokenType::Double,
            TokenType::Identifier, // structs
        ]
    }
}

impl Display for TokenType {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
    write!(formatter, "{}", match *self {
        TokenType::Equals => "=",
        TokenType::Exclamation => "!",
        TokenType::NumberConstant => "number",
        TokenType::Text => "text",
        TokenType::Identifier=> "identifier",
        TokenType::LParen => "(",
        TokenType::RParen => ")",
        TokenType::LBrace => "{",
        TokenType::RBrace => "}",
        TokenType::LBracket => "[",
        TokenType::RBracket => "]",
        TokenType::Comma => ",",
        TokenType::Dot => ".",
        TokenType::SemiColon => ";",
        TokenType::Colon => ":",
        TokenType::If => "if",
        TokenType::Else => "else",
        TokenType::While => "while",
        TokenType::For => "for",
        TokenType::Let => "let",
        TokenType::Const => "const",
        TokenType::Val => "val",
        TokenType::Fn => "fn",
        TokenType::Return => "return",
        TokenType::BooleanConstant => "boolean",
        TokenType::New => "new",
        TokenType::Class => "class",
        TokenType::Struct=> "struct",
        TokenType::Public => "public",
        TokenType::Protected => "protected",
        TokenType::Private => "private",
        TokenType::Extern => "extern",
        TokenType::Float => "float",
        TokenType::Double => "double",
        TokenType::Byte => "byte",
        TokenType::Short => "short",
        TokenType::Integer => "int",
        TokenType::Long => "long",
        TokenType::Boolean => "bool",
        TokenType::Void => "void",
        TokenType::String => "string",
        TokenType::As => "as",
        TokenType::Plus => "+",
        TokenType::Minus => "-",
        TokenType::Star => "*",
        TokenType::Percentage => "%",
        TokenType::ForwardSlash => "/",
        TokenType::Ampersand => "&",
        TokenType::DoubleAmpersand => "&&",
        TokenType::Pipe=> "|",
        TokenType::DoublePipe => "||",
        TokenType::Caret => "^",
        TokenType::Tilde => "~",
        TokenType::DoubleArrowLeft => "<<",
        TokenType::DoubleArrowRight => ">>",
        TokenType::TripleArrowRight => ">>>",
        TokenType::ExclamationEquals => "!=",
        TokenType::DoubleEquals => "==",
        TokenType::ArrowLeft => "<",
        TokenType::ArrowRight => ">",
        TokenType::ArrowRightEquals => ">=",
        TokenType::ArrowLeftEquals => "<=",
        TokenType::Eof => "<EOF>",
        TokenType::PlusEquals => "+=",
        TokenType::MinusEquals => "-=",
        TokenType::StarEquals => "*=",
        TokenType::ForwardSlashEquals => "/=",
        TokenType::PercentageEquals => "%=",
        TokenType::DoubleArrowLeftEquals => "<<=",
        TokenType::DoubleArrowRightEquals => ">>=",
        TokenType::TripleArrowRightEquals => ">>>=",
        TokenType::AmpersandEquals => "&=",
        TokenType::PipeEquals => "|=",
        TokenType::CaretEquals => "^=",
        TokenType::Break => "break",
        TokenType::Continue => "continue",
    })
  }
}


// TODO: PartialEq should be implemented explicitly so that
// we can use epsilon for floating point numbers when doing equality comparison
#[derive(PartialEq, Debug, Clone)]
pub enum TokenAttribute {
    Text(Rc<String>),
    FloatConstant(f32), // TODO: Align with integer handling
    DoubleConstant(f64),
  /*
   *  At token stage, we only deal with positive values; later stages will negate numbers, as they
   *  have more context on if minus means negate or is part of arithmetic expression
   *  (for example, a = -45 vs a= 2-45).
   *
   *  This means that we must be able to store INT_MAX + 1, when token is actually INT_MIN.
   *
   *
   */

    // Generic integral constant, no type specified in constant
    IntegralConstant(u128),
    ByteConstant(u128),
    ShortConstant(u128),
    IntegerConstant(u128),
    LongConstant(u128),
    BooleanValue(bool),
    ErrorValue
}

impl Display for TokenAttribute {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
      write!(formatter, "{}", match *self {
          TokenAttribute::Text(ref text) => format!("\"{}\"", text),
          TokenAttribute::FloatConstant(value) => format!("{}f", value.to_string()),
          TokenAttribute::DoubleConstant(value) => format!("{}d", value.to_string()),
          TokenAttribute::IntegralConstant(value) => value.to_string(),
          TokenAttribute::ByteConstant(value) => value.to_string(),
          TokenAttribute::ShortConstant(value) => value.to_string(),
          TokenAttribute::IntegerConstant(value) => value.to_string(),
          TokenAttribute::LongConstant(value) => value.to_string(),
          TokenAttribute::BooleanValue(value) => value.to_string(),
          TokenAttribute::ErrorValue => "<Invalid token>".to_string(),
    })
  }
}


#[derive(Clone, Debug)]
pub struct Token {
  pub token_type: TokenType,
  pub attribute: Option<TokenAttribute>,
  // Todo: Replace with Span
  pub line: i32,
  pub column: i32,
  pub length: i32,
}

impl Display for Token {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
      write!(formatter, "{}{}", self.token_type, if self.attribute.is_some() { format!(" {}", self.attribute.as_ref().unwrap()) } else { "".to_string() })
    }
}


// do not check for line numbers or positions; only check for type\attribute equality
// also, special cases for floating point comparisons
impl PartialEq for Token {

  fn eq(&self, other: &Token) -> bool {
    if self.token_type == other.token_type {
      match self.attribute {
        Some(TokenAttribute::FloatConstant(self_val)) => {
          match other.attribute {
            Some(TokenAttribute::FloatConstant(other_val)) => (self_val - other_val).abs() < 0.0001,
            _=> false
          }
        }
        Some(TokenAttribute::DoubleConstant(self_val)) => {
          match other.attribute {
            Some(TokenAttribute::DoubleConstant(other_val)) => (self_val - other_val).abs() < 0.0001,
            _=> false
          }
        }

        _ => self.attribute == other.attribute
      }

    } else {
      false
    }
  }

}

impl Token {
  pub fn new(token_type: TokenType, attribute: Option<TokenAttribute>, line: i32, column: i32, length : i32) -> Token {
    Token { token_type, attribute, line, column, length }
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



impl From<&Token> for Type {
    fn from(variable_token: &Token) -> Type {
        match variable_token.token_type {
            TokenType::Byte => Type::Byte,
            TokenType::Short => Type::Short,
            TokenType::Integer => Type::Integer,
            TokenType::Long=> Type::Long,
            TokenType::String => Type::String,
            TokenType::Float => Type::Float,
            TokenType::Double => Type::Double,
            TokenType::Boolean => Type::Boolean,
            TokenType::Void => Type::Void,
            TokenType::Identifier => {
                if let Some(TokenAttribute::Text(ref text)) = variable_token.attribute {
                    Type::UserDefined((**text).clone())
                } else {
                    ice!("Unexpected token attribute {:?}", variable_token.attribute);
                }
            },
            _ => ice!("Expected type but was '{}' instead", variable_token),
        }
    }
}

impl From<Token> for Type {
    fn from(variable_token: Token) -> Type {
        Type::from(&variable_token)
    }
}

impl From<&Token> for Rc<String> {
    fn from(variable_token: &Token) -> Rc<String> {
        match variable_token.attribute {
            Some(TokenAttribute::Text(ref text)) => text.clone(),
            _ => ice!("Bad token {:?}", variable_token),
        }
    }
}

impl From<Token> for Rc<String> {
    fn from(variable_token: Token) -> Rc<String> {
        (&variable_token).into()
    }
}