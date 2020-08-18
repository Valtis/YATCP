use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;

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
    Public,
    Protected,
    Private,
    Extern,
    Float,
    Double,
    Byte,
    Integer,
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
}

impl TokenType {
    pub fn get_type_tokens() -> Vec<TokenType> {
        vec![
            TokenType::Integer,
            TokenType::Byte,
            TokenType::Boolean,
            TokenType::Void,
            TokenType::String,
            TokenType::Float,
            TokenType::Double,
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
        TokenType::Public => "public",
        TokenType::Protected => "protected",
        TokenType::Private => "private",
        TokenType::Extern => "extern",
        TokenType::Float => "float",
        TokenType::Double => "double",
        TokenType::Byte => "byte",
        TokenType::Integer => "int",
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
      })
  }
}


// TODO: PartialEq should be implemented explicitly so that
// we can use epsilon for floating point numbers when doing equality comparison
#[derive(PartialEq, Debug, Clone)]
pub enum TokenSubType {
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
    BooleanValue(bool),
    NoSubType,
    ErrorToken
}

impl Display for TokenSubType {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
     write!(formatter, "{}", match *self {
          TokenSubType::NoSubType => "".to_string(),
          _ => "(".to_string(),
     })?;

      write!(formatter, "{}", match *self {
        TokenSubType::Text(ref text) => format!("\"{}\"", text),
        TokenSubType::FloatConstant(value) => format!("{}f", value.to_string()),
        TokenSubType::DoubleConstant(value) => format!("{}d", value.to_string()),
        TokenSubType::IntegralConstant(value) => value.to_string(),
        TokenSubType::BooleanValue(value) => value.to_string(),
        TokenSubType::NoSubType => "".to_string(),
        TokenSubType::ErrorToken => "<Invalid token>".to_string(),
    })?;

    write!(formatter, "{}", match *self {
         TokenSubType::NoSubType => "".to_string(),
         _ => ")".to_string(),
    })
  }
}


#[derive(Clone, Debug)]
pub struct Token {
  pub token_type: TokenType,
  pub token_subtype: TokenSubType,
  pub line: i32,
  pub column: i32,
  pub length: i32,
}

impl Display for Token {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
      write!(formatter, "{}{}", self.token_type, self.token_subtype)
    }
}


// do not check for line numbers or positions; only check for type\subtype equality
// also, special cases for floating point comparisons
impl PartialEq for Token {

  fn eq(&self, other: &Token) -> bool {
    if self.token_type == other.token_type {
      match self.token_subtype {
        TokenSubType::FloatConstant(self_val) => {
          match other.token_subtype {
            TokenSubType::FloatConstant(other_val) => (self_val - other_val).abs() < 0.0001,
            _=> false
          }
        }
        TokenSubType::DoubleConstant(self_val) => {
          match other.token_subtype {
            TokenSubType::DoubleConstant(other_val) => (self_val - other_val).abs() < 0.0001,
            _=> false
          }
        }

        _ => self.token_subtype == other.token_subtype
      }

    } else {
      false
    }
  }

}

impl Token {
  pub fn new(token_type: TokenType, subtype: TokenSubType, line: i32, column: i32, length : i32) -> Token {
    Token { token_type, token_subtype: subtype, line, column, length }
  }
}
