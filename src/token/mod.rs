use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
 
#[derive(Eq, PartialEq, Copy, Clone, Debug)]
pub enum TokenType {
  Assign,
  Equals,
  Less,
  LessOrEq,
  Greater,
  GreaterOrEq,
  NotEq,
  Not,
  Number,
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
  Fn,
  Return,
  Boolean,
  New,
  Class,
  Public,
  Protected,
  Private,
  VarType,
  Plus,
  Minus,
  Multiply,
  Divide,
  Eof,
}

impl Display for TokenType {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
    Display::fmt(
      match *self {
        TokenType::Assign => "=",
        TokenType::Equals => "==",
        TokenType::Less => "<",
        TokenType::Greater => ">",
        TokenType::GreaterOrEq => ">=",
        TokenType::LessOrEq => "<=",
        TokenType::NotEq => "!=",
        TokenType::Not => "!",
        TokenType::Number => "number",
        TokenType::Text => "text",
        TokenType::Identifier => "identifier",
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
        TokenType::Fn => "fn",
        TokenType::Return => "return",
        TokenType::Boolean => "boolean",
        TokenType::New => "new",
        TokenType::Class => "class",
        TokenType::Public => "public",
        TokenType::Protected => "protected",
        TokenType::Private => "private",
        TokenType::VarType => "type",
        TokenType::Plus => "+" ,
        TokenType::Minus => "-",
        TokenType::Multiply => "*",
        TokenType::Divide => "/",
        TokenType::Eof => "<EOF>",

      }, formatter)
  }
}

// TODO: PartialEq should be implemented explicitly so that
// we can use epsilon for floating point numbers when doing equality comparison
#[derive(PartialEq, Debug, Clone)]
pub enum TokenSubType {
  Text(String), // index to text table
  FloatNumber(f32),
  DoubleNumber(f64),
  IntegerNumber(i32),
  Identifier(String), // index to text table
  BooleanValue(bool),
  FloatType,
  DoubleType,
  IntegerType,
  BooleanType,
  VoidType,
  StringType,
  NoSubType,
  ErrorToken
}

impl Display for TokenSubType {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
     try!(write!(formatter, "{}", match *self {
          TokenSubType::NoSubType => "".to_string(),
          _ => "(".to_string(),
     }));
    try!(write!(formatter, "{}", match *self {
        TokenSubType::Text(ref text) => format!("{}{}{}", "\"", text.clone(), "\""),
        TokenSubType::FloatNumber(value) => format!("{}f", value.to_string()),
        TokenSubType::DoubleNumber(value) => format!("{}d", value.to_string()),
        TokenSubType::IntegerNumber(value) => value.to_string(),
        TokenSubType::Identifier(ref text) => format!("{}{}{}", "\"", text, "\""),
        TokenSubType::BooleanValue(value) => value.to_string(),
        TokenSubType::FloatType => "float".to_string(),
        TokenSubType::DoubleType => "double".to_string(),
        TokenSubType::IntegerType => "int".to_string(),
        TokenSubType::BooleanType => "bool".to_string(),
        TokenSubType::VoidType => "void".to_string(),
        TokenSubType::StringType => "string".to_string(),
        TokenSubType::NoSubType => "".to_string(),
        TokenSubType::ErrorToken => "<Invalid token>".to_string(),
    }));

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
        TokenSubType::FloatNumber(self_val) => {
          match other.token_subtype {
            TokenSubType::FloatNumber(other_val) => (self_val - other_val).abs() < 0.0001,
            _=> false
          }
        }
        TokenSubType::DoubleNumber(self_val) => {
          match other.token_subtype {
            TokenSubType::DoubleNumber(other_val) => (self_val - other_val).abs() < 0.0001,
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
    Token { token_type: token_type, token_subtype: subtype, line: line, column: column, length: length }
  }
}
