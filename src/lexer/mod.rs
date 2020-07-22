pub mod token;

use token::*;
use crate::ast::Span as Span;
use crate::error_reporter::{ErrorReporter, ReportKind};

use crate::string_table::StringTable;


use std::io::BufReader;
use std::io::Read;
use std::io::Bytes;

use std::iter::Peekable;
use std::rc::Rc;
use std::cell::RefCell;
/* Unstable API - use this once stabilized */
//use std::num::IntErrorKind::*;

pub const SPACES_PER_TAB: i32 = 4;

pub trait Lexer {
    fn next_token(&mut self) -> Token;
    fn peek_token(&mut self) -> Token;
    fn current_token(&self) -> Option<Token>;
}

pub struct ReadLexer {
    line: i32,
    column: i32,
    token_start_line: i32,
    token_start_column: i32,
    iter: Peekable<Bytes<BufReader<Box<dyn Read>>>>, // FIXME! Change to Chars once api stabilizes. Using Bytes when multi-code point characters are present causes bugs
    next_token: Option<Token>, // used for storing token after peeking
    current_token: Option<Token>, // token that was returned by next_token.
    error_reporter: Rc<RefCell<dyn ErrorReporter>>,
    string_table: StringTable,
}

impl ReadLexer {
    pub fn new(
        input: Box<dyn Read>,
        error_reporter: Rc<RefCell<dyn ErrorReporter>>) -> ReadLexer {

        ReadLexer {
          line: 1,
          column: 1,
          token_start_line: 1,
          token_start_column: 1,
          iter: BufReader::new(input).bytes().peekable(),
          next_token: None,
          current_token: None,
          error_reporter: error_reporter,
          string_table: StringTable::new(),
        }
    }


    fn starts_comment(&mut self, ch: char) -> bool {
        if ch == '/' {
          return match self.peek_char() {
            Some(ch) => ch == '/',
            None => false,
          }
        }

        false
    }

  fn skip_comment(&mut self) {
    loop {
      match self.next_char() {
        Some(ch) => if ch == '\n' { break },
        None => break,
      }
    }
  }

  fn starts_symbol(&mut self, ch: char) -> bool {
      match ch {
          '+' | '-' | '*' | '/' | '%' | '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>' | '=' | ';' | ',' | ':' | '!' | '&' | '|' | '~' | '^' => true,
          '.' => self.does_not_start_desimal_number(),
          _ => false,
      }
  }

  fn handle_symbols(&mut self, ch: char) -> Token {
      match ch {
              '-' => self.create_token(TokenType::Minus, TokenSubType::NoSubType),
          '+' => self.create_token(TokenType::Plus, TokenSubType::NoSubType),
          '*' => self.create_token(TokenType::Star, TokenSubType::NoSubType),
          '/' => self.create_token(TokenType::ForwardSlash, TokenSubType::NoSubType),
          '%' => self.create_token(TokenType::Percentage, TokenSubType::NoSubType),
          '[' => self.create_token(TokenType::LBracket, TokenSubType::NoSubType),
          ']' => self.create_token(TokenType::RBracket, TokenSubType::NoSubType),
          '{' => self.create_token(TokenType::LBrace, TokenSubType::NoSubType),
          '}' => self.create_token(TokenType::RBrace, TokenSubType::NoSubType),
          '(' => self.create_token(TokenType::LParen, TokenSubType::NoSubType),
          ')' => self.create_token(TokenType::RParen, TokenSubType::NoSubType),
          ';' => self.create_token(TokenType::SemiColon, TokenSubType::NoSubType),
          ',' => self.create_token(TokenType::Comma, TokenSubType::NoSubType),
          '.' => self.create_token(TokenType::Dot, TokenSubType::NoSubType),
          ':' => self.create_token(TokenType::Colon, TokenSubType::NoSubType),
          '^' => self.create_token(TokenType::Caret, TokenSubType::NoSubType),
          '~' => self.create_token(TokenType::Tilde, TokenSubType::NoSubType),
          '=' => self.multi_char_operator_helper(
              '=',
              (TokenType::DoubleEquals, TokenSubType::Equals),
              (TokenType::Equals, TokenSubType::NoSubType)),
          '>' => self.multi_char_operator_helper(
              '=',
              (TokenType::DoubleEquals, TokenSubType::GreaterOrEq),
              (TokenType::DoubleEquals, TokenSubType::Greater)),
          '<' => self.multi_char_operator_helper(
              '=',
              (TokenType::DoubleEquals, TokenSubType::LessOrEq),
              (TokenType::DoubleEquals, TokenSubType::Less)),
          '!' => self.multi_char_operator_helper(
              '=',
              (TokenType::DoubleEquals, TokenSubType::NotEquals),
              (TokenType::Exclamation, TokenSubType::NoSubType)),
          '&' => self.multi_char_operator_helper(
              '&',
              (TokenType::DoubleAmpersand, TokenSubType::NoSubType),
              (TokenType::Ampersand, TokenSubType::NoSubType)),
          '|' => self.multi_char_operator_helper(
              '|',
              (TokenType::DoublePipe, TokenSubType::NoSubType),
              (TokenType::Pipe, TokenSubType::NoSubType)),
          _ => ice!("Unexpected symbol '{}' passed to operator handler", ch),
      }
  }

  fn does_not_start_desimal_number(&mut self) -> bool {
    self.peek_char().map_or(false, |c| !c.is_digit(10))
  }

  fn multi_char_operator_helper (
    &mut self,
    optional_second_char: char,
    type_if_matches: (TokenType, TokenSubType),
    type_if_no_match: (TokenType, TokenSubType)) -> Token {

        let mut next_char = ' ';

        if let Some(ch) = self.peek_char() {
            next_char = ch;
        }

        let (t, st) = if next_char == optional_second_char {
            // consume the next character
            self.next_char();
            type_if_matches
        } else {
            type_if_no_match
        };

        self.create_token(t, st)
    }


  fn starts_identifier(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
  }

  fn is_valid_identifier_character(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
  }

  fn handle_identifier(&mut self, ch: char) -> Token {

    let mut identifier = ch.to_string();

    loop {
      // workaround for multiple mutable borrows
      let value = self.peek_char();

      match value {
        Some(ch) => {
          if ReadLexer::is_valid_identifier_character(ch) {
            identifier.push(ch);
            self.next_char();
          } else {
            break;
          }
        }
        None => break
      }
    }


    match self.handle_keywords(&identifier) {
        Some(token) => token,
        None => {
            let index = self.string_table.insert(identifier);

            self.create_token(
                TokenType::Identifier,
                TokenSubType::Identifier(index))
        },
    }
}

  /*if, else, while, for, let, fn, return, new, class,
  public, protected, private, true, false, int, float, double, bool, void*/
  fn handle_keywords(&self, identifier: &str) -> Option<Token> {
    match identifier {
      "if" => Some(self.create_token(TokenType::If, TokenSubType::NoSubType)),
      "else" => Some(self.create_token(TokenType::Else, TokenSubType::NoSubType)),
      "while" => Some(self.create_token(TokenType::While, TokenSubType::NoSubType)),
      "for" => Some(self.create_token(TokenType::For, TokenSubType::NoSubType)),
      "let" => Some(self.create_token(TokenType::Let, TokenSubType::NoSubType)),
      "fn" => Some(self.create_token(TokenType::Fn, TokenSubType::NoSubType)),
      "return" => Some(self.create_token(TokenType::Return, TokenSubType::NoSubType)),
      "new" => Some(self.create_token(TokenType::New, TokenSubType::NoSubType)),
      "class" => Some(self.create_token(TokenType::Class, TokenSubType::NoSubType)),
      "public" => Some(self.create_token(TokenType::Public, TokenSubType::NoSubType)),
      "protected" => Some(self.create_token(TokenType::Protected, TokenSubType::NoSubType)),
      "private" => Some(self.create_token(TokenType::Private, TokenSubType::NoSubType)),
      "extern" => Some(self.create_token(TokenType::Extern, TokenSubType::NoSubType)),
      "true" => Some(self.create_token(TokenType::Boolean, TokenSubType::BooleanValue(true))),
      "false" => Some(self.create_token(TokenType::Boolean, TokenSubType::BooleanValue(false))),
      "int" => Some(self.create_token(TokenType::VarType, TokenSubType::IntegerType)),
      "float" => Some(self.create_token(TokenType::VarType, TokenSubType::FloatType)),
      "double" => Some(self.create_token(TokenType::VarType, TokenSubType::DoubleType)),
      "bool" => Some(self.create_token(TokenType::VarType, TokenSubType::BooleanType)),
      "void" => Some(self.create_token(TokenType::VarType, TokenSubType::VoidType)),
      "string" => Some(self.create_token(TokenType::VarType, TokenSubType::StringType)),
      _ => None
    }
  }

  // either a number, or dot followed by a number
  fn starts_number(&mut self, ch: char) -> bool {
    if ch.is_digit(10) {
      true
    } else if ch == '.' {
      match self.peek_char() {
        Some(new_ch) => {
          new_ch.is_digit(10)
        }
        None => {
          false
        }
      }
    } else {
      false
    }
  }

  fn handle_number(&mut self, ch: char) -> Token {

    let mut number_str = ch.to_string();

    if ch == '.' {
      return self.handle_decimal_number(number_str);
    }
    loop {
      // workaround for multiple mutable borrows
      let value = match self.peek_char() {
          Some(ch) => Some (ch),
          None => None,
      };


      match value {
        Some(ch) => {
          if ch.is_digit(10) {
            number_str.push(ch);
            self.next_char();
          } else if ch == '.' {
            number_str.push(ch);
            self.next_char();
            return self.handle_decimal_number(number_str);
          } else if ch.is_alphabetic() {
            return self.handle_number_type_str(number_str);
          } else {
            break;
          }
        }
        None => break
      }
    }

    match number_str.parse() {
      Ok(number) => self.create_token(TokenType::Number, TokenSubType::IntegerNumber(number)),


      Err(e) => {
          // UNSTABLE RIGHT NOW - use once stabilized
          /*match e.kind() {
            Overflow => {
                self.report_error(
                    ReportKind::TokenError,
                    self.line,
                    self.column,
                    (self.column - self.token_start_column) as usize,
                    format!("Error message here"),
                );

                ErrorToken
            },
            _ =>  ice!("Non-numeric characters in number token at {}:{} ({})",
            self.line, self.column, e),
          }*/

          // workaround, as the kind() is not stable.
          if e.to_string().contains("too large to fit") {
              self.report_error(
                  ReportKind::TokenError,
                  self.line,
                  self.token_start_column,
                  (self.column - self.token_start_column) as usize,
                  format!("Number does not fit inside 32 bit signed integer"),
              );

              return self.create_token(TokenType::Number, TokenSubType::ErrorToken);
          }

          ice!("Non-numeric characters in number token at {}:{} ({})",
                self.line, self.column, e)
      },
    }
  }

  fn handle_decimal_number(&mut self, mut number_str: String) -> Token {
    let mut separator_error = false;
    loop {

        let value = self.peek_char();

        match value {
            Some(ch) => {
                if ch.is_digit(10) {
                    number_str.push(ch);
                    self.next_char();
                } else if ch.is_alphabetic() {
                    return self.handle_number_type_str(number_str);
                } else if ch == '.' {
                    if separator_error == false {
                    let (line, column) = (self.line, self.column);
                    self.report_error(
                        ReportKind::TokenError,
                        line, column, 1,
                        "Multiple decimal separators in number".to_string());
                    separator_error = true;
                  }
                  self.next_char();
                } else {
                    break;
                }
            },
            None => break
        }
    }

    if separator_error {
      return self.create_token(TokenType::Number, TokenSubType::ErrorToken);
    }

    match number_str.parse() {
      Ok(number) => self.create_token(TokenType::Number, TokenSubType::DoubleNumber(number)),
      Err(e) => ice!("Non-numeric characters in number token at {}:{} ({})",
            self.line, self.column, e),
    }
  }

  fn handle_number_type_str(&mut self, number_str: String) -> Token {
    let mut type_str : String = String::new();

    loop {
      let next_ch_opt = self.peek_char();
      if let Some(next_ch) = next_ch_opt {
        if next_ch.is_alphanumeric() {
          type_str.push(next_ch);
          self.next_char();
        } else {
          break;
        }
      } else {
        break;
      }
    }

    if &type_str == "d" || &type_str == "f" {
      self.create_number_token(type_str, number_str)
    } else {
        let (line, column) = (self.line, self.column - type_str.len() as i32);
        self.report_error(
            ReportKind::TokenError,
            line,
            column,
            type_str.len(), // length of a single character
          format!("Invalid type string '{}'", type_str));

          self.create_token(
            TokenType::Number,
            TokenSubType::ErrorToken)
    }
  }

  fn create_number_token(&mut self, type_str: String, number_str: String) -> Token {

    if &type_str == "d" {
      match number_str.parse() {
        Ok(number) => self.create_token(TokenType::Number, TokenSubType::DoubleNumber(number)),
        Err(e) =>
            ice!("Non-numeric characters in number token at {}:{} ({})",
              self.line, self.column, e),
      }
    } else if &type_str == "f" {
      match number_str.parse() {
        Ok(number) => self.create_token(TokenType::Number, TokenSubType::FloatNumber(number)),
        Err(e) =>
            ice!("Non-numeric characters in number token at {}:{} ({})",
                self.line, self.column, e),
      }
    } else {
      ice!("Unexpected type string '{}'", type_str);
    }
  }

  fn starts_string(ch: char) -> bool {
    ch == '"'
  }

  fn handle_string(&mut self) -> Token {

    let mut value = String::new();

    loop {
      match self.next_char() {
        Some(ch) => {
          if ch == '\\' {
            value.push(self.handle_escape_sequence());
          } else if ch == '"' {
            break;
          }
          else if ch == '\n' {
              let (line, column) = (
                self.token_start_line,
                self.token_start_column);

              self.report_error(
                  ReportKind::TokenError,
                  line,
                  column,
                  value.len(),
                  "Unterminated string".to_string());
              return self.create_token(
                TokenType::Text,
                TokenSubType::ErrorToken);
          } else {
            value.push(ch);
          }
        }
        None => {
          let (line, column) = (self.token_start_line, self.token_start_column);
          self.report_error(
              ReportKind::TokenError,
              line,
              column,
              value.len() + 1,
              "Unexpected end of file when processing string".to_string());

          return self.create_token(
                TokenType::Text,
                TokenSubType::ErrorToken);
        },
      }
    }

    let index = self.string_table.insert(value);
    self.create_token(TokenType::Text, TokenSubType::Text(index))
  }

  fn handle_escape_sequence(&mut self) -> char {
    match self.next_char() {
      Some(ch) => match ch {
        'n' => '\n',
        't' => '\t',
        '\\' => '\\',
        '"' => '"',
        _ => {
          let (line, column) = (self.line, self.column-2);
          self.report_error(
              ReportKind::TokenError,
              line,
              column,
              2,
              format!("Invalid escape sequence '\\{}'", ch));
          ' '
        },
      },
      None => {
        let (line, column) = (self.line, self.column-1);
          self.report_error(
              ReportKind::TokenError,
              line,
              column,
              1,
              "Unexpected end of file when processing escape sequence".to_string());
          ' '
      }
    }
  }

  fn create_token(&self, token_type: TokenType, token_subtype: TokenSubType) -> Token {
    Token::new(token_type, token_subtype, self.token_start_line, self.token_start_column, self.column - self.token_start_column)
  }


  fn next_char(&mut self) -> Option<char> {
    // TODO: Change bytes to chars once API stabilizes
    match self.iter.next() {
      Some(ch) => match ch {
        Ok(ch) => {
            if ch as char == '\n' {
              self.line += 1;
              self.column = 1;
            } else if ch as char == '\t' { // TODO: Handle better
              self.column += SPACES_PER_TAB;
            }
            else {
              self.column += 1;
            }

            Some(ch as char)
        },
        Err(e) => panic!("IO error while reading file: {}", e),
      },
      None => None,
    }
  }

  fn peek_char(&mut self) -> Option<char> {
      match self.iter.peek() {
          Some(res) => {
             match *(res.clone()) {
                 Ok(ch) => Some(ch as char),
                 Err(ref e) => panic!("IO error while reading file: {}", e),
             }
          },
          None => None
      }
  }

    fn skip_whitespace(&mut self) {
        loop {
          // workaround for multiple mutable borrows
          let value = self.peek_char();

          match value {
            Some(ch) => match ch {
              ' ' | '\n' | '\t' | '\r' => self.next_char(),
              _ => break,
            },
            None => break,
          };
        }
    }


  fn report_error(
      &mut self,
      error_type: ReportKind,
      line: i32,
      column: i32,
      length: usize,
      reason: String) {
      self.error_reporter.borrow_mut().report_error(
        error_type,
        Span::new(line, column, length as i32),
        reason);
  }
}

impl Lexer for ReadLexer {
  fn next_token(&mut self) -> Token {
    if self.next_token != None {
        let token = self.next_token.clone().unwrap();
        self.next_token = None;
        return token;
    }

    self.skip_whitespace();

    self.token_start_line = self.line;
    self.token_start_column = self.column;

    let token = match self.next_char() {
      Some(ch) => {
        if self.starts_comment(ch) {
          self.skip_comment();
          self.next_token()
        } else if self.starts_symbol(ch) {
          self.handle_symbols(ch)
        } else if ReadLexer::starts_identifier(ch) {
          self.handle_identifier(ch)
        } else if self.starts_number(ch) {
          self.handle_number(ch)
        } else if ReadLexer::starts_string(ch) {
          self.handle_string()
        } else {
            let (line, column) = (self.token_start_line, self.token_start_column);
            self.report_error(
                ReportKind::TokenError,
                line,
                column,
                1,
                format!(
                    "Symbol '{}' does not start a valid token", ch));
            self.next_token()
        }
      }
      None => self.create_token(TokenType::Eof, TokenSubType::NoSubType),
    };
    self.current_token = Some(token.clone());
    token
  }

  fn peek_token(&mut self) -> Token {
      if let Some(ref token) = self.next_token  {
          return token.clone();
      }

      let res = self.next_token();
      self.next_token = Some(res.clone());
      res
  }

  fn current_token(&self) -> Option<Token> {
    self.current_token.clone()
  }

}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::error_reporter::{ReportKind, Message, null_reporter::NullReporter};

    use std::io::Read;
    use std::io;
    use std::rc::Rc;
    use std::cell::RefCell;

    struct StringReader {
        text: String,
        pos: usize,
    }

    impl StringReader {
        fn new(text: &str) -> StringReader {
            StringReader {
                text: text.to_string(),
                pos: 0,
            }
        }
    }

    impl Read for StringReader {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            let vec = self.
                text.
                bytes().
                skip(self.pos).
                take(buf.len()).
                collect::<Vec<u8>>();

            self.pos += vec.len();
            let mut i = 0;
            for b in vec.iter() {
                buf[i] = *b;
                i += 1;
            }

            Ok(vec.len())
        }
    }

    macro_rules! assert_eq_token {
        ($token:expr, $t_type:expr, $t_subtype:expr) => (
          {
            let token = $token;
            assert_eq!(token.token_type, $t_type);
            assert_eq!(token.token_subtype, $t_subtype);
          }
        )
    }

    fn create_lexer(text: &str) -> (
        ReadLexer,
        Rc<RefCell<NullReporter>>) {
        let reporter = Rc::new(RefCell::new(NullReporter::new()));
        let reader = Box::new(StringReader::new(text));
        (
            ReadLexer::new(reader, reporter.clone()),
            reporter)
    }

    #[test]
    fn empty_stream_returns_eofs() {
        let (mut lexer, reporter) = create_lexer(r"");
        assert_eq_token!(lexer.next_token(),
          TokenType::Eof,
          TokenSubType::NoSubType);

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_integers_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"1234 111222 99887766");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::IntegerNumber(1234));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::IntegerNumber(111222));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::IntegerNumber(99887766));

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_floats_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"123f 456.78f .99f");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::FloatNumber(123 as f32));

        assert_eq_token!(lexer.next_token(),
            TokenType::Number,
            TokenSubType::FloatNumber(456.78 as f32));

        assert_eq_token!(lexer.next_token(),
            TokenType::Number,
            TokenSubType::FloatNumber(0.99 as f32));

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_doubles_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"123d 456.78 .99");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::DoubleNumber(123 as f64));

        assert_eq_token!(lexer.next_token(),
            TokenType::Number,
            TokenSubType::DoubleNumber(456.78 as f64));

        assert_eq_token!(lexer.next_token(),
            TokenType::Number,
            TokenSubType::DoubleNumber(0.99 as f64));

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }


    #[test]
    fn keywords_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"if else while for let fn return new class
        public protected private extern int float double bool void string");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::If,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Else,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::While,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::For,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Let,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Fn,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Return,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::New,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Class,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Public,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Protected,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Private,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Extern,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::VarType,
            TokenSubType::IntegerType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::VarType,
            TokenSubType::FloatType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::VarType,
            TokenSubType::DoubleType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::VarType,
            TokenSubType::BooleanType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::VarType,
            TokenSubType::VoidType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::VarType,
            TokenSubType::StringType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn identifiers_are_accepted() {
        let (mut lexer, reporter) = create_lexer(
            r"id ident while_ident ifff _a_ a123 a_1_2_3");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("id".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("ident".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("while_ident".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("ifff".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("_a_".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("a123".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("a_1_2_3".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn operators_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"< <= == >= > ! != = + - * / & && | || ^ ~ %");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            TokenSubType::Less);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            TokenSubType::LessOrEq);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            TokenSubType::Equals);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            TokenSubType::GreaterOrEq);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            TokenSubType::Greater);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Exclamation,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            TokenSubType::NotEquals);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Equals,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Plus,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Minus,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Star,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ForwardSlash,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Ampersand,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleAmpersand,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Pipe,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoublePipe,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Caret,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Tilde,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Percentage,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);


        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn parenthesis_and_other_symbols_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"( ) { } [ ] ; : . ,");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LParen,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RParen,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBrace,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBrace,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBracket,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBracket,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::SemiColon,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Colon,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Dot,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Comma,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn whitespace_does_not_affect_parenthesis() {
        let (mut lexer, reporter) = create_lexer(r"(){}[];:.,");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LParen,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RParen,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBrace,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBrace,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBracket,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBracket,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::SemiColon,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Colon,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Dot,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Comma,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn strings_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r#""hello world" "test""yarhar"identifier"#);

        assert_eq_token!(
            lexer.next_token(),
        TokenType::Text,
            TokenSubType::Text(Rc::new("hello world".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::Text(Rc::new("test".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::Text(Rc::new("yarhar".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("identifier".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn invalid_number_type_letter_is_reported() {
        let (mut lexer, reporter) = create_lexer(r#"12rrr 123.4x123 .122y"#);

        lexer.next_token();
        lexer.next_token();
        lexer.next_token();

        let borrowed = reporter.borrow();
        assert_eq!(borrowed.errors(), 3);

        let messages = borrowed.get_messages();

        assert_eq!(
            Message::highlight_message(
                    ReportKind::TokenError,
                    Span::new(1, 3, 3),
                    "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
            ReportKind::TokenError,
            Span::new(1, 12, 4),
            "".to_owned()),
            messages[1]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(1, 21, 1),
                "".to_owned()),
            messages[2]);
    }

    #[test]
    fn lexer_returns_error_token_number_when_number_has_invalid_type_letter() {
        let (mut lexer, reporter) = create_lexer(r#"12r 123.4x .122abcd"#);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 3);



    }

    #[test]
    fn unterminated_string_is_reported() {
        let (mut lexer, reporter) = create_lexer(r#"
        "unterminated string
        valid_token
        "another unterminated string
        123
        + - "yet another unterminated token"#);

        lexer.next_token();
        lexer.next_token();
        lexer.next_token();
        lexer.next_token();
        lexer.next_token();
        lexer.next_token();
        lexer.next_token();

        let borrowed = reporter.borrow();
        assert_eq!(borrowed.errors(), 3);

        let messages = borrowed.get_messages();

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(2, 9, 19),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(4, 9, 27),
                "".to_owned()),
            messages[1]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(6, 13, 31),
                "".to_owned()),
            messages[2]);
    }

    #[test]
    fn unterminated_string_produces_correct_error_tokens() {
        let (mut lexer, reporter) = create_lexer(r#"
        "unterminated string
        valid_token
        "another unterminated string
        123
        + - "yet another unterminated token"#);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("valid_token".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::IntegerNumber(123));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Plus,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Minus,
            TokenSubType::NoSubType);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 3);
    }

    #[test]
    fn unexpected_escape_characters_are_reported() {
        let (mut lexer, reporter) = create_lexer(r#"
        "foo\x"
        "\y\z"
        "unterminated\"#);

        lexer.next_token();
        lexer.next_token();
        lexer.next_token();

        let borrowed = reporter.borrow();
        assert_eq!(borrowed.errors(), 5);
        let messages = borrowed.get_messages();

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(2, 13, 2),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(3, 10, 2),
                "".to_owned()),
            messages[1]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(3, 12, 2),
                "".to_owned()),
            messages[2]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(4, 22, 1),
                "".to_owned()),
            messages[3]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(4, 9, 14),
                "".to_owned()),
            messages[4]);
    }

    #[test]
    fn unexpected_escape_characters_produce_correct_tokens() {
        let (mut lexer, reporter) = create_lexer(r#"
        "foo\x"
        "\y\z"
        "unterminated\"#);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::Text(Rc::new("foo ".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::Text(Rc::new("  ".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 5);
    }

    #[test]
    fn multiple_decimal_separators_are_reported() {
        let (mut lexer, reporter) = create_lexer(r"
        12.3.4
        .342.1
        23..5
        ");

        lexer.next_token();
        lexer.next_token();
        lexer.next_token();

        let borrowed = reporter.borrow();
        assert_eq!(borrowed.errors(), 3);
        let messages = borrowed.get_messages();

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(2, 13, 1),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(3, 13, 1),
                "".to_owned()),
            messages[1]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(4, 12, 1),
                "".to_owned()),
            messages[2]);
    }


    #[test]
    fn multiple_decimal_separators_generate_error_tokens() {
        let (mut lexer, reporter) = create_lexer(r"
        12.3.4
        .342.1
        23..5
        ");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Number,
            TokenSubType::ErrorToken);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 3);
    }

    #[test]
    fn unexpected_starting_symbol_are_reported() {
        let (mut lexer, reporter) = create_lexer(r"
        `hello
        $foo");
        lexer.next_token();
        lexer.next_token();

        let borrowed = reporter.borrow();
        assert_eq!(borrowed.errors(), 2);
        let messages = borrowed.get_messages();

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(2, 9, 1),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(3, 9, 1),
                "".to_owned()),
            messages[1]);
    }

    #[test]
    fn unexpected_starting_symbol_are_ignored_when_getting_tokens() {
        let (mut lexer, reporter) = create_lexer(r"
        `hello
        $foo");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("hello".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            TokenSubType::Identifier(Rc::new("foo".to_string())));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            TokenSubType::NoSubType);

        assert_eq!(reporter.borrow().errors(), 2);
    }
}
