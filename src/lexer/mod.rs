use token::Token;
use token::TokenType;
use token::TokenSubType;

use error_reporter::ErrorReporter;
use error_reporter::Error;

use string_table::StringTable;

use std::io::BufReader;
use std::io::Read;
use std::io::Bytes;

use std::iter::Peekable;

use std::rc::Rc;
use std::cell::RefCell;

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
    iter: Peekable<Bytes<BufReader<Box<Read>>>>, // FIXME! Change to Chars once api stabilizes. Using Bytes when multi-code point characters are present causes bugs
    next_token: Option<Token>, // used for storing token after peeking
    current_token: Option<Token>, // token that was returned by next_token.
    error_reporter: Rc<RefCell<ErrorReporter>>,
    string_table: StringTable,
}

impl ReadLexer {
    pub fn new(
        input: Box<Read>,
        error_reporter: Rc<RefCell<ErrorReporter>>) -> ReadLexer {

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
      '+' | '-' | '*' | '/' | '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>' | '=' | ';' | ',' | ':' | '!' => true,
      '.' => self.does_not_start_desimal_number(),
      _ => false,
    }
  }

  fn handle_symbols(&mut self, ch: char) -> Token {
    match ch {
      '+' => self.create_token(TokenType::Plus, TokenSubType::NoSubType),
      '-' => self.create_token(TokenType::Minus, TokenSubType::NoSubType),
      '*' => self.create_token(TokenType::Multiply, TokenSubType::NoSubType),
      '/' => self.create_token(TokenType::Divide, TokenSubType::NoSubType),
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
      '=' => self.multi_char_operator_helper(
        '=',
        (TokenType::Comparison, TokenSubType::Equals),
        (TokenType::Assign, TokenSubType::NoSubType)),
      '>' => self.multi_char_operator_helper(
        '=',
        (TokenType::Comparison, TokenSubType::GreaterOrEq),
        (TokenType::Comparison, TokenSubType::Greater)),
      '<' => self.multi_char_operator_helper(
        '=',
        (TokenType::Comparison, TokenSubType::LessOrEq),
        (TokenType::Comparison, TokenSubType::Less)),
      '!' => self.multi_char_operator_helper(
        '=',
        (TokenType::Comparison, TokenSubType::NotEq),
        (TokenType::Not, TokenSubType::NoSubType)),
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
      Err(e) => ice!("Non-numeric characters in number token at {}:{} ({})",
            self.line, self.column, e),
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
                    Error::TokenError,
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

    if type_str == "d" || type_str == "f" {
      self.create_number_token(type_str, number_str)
    } else {
        let (line, column) = (self.line, self.column - type_str.len() as i32);
        self.report_error(
          Error::TokenError,
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

    if type_str == "d" {
      match number_str.parse() {
        Ok(number) => self.create_token(TokenType::Number, TokenSubType::DoubleNumber(number)),
        Err(e) =>
            ice!("Non-numeric characters in number token at {}:{} ({})",
              self.line, self.column, e),
      }
    } else if type_str == "f" {
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
                Error::TokenError,
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
            Error::TokenError,
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
            Error::TokenError,
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
            Error::TokenError,
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
              self.column += 4;
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
    error_type: Error,
    line: i32,
    column: i32,
    length: usize,
    reason: String) {
      self.error_reporter.borrow_mut().report_error(
        error_type,
        line,
        column,
        length as i32,
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
                Error::TokenError,
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
