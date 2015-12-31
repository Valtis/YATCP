use token::SyntaxToken;
use token::TokenType;
use token::TokenSubType;
use std::fs::File;
use std::io::BufReader;
use std::io::Read;
use std::io::Bytes;
use std::iter::Peekable;

pub struct Lexer {
  line: i32,
  column: i32,
  token_start_line: i32,
  token_start_column: i32,
  iter: Peekable<Bytes<BufReader<File>>>, // TODO! Change to Chars once api stabilizes
  next_token: Option<SyntaxToken>, // used for storing token after peeking
}

impl Lexer {

  pub fn new(file_path: &str) -> Lexer {
    let f = match File::open(file_path) {
        Ok(file) => file,
        Err(e) => panic!("Failed to open file {}: {}", file_path, e),
    };

    Lexer {
      line: 1,
      column: 1,
      token_start_line: 1,
      token_start_column: 1,
      iter: BufReader::new(f).bytes().peekable(),
      next_token: None,
    }
  }

  pub fn next_token(&mut self) -> Result<SyntaxToken, String> {
    if self.next_token != None {
        let token = self.next_token.clone().unwrap();
        self.next_token = None;
        return Ok(token);
    }

    self.skip_whitespace();

    self.token_start_line = self.line;
    self.token_start_column = self.column;

    match self.next_char() {
      Some(ch) => {
        if self.starts_comment(ch) {
          self.skip_comment();
          self.next_token()
        } else if Lexer::starts_symbol(ch) {
          self.handle_symbols(ch)
        } else if Lexer::starts_identifier(ch) {
          self.handle_identifier(ch)
        } else if self.starts_number(ch) {
          self.handle_number(ch)
        } else if Lexer::starts_string(ch) {
          self.handle_string()
        } else {
          Err(format!("Unexpected symbol '{}' at {}:{}", ch, self.token_start_line, self.token_start_column))
        }
      }
      None => Ok(self.create_token(TokenType::Eof, TokenSubType::NoSubType)),
    }
  }

  pub fn peek_token(&mut self) -> Result<SyntaxToken, String> {
      if self.next_token != None {
          return Ok(self.next_token.clone().unwrap());
      }

      let res = try!(self.next_token());
      self.next_token = Some(res.clone());
      Ok(res)
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

  fn starts_symbol(ch: char) -> bool {
    match ch {
      '+' | '-' | '*' | '/' | '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>' | '=' | ';' | ',' | ':' | '!' => true,
      _ => false,
    }
  }

  fn handle_symbols(&mut self, ch: char) -> Result<SyntaxToken, String> {

    match ch {
      '+' => Ok(self.create_token(TokenType::Plus, TokenSubType::NoSubType)),
      '-' => Ok(self.create_token(TokenType::Minus, TokenSubType::NoSubType)),
      '*' => Ok(self.create_token(TokenType::Multiply, TokenSubType::NoSubType)),
      '/' => Ok(self.create_token(TokenType::Divide, TokenSubType::NoSubType)),
      '[' => Ok(self.create_token(TokenType::LBracket, TokenSubType::NoSubType)),
      ']' => Ok(self.create_token(TokenType::RBracket, TokenSubType::NoSubType)),
      '{' => Ok(self.create_token(TokenType::LBrace, TokenSubType::NoSubType)),
      '}' => Ok(self.create_token(TokenType::RBrace, TokenSubType::NoSubType)),
      '(' => Ok(self.create_token(TokenType::LParen, TokenSubType::NoSubType)),
      ')' => Ok(self.create_token(TokenType::RParen, TokenSubType::NoSubType)),
      ';' => Ok(self.create_token(TokenType::SemiColon, TokenSubType::NoSubType)),
      ',' => Ok(self.create_token(TokenType::Comma, TokenSubType::NoSubType)),
      ':' => Ok(self.create_token(TokenType::Colon, TokenSubType::NoSubType)),
      '=' => self.multi_char_operator_helper('=', TokenType::Equals, TokenType::Assign),
      '>' => self.multi_char_operator_helper('=', TokenType::GreaterOrEq, TokenType::Greater),
      '<' => self.multi_char_operator_helper('=', TokenType::LessOrEq, TokenType::Less),
      '!' => self.multi_char_operator_helper('=', TokenType::NotEq, TokenType::Not),
      _ => Err(format!("Not an operator: {}", ch))
    }
  }

  fn multi_char_operator_helper (
    &mut self,
    optional_second_char: char,
    type_if_matches: TokenType,
    type_if_no_match: TokenType) -> Result<SyntaxToken, String> {

      let mut next_char = ' ';

      match self.peek_char() {
        Some(ch) => next_char = ch,
        None => { /* do nothing */}
      };

      if next_char == optional_second_char {
        // consume the next character
        self.next_char();
        Ok(self.create_token(type_if_matches, TokenSubType::NoSubType))
      } else {
        Ok(self.create_token(type_if_no_match, TokenSubType::NoSubType))
      }
    }


  fn starts_identifier(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
  }

  fn is_valid_identifier_character(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
  }

  fn handle_identifier(&mut self, ch: char) -> Result<SyntaxToken, String> {

    let mut identifier = ch.to_string();

    loop {
      // workaround for multiple mutable borrows
      let value = self.peek_char();

      match value {
        Some(ch) => {
          if Lexer::is_valid_identifier_character(ch) {
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
      Some(token) => Ok(token),
      None => {
        Ok(self.create_token(TokenType::Identifier, TokenSubType::Identifier(identifier)))
      }
    }
  }
  /*if, else, while, for, let, fn, return, new, class,
  public, protected, private, true, false, int, float, double, bool, void*/
  fn handle_keywords(&self, identifier: &str) -> Option<SyntaxToken> {
    match identifier {
      "if" => Some(self.create_token(TokenType::If, TokenSubType::NoSubType)),
      "elif" => Some(self.create_token(TokenType::ElseIf, TokenSubType::NoSubType)),
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

  fn handle_number(&mut self, ch: char) -> Result<SyntaxToken, String> {

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
            self.next_char();
            return self.handle_number_type_char(ch, number_str);
          } else {
            break;
          }
        }
        None => break
      }
    }

    match number_str.parse() {
      Ok(number) => Ok(self.create_token(TokenType::Number, TokenSubType::IntegerNumber(number))),
      Err(e) => Err(
          format!("Internal compiler error: non-numeric characters in number token at {}:{} ({})",
            self.line, self.column, e)),
    }
  }

  fn handle_decimal_number(&mut self, mut number_str: String) -> Result<SyntaxToken, String> {
    loop {

      let value = self.peek_char();

      match value {
        Some(ch) => {
          if ch.is_digit(10) {
            number_str.push(ch);
            self.next_char();
          } else if ch.is_alphabetic() {
            self.next_char();
            return self.handle_number_type_char(ch, number_str);
          } else if ch == '.' {
            return Err("Multiple decimal separators in number".to_string());
          } else {
            break;
          }
        }
        None => break
      }
    }

    match number_str.parse() {
      Ok(number) => Ok(self.create_token(TokenType::Number, TokenSubType::DoubleNumber(number))),
      Err(e) => Err(
          format!("Internal compiler error: non-numeric characters in number token at {}:{} ({})",
            self.line, self.column, e)),
    }
  }

  fn handle_number_type_char(&mut self, type_char: char, number_str: String) -> Result<SyntaxToken, String> {

    match type_char {
      'd'|'f' => {
        self.create_number_token(type_char, number_str)
      }
      _ => Err(format!("Invalid type character: {}", type_char)),
    }
  }

  fn create_number_token(&mut self, type_char: char, number_str: String) -> Result<SyntaxToken, String> {
    // check that character following the type char is not alphanumeric
    match self.peek_char() {
      Some(ch) => {
        if ch.is_alphanumeric() {
          return Err(format!("Invalid character following number type character: {}", ch));
        }
      }
      None => { /* do nothing */}
    }

    if type_char == 'd' {
      match number_str.parse() {
        Ok(number) => Ok(self.create_token(TokenType::Number, TokenSubType::DoubleNumber(number))),
        Err(e) => Err(
            format!("Internal compiler error: non-numeric characters in number token at {}:{} ({})",
              self.line, self.column, e)),
      }
    } else {
      match number_str.parse() {
        Ok(number) => Ok(self.create_token(TokenType::Number, TokenSubType::FloatNumber(number))),
        Err(e) => Err(
            format!("Internal compiler error: non-numeric characters in number token at {}:{} ({})",
                self.line, self.column, e)),
      }
    }
  }


  fn starts_string(ch: char) -> bool {
    ch == '"'
  }

  fn handle_string(&mut self) -> Result<SyntaxToken, String> {

    let mut value = String::new();

    loop {
      match self.next_char() {
        Some(ch) => {
          if ch == '\\' {
            value.push(try!(self.handle_escape_sequence()));
          } else if ch == '"' {
            // check that there are no alphanumeric characters following the '"'
            match self.peek_char() {
              Some(ch) => {
                if ch.is_alphanumeric() {
                  return Err(format!("Invalid character following closing\" in string: {}", ch));
                }
              },
              None => { /* do nothing*/}
            }
            break;
          } else {
            value.push(ch);
          }
        }
        None => return Err(format!("Unterminated string at {}:{}",
            self.token_start_line, self.token_start_column)),
      }
    }

    Ok(self.create_token(TokenType::Text, TokenSubType::Text(value)))
  }

  fn handle_escape_sequence(&mut self) -> Result<char, String> {
    match self.next_char() {
      Some(ch) => match ch {
        'n' => Ok('\n'),
        't' => Ok('\t'),
        '\\' => Ok('\\'),
        '"' => Ok('"'),
        _ => Err(format!("Invalid escape sequence \\{}", ch))
      },
      None => Err("Invalid escape sequence - no character following \\".to_string()),
    }
  }

  fn create_token(&self, token_type: TokenType, token_subtype: TokenSubType) -> SyntaxToken {
    SyntaxToken::new(token_type, token_subtype, self.token_start_line, self.token_start_column)
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
}
