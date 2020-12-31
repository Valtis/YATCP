pub mod token;

use token::*;

use crate::common::{
    node_info::Span,
    constants::SPACES_PER_TAB,
    error_reporter::{ErrorReporter, ReportKind},
    string_table::StringTable,
};


use std::io::BufReader;
use std::io::Read;
use std::io::Bytes;

use std::iter::Peekable;
use std::rc::Rc;
use std::cell::RefCell;
/* Unstable API - use this once stabilized */
//use std::num::IntErrorKind::*;


pub trait Lexer {
    fn next_token(&mut self) -> Token;
    fn peek_token(&mut self) -> Token;
    fn current_token(&self) -> Option<Token>;
    fn previous_token(&self) -> Option<Token>;

}

pub struct ReadLexer {
    line: i32,
    column: i32,
    token_start_line: i32,
    token_start_column: i32,
    iter: Peekable<Bytes<BufReader<Box<dyn Read>>>>, // FIXME! Change to Chars once api stabilizes. Using Bytes when multi-code point characters are present causes bugs
    next_token: Option<Token>, // used for storing token after peeking
    current_token: Option<Token>, // token that was returned by next_token.
    previous_token: Option<Token>,
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
            previous_token: None,
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
            '+' => self.multi_char_operator_helper(
                ch, vec![
                    ("+", TokenType::Plus),
                    ("+=", TokenType::PlusEquals)]),
            '-' => self.multi_char_operator_helper(
                ch, vec![
                    ("-", TokenType::Minus),
                    ("-=", TokenType::MinusEquals)]),
            '*' => self.multi_char_operator_helper(
                ch, vec![
                    ("*", TokenType::Star),
                    ("*=", TokenType::StarEquals)]),
            '/' => self.multi_char_operator_helper(
                ch, vec![
                    ("/", TokenType::ForwardSlash),
                    ("/=", TokenType::ForwardSlashEquals)]),
            '%' => self.multi_char_operator_helper(
                ch, vec![
                    ("%", TokenType::Percentage),
                    ("%=", TokenType::PercentageEquals)]),
            '[' => self.create_token(TokenType::LBracket),
            ']' => self.create_token(TokenType::RBracket),
            '{' => self.create_token(TokenType::LBrace),
            '}' => self.create_token(TokenType::RBrace),
            '(' => self.create_token(TokenType::LParen),
            ')' => self.create_token(TokenType::RParen),
            ';' => self.create_token(TokenType::SemiColon),
            ',' => self.create_token(TokenType::Comma),
            '.' => self.create_token(TokenType::Dot),
            ':' => self.create_token(TokenType::Colon),
            '^' => self.multi_char_operator_helper(
                ch, vec![
                    ("^", TokenType::Caret),
                    ("^=", TokenType::CaretEquals)]),
            '~' => self.create_token(TokenType::Tilde),
            '=' => self.multi_char_operator_helper(
                ch, vec![
                    ("=", TokenType::Equals),
                    ("==", TokenType::DoubleEquals)]),
            '>' => self.multi_char_operator_helper(
                ch, vec![
                    (">", TokenType::ArrowRight),
                    (">=", TokenType::ArrowRightEquals),
                    (">>", TokenType::DoubleArrowRight),
                    (">>=", TokenType::DoubleArrowRightEquals),
                    (">>>", TokenType::TripleArrowRight),
                    (">>>=", TokenType::TripleArrowRightEquals),
                ]),
            '<' => self.multi_char_operator_helper(
                ch, vec![
                    ("<", TokenType::ArrowLeft),
                    ("<=", TokenType::ArrowLeftEquals),
                    ("<<", TokenType::DoubleArrowLeft),
                    ("<<=", TokenType::DoubleArrowLeftEquals)
                ]),
            '!' => self.multi_char_operator_helper(
                ch, vec![
                    ("!=", TokenType::ExclamationEquals),
                    ("!", TokenType::Exclamation)]),
            '&' => self.multi_char_operator_helper(
                ch, vec![
                    ("&&", TokenType::DoubleAmpersand),
                    ("&=", TokenType::AmpersandEquals),
                    ("&", TokenType::Ampersand)]),
            '|' => self.multi_char_operator_helper(
                ch, vec![
                    ("||", TokenType::DoublePipe),
                    ("|=", TokenType::PipeEquals),
                    ("|", TokenType::Pipe)]),
            _ => ice!("Unexpected symbol '{}' passed to operator handler", ch),
        }
    }

    fn does_not_start_desimal_number(&mut self) -> bool {
        self.peek_char().map_or(false, |c| !c.is_digit(10))
    }

    fn multi_char_operator_helper(
        &mut self,
        start_char: char,
        operator_candidates: Vec<(&'static str, TokenType)>) -> Token {

        let mut candidate_string = start_char.to_string();
        let mut candidate = None;
        loop {
            let current_iteration_candidates = operator_candidates.iter().filter(|&(operator, _)| operator == &candidate_string).map(|(_, token_type)| token_type).collect::<Vec<_>>();

            candidate = if current_iteration_candidates.len() == 1 {
                if candidate.is_some() {
                   self.next_char();
                }
                Some(current_iteration_candidates[0])
            } else {
                if let Some(token_type) = candidate {
                    return self.create_token(*token_type);
                } else {
                    ice!("No match for operator starting with '{}', current candidate string '{}', candidate operators {:?}",
                        start_char,
                        candidate_string,
                        operator_candidates);
                }
            };

            candidate_string.push(if let Some(ch) = self.peek_char() {
                ch
            } else {
                if let Some(token_type) = candidate {
                    return self.create_token(*token_type);
                } else {
                    ice!("No match for operator starting with '{}', current candidate string '{}', candidate operators {:?}",
                        start_char,
                        candidate_string,
                        operator_candidates);
                }
            });
        }
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
                _ => break
            }
        }


        match self.handle_keywords(&identifier) {
            Some(token) => token,
            _ => {
                let index = self.string_table.insert(identifier);

                self.create_token_with_attribute(
                    TokenType::Identifier,
                    TokenAttribute::Text(index))
            },
        }
    }

    /*if, else, while, for, let, fn, return, new, class,
    public, protected, private, true, false, int, float, double, bool, void*/
    fn handle_keywords(&self, identifier: &str) -> Option<Token> {
        match identifier {
            "if" => Some(self.create_token(TokenType::If)),
            "else" => Some(self.create_token(TokenType::Else)),
            "while" => Some(self.create_token(TokenType::While)),
            "for" => Some(self.create_token(TokenType::For)),
            "let" => Some(self.create_token(TokenType::Let)),
            "const" => Some(self.create_token(TokenType::Const)),
            "val" => Some(self.create_token(TokenType::Val)),
            "fn" => Some(self.create_token(TokenType::Fn)),
            "return" => Some(self.create_token(TokenType::Return)),
            "new" => Some(self.create_token(TokenType::New)),
            "class" => Some(self.create_token(TokenType::Class)),
            "public" => Some(self.create_token(TokenType::Public)),
            "protected" => Some(self.create_token(TokenType::Protected)),
            "private" => Some(self.create_token(TokenType::Private)),
            "extern" => Some(self.create_token(TokenType::Extern)),
            "true" => Some(self.create_token_with_attribute(TokenType::BooleanConstant, TokenAttribute::BooleanValue(true))),
            "false" => Some(self.create_token_with_attribute(TokenType::BooleanConstant, TokenAttribute::BooleanValue(false))),
            "int" => Some(self.create_token(TokenType::Integer)),
            "float" => Some(self.create_token(TokenType::Float)),
            "double" => Some(self.create_token(TokenType::Double)),
            "byte" => Some(self.create_token(TokenType::Byte)),
            "bool" => Some(self.create_token(TokenType::Boolean)),
            "void" => Some(self.create_token(TokenType::Void)),
            "string" => Some(self.create_token(TokenType::String)),
            "as" => Some(self.create_token(TokenType::As)),
            "break" => Some(self.create_token(TokenType::Break)),
            "continue" => Some(self.create_token(TokenType::Continue)),
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
            return self.handle_decimal_number(number_str, 10);
        }

        let radix = if ch == '0' {
            let next = self.peek_char();
            if let Some(type_char) = next {
                if type_char == 'x' {
                    self.next_char();
                    number_str.clear();
                    16
                } else if type_char == 'b' {
                    self.next_char();
                    number_str.clear();
                    2
                } else {
                    10
                }
            } else {
                10
            }
        } else {
            10
        };

        loop {
            // workaround for multiple mutable borrows
            let value = match self.peek_char() {
                Some(ch) => Some (ch),
                None => None,
            };


            match value {
                Some(ch) => {
                    if ch.is_digit(radix) {
                        number_str.push(ch);
                        self.next_char();
                    } else if ch == '.' {
                        number_str.push(ch);
                        self.next_char();
                        return self.handle_decimal_number(number_str, radix);
                    } else if ch.is_alphabetic() {
                        return self.handle_number_type_str(number_str, radix);
                    } else {
                        break;
                    }
                }
                None => break
            }
        }

        match u128::from_str_radix(&number_str, radix) {
            Ok(number) => self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::IntegralConstant(number)),


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

                    return self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ErrorValue);
                }

                ice!("Non-numeric characters in number token at {}:{} ({})",
                self.line, self.column, e)
            },
        }
    }

    fn handle_decimal_number(&mut self, mut number_str: String, radix: u32) -> Token {
        let mut separator_error = false;
        loop {

            let value = self.peek_char();

            match value {
                Some(ch) => {
                    if ch.is_digit(10) {
                        number_str.push(ch);
                        self.next_char();
                    } else if ch.is_alphabetic() {
                        return self.handle_number_type_str(number_str, radix);
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
            return self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ErrorValue);
        }

        match number_str.parse() {
            Ok(number) => self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::DoubleConstant(number)),
            Err(e) => ice!("Non-numeric characters in number token at {}:{} ({})",
            self.line, self.column, e),
        }
    }

    fn handle_number_type_str(&mut self, number_str: String, radix: u32) -> Token {
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

        if &type_str == "d" || &type_str == "f" || &type_str == "i" || &type_str == "b" {
            self.create_number_token(type_str, number_str, radix)
        } else {
            let (line, column) = (self.line, self.column - type_str.len() as i32);
            self.report_error(
                ReportKind::TokenError,
                line,
                column,
                type_str.len(), // length of a single character
                format!("Invalid type string '{}'", type_str));

            self.create_token_with_attribute(
                TokenType::NumberConstant,
                TokenAttribute::ErrorValue)
        }
    }

    fn create_number_token(&mut self, type_str: String, number_str: String, radix: u32) -> Token {

        if &type_str == "d" {
            match number_str.parse() {
                Ok(number) => self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::DoubleConstant(number)),
                Err(e) =>
                    ice!("Non-numeric characters in number token at {}:{} ({})",
              self.line, self.column, e),
            }
        } else if &type_str == "f" {
            match number_str.parse() {
                Ok(number) => self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::FloatConstant(number)),
                Err(e) =>
                    ice!("Non-numeric characters in number token at {}:{} ({})",
                self.line, self.column, e),
            }
        } else if &type_str == "i" {
                match u128::from_str_radix(&number_str, radix) {
                Ok(number) => self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::IntegerConstant(number)),
                Err(e) => {
                    if e.to_string().contains("too large to fit") {
                        self.report_error(
                            ReportKind::TokenError,
                            self.line,
                            self.token_start_column,
                            (self.column - self.token_start_column) as usize,
                            format!("Constant too large to be represented"),
                        );

                        return self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ErrorValue);
                    }

                    ice!("Non-numeric characters in number token at {}:{} ({})", self.line, self.column, e)
                }
            }
        } else if &type_str == "b" {
            match u128::from_str_radix(&number_str, radix) {
                Ok(number) => self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ByteConstant(number)),
                Err(e) => {
                    if e.to_string().contains("too large to fit") {
                        self.report_error(
                            ReportKind::TokenError,
                            self.line,
                            self.token_start_column,
                            (self.column - self.token_start_column) as usize,
                            format!("Constant too large to be represented"),
                        );

                        return self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ErrorValue);
                    }

                    ice!("Non-numeric characters in number token at {}:{} ({})", self.line, self.column, e)
                }
            }
        } else {
            ice!("Unexpected type string '{}'", type_str);
        }
    }


    fn starts_character(ch: char) -> bool {
        ch == '\''
    }

    fn handle_character(&mut self) -> Token {
        let value = self.handle_stringlike_sequence('\'');

        if let Some(bytes) = value {
            let text = String::from_utf8_lossy(&bytes).to_string();
            let graphemes = text.chars().count();
            if text.len() != 1  {
                self.report_error(
                    ReportKind::TokenError,
                    self.token_start_line,
                    self.token_start_column,
                    graphemes + 2, // +2 for the open/close '-symbols
                    "Invalid character length; expected exactly one character".to_owned()
                );

                if graphemes == 1  {
                    self.report_error(
                        ReportKind::Note,
                        self.token_start_line,
                        self.token_start_column,
                        graphemes + 2, // +2 for the open/close '-symbols
                        format!("This grapheme has length of {} bytes and can't be represented as a character. Use string instead", text.len())
                    );
                }

                self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ErrorValue)
            } else {
                self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ByteConstant(text.as_bytes()[0] as u128))
            }
        } else {
            self.create_token_with_attribute(TokenType::NumberConstant, TokenAttribute::ErrorValue)
        }
    }

    fn starts_string(ch: char) -> bool {
        ch == '"'
    }

    fn handle_string(&mut self) -> Token {
        let value = self.handle_stringlike_sequence('"');

        if let Some(bytes) = value {
            let text = String::from_utf8_lossy(&bytes).to_string();
            let index = self.string_table.insert(text);
            self.create_token_with_attribute(TokenType::Text, TokenAttribute::Text(index))
        } else {
            self.create_token_with_attribute(
                TokenType::Text,
                TokenAttribute::ErrorValue)
        }
    }


    // return Vec of bytes, as otherwise utf8 sequence parsing is wonky
    fn handle_stringlike_sequence(&mut self, delimiter: char) -> Option<Vec<u8>> {

        let mut value = vec![];

        loop {
            match self.next_char() {
                Some(ch) => {

                    if ch == delimiter {
                        break;
                    }

                    if ch == '\\' {
                        value.push(self.handle_escape_sequence() as u8);
                    } else if ch == '\n' {
                        let (line, column) = (
                            self.token_start_line,
                            self.token_start_column);

                        self.report_error(
                            ReportKind::TokenError,
                            line,
                            column,
                            value.len(),
                            "Unterminated character sequence".to_string());
                        return None;

                    } else {
                        value.push(ch as u8);
                    }
                }
                None => {
                    let (line, column) = (self.token_start_line, self.token_start_column);
                    self.report_error(
                        ReportKind::TokenError,
                        line,
                        column,
                        value.len() + 1,
                        "Unexpected end of file when lexing character sequence".to_string());
                    return None;

                },
            }
        }

        Some(value)
    }

    fn handle_escape_sequence(&mut self) -> char {
        match self.next_char() {
            Some(ch) => match ch {
                'n' => '\n',
                'e' => 27 as char,
                '\'' => '\'',
                '0' => 0 as char,
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

    fn create_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, None, self.token_start_line, self.token_start_column, self.column - self.token_start_column)
    }

    fn create_token_with_attribute(&self, token_type: TokenType, attribute: TokenAttribute) -> Token {
        Token::new(token_type, Some(attribute), self.token_start_line, self.token_start_column, self.column - self.token_start_column)
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

        self.previous_token = self.current_token.clone();

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
                } else if ReadLexer::starts_character(ch) {
                    self.handle_character()
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
            None => self.create_token(TokenType::Eof),
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

    fn previous_token(&self) -> Option<Token> {
        self.previous_token.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::common::error_reporter::{ReportKind, Message, null_reporter::NullReporter};

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
        ($token:expr, $t_type:expr, $t_attribute:expr) => (
          {
            let token = $token;
            assert_eq!(token.token_type, $t_type);
            assert_eq!(token.attribute, $t_attribute);
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
            None);

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_integers_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"1234 111222 99887766");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(1234)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(111222)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(99887766)));

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_floats_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"123f 456.78f .99f");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::FloatConstant(123 as f32)));

        assert_eq_token!(lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::FloatConstant(456.78 as f32)));

        assert_eq_token!(lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::FloatConstant(0.99 as f32)));

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn valid_doubles_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"123d 456.78 .99");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::DoubleConstant(123 as f64)));

        assert_eq_token!(lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::DoubleConstant(456.78 as f64)));

        assert_eq_token!(lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::DoubleConstant(0.99 as f64)));

        assert_eq_token!(lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }


    #[test]
    fn keywords_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"if else while for let fn return new class
        public protected private extern int float double bool void string as break continue");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::If,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Else,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::While,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::For,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Let,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Fn,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Return,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::New,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Class,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Public,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Protected,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Private,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Extern,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Integer,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Float,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Double,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Boolean,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Void,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::String,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::As,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Break,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Continue,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn identifiers_are_accepted() {
        let (mut lexer, reporter) = create_lexer(
            r"id ident while_ident ifff _a_ a123 a_1_2_3");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("id".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("ident".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("while_ident".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("ifff".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("_a_".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("a123".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("a_1_2_3".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn operators_are_accepted() {
        let (mut lexer, reporter) = create_lexer(
            r"< <= == >= > ! != = + - * / & && | || ^ ~ % << >> >>> += -= *= /= %= <<= >>= >>>= &= |= ^=");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ArrowLeft,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ArrowLeftEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ArrowRightEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ArrowRight,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Exclamation,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ExclamationEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Equals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Plus,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Minus,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Star,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ForwardSlash,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Ampersand,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleAmpersand,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Pipe,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoublePipe,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Caret,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Tilde,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Percentage,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleArrowLeft,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleArrowRight,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::TripleArrowRight,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::PlusEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::MinusEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::StarEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::ForwardSlashEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::PercentageEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleArrowLeftEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::DoubleArrowRightEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::TripleArrowRightEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::AmpersandEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::PipeEquals,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::CaretEquals,
            None);


        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);


        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn parenthesis_and_other_symbols_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"( ) { } [ ] ; : . ,");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LParen,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RParen,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBrace,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBrace,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBracket,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBracket,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::SemiColon,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Colon,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Dot,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Comma,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn whitespace_does_not_affect_parenthesis() {
        let (mut lexer, reporter) = create_lexer(r"(){}[];:.,");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LParen,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RParen,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBrace,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBrace,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::LBracket,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::RBracket,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::SemiColon,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Colon,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Dot,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Comma,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn strings_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r#""hello world" "test""yarhar"identifier"#);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::Text(Rc::new("hello world".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::Text(Rc::new("test".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::Text(Rc::new("yarhar".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("identifier".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

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
            TokenType::NumberConstant,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

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
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("valid_token".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(123)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Plus,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Minus,
            None);

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

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
            Some(TokenAttribute::Text(Rc::new("foo ".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::Text(Rc::new("  ".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Text,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

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
            TokenType::NumberConstant,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ErrorValue));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

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
            Some(TokenAttribute::Text(Rc::new("hello".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Identifier,
            Some(TokenAttribute::Text(Rc::new("foo".to_string()))));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 2);
    }

    #[test]
    fn hexadecimals_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"
            0xff 0xFF 0x80 0x18C8A1A
        ");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(255)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(255)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(128)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(25987610)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn binary_numbers_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"
            0b10 0b11111111 0b101010101 0b00011011100101111011
        ");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(2)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(255)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(341)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegralConstant(113019)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn numbers_with_integer_type_letter_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"
            0i 15i
        ");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegerConstant(0)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::IntegerConstant(15)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }


    #[test]
    fn character_literals_are_accepted() {
        let (mut lexer, reporter) = create_lexer(r"
          'a' '0' '\n' '\0' '\''
        ");

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ByteConstant(97)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ByteConstant(48)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ByteConstant(10)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ByteConstant(0)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::NumberConstant,
            Some(TokenAttribute::ByteConstant(39)));

        assert_eq_token!(
            lexer.next_token(),
            TokenType::Eof,
            None);

        assert_eq!(reporter.borrow().errors(), 0);
    }

    #[test]
    fn bad_character_literals_are_reported() {
      let (mut lexer, reporter) = create_lexer(r"
        'hello'
        '🔥'
        ");
        lexer.next_token();
        lexer.next_token();

        let borrowed = reporter.borrow();
        assert_eq!(borrowed.errors(), 2);
        let messages = borrowed.get_messages();

        assert_eq!(messages.len(), 3);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(2, 9, 7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TokenError,
                Span::new(3, 9, 3),
                "".to_owned()),
            messages[1]);

        assert_eq!(
            Message::highlight_message(
                ReportKind::Note,
                Span::new(3, 9, 3),
                "".to_owned()),
            messages[2]);
    }
}
