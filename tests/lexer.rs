extern crate compiler;
mod test_reporter;

use compiler::lexer::ReadLexer;
use compiler::lexer::Lexer;
use compiler::token::*;
use compiler::error_reporter::ErrorReporter;
use compiler::error_reporter::Error;

use self::test_reporter::TestReporter;
use self::test_reporter::ReportedError;

use std::io::Read;
use std::io;

use std::rc::Rc;
use std::cell::RefCell;


#[cfg(test)]
struct StringReader {
    text: String,
    pos: usize,
}

#[cfg(test)]
impl StringReader {
    fn new(text: &str) -> StringReader {
      StringReader {
        text: text.to_string(),
        pos: 0,
      }
    }
}

#[cfg(test)]
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

macro_rules! assert_eq_error {
    ($error: expr, $e_type:expr, $line:expr, $column:expr, $length:expr) => (
        {
            assert_eq!($error.error_type, $e_type);
            assert_eq!($error.line, $line);
            assert_eq!($error.column, $column);
            assert_eq!($error.token_length, $length);
        }
    )

}

#[cfg(test)]
fn create_lexer(text: &str) -> (ReadLexer, Rc<RefCell<TestReporter>>) {
    let error_handler = Rc::new(RefCell::new(TestReporter::new()));

    let reader = Box::new(StringReader::new(text));
    (ReadLexer::new(reader, error_handler.clone()), error_handler)
}

#[test]
fn empty_stream_returns_eofs() {
    let (mut lexer, _) = create_lexer(r"");
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
}

#[test]
fn valid_integers_are_accepted() {

    let (mut lexer, _) = create_lexer(r"1234 111222 99887766");

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Number, 
      TokenSubType::IntegerNumber(1234));

    assert_eq_token!(lexer.next_token(), 
      TokenType::Number, 
      TokenSubType::IntegerNumber(111222));

    assert_eq_token!(lexer.next_token(), 
      TokenType::Number, 
      TokenSubType::IntegerNumber(99887766));

    assert_eq_token!(lexer.next_token(), 
      TokenType::Eof, 
      TokenSubType::NoSubType);
}

#[test]
fn valid_floats_are_accepted() {
    let (mut lexer, _) = create_lexer(r"123f 456.78f .99f");

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
}

#[test]
fn valid_doubles_are_accepted() {
    let (mut lexer, _) = create_lexer(r"123d 456.78 .99");

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
}


#[test]
fn keywords_are_accepted() {
    let (mut lexer, _) = create_lexer(r"if else while for let fn return new class 
        public protected private int float double bool void string");
    
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
}

#[test]
fn identifiers_are_accepted() {
    let (mut lexer, _) = create_lexer(r"id ident while_ident ifff _a_ a123 a_1_2_3");
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("id".to_string()));

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("ident".to_string()));
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("while_ident".to_string()));
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("ifff".to_string()));
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("_a_".to_string()));
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("a123".to_string()));
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("a_1_2_3".to_string()));
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Eof, 
      TokenSubType::NoSubType);
}

#[test]
fn operators_are_accepted() {
    let (mut lexer, _) = create_lexer(r"< <= == >= > ! != = + - * /");

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Less, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::LessOrEq, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Equals, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::GreaterOrEq, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Greater, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Not, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::NotEq, 
      TokenSubType::NoSubType);
   
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Assign, 
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
      TokenType::Multiply, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Divide, 
      TokenSubType::NoSubType);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Eof, 
      TokenSubType::NoSubType);
}

#[test]
fn parenthesis_and_other_symbols_are_accepted() {
    let (mut lexer, _) = create_lexer(r"( ) { } [ ] ; : . ,");

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
}

#[test]
fn whitespace_does_not_affect_parenthesis() {
    let (mut lexer, _) = create_lexer(r"(){}[];:.,");

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
}

// TODO - Test cases for bad tokens. Currently thinking of refactoring
// the error handling so these test cases aren't implemented yet

#[test]
fn strings_are_accepted() {
    let (mut lexer, _) = create_lexer(r#""hello world" "test""yarhar"identifier"#);

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Text, 
      TokenSubType::Text("hello world".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Text, 
      TokenSubType::Text("test".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Text, 
      TokenSubType::Text("yarhar".to_string())); 
    
    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("identifier".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Eof, 
      TokenSubType::NoSubType); 
}

#[test] 
fn invalid_number_type_letter_is_reported() {
    let (mut lexer, handler) = create_lexer(r#"12rrr 123.4x123 .122y"#); 

    lexer.next_token();
    lexer.next_token();
    lexer.next_token();
    
    assert_eq!(handler.borrow().error_count(), 3); 

    assert_eq_error!(handler.borrow().errors()[0], 
        Error::TokenError,
        1,
        3,
        3);

    assert_eq_error!(handler.borrow().errors()[1], 
        Error::TokenError,
        1,
        12,
        4);

    assert_eq_error!(handler.borrow().errors()[2], 
        Error::TokenError,
        1,
        21,
        1); 
}

#[test] 
fn lexer_returns_error_token_number_when_number_has_invalid_type_letter() {
    let (mut lexer, handler) = create_lexer(r#"12r 123.4x .122abcd"#);
   
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

    assert_eq!(handler.borrow().error_count(), 3); 
}

#[test] 
fn unterminated_string_is_reported() {
    let (mut lexer, handler) = create_lexer(r#"
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
    
    assert_eq!(handler.borrow().error_count(), 3); 

    assert_eq_error!(handler.borrow().errors()[0], 
        Error::TokenError,
        2,
        9,
        19);

    assert_eq_error!(handler.borrow().errors()[1], 
        Error::TokenError,
        4,
        9,
        27);

    assert_eq_error!(handler.borrow().errors()[2], 
        Error::TokenError,
        6,
        13,
        31);
}

#[test] 
fn unterminated_string_produces_correct_error_tokens() {
    let (mut lexer, handler) = create_lexer(r#"
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
      TokenSubType::Identifier("valid_token".to_string())); 

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

    assert_eq!(handler.borrow().error_count(), 3); 
}

#[test]
fn unexpected_escape_characters_are_reported() {
    let (mut lexer, handler) = create_lexer(r#"
        "foo\x"
        "\y\z"
        "unterminated\"#); 

    lexer.next_token();
    lexer.next_token();
    lexer.next_token();

    assert_eq!(handler.borrow().error_count(), 5); 


    assert_eq_error!(handler.borrow().errors()[0], 
        Error::TokenError,
        2,
        13,
        2);

    assert_eq_error!(handler.borrow().errors()[1], 
        Error::TokenError,
        3,
        10,
        2);

    assert_eq_error!(handler.borrow().errors()[2], 
        Error::TokenError,
        3,
        12,
        2);

    assert_eq_error!(handler.borrow().errors()[3], 
        Error::TokenError,
        4,
        22,
        1);

    assert_eq_error!(handler.borrow().errors()[4], 
        Error::TokenError,
        4,
        9,
        14);
}

#[test]
fn unexpected_escape_characters_produce_correct_tokens() {
       let (mut lexer, handler) = create_lexer(r#"
        "foo\x"
        "\y\z"
        "unterminated\"#); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Text, 
      TokenSubType::Text("foo ".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Text, 
      TokenSubType::Text("  ".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Text, 
      TokenSubType::ErrorToken); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Eof, 
      TokenSubType::NoSubType); 

    assert_eq!(handler.borrow().error_count(), 5); 
}


#[test]
fn multiple_decimal_separators_are_reported() {
    let (mut lexer, handler) = create_lexer(r"
        12.3.4
        .342.1
        23..5
        ");

    lexer.next_token();
    lexer.next_token();
    lexer.next_token();

    assert_eq!(handler.borrow().error_count(), 3); 


    assert_eq_error!(handler.borrow().errors()[0], 
        Error::TokenError,
        2,
        13,
        1);

    assert_eq_error!(handler.borrow().errors()[1], 
        Error::TokenError,
        3,
        13,
        1);

    assert_eq_error!(handler.borrow().errors()[2], 
        Error::TokenError,
        4,
        12,
        1);
}


#[test]
fn multiple_decimal_separators_generate_error_tokens() {
    let (mut lexer, handler) = create_lexer(r"
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

    assert_eq!(handler.borrow().error_count(), 3); 
}

#[test]
fn unexpected_starting_symbol_are_reported() {
    let (mut lexer, handler) = create_lexer(r"
        `hello
        |foo");
    lexer.next_token();
    lexer.next_token();

    assert_eq!(handler.borrow().error_count(), 2); 


    assert_eq_error!(handler.borrow().errors()[0], 
        Error::TokenError,
        2,
        9,
        1);

    assert_eq_error!(handler.borrow().errors()[1], 
        Error::TokenError,
        3,
        9,
        1);

}

#[test]
fn unexpected_starting_symbol_are_ignored() {
    let (mut lexer, handler) = create_lexer(r"
        `hello
        |foo");

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("hello".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Identifier, 
      TokenSubType::Identifier("foo".to_string())); 

    assert_eq_token!(
      lexer.next_token(), 
      TokenType::Eof, 
      TokenSubType::NoSubType); 


    assert_eq!(handler.borrow().error_count(), 2); 
}