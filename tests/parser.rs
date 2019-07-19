extern crate compiler;
#[macro_use]
mod test_reporter;

use compiler::ast::AstNode;
use compiler::ast::ArithmeticInfo;
use compiler::ast::FunctionInfo;
use compiler::ast::DeclarationInfo;
use compiler::ast::NodeInfo;

use compiler::error_reporter::ReportKind;
use compiler::lexer::Lexer;
use compiler::parser::Parser;
use compiler::semcheck::Type;
use compiler::token::Token;
use compiler::token::TokenType;
use compiler::token::TokenSubType;

use compiler::string_table::StringTable;

use self::test_reporter::TestReporter;

use std::rc::Rc;
use std::cell::RefCell;

struct TestLexer {
    tokens: Vec<Token>,
    pos: usize,
}

impl Lexer for TestLexer {
    fn next_token(&mut self) -> Token {
        let token = self.peek_token();
        self.pos += 1;
        token
    }

    fn peek_token(&mut self) -> Token {
        if self.pos < self.tokens.len() {
            self.tokens[self.pos].clone()
        } else {
            Token::new(TokenType::Eof, TokenSubType::NoSubType, 0, 0, 0)
        }
    }

    fn current_token(&self) -> Option<Token> {
        if self.pos == 0 {
            None
        } else if self.pos < self.tokens.len() {
            Some(self.tokens[self.pos - 1].clone())
        } else {
            Some(Token::new(TokenType::Eof, TokenSubType::NoSubType, 0, 0, 0))
        }
    }
}

impl TestLexer {
    fn new(tokens: Vec<Token>) -> TestLexer {
        TestLexer {
            tokens: tokens,
            pos: 0,
        }
    }
}

fn create_parser(
    tokens: Vec<Token>) -> (Parser, Rc<RefCell<TestReporter>>) {
    let reporter = Rc::new(RefCell::new(TestReporter::new()));
    (Parser::new(
        Box::new(TestLexer::new(tokens)),
        reporter.clone()),
     reporter)
}

#[test]
fn empty_function_produces_correct_ast() {

    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);


    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn single_variable_declaration_with_integer_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            4,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn single_variable_declaration_with_float_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::FloatType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::FloatNumber(4.2), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Float(
                                            4.2,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Float,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn single_variable_declaration_with_double_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::DoubleType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::DoubleNumber(4.2), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Double(
                                            4.2,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Double,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn single_variable_declaration_with_string_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::StringType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Text, TokenSubType::Text(Rc::new("hello, world".to_string())), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Text(
                                            Rc::new("hello, world".to_string()),
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::String,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn single_variable_declaration_with_boolean_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::BooleanType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Boolean, TokenSubType::BooleanValue(true), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Boolean(
                                            true,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Boolean,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn assignment_with_negative_number_produces_correct_ast() {
        let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(85), 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Minus(
                                    Box::new(AstNode::Negate(
                                        Box::new(AstNode::Integer(
                                            85,
                                            NodeInfo::new(0, 0, 0))),
                                        ArithmeticInfo::new_alt(0, 0, 0)
                                    )),
                                    Box::new(AstNode::Multiply(
                                        Box::new(AstNode::Integer(
                                            4,
                                            NodeInfo::new(0, 0, 0)
                                        )),
                                        Box::new(AstNode::Integer(
                                            8,
                                            NodeInfo::new(0, 0, 0)
                                        )),
                                        ArithmeticInfo::new_alt(0, 0, 0)
                                    )),

                                    ArithmeticInfo::new_alt(0, 0, 0)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    0, 0, 0),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn variable_in_expression_produces_correct_ast() {
           let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("ident".to_string())),
                0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Identifier(
                                    Rc::new("ident".to_string()),
                                    NodeInfo::new(0, 0, 0))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    0, 0, 0),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn function_with_single_variable_declaration_with_addition_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Plus(
                                            Box::new(
                                                AstNode::Integer(
                                                    4,
                                                    NodeInfo::new(0, 0, 0)
                                                )
                                            ),
                                            Box::new(
                                                AstNode::Integer(
                                                    8,
                                                    NodeInfo::new(0, 0, 0)
                                                ),
                                            ),
                                            ArithmeticInfo::new_alt(0, 0, 0)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn function_with_single_variable_declaration_with_subtraction_and_addition_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(85), 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Plus(
                                    Box::new(AstNode::Minus(
                                        Box::new(AstNode::Integer(
                                            85,
                                            NodeInfo::new(0, 0, 0)
                                        )),
                                        Box::new(AstNode::Integer(
                                            4,
                                            NodeInfo::new(0, 0, 0)
                                        )),
                                        ArithmeticInfo::new_alt(0, 0, 0)
                                    )),
                                    Box::new(AstNode::Integer(
                                        8,
                                        NodeInfo::new(0, 0, 0),
                                    )),
                                    ArithmeticInfo::new_alt(0, 0, 0)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    0, 0, 0),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn function_with_single_variable_declaration_with_complex_initialization_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(3), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Divide, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(5), 0, 0, 0),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(6), 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(7), 0, 0, 0),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(9), 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Minus(
                            Box::new(AstNode::Plus(
                                Box::new(AstNode::Plus(
                                    Box::new(AstNode::Integer(
                                        1,
                                        NodeInfo::new(0, 0, 0),
                                    )),
                                    Box::new(AstNode::Multiply(
                                        Box::new(AstNode::Integer(
                                            2,
                                            NodeInfo::new(0, 0, 0),
                                        )),
                                        Box::new(AstNode::Integer(
                                            3,
                                            NodeInfo::new(0, 0, 0),
                                        )),
                                        ArithmeticInfo::new_alt(0, 0, 0)
                                    )),
                                    ArithmeticInfo::new_alt(0, 0, 0),
                                )),
                                Box::new(AstNode::Multiply(
                                    Box::new(AstNode::Divide(
                                        Box::new(AstNode::Integer(
                                            4,
                                            NodeInfo::new(0, 0, 0),
                                        )),
                                        Box::new(AstNode::Integer(
                                            5,
                                            NodeInfo::new(0, 0, 0),
                                        )),
                                        ArithmeticInfo::new_alt(0, 0, 0),
                                    )),
                                    Box::new(AstNode::Integer(
                                        6,
                                        NodeInfo::new(0, 0, 0),
                                    )),
                                    ArithmeticInfo::new_alt(0, 0, 0),
                                )),
                                ArithmeticInfo::new_alt(0, 0, 0)
                            )),
                            Box::new(AstNode::Multiply(
                                Box::new(AstNode::Integer(
                                    7,
                                    NodeInfo::new(0, 0, 0),
                                )),
                                Box::new(AstNode::Plus(
                                    Box::new(AstNode::Integer(
                                        8,
                                        NodeInfo::new(0, 0, 0),
                                    )),
                                    Box::new(AstNode::Integer(
                                        9,
                                        NodeInfo::new(0, 0, 0),
                                    )),
                                    ArithmeticInfo::new_alt(0, 0, 0),
                                )),
                                ArithmeticInfo::new_alt(0, 0, 0),
                            )),
                            ArithmeticInfo::new_alt(0, 0, 0),
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            0, 0, 0),
                    )
                ],
                None,
                NodeInfo::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node)
}

#[test]
fn function_with_return_without_expression_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Return, TokenSubType::NoSubType, 5, 4, 1),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::Return(
                        None,
                        ArithmeticInfo::new_alt(5, 4, 1),
                    )
                ],
                None,
                NodeInfo::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node);
}

#[test]
fn function_with_return_with_expression_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Return, TokenSubType::NoSubType, 5, 4, 1),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 3, 4, 5),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 8, 7, 6),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 55, 44, 33),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 1, 15, 53),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(3), 88, 77, 66),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::Return(
                        Some(Box::new(AstNode::Plus(
                            Box::new(AstNode::Identifier(
                                Rc::new("a".to_string()),
                                NodeInfo::new(3, 4, 5)
                                )),
                            Box::new(AstNode::Multiply(
                                    Box::new(AstNode::Integer(
                                        2,
                                        NodeInfo::new(55, 44, 33),
                                    )),
                                   Box::new(AstNode::Integer(
                                        3,
                                        NodeInfo::new(88, 77, 66),
                                    )),
                                    ArithmeticInfo::new_alt(1, 15, 53),
                                )),
                            ArithmeticInfo::new_alt(8, 7, 6)
                        ))),
                        ArithmeticInfo::new_alt(5, 4, 1),
                    )
                ],
                None,
                NodeInfo::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node);
}

#[test]
fn simple_less_expression_produces_correct_ast() {
   let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 7, 6, 5),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::BooleanType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::Comparison, TokenSubType::Less, 5, 6, 7),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 8, 9, 2),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

       assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Less(
                            Box::new(AstNode::Integer(
                                1,
                                NodeInfo::new(1, 2, 3),
                            )),
                            Box::new(AstNode::Integer(
                                2,
                                NodeInfo::new(8, 9, 2),
                            )),
                            NodeInfo::new(5, 6, 7)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Boolean,
                            7, 6, 5),
                    )
                ],
                None,
                NodeInfo::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node)
}

#[test]
fn while_loop_produces_correct_ast() {
       let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::While, TokenSubType::NoSubType, 5, 6, 7),
            Token::new(TokenType::Boolean, TokenSubType::BooleanValue(true), 8, 6, 7),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 4, 5, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::While(
                        Box::new(AstNode::Boolean(
                            true,
                            NodeInfo::new(8, 6, 7)
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    1,
                                    NodeInfo::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            NodeInfo::new(0, 0, 0),
                        )),
                        NodeInfo::new(5, 6, 7),
                    ),
                ],
                None,
                NodeInfo::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node)
}

#[test]
fn while_loop_with_complex_expression_produces_correct_ast() {
       let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::While, TokenSubType::NoSubType, 5, 6, 7),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aab".to_string())), 11, 22, 33),
            Token::new(TokenType::Comparison, TokenSubType::Equals, 12, 23, 34),
            Token::new(
                TokenType::Number,
                TokenSubType::IntegerNumber(23), 13, 14, 15),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 4, 5, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::While(
                        Box::new(AstNode::Equals(
                            Box::new(AstNode::Identifier(
                                Rc::new("aab".to_string()),
                                NodeInfo::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                23,
                                NodeInfo::new(13, 14, 15))),
                            NodeInfo::new(12, 23, 34)
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    1,
                                    NodeInfo::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            NodeInfo::new(0, 0, 0),
                        )),
                        NodeInfo::new(5, 6, 7),
                    ),
                ],
                None,
                NodeInfo::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node)
}

#[test]
fn if_statement_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::If, TokenSubType::NoSubType, 5, 6, 7),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aab".to_string())), 11, 22, 33),
            Token::new(TokenType::Comparison, TokenSubType::LessOrEq, 12, 23, 34),
            Token::new(
                TokenType::Number,
                TokenSubType::IntegerNumber(23), 13, 14, 15),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 4, 5, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
    AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::LessOrEq(
                        Box::new(AstNode::Identifier(
                            Rc::new("aab".to_string()),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            23,
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                1,
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    None,
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0)),
    node)
}

#[test]
fn if_statement_with_else_produces_correct_ast() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::If, TokenSubType::NoSubType, 5, 6, 7),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aab".to_string())), 11, 22, 33),
            Token::new(TokenType::Comparison, TokenSubType::GreaterOrEq, 12, 23, 34),
            Token::new(
                TokenType::Number,
                TokenSubType::IntegerNumber(23), 13, 14, 15),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 4, 5, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Else, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 41, 51, 61),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::DoubleType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::DoubleNumber(1.23   ), 7, 6, 5),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
    AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Identifier(
                            Rc::new("aab".to_string()),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            23,
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                1,
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Double(
                                1.23,
                                NodeInfo::new(7, 6, 5),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("b".to_string()),
                            Type::Double,
                            41, 51, 61)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0)),
    node)
}

#[test]
fn if_statement_with_else_if_and_else_produces_correct_ast() {

    /*
    fn foo () : int {
        if aab >= 23 {
            let a : int = 1;
        } else if aab > 53 {
            let b : double = 1.23;
        } else {
            let c : string = "foo";
        }
    }
    */

    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::If, TokenSubType::NoSubType, 5, 6, 7),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aab".to_string())), 11, 22, 33),
            Token::new(TokenType::Comparison, TokenSubType::GreaterOrEq, 12, 23, 34),
            Token::new(
                TokenType::Number,
                TokenSubType::IntegerNumber(23), 13, 14, 15),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 4, 5, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Else, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::If, TokenSubType::NoSubType, 55, 66, 77),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aab".to_string())), 11, 22, 33),
            Token::new(TokenType::Comparison, TokenSubType::Greater, 12, 23, 34),
            Token::new(
                TokenType::Number,
                TokenSubType::IntegerNumber(53), 13, 14, 15),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 41, 51, 61),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::DoubleType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::DoubleNumber(1.23), 7, 6, 5),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Else, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("c".to_string())), 42, 52, 62),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::StringType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Text, TokenSubType::Text(Rc::new("foo".to_string())), 71, 61, 51),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
    AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Identifier(
                            Rc::new("aab".to_string()),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            23,
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                1,
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::If(
                        Box::new(AstNode::Greater(
                            Box::new(AstNode::Identifier(
                                Rc::new("aab".to_string()),
                                NodeInfo::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                53,
                                NodeInfo::new(13, 14, 15))),
                            NodeInfo::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                    Box::new(AstNode::Double(
                                        1.23,
                                        NodeInfo::new(7, 6, 5),
                                    )),
                                    DeclarationInfo::new_alt(
                                    Rc::new("b".to_string()),
                                    Type::Double,
                                    41, 51, 61))
                            ],
                            None,
                            NodeInfo::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Text(
                                    Rc::new("foo".to_string()),
                                    NodeInfo::new(71, 61, 51),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("c".to_string()),
                                Type::String,
                                42, 52, 62)),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        ))),
                        NodeInfo::new(55, 66, 77),
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0)),
    node)
}

#[test]
fn function_call_without_arguments_produces_correct_ast() {
    /*
        fn foo() : void {
            bar();
        }

    */

     let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::VoidType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
                Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("bar".to_string())), 5, 6, 7),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
            ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
    AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::FunctionCall(
                    vec![],
                    Rc::new("bar".to_string()),
                    NodeInfo::new(5, 6, 7)),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0)),
    node);
}

#[test]
fn function_call_with_arguments_produces_correct_ast() {
    /*
        fn foo() : void {
            bar(2, a + 4);
        }
    */

     let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::VoidType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
                Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("bar".to_string())), 5, 6, 7),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::Comma, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
            ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
    AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::FunctionCall(
                    vec![
                        AstNode::Integer(
                            2,
                            NodeInfo::new(0, 0, 0)),
                        AstNode::Plus(
                            Box::new(AstNode::Identifier(
                                Rc::new("a".to_string()),
                                NodeInfo::new(0, 0, 0))),
                            Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(0, 0, 0))),
                            ArithmeticInfo::new_alt(0, 0, 0)),
                    ],
                    Rc::new("bar".to_string()),
                    NodeInfo::new(5, 6, 7)),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0)),
    node);
}


#[test]
fn function_call_in_expression_produces_correct_ast() {
    /*
        fn foo() : void {
            let a : int = 4 + bar(2);
        }
    */

     let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::VoidType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("bar".to_string())), 5, 6, 7),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
            ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    assert_eq!(
    AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::VariableDeclaration(
                    Box::new(AstNode::Plus(
                        Box::new(AstNode::Integer(
                            4,
                            NodeInfo::new(0, 0, 0))),
                        Box::new(AstNode::FunctionCall(
                            vec![
                                AstNode::Integer(
                                2,
                                NodeInfo::new(0, 0, 0))
                            ],
                            Rc::new("bar".to_string()),
                            NodeInfo::new(5, 6, 7))),
                        ArithmeticInfo::new_alt(0, 0, 0))),
                    DeclarationInfo::new_alt(
                        Rc::new("a".to_string()),
                        Type::Integer,
                        0, 0, 0)),

            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0)),
    node);
}

#[test]
fn function_definition_with_parameters_produces_correct_ast() {
    /*
        fn foo(a : int, b : string) : void {

        }
    */

    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Comma, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::VarType, TokenSubType::StringType, 0, 0, 0),

            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::VoidType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
            ]);
    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    let mut function_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        0, 0, 0);

    function_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0));

    function_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            0, 0, 0));

    assert_eq!(
        AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                    function_info)
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node);
}

#[test]
fn extern_function_definition_produces_correct_ast() {
    /*
        extern fn foo(a : int, b : string) : void;
    */

    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Extern, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Comma, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::VarType, TokenSubType::StringType, 0, 0, 0),

            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::VoidType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 0);

    let mut function_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        0, 0, 0);

    function_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0));

    function_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            0, 0, 0));

    assert_eq!(
        AstNode::Block(vec![
                AstNode::ExternFunction(function_info),
            ],
            None,
            NodeInfo::new(0, 0, 0)),
        node);
}

#[test]
fn missing_name_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 2, 3, 1),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        2,
        3,
        1);

    assert_eq!(
        AstNode::Block(
            vec![  ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}


#[test]
fn missing_lparen_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 5, 6, 1),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        5,
        6,
        1);

    assert_eq!(
        AstNode::Block(
            vec![  ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}

#[test]
fn missing_rparen_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 8, 9, 1),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        8,
        9,
        1);

    assert_eq!(
        AstNode::Block(
            vec![  ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}


#[test]
fn missing_colon_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 8, 1, 1),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        8,
        1,
        1);

    assert_eq!(
        AstNode::Block(
            vec![  ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}

#[test]
fn missing_variable_type_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 1, 1, 23),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        1,
        1,
        23);

    assert_eq!(
        AstNode::Block(
            vec![  ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}

#[test]
fn missing_lbrace_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 4, 4, 2),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        4,
        4,
        2);

    assert_eq!(
        AstNode::Block(
            vec![  ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}

#[test]
fn missing_rbrace_is_reported_in_function_definition() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        0,
        0,
        0);

    assert_eq!(
        AstNode::Block(
            vec![],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}

#[test]
fn variable_declaration_without_initialization_is_error() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aaaaa".to_string())), 5, 6, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        5,
        6,
        6);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn variable_declaration_after_variable_with_missing_declaration_is_handled_correctly() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aaaaa".to_string())), 5, 6, 6),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("bbb".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(14), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0)
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        5,
        6,
        6);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                        Box::new(
                                            AstNode::Integer(
                                                14,
                                                NodeInfo::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("bbb".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn variable_declaration_after_bad_declaration_is_handled_correctly() {
    let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("aaaaa".to_string())), 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 6, 7, 8),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(16), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("bbb".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(14), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0)
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        6,
        7,
        8);

    assert_eq!(
        AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                        Box::new(
                                            AstNode::Integer(
                                                14,
                                                NodeInfo::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("bbb".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node)
}

#[test]
fn missing_operand_in_arithmetic_operation_is_reported() {
        let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 9, 8, 7),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        9,
        8,
        7);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![],
                        None,
                        NodeInfo::new(0, 0, 0)
                    )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}

#[test]
fn missing_operator_in_arithmetic_operation_is_reported() {
            let (mut parser, reporter) = create_parser(vec![
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Assign, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 8, 7, 6),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

    let node = parser.parse();

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::SyntaxError,
        8,
        7,
        6);

    assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![],
                        None,
                        NodeInfo::new(0, 0, 0)
                    )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        ),
        node);
}