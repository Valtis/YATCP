use token::SyntaxToken;
use token::TokenType;
use token::TokenSubType;
use lexer::Lexer;
use ast::AstNode;
use ast::AstType;
use ast::FunctionInfo;
use ast::DeclarationInfo;
use ast::ArithmeticInfo;
use semcheck::Type;
/*
  Recursive descent parser that for now merely checks if input conforms to
  grammar. No syntax tree is built.

  Check documentation for grammar.
*/

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer: lexer,
        }
    }

    pub fn parse(&mut self) -> Result<AstNode, String> {
        let mut functions = vec![];
        loop {
            let token = try!(self.lexer.peek_token());
            match token.token_type {
                TokenType::Fn => {
                    let function_node = try!(self.parse_function());
                    functions.push(function_node);
                },
                TokenType::Eof => return Ok(AstNode::new(functions, AstType::Block)),
                _ => return Err(format!("Unexpected token {}. Expected {}", token, TokenType::Fn)),
            }            
        }
    }

    fn parse_function(&mut self) -> Result<AstNode, String> {
        try!(self.expect(TokenType::Fn));
        let identifier = try!(self.expect(TokenType::Identifier));
        try!(self.expect(TokenType::LParen));
        try!(self.expect(TokenType::RParen));
        try!(self.expect(TokenType::Colon));
        try!(self.expect(TokenType::VarType));
        let node = try!(self.parse_block());

        Ok(AstNode::new(vec![node], AstType::Function(FunctionInfo::new(identifier))))
    }

    fn parse_block(&mut self) -> Result<AstNode, String> {
        try!(self.expect(TokenType::LBrace));
        let nodes = try!(self.parse_statements());
        try!(self.expect(TokenType::RBrace));
        Ok(AstNode::new(nodes, AstType::Block))
    }

    fn parse_statements(&mut self) -> Result<Vec<AstNode>, String> {
        let mut nodes = vec![];
        loop {
            let token = try!(self.lexer.peek_token());
            match token.token_type {
                TokenType::Let => {
                    nodes.push(try!(self.parse_variable_declaration()));
                }
                _ => return Ok(nodes)
            };
        }
    }


    fn parse_variable_declaration(&mut self) -> Result<AstNode, String> {
        try!(self.expect(TokenType::Let));
        let identifier = try!(self.expect(TokenType::Identifier));
        try!(self.expect(TokenType::Colon));
        let var_type = try!(self.expect(TokenType::VarType));
        try!(self.expect(TokenType::Assign));
        let node = try!(self.parse_expression());
        try!(self.expect(TokenType::SemiColon));

        Ok(AstNode::new(vec![node], AstType::VariableDeclaration(DeclarationInfo::new(identifier, var_type))))
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        let node = try!(self.parse_term());
        self.parse_plus_minus_expression(node)
    }

    fn parse_term(&mut self) -> Result<AstNode, String> {
        let node = try!(self.parse_factor());
        self.parse_mult_divide_expression(node)
    }

    fn parse_factor(&mut self) -> Result<AstNode, String> {
        let token = try!(self.expect_one_of(vec![
            TokenType::Identifier, TokenType::Number, TokenType::LParen]));
        match token.token_type {
            TokenType::Identifier => {
                match token.token_subtype {
                    TokenSubType::Identifier(text) => Ok(AstNode::new(vec![], AstType::Identifier(text))),
                    _ => panic!(
                        "Internal compiler error - invalid token {} passed when identifier expected", token),
                }
            },
            TokenType::Number => {
                match token.token_subtype {
                    TokenSubType::IntegerNumber(i) => {
                        Ok(AstNode::new(vec![], AstType::Integer(i)))
                    },
                    TokenSubType::DoubleNumber(i) => {
                        Ok(AstNode::new(vec![], AstType::Double(i)))
                    }
                    _ => panic!("Not implemented for: {}", token)
                }
            },
            TokenType::LParen => {
                let node = try!(self.parse_expression());
                try!(self.expect(TokenType::RParen));
                Ok(node)
            }
            _ => return Err(
                format!("Internal compiler error - invalid token '{}' passed to match statement in parse_factor", token)),
        }
    }

    fn parse_plus_minus_expression(&mut self, node: AstNode) -> Result<AstNode, String> {
        let next_token = try!(self.lexer.peek_token());
        if next_token.token_type == TokenType::Plus || next_token.token_type == TokenType::Minus {
            let mut nodes = vec![node];

            try!(self.lexer.next_token());
            nodes.push(try!(self.parse_expression()));
            if next_token.token_type == TokenType::Plus {
                return Ok(AstNode::new(nodes, AstType::Plus(ArithmeticInfo::new(next_token))));
            } else {
                return Ok(AstNode::new(nodes, AstType::Minus(ArithmeticInfo::new(next_token))));
            }
        }

        Ok(node)
    }
    fn parse_mult_divide_expression(&mut self, node: AstNode) -> Result<AstNode, String> {

        let next_token = try!(self.lexer.peek_token());
        if next_token.token_type == TokenType::Multiply || next_token.token_type == TokenType::Divide {
            let mut nodes = vec![node];

            try!(self.lexer.next_token());
            nodes.push(try!(self.parse_term()));
            if next_token.token_type == TokenType::Multiply {
                return Ok(AstNode::new(nodes, AstType::Multiply(ArithmeticInfo::new(next_token))));
            } else {
                return Ok(AstNode::new(nodes, AstType::Divide(ArithmeticInfo::new(next_token))));
            }
        }

        Ok(node)
    }

    fn expect(&mut self, token_type: TokenType) -> Result<SyntaxToken, String> {
        let next_token = try!(self.lexer.next_token());
        if next_token.token_type != token_type {
            Err(format!("Unexpected token '{}' at {}:{}. {} was expected instead",
                next_token,
                next_token.line,
                next_token.column,
                token_type))
        } else {
            Ok(next_token)
        }
    }

    fn expect_one_of(&mut self, types: Vec<TokenType>) -> Result<SyntaxToken, String> {
        let next_token = try!(self.lexer.next_token());
        for token_type in &types {
            if next_token.token_type == *token_type {
                return Ok(next_token);
            }
        }
        let mut type_string: String = "".to_string();
        for token_type in types {
            type_string = format!("{} {}", type_string, token_type);
        }

        Err(format!("Unexpected token '{}' at {}:{}. Expected one of:{}",
            next_token,
            next_token.line,
            next_token.column,
            type_string))
    }
}
