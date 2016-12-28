use token::SyntaxToken;
use token::TokenType;
use token::TokenSubType;
use lexer::Lexer;
use ast::AstNode;
use ast::AstType;
use ast::FunctionInfo;
use ast::DeclarationInfo;
use ast::ArithmeticInfo;
use ast::IdentifierInfo;
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
                TokenType::Eof => return Ok(AstNode::new(&token, functions, AstType::Block(None))),
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
        let type_token = try!(self.expect(TokenType::VarType));
        let node = try!(self.parse_block());

        Ok(AstNode::new(&identifier, vec![node], AstType::Function(
            FunctionInfo::new(&identifier, &type_token))))
    }

    fn parse_block(&mut self) -> Result<AstNode, String> {
        let token = try!(self.expect(TokenType::LBrace));
        let nodes = try!(self.parse_statements());
        try!(self.expect(TokenType::RBrace));
        Ok(AstNode::new(&token, nodes, AstType::Block(None)))
    }

    fn parse_statements(&mut self) -> Result<Vec<AstNode>, String> {
        let mut nodes = vec![];
        loop {
            let token = try!(self.lexer.peek_token());
            match token.token_type {
                TokenType::Let => {
                    nodes.push(try!(self.parse_variable_declaration()));
                },
                TokenType::Identifier => {
                    nodes.push(try!(self.parse_function_call_or_assignment()));
                },
                TokenType::Return => {
                    nodes.push(try!(self.parse_return_statement()));
                },
                TokenType::If => {
                    nodes.push(try!(self.parse_if_statement()));
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

        Ok(AstNode::new(&identifier, vec![node], AstType::VariableDeclaration(DeclarationInfo::new(&identifier, &var_type))))
    }


    fn parse_function_call_or_assignment(&mut self) -> Result<AstNode, String> {
        let identifier = try!(self.expect(TokenType::Identifier));
        let token  = try!(self.lexer.peek_token());
        match token.token_type {
            TokenType::Assign => self.parse_assignment(identifier),
            TokenType::LParen => panic!("Function call not implemented"), 
            _ => Err(format!("{}:{}: Unexpected token '{}'. Expected '{}' for assignment or '{}' for function call", 
                token.line, token.column, token, TokenType::Assign, TokenType::LParen)),
        }
    }


    fn parse_assignment(&mut self, identifier: SyntaxToken) -> Result<AstNode, String> {
        try!(self.expect(TokenType::Assign));
        let expression_node = try!(self.parse_expression());
        try!(self.expect(TokenType::SemiColon));

        Ok(AstNode::new(&identifier, vec![expression_node], AstType::VariableAssignment(IdentifierInfo::new(&identifier))))
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, String> {
        let return_node = try!(self.expect(TokenType::Return));
        let node = try!(self.parse_expression());
        try!(self.expect(TokenType::SemiColon));

        Ok(AstNode::new(&return_node, vec![node], AstType::Return))
    }


    fn parse_if_statement(&mut self) -> Result<AstNode, String> {
        let if_node = try!(self.expect(TokenType::If));
        let condition_node = try!(self.parse_expression());
        let block = try!(self.parse_block());

        let mut children = vec![];
        children.push(condition_node);
        children.push(block);

        // handle elseif/else
        loop {
            let next_token = try!(self.lexer.peek_token());
            if next_token.token_type == TokenType::ElseIf { 
                children.push(try!(self.parse_else_if_statement()));
            } else if next_token.token_type == TokenType::Else {
                children.push(try!(self.parse_else_statement()));
                break;
            } else {
                break;
            }
        }

        let branch_node = AstNode::new(&if_node, children, AstType::If);

        Ok(branch_node)
    }

    fn parse_else_if_statement(&mut self) -> Result<AstNode, String> {
        let else_if_node = try!(self.expect(TokenType::ElseIf));
        let condition_node = try!(self.parse_expression());
        let block = try!(self.parse_block());

        let mut children = vec![];
        children.push(condition_node);
        children.push(block);

        let branch_node = AstNode::new(&else_if_node, children, AstType::ElseIf);

        Ok(branch_node)
    }

    fn parse_else_statement(&mut self) -> Result<AstNode, String> {
        let else_node = try!(self.expect(TokenType::Else));
        let block = try!(self.parse_block());

        let mut children = vec![];
        children.push(block);

        let branch_node = AstNode::new(&else_node, children, AstType::Else);

        Ok(branch_node)
    }

    fn parse_expression(&mut self) -> Result<AstNode, String> {
        let node = try!(self.parse_arithmetic_expression());
        let mut result = try!(self.parse_comparison_expression(node));

        loop {
            let next_token = try!(self.lexer.peek_token());
            match next_token.token_type {
                TokenType::Equals => 
                    result = try!(self.parse_comparison_expression(result)),
                _ => break,
            }
        }
        Ok(result)
    } 

    fn parse_arithmetic_expression(&mut self) -> Result<AstNode, String> {
        let node = try!(self.parse_term());
        let mut result = try!(self.parse_plus_minus_expression(node));

        // while plus or minus tokens are next, keep parse_assignment
        loop {
            let next_token = try!(self.lexer.peek_token());
            match next_token.token_type {
                TokenType::Plus | TokenType::Minus => 
                    result = try!(self.parse_plus_minus_expression(result)),
                _ => break,
            }
        }
        Ok(result)
    }

    fn parse_term(&mut self) -> Result<AstNode, String> {
        let node = try!(self.parse_factor());

        let mut result = try!(self.parse_mult_divide_expression(node));
        // while mul or div tokens are next, keep parse_assignment
        loop {
            let next_token = try!(self.lexer.peek_token());
            match next_token.token_type {
                TokenType::Multiply | TokenType::Divide => 
                    result = try!(self.parse_mult_divide_expression(result)),
                _ => break,
            }
        }
        Ok(result)
    }

    fn parse_factor(&mut self) -> Result<AstNode, String> {
        let token = try!(self.expect_one_of(vec![
            TokenType::Identifier, TokenType::Number, TokenType::LParen, TokenType::Text]));
        match token.token_type {
            TokenType::Identifier => {
                match token.token_subtype {
                    TokenSubType::Identifier(_) => Ok(AstNode::new(&token, vec![],
                        AstType::Identifier(IdentifierInfo::new(&token)))),
                    _ => panic!(
                        "Internal compiler error - invalid token {} passed when identifier expected", token),
                }
            },
            TokenType::Number => {
                match token.token_subtype {
                    TokenSubType::IntegerNumber(i) => {
                        Ok(AstNode::new(&token, vec![], AstType::Integer(i)))
                    },
                    TokenSubType::DoubleNumber(i) => {
                        Ok(AstNode::new(&token, vec![], AstType::Double(i)))
                    },
                    TokenSubType::FloatNumber(i) => {
                        Ok(AstNode::new(&token, vec![], AstType::Float(i)))
                    },
                    _ => panic!("Internal compiler error: Invalid token {} when number expected", token)
                }
            },
            TokenType::LParen => {
                let node = try!(self.parse_expression());
                try!(self.expect(TokenType::RParen));
                Ok(node)
            },
            TokenType::Text => {
                match token.token_subtype {
                    TokenSubType::Text(ref text) =>
                        Ok(AstNode::new(&token, vec![], AstType::Text(text.clone()))),
                    _ => panic!("Internal compiler error - invalid token {} when text expected", token),
                }
            }
            _ => return Err(
                format!("Internal compiler error - invalid token '{}' passed to match statement in parse_factor", token)),
        }
    }

    fn parse_comparison_expression(&mut self, node: AstNode) -> Result<AstNode, String> {
        let next_token = try!(self.lexer.peek_token());
        if next_token.token_type == TokenType::Equals {
            let mut nodes = vec![node];

            try!(self.lexer.next_token());
            nodes.push(try!(self.parse_arithmetic_expression()));          
            return Ok(AstNode::new(&next_token, nodes, AstType::Equals));            
        }

        Ok(node)
    }

    fn parse_plus_minus_expression(&mut self, node: AstNode) -> Result<AstNode, String> {
        let next_token = try!(self.lexer.peek_token());
        if next_token.token_type == TokenType::Plus || next_token.token_type == TokenType::Minus {
            let mut nodes = vec![node];

            try!(self.lexer.next_token());
            nodes.push(try!(self.parse_term()));
            if next_token.token_type == TokenType::Plus {
                return Ok(AstNode::new(&next_token, nodes, AstType::Plus(ArithmeticInfo::new())));
            } else {
                return Ok(AstNode::new(&next_token, nodes, AstType::Minus(ArithmeticInfo::new())));
            }
        }

        Ok(node)
    }

    fn parse_mult_divide_expression(&mut self, node: AstNode) -> Result<AstNode, String> {

        let next_token = try!(self.lexer.peek_token());
        if next_token.token_type == TokenType::Multiply || next_token.token_type == TokenType::Divide {
            let mut nodes = vec![node];

            try!(self.lexer.next_token());
            nodes.push(try!(self.parse_factor()));
            if next_token.token_type == TokenType::Multiply {
                return Ok(AstNode::new(&next_token, nodes, AstType::Multiply(ArithmeticInfo::new())));
            } else {
                return Ok(AstNode::new(&next_token, nodes, AstType::Divide(ArithmeticInfo::new())));
            }
        }

        Ok(node)
    }

    fn expect(&mut self, token_type: TokenType) -> Result<SyntaxToken, String> {
        let next_token = try!(self.lexer.next_token());
        if next_token.token_type != token_type {
            Err(format!("{}:{}: Unexpected token '{}'. '{}' was expected instead",
                next_token.line,
                next_token.column,
                next_token,
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

        Err(format!("{}:{}: Unexpected token '{}' . Expected one of:{}",
            next_token.line,
            next_token.column,
            next_token,
            type_string))
    }
}
