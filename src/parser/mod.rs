use crate::token::{Token, TokenType, TokenSubType};

use crate::lexer::Lexer;

use crate::error_reporter::{ErrorReporter, ReportKind};

use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, NodeInfo as Span, DeclarationInfo};

use std::cell::RefCell;
use std::rc::Rc;

pub struct Parser {
    lexer: Box<Lexer>,
    error_reporter: Rc<RefCell<ErrorReporter>>,
}

impl Parser {
    pub fn new(
        lexer: Box<Lexer>,
        error_reporter: Rc<RefCell<ErrorReporter>>) -> Parser {
        Parser {
            lexer: lexer,
            error_reporter: error_reporter,
        }
    }

    pub fn parse(&mut self) -> AstNode {

        let top_level_tokens = vec![TokenType::Fn, TokenType::Extern];
        let mut nodes = vec![];
        let mut debug_safeguard = 10000;
        loop {
            // some minor protection against infinite loops
            debug_safeguard -= 1;
            if debug_safeguard == 0 {
                panic!("Debug safeguard triggered");
            }
            let token = self.lexer.peek_token();
            match token.token_type {
                TokenType::Extern => {
                    match self.parse_external_function_declaration() {
                        Ok(declaration) => nodes.push(declaration),
                        Err(_) =>
                            self.skip_to_first_of(top_level_tokens.clone()),
                    }
                },
                TokenType::Fn => {
                    match self.parse_function() {
                        Ok(function_node) => nodes.push(function_node),
                        Err(_) =>
                            self.skip_to_first_of(top_level_tokens.clone()),
                    }
                },
                TokenType::Eof =>
                    return AstNode::Block(
                            nodes,
                            None,
                            Span::new(0, 0, 0),
                        ),
                _ => {
                    self.report_unexpected_token(
                        TokenType::Fn, &token);
                    self.skip_to_first_of(top_level_tokens.clone());
                },
            }
        }
    }

    fn parse_function(&mut self) -> Result<AstNode, ()> {
        self.expect(TokenType::Fn)?;
        let identifier = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::LParen)?;
        let params = self.parse_parameter_list()?;
        self.expect(TokenType::RParen)?;
        self.expect(TokenType::Colon)?;
        let type_token = self.expect(TokenType::VarType)?;

        let node = self.parse_block()?;
        let mut func_info = FunctionInfo::new(
                &identifier,
                &type_token);

        func_info.parameters = params;

        let func_node = AstNode::Function(
            Box::new(node),
            func_info);

        Ok(func_node)
    }

    fn parse_external_function_declaration(&mut self) -> Result<AstNode, ()> {
        self.expect(TokenType::Extern)?;
        self.expect(TokenType::Fn)?;
        let identifier = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::LParen)?;
        let params = self.parse_parameter_list()?;
        self.expect(TokenType::RParen)?;
        self.expect(TokenType::Colon)?;
        let type_token = self.expect(TokenType::VarType)?;
        self.expect(TokenType::SemiColon)?;


        let mut func_info = FunctionInfo::new(
                &identifier,
                &type_token);

        func_info.parameters = params;

        Ok(AstNode::ExternFunction(func_info))
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<DeclarationInfo>, ()> {
        let mut params = vec![];
        if self.lexer.peek_token().token_type == TokenType::Identifier {
            loop {
                let identifier = self.expect(TokenType::Identifier)?;
                self.expect(TokenType::Colon)?;
                let var_type = self.expect(TokenType::VarType)?;
                params.push(DeclarationInfo::new(&identifier, &var_type));

                if let TokenType::Comma = self.lexer.peek_token().token_type {
                    self.expect(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }

        Ok(params)
    }

    fn parse_block(&mut self) -> Result<AstNode, ()> {
        self.expect(TokenType::LBrace)?;
        let nodes = self.parse_statements()?;
        self.expect(TokenType::RBrace)?;

        let block = AstNode::Block(
            nodes,
            None,
            Span::new(0, 0, 0),
        );

        Ok(block)
    }

    fn parse_statements(&mut self) -> Result<Vec<AstNode>, ()> {
        let mut nodes = vec![];
        loop {
            let token = self.lexer.peek_token();
            let res = match token.token_type {
                TokenType::SemiColon => {
                    self.lexer.next_token();
                    continue;
                },
                TokenType::Let =>
                    self.parse_variable_declaration(),
                TokenType::LBrace =>
                    self.parse_block(),
                TokenType::Identifier =>
                    self.parse_function_call_or_assignment(),
                TokenType::Return =>
                    self.parse_return_statement(),
                TokenType::While =>
                    self.parse_while_statement(),
                TokenType::If =>
                    self.parse_if_statement(),
                _ => return Ok(nodes)
            };

            match res {
                Ok(node) => nodes.push(node),
                Err(_) => self.skip_to_first_of(
                    vec![
                        TokenType::SemiColon,
                        TokenType::RBrace]),
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<AstNode, ()> {
        self.expect(TokenType::Let)?;
        let identifier = self.expect(TokenType::Identifier)?;
        self.expect(TokenType::Colon)?;
        let var_type = self.expect(TokenType::VarType)?;

        // Custom error handling
        let maybe_assign = self.lexer.next_token();
        match maybe_assign.token_type {
            TokenType::Assign => {}, // ok
            TokenType::SemiColon => {
                self.report_error(
                    ReportKind::SyntaxError,
                    Span::new(identifier.line, identifier.column, identifier.length),
                    format!("Variable declaration must be followed by initialization"));
                return Err(());
            },
            _ => {
                self.report_unexpected_token(
                    TokenType::Assign,
                    &maybe_assign);
                return Err(());
            },
        }

        let node = self.parse_expression()?;
        self.expect(TokenType::SemiColon)?;
        let declaration = AstNode::VariableDeclaration(
                Box::new(node),
                DeclarationInfo::new(&identifier, &var_type));

        Ok(declaration)
    }

    fn parse_function_call_or_assignment(&mut self) -> Result<AstNode, ()> {
        let identifier = self.expect(TokenType::Identifier)?;
        let token  = self.lexer.peek_token();
        match token.token_type {
            TokenType::Assign => self.parse_assignment(identifier),
            TokenType::LParen => self.parse_function_call(identifier),
            _ => {
                self.report_unexpected_token_mul(
                    &vec![TokenType::Assign, TokenType::LParen],
                    &token);
                Err(())
            },
        }
    }

    fn parse_assignment(&mut self, identifier: Token) -> Result<AstNode, ()> {
        self.expect(TokenType::Assign)?;
        let expression_node = self.parse_expression()?;
        self.expect(TokenType::SemiColon)?;

        let name = if let TokenSubType::Identifier(ident) = identifier.token_subtype {
            ident.clone()
        } else {
            ice!("Non-identifier token '{}' passed to parse_assignment",
                identifier);
        };

        Ok(AstNode::VariableAssignment(
            Box::new(expression_node),
            name,
            Span::new(
                identifier.line,
                identifier.column,
                identifier.length)))
    }

    fn parse_function_call(
        &mut self,
        identifier: Token) -> Result<AstNode, ()>  {

        let name = if let TokenSubType::Identifier(ident) = identifier.token_subtype {
            ident.clone()
        } else {
            ice!("Non-identifier token '{}' passed to parse_function_call",
                identifier);
        };

        let mut args = vec![];
        self.expect(TokenType::LParen)?;
        if self.lexer.peek_token().token_type != TokenType::RParen {
            loop {
                args.push(self.parse_expression()?);
                if self.lexer.peek_token().token_type == TokenType::Comma {
                    self.expect(TokenType::Comma)?;
                } else {
                    break;
                }
            }
        }

        self.expect(TokenType::RParen)?;
        Ok(AstNode::FunctionCall(
            args,
            name,
            Span::new(identifier.line, identifier.column, identifier.length)
            ))
    }

    fn parse_return_statement(&mut self) -> Result<AstNode, ()> {
        let return_node = self.expect(TokenType::Return)?;

        let token = self.lexer.peek_token();
        let node = if token.token_type != TokenType::SemiColon {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        self.expect(TokenType::SemiColon)?;


        Ok(AstNode::Return(
            node,
            ArithmeticInfo::new(&return_node)))
    }

    fn parse_while_statement(&mut self) -> Result<AstNode, ()> {
        let while_node = self.expect(TokenType::While)?;
        let expr = self.parse_expression()?;
        let block = self.parse_block()?;

        Ok(AstNode::While(
            Box::new(expr),
            Box::new(block),
            Span::new(
                while_node.line,
                while_node.column,
                while_node.length)))
    }

    fn parse_if_statement(&mut self) -> Result<AstNode, ()> {
        let if_node = self.expect(TokenType::If)?;
        let expr = self.parse_expression()?;
        let block = self.parse_block()?;

        let token = self.lexer.peek_token();

        let opt_else_blk = if token.token_type == TokenType::Else {
            let else_blk = self.parse_else_block()?;
            Some(Box::new(else_blk))
        } else {
            None
        };

        Ok(AstNode::If(
            Box::new(expr),
            Box::new(block),
            opt_else_blk,
            Span::new(
                if_node.line,
                if_node.column,
                if_node.length)))
    }

    fn parse_else_block(&mut self) -> Result<AstNode, ()> {
        self.expect(TokenType::Else)?;
        let token = self.lexer.peek_token();
        if token.token_type == TokenType::If {
            self.parse_if_statement()
        } else {
            self.parse_block()
        }
    }

    fn parse_expression(&mut self) -> Result<AstNode, ()> {
        let mut node = self.parse_arithmetic_expression()?;

        loop {
            let next_token = self.lexer.peek_token();
            if next_token.token_type == TokenType::Comparison {
                node = self.parse_comparison_expression(node)?;
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_arithmetic_expression(&mut self) -> Result<AstNode, ()> {
        let mut node = self.parse_term()?;

        loop {
            let next_token = self.lexer.peek_token();
            // custom error handling for missing operator
            if self.starts_operand(&next_token) {
                self.report_error(
                    ReportKind::SyntaxError,
                    Span::new(next_token.line, next_token.column, next_token.length),
                    format!(
                        "Unexpected token '{}'. Missing operator?",
                        next_token));

                return Err(());
            }

            match next_token.token_type {
                TokenType::Plus | TokenType::Minus =>
                    node = self.parse_plus_minus_expression(node)?,
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_term(&mut self) -> Result<AstNode, ()> {
        let mut node = self.parse_factor()?;

        // while mul or div tokens are next, keep parsing
        loop {
            let next_token = self.lexer.peek_token();
            match next_token.token_type {
                TokenType::Multiply | TokenType::Divide =>
                    node = self.parse_mult_divide_expression(node)?,
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<AstNode, ()> {
        let token = self.expect_one_of(vec![
            TokenType::Identifier, TokenType::Number, TokenType::LParen, TokenType::Text, TokenType::Minus, TokenType::Boolean])?;

        match token.token_type {
            TokenType::Minus => {
                let node = self.parse_factor()?;
                match node {
                    AstNode::Integer(AstInteger::Int(val), info) => {
                        Ok(AstNode::Integer(AstInteger::Int(-val), info.clone()))
                    },
                    AstNode::Integer(AstInteger::IntMaxPlusOne, info) => {
                        Ok(AstNode::Integer(
                            AstInteger::Int(i32::min_value()),
                            info.clone()))
                    },
                    _ => {
                        Ok(AstNode::Negate(
                            Box::new(node),
                            ArithmeticInfo::new(&token)))
                    }
                }

            },
            TokenType::Identifier => {
                if self.lexer.peek_token().token_type == TokenType::LParen {
                    self.parse_function_call(token)
                } else {
                    match token.token_subtype {
                        TokenSubType::Identifier(ref identifier) =>
                            Ok(AstNode::Identifier(
                                identifier.clone(),
                                Span::new(
                                    token.line, token.column, token.length))),
                        TokenSubType::ErrorToken =>
                                Ok(AstNode::ErrorNode),
                        _ => ice!(
                            "invalid token '{}' passed when identifier expected", token),
                    }
                }
            },
            TokenType::Number => {
                match token.token_subtype {
                    TokenSubType::IntegerNumber(i) => {
                        Ok(AstNode::Integer(AstInteger::from(i), Span::new(
                            token.line, token.column, token.length)))
                    },
                    TokenSubType::DoubleNumber(i) => {
                        Ok(AstNode::Double(i, Span::new(
                            token.line, token.column, token.length)))
                    },
                    TokenSubType::FloatNumber(i) => {
                        Ok(AstNode::Float(i, Span::new(
                            token.line, token.column, token.length)))
                    },
                    TokenSubType::ErrorToken => {
                        Ok(AstNode::ErrorNode)
                    },
                    _ => ice!("Invalid token '{}' passed when number expected", token)
                }
            },
            TokenType::Boolean => {
                match token.token_subtype {
                    TokenSubType::BooleanValue(v) => {
                        Ok(AstNode::Boolean(v, Span::new(
                            token.line, token.column, token.length)))
                    },
                    _ => ice!("Invalid token '{}' passed when boolean value expected", token)
                }
            }
            TokenType::LParen => {
                let node = self.parse_expression()?;
                self.expect(TokenType::RParen)?;
                Ok(node)
            },
            TokenType::Text => {
                match token.token_subtype {
                    TokenSubType::Text(ref text) =>
                        Ok(AstNode::Text(
                            text.clone(),
                            Span::new(
                                token.line, token.column, token.length))),
                    TokenSubType::ErrorToken =>
                        Ok(AstNode::ErrorNode),
                    _ => ice!("Invalid token '{}' passed when text expected", token),
                }
            }
            _ => ice!(
                    "Invalid token '{}' passed to match statement in parse_factor",
                    token),
        }
    }

    fn starts_operand(&self, token: &Token) -> bool {
        token.token_type == TokenType::Identifier ||
        token.token_type == TokenType::Number ||
        token.token_type == TokenType::LParen ||
        token.token_type ==TokenType::Text
    }

    fn parse_comparison_expression(
        &mut self, node: AstNode) -> Result<AstNode, ()> {

        let next_token = self.lexer.peek_token();
        if next_token.token_type == TokenType::Comparison {
            self.lexer.next_token();
            let n_node = self.parse_arithmetic_expression()?;

            match next_token.token_subtype {
                TokenSubType::Less => {
                    let less_node = AstNode::Less(
                        Box::new(node),
                        Box::new(n_node),
                        Span::new(
                            next_token.line,
                            next_token.column,
                            next_token.length));
                    Ok(less_node)
                },
                TokenSubType::LessOrEq => {
                    let less_or_eq_node = AstNode::LessOrEq(
                        Box::new(node),
                        Box::new(n_node),
                        Span::new(
                            next_token.line,
                            next_token.column,
                            next_token.length));
                    Ok(less_or_eq_node)
                },
                TokenSubType::Equals => {
                    let equals_node = AstNode::Equals(
                        Box::new(node),
                        Box::new(n_node),
                        Span::new(
                            next_token.line,
                            next_token.column,
                            next_token.length));
                    Ok(equals_node)
                },
                TokenSubType::NotEquals => {
                    let equals_node = AstNode::NotEquals(
                        Box::new(node),
                        Box::new(n_node),
                        Span::new(
                            next_token.line,
                            next_token.column,
                            next_token.length));
                    Ok(equals_node)
                },
                TokenSubType::GreaterOrEq => {
                    let greater_or_eq_node = AstNode::GreaterOrEq(
                        Box::new(node),
                        Box::new(n_node),
                        Span::new(
                            next_token.line,
                            next_token.column,
                            next_token.length));
                    Ok(greater_or_eq_node)
                },
                TokenSubType::Greater => {
                    let greater_node = AstNode::Greater(
                        Box::new(node),
                        Box::new(n_node),
                        Span::new(
                            next_token.line,
                            next_token.column,
                            next_token.length));
                    Ok(greater_node)
                },
                _ => ice!("Invalid token subtype '{}' for comparison", next_token),
            }
        } else {
            Ok(node)
        }

    }

    fn parse_plus_minus_expression(&mut self, node: AstNode) -> Result<AstNode, ()> {
        let next_token = self.lexer.peek_token();

        match next_token.token_type {
            TokenType::Plus => {
                self.lexer.next_token();
                let n_node = self.parse_term()?;
                let plus_node = AstNode::Plus(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));

                Ok(plus_node)
            },
            TokenType::Minus => {
                self.lexer.next_token();
                let n_node = self.parse_term()?;
                let minus_node = AstNode::Minus(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));

                Ok(minus_node)
            },
            _ => Ok(node)
        }
    }

    fn parse_mult_divide_expression(&mut self, node: AstNode) -> Result<AstNode, ()> {

        let next_token = self.lexer.peek_token();
        match next_token.token_type {
            TokenType::Multiply => {
                self.lexer.next_token();
                let n_node = self.parse_factor()?;
                let mult_node = AstNode::Multiply(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));

                Ok(mult_node)
            },
            TokenType::Divide => {
                self.lexer.next_token();
                let n_node = self.parse_factor()?;
                let div_node = AstNode::Divide(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));
                Ok(div_node)
            },
            _ => Ok(node),
        }
    }



    fn expect(&mut self, token_type: TokenType) -> Result<Token, ()> {
        let next_token = self.lexer.next_token();

        if next_token.token_type != token_type {
            self.report_unexpected_token(token_type, &next_token);
            Err(())
        } else {
            Ok(next_token)
        }
    }

    fn expect_one_of(&mut self, types: Vec<TokenType>) -> Result<Token, ()> {
        let next_token = self.lexer.next_token();

        if types.iter().any(|t| *t == next_token.token_type) {
            Ok(next_token)
        } else {
            let mut type_string: String = "".to_string();

            for ref token_type in types.iter() {
                type_string = format!("{} {}", type_string, *token_type);
            }

            self.report_unexpected_token_mul(&types, &next_token);

            Err(())
        }
    }

    fn skip_to_first_of(&mut self, mut tokens: Vec<TokenType>) {
        tokens.push(TokenType::Eof);

        // check first that current token does not match the token that is acceptable
        if let Some(cur_token) = self.lexer.current_token() {
            if tokens.iter().any(|t| *t == cur_token.token_type) {
                return;
            }
        }

        loop {
            let next_token = self.lexer.peek_token();
            if tokens.iter().any(|t| *t == next_token.token_type) {
                break;
            }
            self.lexer.next_token();
        }
    }

    fn report_unexpected_token(&mut self, expected: TokenType, actual: &Token) {

        if actual.token_type == TokenType::Eof {
            self.report_error(ReportKind::SyntaxError,
                              Span::new(actual.line, actual.column, actual.length),
                              format!("Unexpected end of file when '{}' was expected",
                    expected));

            return;
        }

        self.report_error(ReportKind::SyntaxError,
                          Span::new(actual.line, actual.column, actual.length),
                          format!("Unexpected token '{}' when '{}' was expected",
                actual.token_type,
                expected));
    }

    fn report_unexpected_token_mul(&mut self, expected: &Vec<TokenType>, actual: &Token) {

        let mut expected_str = expected.
            iter().
            fold(String::new(), |acc, &t| format!("'{}', {}", t, acc));

        expected_str.pop();
        expected_str.pop(); // get rid off the last comma

        if actual.token_type == TokenType::Eof {
            self.report_error(ReportKind::SyntaxError,
                              Span::new(actual.line, actual.column, actual.length),
                              format!("Unexpected end of file when one of {} were expected",
                    expected_str));

            return;
        }

        self.report_error(ReportKind::SyntaxError,
                          Span::new(actual.line, actual.column, actual.length),
                          format!("Unexpected token '{}' when one of {} were expected",
                actual.token_type,
                expected_str));
    }


    fn report_error(
        &mut self,
        error_type: ReportKind,
        span: Span,
        reason: String) {
        self.error_reporter.borrow_mut().report_error(
            error_type,
            span,
            reason);
    }
}
