use crate::token::{Token, TokenType, TokenSubType};

use crate::lexer::Lexer;

use crate::error_reporter::{ErrorReporter, ReportKind};

use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, NodeInfo as Span, DeclarationInfo, NodeInfo};

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
        let mut node = self.parse_arithmetic_expression_with_boolean_and()?;

        loop {
            let next_token = self.lexer.peek_token();

            match next_token.token_type {
                TokenType::DoublePipe => {
                    node = self.parse_boolean_or_expression(node)?
                }
                _ => break,
            }
        }

        Ok(node)
    }

    fn parse_arithmetic_expression_with_boolean_and(&mut self) -> Result<AstNode, ()> {
        let mut node = self.parse_arithmetic_expression_with_equals_not_equals_comparisons()?;

        loop {
            let next_token = self.lexer.peek_token();
            if next_token.token_type == TokenType::DoubleAmpersand {
                node = self.parse_boolean_and_expression(node)?;
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn parse_arithmetic_expression_with_equals_not_equals_comparisons(&mut self) -> Result<AstNode, ()> {
        let mut node = self.parse_arithmetic_expression_with_greater_less_comparisons()?;

        loop {
            let next_token = self.lexer.peek_token();
            if next_token.token_type == TokenType::Comparison &&
                (next_token.token_subtype == TokenSubType::Equals ||
                next_token.token_subtype == TokenSubType::NotEquals) {
                node = self.parse_equals_not_equals_comparison_expression(node)?;
            } else {
                break;
            }
        }

        Ok(node)
    }

    fn parse_arithmetic_expression_with_greater_less_comparisons(&mut self) -> Result<AstNode, ()> {
        let mut node = self.parse_arithmetic_expression()?;

        loop {
            let next_token = self.lexer.peek_token();
            if next_token.token_type == TokenType::Comparison &&
                !(next_token.token_subtype == TokenSubType::Equals ||
                    next_token.token_subtype == TokenSubType::NotEquals) {
                    node = self.parse_greater_less_comparison_expression(node)?;
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
        let mut node = self.parse_boolean_not()?;

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

    fn parse_boolean_not(&mut self) -> Result<AstNode, ()> {

        let next_token = self.lexer.peek_token();
        match next_token.token_type {
            TokenType::Not => {
                return self.parse_boolean_not_expression();
            },
            _ => (),
        }

        let mut node = self.parse_factor()?;

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

    fn parse_equals_not_equals_comparison_expression(
        &mut self, node: AstNode) -> Result<AstNode, ()> {

        let next_token = self.expect(TokenType::Comparison)?;
        let n_node = self.parse_arithmetic_expression_with_greater_less_comparisons()?;

        match next_token.token_subtype {
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
                let not_equals_node = AstNode::NotEquals(
                    Box::new(node),
                    Box::new(n_node),
                    Span::new(
                        next_token.line,
                        next_token.column,
                        next_token.length));
                Ok(not_equals_node)
            },
            _ => ice!("Got token {} when equals/not equals token was expected", next_token),
        }
    }

    fn parse_greater_less_comparison_expression(
        &mut self, node: AstNode) -> Result<AstNode, ()> {

        let next_token = self.expect(TokenType::Comparison)?;
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
            _ => ice!("Got token {} when less/less or eq/greater or eq/greater token was expected", next_token),
        }
    }

    fn parse_boolean_or_expression(&mut self, node: AstNode) -> Result<AstNode, ()> {
        let token = self.expect(TokenType::DoublePipe)?;
        let n_node = self.parse_arithmetic_expression_with_boolean_and()?;
        let boolean_or_node = AstNode::BooleanOr(
            Box::new(node),
            Box::new(n_node),
            Span::new(
                token.line,
                token.column,
                token.length,
            ));

        Ok(boolean_or_node)
    }

    fn parse_boolean_and_expression(&mut self, node: AstNode) -> Result<AstNode, ()> {
        let token = self.expect(TokenType::DoubleAmpersand)?;
        let n_node = self.parse_arithmetic_expression_with_equals_not_equals_comparisons()?;
        let boolean_or_node = AstNode::BooleanAnd(
            Box::new(node),
            Box::new(n_node),
            Span::new(
                token.line,
                token.column,
                token.length,
            ));

        Ok(boolean_or_node)
    }

    fn parse_boolean_not_expression(&mut self) -> Result<AstNode, ()> {

        let token = self.expect(TokenType::Not)?;

        let mut need_not = true;
        loop {
            match self.lexer.peek_token().token_type {
                TokenType::Not => {
                    self.expect(TokenType::Not)?;
                    need_not = !need_not;
                },
                _ => break,
            }
        }

        let mut not_node = self.parse_factor()?;

        if need_not {
            not_node = AstNode::Not(
                Box::new(not_node),
                Span::new(token.line, token.column, token.length),
            );
        }
        Ok(not_node)
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
                let n_node = self.parse_boolean_not()?;
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

#[cfg(test)]
mod tests {

    use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, DeclarationInfo, NodeInfo as Span};
    use crate::error_reporter::{ReportKind, ErrorReporter, Message};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semcheck::Type;
    use crate::token::{Token, TokenType, TokenSubType};
    use crate::error_reporter::null_reporter::NullReporter;

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
                tokens,
                pos: 0,
            }
        }
    }

    fn create_parser(
        tokens: Vec<Token>) -> (Parser, Rc<RefCell<NullReporter>>) {
        let reporter = Rc::new(RefCell::new(NullReporter::new()));
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                AstInteger::from(4),
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Float,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Double,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::String,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Boolean,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Minus(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(-85),
                                            Span::new(0, 0, 0))),
                                        Box::new(AstNode::Multiply(
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(4),
                                                Span::new(0, 0, 0)
                                            )),
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(8),
                                                Span::new(0, 0, 0)
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
                            Span::new(0, 0, 0)
                        )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node)
    }

    #[test]
    fn assignment_with_int_min_produces_correct_ast() {
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
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2147483648), 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Minus(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(-2147483648),
                                            Span::new(0, 0, 0))),
                                        Box::new(AstNode::Multiply(
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(4),
                                                Span::new(0, 0, 0)
                                            )),
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(8),
                                                Span::new(0, 0, 0)
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
                            Span::new(0, 0, 0)
                        )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node)
    }

    #[test]
    fn int_max_plus_one_generates_correct_ast() {
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
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2147483648), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                AstInteger::IntMaxPlusOne,
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node)
    }

    #[test]
    fn integer_larger_than_i32_max_plus_one_generates_correct_ast() {
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
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2147483649), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),


            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                AstInteger::Invalid(2147483649),
                                                Span::new(0, 0, 0)
                                            )
                                        ),
                                        DeclarationInfo::new_alt(
                                            Rc::new("a".to_string()),
                                            Type::Integer,
                                            0, 0, 0),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Identifier(
                                        Rc::new("ident".to_string()),
                                        Span::new(0, 0, 0))),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            Span::new(0, 0, 0)
                        )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                        AstInteger::from(4),
                                                        Span::new(0, 0, 0)
                                                    )
                                                ),
                                                Box::new(
                                                    AstNode::Integer(
                                                        AstInteger::from(8),
                                                        Span::new(0, 0, 0)
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
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                                                AstInteger::from(85),
                                                Span::new(0, 0, 0)
                                            )),
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(4),
                                                Span::new(0, 0, 0)
                                            )),
                                            ArithmeticInfo::new_alt(0, 0, 0)
                                        )),
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(8),
                                            Span::new(0, 0, 0),
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
                            Span::new(0, 0, 0)
                        )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Minus(
                                Box::new(AstNode::Plus(
                                    Box::new(AstNode::Plus(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(1),
                                            Span::new(0, 0, 0),
                                        )),
                                        Box::new(AstNode::Multiply(
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(2),
                                                Span::new(0, 0, 0),
                                            )),
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(3),
                                                Span::new(0, 0, 0),
                                            )),
                                            ArithmeticInfo::new_alt(0, 0, 0)
                                        )),
                                        ArithmeticInfo::new_alt(0, 0, 0),
                                    )),
                                    Box::new(AstNode::Multiply(
                                        Box::new(AstNode::Divide(
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(4),
                                                Span::new(0, 0, 0),
                                            )),
                                            Box::new(AstNode::Integer(
                                                AstInteger::from(5),
                                                Span::new(0, 0, 0),
                                            )),
                                            ArithmeticInfo::new_alt(0, 0, 0),
                                        )),
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(6),
                                            Span::new(0, 0, 0),
                                        )),
                                        ArithmeticInfo::new_alt(0, 0, 0),
                                    )),
                                    ArithmeticInfo::new_alt(0, 0, 0)
                                )),
                                Box::new(AstNode::Multiply(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(7),
                                        Span::new(0, 0, 0),
                                    )),
                                    Box::new(AstNode::Plus(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(8),
                                            Span::new(0, 0, 0),
                                        )),
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(9),
                                            Span::new(0, 0, 0),
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
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
                ],
                None,
                Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
                ],
                None,
                Span::new(0, 0, 0)),
            node);
    }

    #[test]
    fn returning_negative_number_produces_correct_ast() {
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
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 8, 7, 6),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 55, 44, 33),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::Return(
                            Some(Box::new(AstNode::Integer(
                                AstInteger::from(-2),
                                Span::new(55, 44, 33))
                            )),
                            ArithmeticInfo::new_alt(5, 4, 1),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::Return(
                            Some(Box::new(AstNode::Plus(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    Span::new(3, 4, 5)
                                    )),
                                Box::new(AstNode::Multiply(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(2),
                                            Span::new(55, 44, 33),
                                        )),
                                       Box::new(AstNode::Integer(
                                            AstInteger::from(3),
                                            Span::new(88, 77, 66),
                                        )),
                                        ArithmeticInfo::new_alt(1, 15, 53),
                                    )),
                                ArithmeticInfo::new_alt(8, 7, 6)
                            ))),
                            ArithmeticInfo::new_alt(5, 4, 1),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
                ],
                None,
                Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

           assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Less(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(2),
                                    Span::new(8, 9, 2),
                                )),
                                Span::new(5, 6, 7)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Boolean,
                                7, 6, 5),
                        )
                    ],
                    None,
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
                ],
                None,
                Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::While(
                            Box::new(AstNode::Boolean(
                                true,
                                Span::new(8, 6, 7)
                            )),
                            Box::new(AstNode::Block(vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(1),
                                        Span::new(1, 2, 3),
                                    )),
                                    DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    4, 5, 6)),
                                    ],
                                None,
                                Span::new(0, 0, 0),
                            )),
                            Span::new(5, 6, 7),
                        ),
                    ],
                    None,
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
                ],
                None,
                Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
                        AstNode::While(
                            Box::new(AstNode::Equals(
                                Box::new(AstNode::Identifier(
                                    Rc::new("aab".to_string()),
                                    Span::new(11, 22, 33),
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(23),
                                    Span::new(13, 14, 15))),
                                Span::new(12, 23, 34)
                            )),
                            Box::new(AstNode::Block(vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(1),
                                        Span::new(1, 2, 3),
                                    )),
                                    DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    4, 5, 6)),
                                    ],
                                None,
                                Span::new(0, 0, 0),
                            )),
                            Span::new(5, 6, 7),
                        ),
                    ],
                    None,
                    Span::new(0, 0, 0),
                    )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
                ],
                None,
                Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::LessOrEq(
                            Box::new(AstNode::Identifier(
                                Rc::new("aab".to_string()),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        None,
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Identifier(
                                Rc::new("aab".to_string()),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Double(
                                    1.23,
                                    Span::new(7, 6, 5),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                41, 51, 61)),
                            ],
                            None,
                            Span::new(0, 0, 0)
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::If(
                        Box::new(AstNode::GreaterOrEq(
                            Box::new(AstNode::Identifier(
                                Rc::new("aab".to_string()),
                                Span::new(11, 22, 33),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(23),
                                Span::new(13, 14, 15))),
                            Span::new(12, 23, 34),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(1),
                                    Span::new(1, 2, 3),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                4, 5, 6)),
                                ],
                            None,
                            Span::new(0, 0, 0),
                        )),
                        Some(Box::new(AstNode::If(
                            Box::new(AstNode::Greater(
                                Box::new(AstNode::Identifier(
                                    Rc::new("aab".to_string()),
                                    Span::new(11, 22, 33),
                                )),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(53),
                                    Span::new(13, 14, 15))),
                                Span::new(12, 23, 34),
                            )),
                            Box::new(AstNode::Block(vec![
                                AstNode::VariableDeclaration(
                                        Box::new(AstNode::Double(
                                            1.23,
                                            Span::new(7, 6, 5),
                                        )),
                                        DeclarationInfo::new_alt(
                                        Rc::new("b".to_string()),
                                        Type::Double,
                                        41, 51, 61))
                                ],
                                None,
                                Span::new(0, 0, 0),
                            )),
                            Some(Box::new(AstNode::Block(vec![
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Text(
                                        Rc::new("foo".to_string()),
                                        Span::new(71, 61, 51),
                                    )),
                                    DeclarationInfo::new_alt(
                                    Rc::new("c".to_string()),
                                    Type::String,
                                    42, 52, 62)),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            ))),
                            Span::new(55, 66, 77),
                        ))),
                        Span::new(5, 6, 7),
                    ),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::FunctionCall(
                        vec![],
                        Rc::new("bar".to_string()),
                        Span::new(5, 6, 7)),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::FunctionCall(
                        vec![
                            AstNode::Integer(
                                AstInteger::from(2),
                                Span::new(0, 0, 0)),
                            AstNode::Plus(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    Span::new(0, 0, 0))),
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    Span::new(0, 0, 0))),
                                ArithmeticInfo::new_alt(0, 0, 0)),
                        ],
                        Rc::new("bar".to_string()),
                        Span::new(5, 6, 7)),
                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

        assert_eq!(
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Plus(
                            Box::new(AstNode::Integer(
                                AstInteger::from(4),
                                Span::new(0, 0, 0))),
                            Box::new(AstNode::FunctionCall(
                                vec![
                                    AstNode::Integer(
                                    AstInteger::from(2),
                                    Span::new(0, 0, 0))
                                ],
                                Rc::new("bar".to_string()),
                                Span::new(5, 6, 7))),
                            ArithmeticInfo::new_alt(0, 0, 0))),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            0, 0, 0)),

                ],
                None,
                Span::new(0, 0, 0),
                )),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0))
            ],
            None,
            Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                            Span::new(0, 0, 0))),
                        function_info)
                ],
                None,
                Span::new(0, 0, 0)),
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

        assert_eq!(reporter.borrow().errors(), 0);

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
                Span::new(0, 0, 0)),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(2, 3, 1),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![  ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(5, 6, 1),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![  ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(8, 9, 1),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![  ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(8, 1, 1),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![  ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(1, 1, 23),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![  ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(4, 4, 2),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![  ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(0, 0,0),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(5, 6,6),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(5, 6,6),
                "".to_owned()),
            messages[0]);

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
                                                    AstInteger::from(14),
                                                    Span::new(0, 0, 0)
                                                )
                                            ),
                                            DeclarationInfo::new_alt(
                                                Rc::new("bbb".to_string()),
                                                Type::Integer,
                                                0, 0, 0),
                                        ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(6, 7,8),
                "".to_owned()),
            messages[0]);

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
                                                    AstInteger::from(14),
                                                    Span::new(0, 0, 0)
                                                )
                                            ),
                                            DeclarationInfo::new_alt(
                                                Rc::new("bbb".to_string()),
                                                Type::Integer,
                                                0, 0, 0),
                                        ),
                                ],
                                None,
                                Span::new(0, 0, 0)
                            )),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(9, 8,7),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![],
                            None,
                            Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
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

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(8,7,6),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![],
                            None,
                            Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }


    #[test]
    fn boolean_and_is_parsed_correctly() {
        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if a && b { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::DoubleAmpersand, TokenSubType::NoSubType, 8, 9, 10),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                               Box::new(
                                   AstNode::BooleanAnd(
                                       Box::new(
                                           AstNode::Identifier(
                                               Rc::new("a".to_owned()),
                                               Span::new(0, 0, 0))),
                                       Box::new(
                                           AstNode::Identifier(
                                               Rc::new("b".to_owned()),
                                               Span::new(0, 0, 0))),
                                       Span::new(8, 9, 10),
                                   )),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn boolean_or_is_parsed_correctly() {
        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if a && b { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::DoublePipe, TokenSubType::NoSubType, 8, 9, 10),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                                Box::new(
                                    AstNode::BooleanOr(
                                        Box::new(
                                            AstNode::Identifier(
                                                Rc::new("a".to_owned()),
                                                Span::new(0, 0, 0))),
                                        Box::new(
                                            AstNode::Identifier(
                                                Rc::new("b".to_owned()),
                                                Span::new(0, 0, 0))),
                                        Span::new(8, 9, 10),
                                    )),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn boolean_and_boolean_or_have_correct_precedence_with_other_operators() {

        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if a || b && c || d > 2 == e { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::DoublePipe, TokenSubType::NoSubType, 8, 9, 10),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::DoubleAmpersand, TokenSubType::NoSubType, 28, 91, 140),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("c".to_string())), 0, 0, 0),
            Token::new(TokenType::DoublePipe, TokenSubType::NoSubType, 99, 88, 77),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("d".to_string())), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::Greater, 1, 2, 3),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::Equals, 11, 22, 33),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("e".to_string())), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                                Box::new(
                                    AstNode::BooleanOr(
                                        Box::new(
                                            AstNode::BooleanOr(
                                                Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("a".to_owned()),
                                                        Span::new(0, 0, 0))),
                                                Box::new(
                                                    AstNode::BooleanAnd(
                                                        Box::new(
                                                            AstNode::Identifier(
                                                                Rc::new("b".to_owned()),
                                                                Span::new(0, 0, 0))),
                                                        Box::new(
                                                            AstNode::Identifier(
                                                                Rc::new("c".to_owned()),
                                                                Span::new(0, 0, 0))),
                                                        Span::new(28, 91, 140))),
                                                Span::new(8, 9, 10))),
                                        Box::new(
                                            AstNode::Equals(
                                                Box::new(AstNode::Greater(
                                                    Box::new(
                                                        AstNode::Identifier(
                                                            Rc::new("d".to_owned()),
                                                            Span::new(0, 0, 0))),
                                                    Box::new(
                                                        AstNode::Integer(
                                                            AstInteger::Int(2),
                                                            Span::new(0, 0, 0))),
                                                    Span::new(1, 2, 3))),
                                                Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("e".to_owned()),
                                                        Span::new(0, 0, 0))),
                                                Span::new(11, 22, 33))),
                                        Span::new(99, 88, 77))),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn boolean_not_is_parsed_correctly() {
       let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if !a { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Not, TokenSubType::NoSubType, 8, 9, 10),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                               Box::new(
                                   AstNode::Not(
                                       Box::new(
                                           AstNode::Identifier(
                                               Rc::new("a".to_owned()),
                                               Span::new(0, 0, 0))),
                                       Span::new(8, 9, 10),
                                   )),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn can_use_boolean_not_multiple_times_on_same_value() {
        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if !!a { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Not, TokenSubType::NoSubType, 8, 9, 10),
            Token::new(TokenType::Not, TokenSubType::NoSubType, 9, 10, 11),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                               Box::new(
                                   AstNode::Not(
                                       Box::new(
                                           AstNode::Not(
                                               Box::new(
                                                   AstNode::Identifier(
                                                       Rc::new("a".to_owned()),
                                                       Span::new(0, 0, 0))),
                                               Span::new(9, 10, 11))),
                                       Span::new(8, 9, 10)
                                   )),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn boolean_not_has_correct_precedence() {
        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if 3 + !a * (2 || 4) { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(3), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Not, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Multiply, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::DoublePipe, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                                Box::new(
                                    AstNode::Plus(
                                        Box::new(
                                            AstNode::Integer(AstInteger::Int(3), Span::new(0, 0, 0))),
                                        Box::new(
                                            AstNode::Multiply(
                                                Box::new(
                                                    AstNode::Not(
                                                        Box::new(
                                                            AstNode::Identifier(
                                                                Rc::new("a".to_owned()),
                                                                Span::new(0, 0, 0))),
                                                        Span::new(0, 0, 0))),
                                                Box::new(
                                                    AstNode::BooleanOr(
                                                        Box::new(
                                                            AstNode::Integer(AstInteger::Int(2), Span::new(0, 0, 0))),
                                                        Box::new(
                                                            AstNode::Integer(AstInteger::Int(4), Span::new(0, 0, 0))),
                                                        Span::new(0, 0, 0)
                                                    )
                                                ),
                                                ArithmeticInfo {
                                                    node_type: Type::Uninitialized,
                                                    node_info: Span::new(0, 0, 0),
                                                })),
                                        ArithmeticInfo {
                                            node_type: Type::Uninitialized,
                                            node_info: Span::new(0, 0, 0)
                                        })),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }


    #[test]
    fn equals_has_lower_precedence_than_greater_less_operators() {

        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if a > 2 == b <= 8 { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::Greater, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::Equals, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::LessOrEq, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                                Box::new(
                                    AstNode::Equals(
                                        Box::new(
                                        AstNode::Greater(
                                            Box::new(
                                                AstNode::Identifier(
                                                    Rc::new("a".to_owned()),
                                                    Span::new(0, 0, 0))),
                                            Box::new(
                                                AstNode::Integer(
                                                    AstInteger::Int(2),
                                                    Span::new(0, 0, 0))),
                                            Span::new(0, 0, 0))),
                                        Box::new(
                                            AstNode::LessOrEq(
                                                Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("b".to_owned()),
                                                        Span::new(0, 0, 0))),
                                                Box::new(
                                                    AstNode::Integer(
                                                        AstInteger::Int(8),
                                                        Span::new(0, 0, 0))),
                                                Span::new(0, 0, 0))),
                                        Span::new(0, 0, 0),
                                    )),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn not_equals_has_lower_precedence_than_greater_less_operators() {
        let (mut parser, reporter) = create_parser(vec![
            // fn foo() : int {
            Token::new(TokenType::Fn, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("foo".to_string())), 0, 0, 0),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),

            // if a > 2 != b <= 8 { }
            Token::new(TokenType::If, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::Greater, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::NotEquals, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::Comparison, TokenSubType::LessOrEq, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(vec![
                            AstNode::If(
                                Box::new(
                                    AstNode::NotEquals(
                                        Box::new(
                                            AstNode::Greater(
                                                Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("a".to_owned()),
                                                        Span::new(0, 0, 0))),
                                                Box::new(
                                                    AstNode::Integer(
                                                        AstInteger::Int(2),
                                                        Span::new(0, 0, 0))),
                                                Span::new(0, 0, 0))),
                                        Box::new(
                                            AstNode::LessOrEq(
                                                Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("b".to_owned()),
                                                        Span::new(0, 0, 0))),
                                                Box::new(
                                                    AstNode::Integer(
                                                        AstInteger::Int(8),
                                                        Span::new(0, 0, 0))),
                                                Span::new(0, 0, 0))),
                                        Span::new(0, 0, 0),
                                    )),
                                Box::new(
                                    AstNode::Block(vec![], None, Span::new(0,0,0))),
                                None,
                                Span::new(0, 0, 0))
                        ],
                                       None,
                                       Span::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )],
                           None,
                           Span::new(0, 0, 0),
            ),
            node);
    }
}
