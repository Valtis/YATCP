use crate::lexer::Lexer;
use crate::lexer::token::{Token, TokenType, TokenSubType};

use crate::error_reporter::{ErrorReporter, ReportKind};

use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, Span as Span, DeclarationInfo, ExtraDeclarationInfo};

use crate::semcheck::Type;

use std::cell::RefCell;
use std::rc::Rc;

pub struct Parser {
    lexer: Box<dyn Lexer>,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>,
}

impl Parser {
    pub fn new(
        lexer: Box<dyn Lexer>,
        error_reporter: Rc<RefCell<dyn ErrorReporter>>) -> Parser {
        Parser {
            lexer: lexer,
            error_reporter: error_reporter,
        }
    }

    pub fn parse(&mut self) -> AstNode {

        let top_level_tokens = vec![TokenType::Fn, TokenType::Extern];
        let mut nodes = vec![];
        loop {
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
                    return AstNode::Block{
                        statements: nodes,
                        block_symbol_table_entry: None,
                        span: Span::new(0, 0, 0),
                    },
                _ => {
                    self.report_unexpected_token_mul(
                        &top_level_tokens, &token);
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

        let type_token = if self.lexer.peek_token().token_type == TokenType::Colon {
            self.expect(TokenType::Colon)?;
            self.expect(TokenType::VarType)?
        } else {
            Token {
                token_type: TokenType::VarType,
                token_subtype: TokenSubType::VoidType,
                line: 0,
                column: 0,
                length: 0
            }
        };

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

        let type_token = if self.lexer.peek_token().token_type == TokenType::Colon {
            self.expect(TokenType::Colon)?;
            self.expect(TokenType::VarType)?
        } else {
            Token {
                token_type: TokenType::VarType,
                token_subtype: TokenSubType::VoidType,
                line: 0,
                column: 0,
                length: 0
            }
        };

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

                if let TokenType::LBracket = self.lexer.peek_token().token_type {
                    self.expect(TokenType::LBracket)?;
                    self.expect(TokenType::RBracket)?;

                    // update type to array type
                    params.last_mut().unwrap().variable_type = Type::Reference(Box::new(params.last().unwrap().variable_type.get_array_type_from_basic_type()));
                }

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

        let block = AstNode::Block {
            statements: nodes,
            block_symbol_table_entry: None,
            span: Span::new(0, 0, 0),
        };

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
                TokenType::Let => {
                    let res = self.parse_variable_declaration();
                    if let Err(_) = res {
                        self.skip_to_first_of(vec![TokenType::SemiColon]);
                    }
                    self.expect(TokenType::SemiColon)?;
                    res
                }
                TokenType::LBrace =>
                    self.parse_block(),
                TokenType::Identifier =>
                    self.parse_function_call_or_assignment(),
                TokenType::Return =>
                    self.parse_return_statement(),
                TokenType::While =>
                    self.parse_while_statement(),
                TokenType::For =>
                    self.parse_for_statement(),
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

        // Array parse & custom error handling for missing initialization
        let mut next_token = self.lexer.peek_token();
        let extra_info = match next_token.token_type {
            TokenType::LBracket => {
                self.lexer.next_token();
                let dimension = match self.parse_array_declaration() {
                    Ok(x) => x,
                    Err(_) => {
                        self.skip_to_first_of(
                            vec![
                                TokenType::SemiColon
                            ]
                        );
                        return Err(());
                    }
                };
                next_token = self.lexer.peek_token();

                Some(ExtraDeclarationInfo::ArrayDimension(dimension))
            },
            _ => None,
        };


        let is_array = if let Some(_) = extra_info {
            true
        } else {
            false
        };

        // Custom error handling for case where expression is missing
        match next_token.token_type {
            TokenType::Equals => {
                    self.lexer.next_token(); // pop the token we peeked before array handling
            }, // ok
            TokenType::SemiColon => {
                self.report_error(
                    ReportKind::SyntaxError,
                    Span::new(identifier.line, identifier.column, identifier.length),
                    format!("Variable declaration must be followed by initialization"));
                return Err(());
            },
            _ => {
                self.report_unexpected_token(
                    TokenType::Equals,
                    &next_token);
                return Err(());
            },
        }


        let mut declaration_info = DeclarationInfo::new_with_extra_info(&identifier, &var_type, extra_info);

        if is_array {
            declaration_info.variable_type = match declaration_info.variable_type {
                Type::Integer => Type::IntegerArray,
                Type::Boolean => Type::BooleanArray,
                _ => {
                    self.report_error(
                        ReportKind::SyntaxError,
                        declaration_info.span.clone(),
                        format!("{} is not valid base type for an array", declaration_info.variable_type)
                    );
                    Type::Invalid
                },
            }
        }

        let node = self.parse_expression()?;
        let declaration = AstNode::VariableDeclaration(
                Box::new(node),
                declaration_info);

        Ok(declaration)
    }

    fn parse_array_declaration(&mut self) -> Result<Vec<AstInteger>, ()> {

        // TODO: Support for multidimensional arrays
        let size = self.parse_expression()?;
        self.expect(TokenType::RBracket)?;

        if let AstNode::Integer(dim, _) = size {
            Ok(vec![AstInteger::from(dim)])
        } else {
            self.report_error(
                ReportKind::TypeError,
                size.span(),
                "Array dimension must be an integer constant".to_owned());
            Err(())
        }
    }

    fn parse_function_call_or_assignment(&mut self) -> Result<AstNode, ()> {
        let identifier = self.expect(TokenType::Identifier)?;
        let token  = self.lexer.peek_token();
        match token.token_type {

            TokenType::LBracket |
            TokenType::Equals |
            TokenType::Plus |
            TokenType::Minus |
            TokenType::Star |
            TokenType::ForwardSlash |
            TokenType::Percentage => {
                let node = self.parse_assignment_expression(identifier)?;
                self.expect(TokenType::SemiColon)?;
                return Ok(node);

            }
            TokenType::LParen => {
                let node = self.parse_function_call(identifier)?;
                self.expect(TokenType::SemiColon)?;
                return Ok(node);
            }

            _ => {
                self.report_unexpected_token_mul(
                    &vec![
                        TokenType::Equals,
                        TokenType::Plus,
                        TokenType::Minus,
                        TokenType::Star,
                        TokenType::ForwardSlash,
                        TokenType::Percentage,
                        TokenType::LParen,
                        TokenType::LBracket],
                    &token);
                Err(())
            },
        }
    }

    fn parse_assignment_expression(&mut self, identifier: Token) -> Result<AstNode, ()> {
        match self.lexer.peek_token().token_type {
            token_type @ TokenType::Equals |
            token_type @ TokenType::Plus |
            token_type @ TokenType::Minus |
            token_type @ TokenType::Star |
            token_type @ TokenType::ForwardSlash |
            token_type @ TokenType::Percentage => {
                self.parse_assignment(identifier, token_type)
            },
            TokenType::LBracket => {
                self.parse_array_assignment(identifier)
            },
            bad_token => ice!("Unexpected token {}", bad_token),
        }
    }

    fn parse_assignment(&mut self, identifier: Token, token_type: TokenType) -> Result<AstNode, ()> {
        let op = self.expect(token_type)?;

        if token_type != TokenType::Equals {
            self.expect(TokenType::Equals)?;
        }
        let expression_node = self.parse_expression()?;

        let name = if let TokenSubType::Identifier(ref ident) = identifier.token_subtype {
            ident.clone()
        } else {
            ice!("Non-identifier token '{}' passed to parse_assignment",
                identifier);
        };

        let expression_node = match token_type {
            TokenType::Plus => AstNode::Plus(
                Box::new(AstNode::Identifier(name.clone(), Span::from(&identifier))),
                Box::new(expression_node),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Minus=> AstNode::Minus(
                Box::new(AstNode::Identifier(name.clone(), Span::from(&identifier))),
                Box::new(expression_node),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Star => AstNode::Multiply(
                Box::new(AstNode::Identifier(name.clone(), Span::from(&identifier))),
                Box::new(expression_node),
                ArithmeticInfo::new(&op),
            ),
            TokenType::ForwardSlash => AstNode::Divide(
                Box::new(AstNode::Identifier(name.clone(), Span::from(&identifier))),
                Box::new(expression_node),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Percentage=> AstNode::Modulo(
                Box::new(AstNode::Identifier(name.clone(), Span::from(&identifier))),
                Box::new(expression_node),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Equals => expression_node,
            bad => ice!("Bad expression type {}", bad),
        };

        Ok(AstNode::VariableAssignment(
            Box::new(expression_node),
            name,
            Span::from(&identifier)))
    }

    fn parse_function_call(
        &mut self,
        identifier: Token) -> Result<AstNode, ()>  {

        let name = if let TokenSubType::Identifier(ref ident) = identifier.token_subtype {
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
            Span::from(identifier)
            ))
    }

    fn parse_array_assignment(&mut self, identifier: Token) -> Result<AstNode, ()> {
        self.expect(TokenType::LBracket)?;

        let name = if let TokenSubType::Identifier(ref ident) = identifier.token_subtype {
            ident.clone()
        } else {
            ice!("Non-identifier token '{}' passed to parse_function_call",
                identifier);
        };

        let index_expression = self.parse_expression()?;
        self.expect(TokenType::RBracket)?;

        let op = self.expect_one_of(vec![
            TokenType::Equals,
            TokenType::Plus,
            TokenType::Minus,
            TokenType::Star,
            TokenType::ForwardSlash,
            TokenType::Percentage,
        ])?;

        if op.token_type != TokenType::Equals {
            self.expect(TokenType::Equals)?;
        }

        let assignment_expression = self.parse_expression()?;

        let get_access = || {
            AstNode::ArrayAccess {
                index_expression: Box::new(index_expression.clone()),
                indexable_expression: Box::new(AstNode::Identifier(name.clone(), Span::from(&identifier))),
            }
        };
        let assignment_expression  = match op.token_type {
            TokenType::Plus => AstNode::Plus(
                Box::new(get_access()),
                Box::new(assignment_expression),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Minus=> AstNode::Minus(
                Box::new(get_access()),
                Box::new(assignment_expression),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Star => AstNode::Multiply(
                Box::new(get_access()),
                Box::new(assignment_expression),
                ArithmeticInfo::new(&op),
            ),
            TokenType::ForwardSlash => AstNode::Divide(
                Box::new(get_access()),
                Box::new(assignment_expression),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Percentage=> AstNode::Modulo(
                Box::new(get_access()),
                Box::new(assignment_expression),
                ArithmeticInfo::new(&op),
            ),
            TokenType::Equals => assignment_expression,
            bad => ice!("Bad expression type {}", bad),
        };


        Ok(AstNode::ArrayAssignment{
            index_expression: Box::new(index_expression),
            assignment_expression: Box::new(assignment_expression),
            variable_name: name,
            span: Span::from(&identifier) })
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


    // syntactic sugar around while statement
    //
    //  for (init_statement,init_statement; condition; post-expression, post-expression) {
    //      body
    //  }
    //
    //  generates
    //
    //  {
    //      init_statements;
    //      while condition {
    //          body
    //          post-expressions;
    //      }
    //   }
    //
    //  Init statement may be variable declaration, or assignment expression. Other statements are not allowed
    //  Post expression must be variable assignment

    fn parse_for_statement(&mut self) -> Result<AstNode, ()> {

        let for_node = self.expect(TokenType::For)?;

        let mut statements = vec![];
        let mut post_statements = vec![];

        loop {
            let t_type = self.lexer.peek_token().token_type;
            let node = if t_type == TokenType::SemiColon {
                break;
            } else if t_type  == TokenType::Let {
                self.parse_variable_declaration()
            } else if t_type == TokenType::Identifier {
                let identifier = self.lexer.next_token();
                self.parse_assignment_expression(identifier)
            } else {
                let span = Span::from(self.lexer.peek_token());
                self.report_error(
                    ReportKind::SyntaxError,
                    span,
                    "Expected variable initialization or assignment".to_owned()
                );
                Err(())
            };

            // custom error handling, try skipping and parsing rest of the statements
            let s = match node {
                Err(_) | Ok(AstNode::ErrorNode) => {
                    self.skip_to_first_of(
                    vec![
                        TokenType::LBrace,
                        TokenType::RBrace,
                        TokenType::SemiColon,
                        ]
                    );
                    AstNode::ErrorNode
                }
                Ok(x) => x,
            };

            statements.push(s);

            if self.lexer.peek_token().token_type == TokenType::Comma {
               self.lexer.next_token();
            } else {
                break;
            }
        }

        self.expect(TokenType::SemiColon)?;

        let cond_expression =  if self.lexer.peek_token().token_type == TokenType::SemiColon {
            AstNode::Boolean(true, Span::new(0, 0, 0))
        } else {
            match self.parse_expression() {
                Err(_) | Ok(AstNode::ErrorNode) => {
                    self.skip_to_first_of(
                        vec![
                            TokenType::LBrace,
                            TokenType::RBrace,
                            TokenType::SemiColon,
                        ]
                    );

                    AstNode::ErrorNode
                }
                Ok(x) => x
            }
        };

        self.expect(TokenType::SemiColon)?;

        loop {

            if self.lexer.peek_token().token_type == TokenType::LBrace {
               break;
            }

            let statement = match self.expect(TokenType::Identifier) {
                Ok(identifier) if identifier.token_type == TokenType::Identifier => {
                    match self.parse_assignment_expression(identifier) {
                        Err(_) | Ok(AstNode::ErrorNode) => {
                            self.skip_to_first_of(
                                vec![
                                    TokenType::LBrace,
                                    TokenType::RBrace,
                                    TokenType::SemiColon,
                                ]
                            );
                            AstNode::ErrorNode
                        },
                        Ok(x) => x,
                    }
                },
                _ =>  {
                    self.skip_to_first_of(
                        vec![
                            TokenType::LBrace,
                            TokenType::RBrace,
                            TokenType::SemiColon,
                        ]
                    );
                    AstNode::ErrorNode
                }
            };


            post_statements.push(statement);

            if self.lexer.peek_token().token_type == TokenType::Comma {
                self.lexer.next_token();
            } else {
                break;
            }
        }

        let mut while_block = self.parse_block()?;

        if let AstNode::Block{ref mut statements, .. } = while_block {
            for s in post_statements {
                statements.push(s);
            }
        } else {
            ice!("Non-block statement {} when block statement expected", while_block);
        }

        statements.push(
            AstNode::While(
                Box::new(cond_expression),
                Box::new(while_block),
                Span::from(for_node)));

        let block = AstNode::Block{
            statements: statements,
            block_symbol_table_entry: None,
            span: Span::new(0,0,0)
        };

        Ok(block)
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
            if next_token.token_type == TokenType::DoubleEquals &&
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
            if next_token.token_type == TokenType::DoubleEquals &&
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
                        next_token.token_type));

                // consume the token
                self.lexer.next_token();
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
                TokenType::Star | TokenType::ForwardSlash | TokenType::Percentage =>
                    node = self.parse_mult_divide_modulo_expression(node)?,
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_boolean_not(&mut self) -> Result<AstNode, ()> {

        let next_token = self.lexer.peek_token();
        match next_token.token_type {
            TokenType::Exclamation => {
                return self.parse_boolean_not_expression();
            },
            _ => (),
        }

        let node = self.parse_factor_with_member_or_array_access()?;

        Ok(node)
    }

    fn parse_factor_with_member_or_array_access(&mut self) -> Result<AstNode, ()> {

        let mut expr = self.parse_factor()?;

        loop {
            let next_token = self.lexer.peek_token();
            match next_token.token_type {
                TokenType::Dot => {
                    expr = self.parse_member_access(expr)?;
                },
                TokenType::LBracket => {
                    expr =
                        self.parse_array_access(expr)?;
                }
                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_array_access(&mut self, indexable_expression: AstNode) -> Result<AstNode, ()> {
        self.expect(TokenType::LBracket)?;
        let index_expression = self.parse_expression()?;
        self.expect(TokenType::RBracket)?;

        Ok(AstNode::ArrayAccess {
            index_expression: Box::new(index_expression),
            indexable_expression: Box::new(indexable_expression),
        })
    }

    fn parse_member_access(&mut self, expression: AstNode) -> Result<AstNode, ()> {
        let dot = self.expect(TokenType::Dot)?;
        let member = self.expect(TokenType::Identifier)?;

        let name = if let TokenSubType::Identifier(id) = member.token_subtype {
            id
        } else {
            ice!("Invalid token '{:#?'} received when identifier was expected");
        };



        Ok(AstNode::MemberAccess {
            object: Box::new(expression),
            member: Box::new(AstNode::Identifier(name, Span::new(member.line, member.column, member.length))),
            span: Span::new(dot.line, dot.column, dot.length),
        })
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
                        TokenSubType::Identifier(ref name) =>
                            Ok(AstNode::Identifier(
                                name.clone(),
                                Span::new(token.line, token.column, token.length))),
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

        let next_token = self.expect(TokenType::DoubleEquals)?;
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

        let next_token = self.expect(TokenType::DoubleEquals)?;
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

        let token = self.expect(TokenType::Exclamation)?;

        let next_token = self.lexer.peek_token();

        let node = if next_token.token_type == TokenType::Exclamation {
            self.parse_boolean_not_expression()?
        } else {
            self.parse_factor_with_member_or_array_access()?
        };

        let not_node = AstNode::Not(
            Box::new(node),
            Span::new(token.line, token.column, token.length),
        );

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

    fn parse_mult_divide_modulo_expression(&mut self, node: AstNode) -> Result<AstNode, ()> {

        let next_token = self.lexer.peek_token();
        match next_token.token_type {
            TokenType::Star => {
                self.lexer.next_token();
                let n_node = self.parse_boolean_not()?;
                let mult_node = AstNode::Multiply(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));

                Ok(mult_node)
            },
            TokenType::ForwardSlash => {
                self.lexer.next_token();
                let n_node = self.parse_boolean_not()?;
                let div_node = AstNode::Divide(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));
                Ok(div_node)
            },
            TokenType::Percentage => {
                self.lexer.next_token();
                let n_node = self.parse_boolean_not()?;
                let modulo_node = AstNode::Modulo(
                    Box::new(node),
                    Box::new(n_node),
                    ArithmeticInfo::new(&next_token));
                Ok(modulo_node)
            },
            _ => Ok(node),
        }
    }

    fn expect(&mut self, token_type: TokenType) -> Result<Token, ()> {
        let next_token = self.lexer.peek_token();

        if next_token.token_type != token_type {
            self.report_unexpected_token(token_type, &next_token);

            // with these tokens, show the previous token as well, as the missing token is probably supposed to be after it
            let previous_note_tokens = vec![
                TokenType::SemiColon,
                TokenType::LBrace,
                TokenType::RBrace,
                TokenType::LBracket,
                TokenType::RBracket,
            ];
            // if token is semicolon, we probably want to highlight the previous token as well
            if previous_note_tokens.contains(&token_type) && self.lexer.previous_token() != None {
                let prev_token = self.lexer.previous_token().unwrap();
                self.report_error(
                    ReportKind::Note,
                    Span::from(prev_token),
                    "Token likely missing here".to_owned(),
                );
            }

            Err(())
        } else {
            self.lexer.next_token();
            Ok(next_token)
        }
    }

    fn expect_one_of(&mut self, types: Vec<TokenType>) -> Result<Token, ()> {
        let next_token = self.lexer.peek_token();

        if types.iter().any(|t| *t == next_token.token_type) {
            self.lexer.next_token();
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
