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
                    return AstNode::Block(
                            nodes,
                            None,
                            Span::new(0, 0, 0),
                        ),
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
                next_token = self.lexer.next_token();

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
                if !is_array {
                    self.lexer.next_token(); // pop the token we peeked before array handling
                }
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

        if let AstNode::Block(ref mut block_statements, _, _) = while_block {
            for s in post_statements {
                block_statements.push(s);
            }
        } else {
            ice!("Non-block statement {} when block statement expected", while_block);
        }

        statements.push(
            AstNode::While(
                Box::new(cond_expression),
                Box::new(while_block),
                Span::from(for_node)));

        let block = AstNode::Block(statements, None, Span::new(0,0,0));
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
                        next_token));

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

#[cfg(test)]
mod tests {

    use crate::ast::{AstNode, AstInteger, ArithmeticInfo, FunctionInfo, DeclarationInfo, Span as Span, ExtraDeclarationInfo};
    use crate::error_reporter::{ReportKind, ErrorReporter, Message};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::semcheck::Type;
    use crate::lexer::token::{Token, TokenType, TokenSubType};
    use crate::error_reporter::null_reporter::NullReporter;

    use std::rc::Rc;
    use std::cell::RefCell;

    struct TestLexer {
        tokens: Vec<Token>,
        current_token: Option<Token>,
        next_token: Option<Token>,
        pos: usize,
    }

    impl Lexer for TestLexer {
        fn next_token(&mut self) -> Token {

            if self.next_token != None {
                let t = self.next_token.clone().unwrap();
                self.next_token = None;
                return t;
            }

            let token = if self.pos < self.tokens.len() {
                self.tokens[self.pos].clone()
            } else {
                Token::new(TokenType::Eof, TokenSubType::NoSubType, 0, 0, 0)
            };

            self.pos += 1;

            self.current_token = Some(token.clone());
            token
        }

        fn peek_token(&mut self) -> Token {
            if let Some(ref t) = self.next_token {
                t.clone()
            } else {
                let t = self.next_token();
                self.next_token = Some(t.clone());
                t
            }
        }

        fn current_token(&self) -> Option<Token> {
            self.current_token.clone()
        }
    }

    impl TestLexer {
        fn new(tokens: Vec<Token>) -> TestLexer {
            TestLexer {
                tokens,
                pos: 0,
                current_token: None,
                next_token: None,
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
                Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
                Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
                Token::new(TokenType::Number, TokenSubType::IntegerNumber(85), 0, 0, 0),
                Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
                Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
                Token::new(TokenType::Star, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2147483648), 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
                   Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(3), 0, 0, 0),
            Token::new(TokenType::Plus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::ForwardSlash, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(5), 0, 0, 0),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(6), 0, 0, 0),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(7), 0, 0, 0),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Star, TokenSubType::NoSubType, 1, 15, 53),
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
           Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
           Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
           Token::new(TokenType::DoubleEquals, TokenSubType::Less, 5, 6, 7),
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
               Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
               Token::new(TokenType::DoubleEquals, TokenSubType::Equals, 12, 23, 34),
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
               Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::DoubleEquals, TokenSubType::LessOrEq, 12, 23, 34),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::DoubleEquals, TokenSubType::GreaterOrEq, 12, 23, 34),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::DoubleEquals, TokenSubType::GreaterOrEq, 12, 23, 34),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(1), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Else, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::If, TokenSubType::NoSubType, 55, 66, 77),
            Token::new(
                    TokenType::Identifier,
                    TokenSubType::Identifier(Rc::new("aab".to_string())), 11, 22, 33),
            Token::new(TokenType::DoubleEquals, TokenSubType::Greater, 12, 23, 34),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
    fn function_call_with_missing_semicolon_is_rejected() {
        /*
            fn foo() : int {
                bar()
                return 0;
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
            Token::new(TokenType::Return, TokenSubType::NoSubType, 5, 6, 7),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(0), 9, 8, 7),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 1, 2, 3),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(5, 6, 7),
                "".to_owned()),
            messages[0]);



        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(vec![
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
             Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
    fn function_definition_with_array_produces_correct_ast() {
        /*
            fn foo(a : int[], b : string) : void {

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
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),
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
                Type::Reference(Box::new(Type::IntegerArray)),
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
        /*
            fn () : int {}
        */
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
        /*
            fn foo() : int {
                let aaaaa: int;
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
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
        /*

            fn foo () : int {
                let aaaaa int = 16l
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

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                    TokenType::Identifier,
                    TokenSubType::Identifier(Rc::new("aaaaa".to_string())), 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 6, 7, 8),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(16), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                    TokenType::Identifier,
                    TokenSubType::Identifier(Rc::new("bbb".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
        /*
            fn foo () : int {
                let a: int = 4 +;
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

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
                    Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
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
            Token::new(TokenType::DoubleEquals, TokenSubType::Greater, 1, 2, 3),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::DoubleEquals, TokenSubType::Equals, 11, 22, 33),
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
           Token::new(TokenType::Exclamation, TokenSubType::NoSubType, 8, 9, 10),
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
            Token::new(TokenType::Exclamation, TokenSubType::NoSubType, 8, 9, 10),
            Token::new(TokenType::Exclamation, TokenSubType::NoSubType, 9, 10, 11),
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
            Token::new(TokenType::Exclamation, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 0, 0, 0),
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
                                                    span: Span::new(0, 0, 0),
                                                })),
                                        ArithmeticInfo {
                                            node_type: Type::Uninitialized,
                                            span: Span::new(0, 0, 0)
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
            Token::new(TokenType::DoubleEquals, TokenSubType::Greater, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::DoubleEquals, TokenSubType::Equals, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::DoubleEquals, TokenSubType::LessOrEq, 0, 0, 0),
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
            Token::new(TokenType::DoubleEquals, TokenSubType::Greater, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 0, 0, 0),
            Token::new(TokenType::DoubleEquals, TokenSubType::NotEquals, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 0, 0, 0),
            Token::new(TokenType::DoubleEquals, TokenSubType::LessOrEq, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(8), 0, 0, 0),
            Token::new(TokenType::LBrace, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),

            // }
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();

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

    #[test]
    fn one_dimensional_integer_array_declaration_is_parsed_correctly() {
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
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(6), 5, 15, 4),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
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
                                                Span::new(1, 2, 3),
                                            ),
                                        ),
                                        DeclarationInfo {
                                            name: Rc::new("a".to_string()),
                                            span: Span::new(8, 12, 2),
                                            variable_type: Type::IntegerArray,
                                            extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::from(6)])),
                                        }
                                    )
                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }


    #[test]
    fn one_dimensional_boolean_array_declaration_is_parsed_correctly() {
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
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::BooleanType, 0, 0, 0),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(6), 5, 15, 4),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 1, 2, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
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
                                                Span::new(1, 2, 3),
                                            ),
                                        ),
                                        DeclarationInfo {
                                            name: Rc::new("a".to_string()),
                                            span: Span::new(8, 12, 2),
                                            variable_type: Type::BooleanArray,
                                            extra_info: Some(ExtraDeclarationInfo::ArrayDimension(vec![AstInteger::from(6)])),
                                        }
                                    )
                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }


    #[test]
    fn array_declaration_with_non_integer_number_dimension_is_reported() {
        /*
            fn foo() : int {
                let a: [32.2] = 4;
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

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::FloatNumber(32.2), 5, 15, 4),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);

        assert_eq!(
            Message::highlight_message(
                ReportKind::TypeError,
                Span::new(5,15,4),
                "".to_owned()),
            messages[0]);

        assert_eq!(
            AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
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
    fn array_declaration_with_non_numeric_dimension_is_reported() {
        /*

            fn foo() : int {
                let a : int[*] = 4;
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

            Token::new(TokenType::Let, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 0, 0, 0),
            Token::new(TokenType::Colon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::VarType, TokenSubType::IntegerType, 0, 0, 0),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 5, 15, 4),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 0, 0, 0),
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
                Span::new(5,15,4),
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
    fn array_assignment_is_parsed_correctly() {

        /*
            fn foo() : void {
                a[4-2] = 2*3;
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
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 5, 15, 4),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 9, 13, 12),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 5, 15, 6),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 1, 2, 3),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 5, 3, 9),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(3), 1, 2, 4),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::ArrayAssignment{
                                        variable_name: Rc::new("a".to_owned()),
                                        span: Span::new(8, 12, 2),
                                        index_expression: Box::new(
                                            AstNode::Minus(
                                                Box::new(
                                                  AstNode::Integer(
                                                      AstInteger::Int(4),
                                                      Span::new(5, 15, 4),
                                                  ),
                                                ),
                                                Box::new(
                                                   AstNode::Integer(
                                                       AstInteger::Int(2),
                                                       Span::new(5, 15, 6),
                                                   )
                                                ),
                                                ArithmeticInfo::new_alt(9, 13, 12),
                                            ),
                                        ),
                                        assignment_expression: Box::new(
                                          AstNode::Multiply(
                                              Box::new(
                                                  AstNode::Integer(
                                                    AstInteger::Int(2),
                                                      Span::new(1, 2, 3),
                                                  ),
                                              ),
                                              Box::new(
                                                AstNode::Integer(
                                                    AstInteger::Int(3),
                                                    Span::new(1, 2, 4)
                                                )
                                              ),
                                              ArithmeticInfo::new_alt(5, 3, 9)
                                          )
                                        ),
                                    }
                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn array_assignment_with_error_in_index_expression_is_reported() {

        /*
            fn foo() : void {
                a[4-] = 2*3;
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
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 5, 15, 4),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 9, 13, 12),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 2, 1, 40),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 1, 2, 3),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 5, 3, 9),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(3), 1, 2, 4),
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
                Span::new(2,1,40),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_assignment_with_error_in_assignment_expression_is_reported() {
        /*
            fn foo() : void {
                a[4-2] = 2*;
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
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(4), 5, 15, 4),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 9, 13, 12),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 5, 15, 6),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 1, 2, 3),
            Token::new(TokenType::Star, TokenSubType::NoSubType, 5, 3, 9),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 4, 30, 1),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 1);
        assert_eq!(
            Message::highlight_message(
                ReportKind::SyntaxError,
                Span::new(4,30,1),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn array_access_is_parsed_correctly() {
        /*
            fn foo() : void {
                b = a[2];
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
                TokenSubType::Identifier(Rc::new("b".to_string())), 8, 12, 2),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 18, 4, 2),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 5, 15, 6),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableAssignment(
                                        Box::new(
                                            AstNode::ArrayAccess {
                                                index_expression: Box::new(
                                                  AstNode::Integer(
                                                      AstInteger::Int(2),
                                                      Span::new(5, 15, 6),
                                                  )
                                                ),
                                                indexable_expression: Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("a".to_owned()),
                                                        Span::new(18, 4, 2),
                                                    )
                                                )
                                            }
                                        ),
                                        Rc::new("b".to_owned()),
                                        Span::new(8, 12, 2),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn array_access_from_function_is_parsed_correctly() {
        /*
            fn foo() : void {
                b = a()[2];
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
                TokenSubType::Identifier(Rc::new("b".to_string())), 8, 12, 2),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 18, 4, 2),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 5, 15, 6),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();
        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableAssignment(
                                        Box::new(
                                            AstNode::ArrayAccess {
                                                index_expression: Box::new(
                                                    AstNode::Integer(
                                                        AstInteger::Int(2),
                                                        Span::new(5, 15, 6),
                                                    )
                                                ),
                                                indexable_expression: Box::new(
                                                    AstNode::FunctionCall(
                                                        vec![],
                                                        Rc::new("a".to_owned()),
                                                        Span::new(18, 4, 2),
                                                    )
                                                )
                                            }
                                        ),
                                        Rc::new("b".to_owned()),
                                        Span::new(8, 12, 2),
                                    ),
                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }
    #[test]
    fn array_access_with_error_in_index_expression_is_rejected() {
        /*
             fn foo() : void {
                 b = a[2-];
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
                TokenSubType::Identifier(Rc::new("b".to_string())), 8, 12, 2),

            Token::new(TokenType::Equals, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 18, 4, 2),
            Token::new(TokenType::LBracket, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Number, TokenSubType::IntegerNumber(2), 5, 15, 6),
            Token::new(TokenType::Minus, TokenSubType::NoSubType, 12, 567, 5),
            Token::new(TokenType::RBracket, TokenSubType::NoSubType, 9, 3, 12),
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
                Span::new(9,3,12),
                "".to_owned()),
            messages[0]);
    }

    #[test]
    fn valid_member_access_is_parsed_correctly() {
        /*
            fn foo() : void {

                c = a.b;
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
                TokenSubType::Identifier(Rc::new("c".to_string())), 8, 12, 2),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 5, 1, 2),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),

            Token::new(TokenType::Dot, TokenSubType::NoSubType, 1, 6, 31),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 18, 4, 2),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableAssignment(
                                        Box::new(
                                            AstNode::MemberAccess {
                                                object: Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("a".to_owned()),
                                                        Span::new(8, 12, 2),
                                                    )
                                                ),
                                                member: Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("b".to_owned()),
                                                        Span::new(18, 4, 2)
                                                    )),
                                                span: Span::new(1, 6, 31),
                                            }
                                        ),
                                        Rc::new("c".to_string()),
                                        Span::new(8, 12, 2)
                                    ),

                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn chained_member_accesses_are_parsed_correctly() {
        /*
    fn foo() : void {

        c = a.b.d;
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
                TokenSubType::Identifier(Rc::new("c".to_string())), 8, 12, 2),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 5, 1, 2),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),

            Token::new(TokenType::Dot, TokenSubType::NoSubType, 1, 6, 31),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 18, 4, 2),
            Token::new(TokenType::Dot, TokenSubType::NoSubType, 4, 1, 1),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("d".to_string())), 11, 7, 3),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableAssignment(
                                        Box::new(
                                            AstNode::MemberAccess {
                                                object: Box::new(
                                                    AstNode::MemberAccess {
                                                        object: Box::new(
                                                            AstNode::Identifier(
                                                                Rc::new("a".to_owned()),
                                                                Span::new(8, 12, 2),
                                                            )
                                                        ),
                                                        member: Box::new(
                                                            AstNode::Identifier(
                                                                Rc::new("b".to_owned()),
                                                                Span::new(18, 4, 2)
                                                            )
                                                        ),
                                                        span: Span::new(1, 6, 31),
                                                    }
                                                ),
                                                member: Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("d".to_owned()),
                                                        Span::new(11, 7, 3),
                                                    )
                                                ),
                                                span: Span::new(4, 1, 1),
                                            }

                                        ),
                                        Rc::new("c".to_string()),
                                        Span::new(8, 12, 2)
                                    ),

                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn member_access_from_function_call_is_parsed_correctly() {
        /*
            fn foo() : void {

                c = a().b;
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
                TokenSubType::Identifier(Rc::new("c".to_string())), 8, 12, 2),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 5, 1, 2),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),
            Token::new(TokenType::LParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RParen, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::Dot, TokenSubType::NoSubType, 1, 6, 31),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 18, 4, 2),
            Token::new(TokenType::SemiColon, TokenSubType::NoSubType, 0, 0, 0),
            Token::new(TokenType::RBrace, TokenSubType::NoSubType, 0, 0, 0),
        ]);

        let node = parser.parse();

        let borrowed = reporter.borrow();
        let messages = borrowed.get_messages();

        assert_eq!(borrowed.errors(), 0);
        assert_eq!(
            AstNode::Block(
                vec![
                    AstNode::Function(
                        Box::new(
                            AstNode::Block(
                                vec![
                                    AstNode::VariableAssignment(
                                        Box::new(
                                            AstNode::MemberAccess {
                                                object: Box::new(
                                                    AstNode::FunctionCall(
                                                        vec![],
                                                        Rc::new("a".to_owned()),
                                                        Span::new(8, 12, 2),
                                                    )
                                                ),
                                                member: Box::new(
                                                    AstNode::Identifier(
                                                        Rc::new("b".to_owned()),
                                                        Span::new(18, 4, 2)
                                                    )),
                                                span: Span::new(1, 6, 31),
                                            }
                                        ),
                                        Rc::new("c".to_string()),
                                        Span::new(8, 12, 2)
                                    ),

                                ],
                                None,
                                Span::new(0, 0, 0))),
                        FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                    )
                ],
                None,
                Span::new(0, 0, 0),
            ),
            node);
    }

    #[test]
    fn member_access_followed_by_non_identifier_is_reported() {
        /*
            fn foo() : void {
               c = a.!b;
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
                TokenSubType::Identifier(Rc::new("c".to_string())), 8, 12, 2),
            Token::new(TokenType::Equals, TokenSubType::NoSubType, 5, 1, 2),

            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("a".to_string())), 8, 12, 2),

            Token::new(TokenType::Dot, TokenSubType::NoSubType, 1, 6, 31),
            Token::new(TokenType::Exclamation, TokenSubType::NoSubType, 1, 3, 9),
            Token::new(
                TokenType::Identifier,
                TokenSubType::Identifier(Rc::new("b".to_string())), 18, 4, 2),
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
                Span::new(1,3,9),
                "".to_owned()),
            messages[0]);
    }

}
