use crate::{
    common::{Pos, Span},
    error::{OperatorKind, ParseError},
    tokenizer::{Keyword, Operator, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum AstKind {
    Block(Vec<Ast>),
    Null,
    NumberLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(String),
    ListLiteral(Vec<Ast>),
    ObjectLiteral(Vec<(String, Ast)>),
    Variable(String),
    Add(Box<Ast>, Box<Ast>),
    Subtract(Box<Ast>, Box<Ast>),
    Multiply(Box<Ast>, Box<Ast>),
    Divide(Box<Ast>, Box<Ast>),
    Modulo(Box<Ast>, Box<Ast>),
    Power(Box<Ast>, Box<Ast>),
    UnaryMinus(Box<Ast>),
    BooleanNegate(Box<Ast>),
    Equality(Box<Ast>, Box<Ast>),
    Inequality(Box<Ast>, Box<Ast>),
    LessThan(Box<Ast>, Box<Ast>),
    GreaterThan(Box<Ast>, Box<Ast>),
    LessThanOrEqual(Box<Ast>, Box<Ast>),
    GreaterThanOrEqual(Box<Ast>, Box<Ast>),
    And(Box<Ast>, Box<Ast>),
    Or(Box<Ast>, Box<Ast>),
    Brackets(Box<Ast>),
    Assign(String, Box<Ast>),
    IndexingAssign {
        value: Box<Ast>,
        index: Box<Ast>,
        rhs: Box<Ast>,
    },
    MemberAssign {
        value: Box<Ast>,
        member: String,
        rhs: Box<Ast>,
    },
    FunctionCall {
        value: Box<Ast>,
        args: Vec<Ast>,
    },
    FunctionDefinition {
        name: String,
        arg_names: Vec<String>,
        body: Box<Ast>,
    },
    UnnamedFunction {
        arg_names: Vec<String>,
        body: Box<Ast>,
    },
    MemberAccess {
        value: Box<Ast>,
        member: String,
    },
    Indexing {
        value: Box<Ast>,
        index: Box<Ast>,
    },
    IfStatement {
        condition: Box<Ast>,
        if_body: Box<Ast>,
        else_body: Option<Box<Ast>>,
    },
    WhileLoop {
        condition: Box<Ast>,
        body: Box<Ast>,
    },
    ForLoop {
        variable: String,
        iterable: Box<Ast>,
        body: Box<Ast>,
    },
    Continue,
    Break,
    Return(Option<Box<Ast>>),
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub kind: AstKind,
    pub span: Span,
}

impl Ast {
    pub fn new(kind: AstKind, span: Span) -> Ast {
        Ast { kind, span }
    }
}

/// Returns the precedence of the operator.
///
/// Higher precedence means that the operator is calculated first (e.g. multiplication has higher
/// precedence than addition).
/// `is_binary` provides information about the operator being used as a
/// unary or binary operator (i.e. if `is_binary` is false, the operator is unary).
fn op_precedence(op: Operator, is_binary: bool, op_pos: Pos) -> Result<u8, ParseError> {
    let precedence = match (op, is_binary) {
        (Operator::Or, true) => 1,
        (Operator::And, true) => 2,
        (
            Operator::Equal
            | Operator::Unequal
            | Operator::LessThan
            | Operator::GreaterThan
            | Operator::LessThanOrEqual
            | Operator::GreaterThanOrEqual,
            true,
        ) => 3,
        (Operator::Plus | Operator::Minus, true) => 4,
        (Operator::Star | Operator::Slash | Operator::Percent, true) => 5,
        (Operator::Caret, true) => 6,
        (Operator::Minus | Operator::Exclamation, false) => 7,
        _ => {
            let kind = if is_binary {
                OperatorKind::Binary
            } else {
                OperatorKind::Unary
            };
            return Err(ParseError::InvalidOperator {
                op,
                kind,
                pos: op_pos,
            });
        }
    };

    Ok(precedence)
}

fn combine_lhs_rhs(op: Operator, lhs: Ast, rhs: Ast) -> Result<Ast, ParseError> {
    let span = lhs.span.until(rhs.span);

    let kind = match op {
        Operator::Plus => AstKind::Add(Box::new(lhs), Box::new(rhs)),
        Operator::Minus => AstKind::Subtract(Box::new(lhs), Box::new(rhs)),
        Operator::Star => AstKind::Multiply(Box::new(lhs), Box::new(rhs)),
        Operator::Slash => AstKind::Divide(Box::new(lhs), Box::new(rhs)),
        Operator::Percent => AstKind::Modulo(Box::new(lhs), Box::new(rhs)),
        Operator::Caret => AstKind::Power(Box::new(lhs), Box::new(rhs)),
        Operator::Equal => AstKind::Equality(Box::new(lhs), Box::new(rhs)),
        Operator::Unequal => AstKind::Inequality(Box::new(lhs), Box::new(rhs)),
        Operator::LessThan => AstKind::LessThan(Box::new(lhs), Box::new(rhs)),
        Operator::GreaterThan => AstKind::GreaterThan(Box::new(lhs), Box::new(rhs)),
        Operator::LessThanOrEqual => AstKind::LessThanOrEqual(Box::new(lhs), Box::new(rhs)),
        Operator::GreaterThanOrEqual => AstKind::GreaterThanOrEqual(Box::new(lhs), Box::new(rhs)),
        Operator::And => AstKind::And(Box::new(lhs), Box::new(rhs)),
        Operator::Or => AstKind::Or(Box::new(lhs), Box::new(rhs)),
        Operator::Exclamation => unreachable!(),
    };
    Ok(Ast::new(kind, span))
}

pub fn parse(tokens: &[Token]) -> Result<Ast, ParseError> {
    Parser::new(tokens).parse()
}

struct Parser<'a> {
    tokens: &'a [Token],
    idx: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, idx: 0 }
    }

    /// Entrypoint to the parser
    fn parse(&mut self) -> Result<Ast, ParseError> {
        let ast = self.parse_block(Span::new(0, 0))?;
        // If some function stopped parsing for some reason and we haven't parsed all tokens, the
        // token at the current position is unexpected.
        //
        // For example: A `}`, where the function stops parsing to let the caller decide whether
        // the token makes sense at this place.
        match self.next() {
            Some(Token {
                kind: TokenKind::Eof,
                ..
            }) => Ok(ast),
            Some(t) => Err(ParseError::UnexpectedToken {
                token: t.clone(),
                expected: None,
            }),
            None => panic!("EOF token was consumed while parsing"),
        }
    }

    fn parse_block(&mut self, start_span: Span) -> Result<Ast, ParseError> {
        let mut lines = Vec::<Ast>::new();
        let mut want_newline_this_iteration = false;
        let end_span = loop {
            let Some(token) = self.peek() else {
                panic_skipped_eof();
            };
            let (line, want_newline_next_iteration) = match token.kind {
                TokenKind::Eof => {
                    let end_span = lines.last().map(|ast| ast.span).unwrap_or(start_span);
                    break end_span;
                }
                TokenKind::Newline => {
                    self.next();
                    (None, false)
                }
                TokenKind::RBrace => break token.span,
                // All following constructs can only appear at the beginning of a line
                _ if want_newline_this_iteration => {
                    return Err(ParseError::UnexpectedToken {
                        token: token.clone(),
                        expected: Some(TokenKind::Newline),
                    });
                }
                TokenKind::Keyword(Keyword::Fn)
                    if matches!(
                        self.peek_next_non_newline_from_offset(2),
                        Some(Token {
                            kind: TokenKind::Identifier(_),
                            ..
                        })
                    ) =>
                {
                    (Some(self.parse_function_definition(true)?), true)
                }
                TokenKind::Keyword(Keyword::If) => (Some(self.parse_if_statement()?), true),
                TokenKind::Keyword(Keyword::While) => (Some(self.parse_while_loop()?), true),
                TokenKind::Keyword(Keyword::For) => (Some(self.parse_for_loop()?), true),
                TokenKind::Keyword(Keyword::Continue) => {
                    let token = self.next().unwrap();
                    (Some(Ast::new(AstKind::Continue, token.span)), true)
                }
                TokenKind::Keyword(Keyword::Break) => {
                    let token = self.next().unwrap();
                    (Some(Ast::new(AstKind::Break, token.span)), true)
                }
                TokenKind::Keyword(Keyword::Return) => (Some(self.parse_return()?), true),
                _ => (Some(self.parse_expression_or_assignment()?), true),
            };

            if let Some(token) = line {
                lines.push(token);
            }

            want_newline_this_iteration = want_newline_next_iteration;
        };

        let span = start_span.until(end_span);
        Ok(Ast::new(AstKind::Block(lines), span))
    }

    fn parse_expression_or_assignment(&mut self) -> Result<Ast, ParseError> {
        let is_potential_assignment = matches!(self.peek_kind(), Some(TokenKind::Identifier(_)));
        let lhs = self.parse_expression()?;
        if is_potential_assignment && self.peek_next_non_newline_kind() == Some(&TokenKind::Equal) {
            match lhs.kind {
                AstKind::Variable(var) => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let rhs = self.parse_expression()?;
                    let span = lhs.span.until(rhs.span);
                    return Ok(Ast::new(AstKind::Assign(var, Box::new(rhs)), span));
                }
                AstKind::Indexing { value, index } => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let rhs = self.parse_expression()?;
                    let span = lhs.span.until(rhs.span);
                    return Ok(Ast::new(
                        AstKind::IndexingAssign {
                            value,
                            index,
                            rhs: Box::new(rhs),
                        },
                        span,
                    ));
                }
                AstKind::MemberAccess { value, member } => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let rhs = self.parse_expression()?;
                    let span = lhs.span.until(rhs.span);
                    return Ok(Ast::new(
                        AstKind::MemberAssign {
                            value,
                            member,
                            rhs: Box::new(rhs),
                        },
                        span,
                    ));
                }
                _ => {}
            }
        }

        Ok(lhs)
    }

    /// Parses an expression.
    ///
    /// This works by calling a helper function that parses a part of an expression. Once that
    /// helper function returned the AST of the expression snipper, this function reads the next
    /// operator and creates a new AST node. The currently parsed AST becomes the left hand side of
    /// the new node and the right hand side is once again determined by the helper function.
    fn parse_expression(&mut self) -> Result<Ast, ParseError> {
        let mut lhs = self.parse_expression_with_min_precedence(0)?;
        while let Some(Token {
            kind: TokenKind::Operator(op),
            span,
        }) = self.peek_next_non_newline()
        {
            let op = *op;
            let pos = span.start;
            self.skip_newlines();
            self.next();
            self.skip_newlines();
            let precedence = op_precedence(op, true, pos)?;
            let rhs = self.parse_expression_with_min_precedence(precedence + 1)?;
            lhs = combine_lhs_rhs(op, lhs, rhs)?;
        }
        Ok(lhs)
    }

    /// Helper function for [`Self::parse_expression`] that parses an expression that includes all
    /// operators with a precedence equal to or higher than `min_precedence`. This means that it
    /// takes all operators until it sees an operator of lower precedence than `min_precedence`.
    ///
    /// When this function meets an operator with a precedence that is equal to or higher than
    /// `min_precedence`, it calls itself again. The recursive call accepts only operators that
    /// have higher predence than the current one. This is done until an operator of lower
    /// precedence is found. In that case the function returns the AST that it parsed so far and
    /// walks up the recursive call stack until that operator's precedence is equal to or higher
    /// than `min_precedence`. The returned AST is then used as the left hand side for that
    /// operator. The right hand side is once again determined by this function as was already
    /// explained.
    ///
    /// ## Example
    ///
    /// Calling the function with the input `1 + 2 ^ 3 * 4` and a `min_precedence` of `0` would
    /// result in the following AST:
    ///
    /// ```notest
    ///              +
    ///            1         *
    ///                  ^     4
    ///                2   3
    /// ```
    ///
    /// Or in another notation: `Add(1, Multiply(Power(2, 3), 4)`
    fn parse_expression_with_min_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Ast, ParseError> {
        match self.peek() {
            Some(token) => {
                let token_span = token.span;
                match token.kind {
                    TokenKind::Operator(Operator::Minus) => {
                        let pos = token.span.start;
                        self.next();
                        self.skip_newlines();
                        let unary_minus_precedence = op_precedence(Operator::Minus, false, pos)?;
                        // Not `+ 1` like in the other cases so we can take multiple unary minus operators
                        // after each other
                        let rhs =
                            self.parse_expression_with_min_precedence(unary_minus_precedence)?;
                        let span = token_span.until(rhs.span);
                        Ok(Ast::new(AstKind::UnaryMinus(Box::new(rhs)), span))
                    }
                    TokenKind::Operator(Operator::Exclamation) => {
                        let pos = token.span.start;
                        self.next();
                        self.skip_newlines();
                        let boolean_negate_precedence =
                            op_precedence(Operator::Exclamation, false, pos)?;
                        // Not `+ 1` like in the other cases so we can take multiple boolean negate operators
                        // after each other
                        let rhs =
                            self.parse_expression_with_min_precedence(boolean_negate_precedence)?;
                        let span = token_span.until(rhs.span);
                        Ok(Ast::new(AstKind::BooleanNegate(Box::new(rhs)), span))
                    }
                    TokenKind::LParen
                    | TokenKind::LBrace
                    | TokenKind::LBracket
                    | TokenKind::Identifier(_)
                    | TokenKind::Number(_)
                    | TokenKind::String(_)
                    | TokenKind::Keyword(Keyword::True | Keyword::False)
                    | TokenKind::Keyword(Keyword::Null)
                    | TokenKind::Keyword(Keyword::Fn) => {
                        let mut lhs = self.parse_identifier_or_value()?;
                        while let Some(Token {
                            kind: TokenKind::Operator(op),
                            span,
                        }) = self.peek_next_non_newline()
                        {
                            let op = *op;
                            let precedence = op_precedence(op, true, span.start)?;
                            if precedence >= min_precedence {
                                self.skip_newlines();
                                self.next();
                                self.skip_newlines();
                                let rhs =
                                    self.parse_expression_with_min_precedence(precedence + 1)?;
                                lhs = combine_lhs_rhs(op, lhs, rhs)?;
                            } else {
                                break;
                            }
                        }
                        Ok(lhs)
                    }
                    _ => Err(ParseError::UnexpectedToken {
                        token: token.clone(),
                        expected: None,
                    }),
                }
            }
            None => panic_skipped_eof(),
        }
    }

    fn parse_identifier_or_value(&mut self) -> Result<Ast, ParseError> {
        let mut ast = match self.next() {
            Some(token) => {
                let token_span = token.span;
                match &token.kind {
                    TokenKind::LParen => {
                        self.skip_newlines();
                        let inner = self.parse_expression()?;
                        self.skip_newlines();
                        let r_paren = self.expect(TokenKind::RParen)?;
                        Ast::new(
                            AstKind::Brackets(Box::new(inner)),
                            token_span.until(r_paren.span),
                        )
                    }
                    TokenKind::LBrace => {
                        // We match on `self.next()`, but the parse function expects the `{` token
                        self.idx -= 1;
                        self.parse_object_literal()?
                    }
                    TokenKind::LBracket => {
                        // We match on `self.next()`, but the parse function expects the `[` token
                        self.idx -= 1;
                        self.parse_list_literal()?
                    }
                    TokenKind::Identifier(name) => {
                        Ast::new(AstKind::Variable(name.clone()), token.span)
                    }
                    TokenKind::Number(num) => Ast::new(AstKind::NumberLiteral(*num), token.span),
                    TokenKind::String(str) => {
                        Ast::new(AstKind::StringLiteral(str.clone()), token.span)
                    }
                    TokenKind::Keyword(Keyword::True) => {
                        Ast::new(AstKind::BooleanLiteral(true), token.span)
                    }
                    TokenKind::Keyword(Keyword::False) => {
                        Ast::new(AstKind::BooleanLiteral(false), token.span)
                    }
                    TokenKind::Keyword(Keyword::Null) => Ast::new(AstKind::Null, token.span),
                    TokenKind::Keyword(Keyword::Fn) => {
                        // We match on `self.next()`, but the parse function expects the `fn` keyword
                        self.idx -= 1;
                        self.parse_function_definition(false)?
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken {
                            token: token.clone(),
                            expected: None,
                        })
                    }
                }
            }
            None => panic_skipped_eof(),
        };
        loop {
            // Don't allow newlines before LParen and LBracket to avoid confusion with expressions
            // in the next non-empty line.
            match self.peek_kind() {
                Some(TokenKind::LParen) => {
                    ast = self.parse_function_call(ast)?;
                }
                Some(TokenKind::LBracket) => {
                    ast = self.parse_indexing(ast)?;
                }
                _ if self.peek_next_non_newline_kind() == Some(&TokenKind::Dot) => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let ident = self.expect_identifier()?;
                    let span = ast.span.until(ident.1);
                    ast = Ast::new(
                        AstKind::MemberAccess {
                            value: Box::new(ast),
                            member: ident.0.to_string(),
                        },
                        span,
                    );
                }
                _ => break,
            }
        }
        Ok(ast)
    }

    fn parse_function_call(&mut self, called_value: Ast) -> Result<Ast, ParseError> {
        // <value>(<val1>, <val2>, ...)
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let mut args = Vec::new();
        while self.peek_kind() != Some(&TokenKind::RParen) {
            let arg = self.parse_expression()?;
            args.push(arg);
            self.skip_newlines();

            if self.peek_kind() != Some(&TokenKind::Comma) {
                break;
            }

            self.next();
            self.skip_newlines();
        }
        let r_paren = self.expect(TokenKind::RParen)?;
        let span = called_value.span.until(r_paren.span);
        Ok(Ast::new(
            AstKind::FunctionCall {
                value: Box::new(called_value),
                args,
            },
            span,
        ))
    }

    fn parse_indexing(&mut self, value: Ast) -> Result<Ast, ParseError> {
        // <value>[<index>]
        self.expect(TokenKind::LBracket)?;
        self.skip_newlines();
        let index = self.parse_expression()?;
        self.skip_newlines();
        let r_bracket = self.expect(TokenKind::RBracket)?;
        let span = value.span.until(r_bracket.span);
        Ok(Ast::new(
            AstKind::Indexing {
                value: Box::new(value),
                index: Box::new(index),
            },
            span,
        ))
    }

    fn parse_object_literal(&mut self) -> Result<Ast, ParseError> {
        // { <key1>[: <val1> | <unnamed_function_definition_without_fn>], ... }
        let l_brace_span = self.expect(TokenKind::LBrace)?.span;
        let mut key_value_pairs = Vec::new();
        while self.peek_kind() != Some(&TokenKind::RBrace) {
            self.skip_newlines();
            let key = self.expect_identifier()?;
            self.skip_newlines();
            let value = match self.peek() {
                Some(Token {
                    kind: TokenKind::Colon,
                    ..
                }) => {
                    self.next();
                    self.skip_newlines();
                    self.parse_expression()?
                }
                Some(Token {
                    kind: TokenKind::LParen,
                    ..
                }) => self.parse_function_definition_without_fn(false, key.1)?,
                Some(Token {
                    kind: TokenKind::Comma | TokenKind::RBrace,
                    span,
                }) => Ast::new(AstKind::Variable(key.0.to_string()), *span),
                Some(t) => {
                    return Err(ParseError::UnexpectedToken {
                        token: t.clone(),
                        expected: Some(TokenKind::Colon),
                    })
                }
                None => panic_skipped_eof(),
            };
            key_value_pairs.push((key.0, value));

            self.skip_newlines();
            if self.peek_kind() != Some(&TokenKind::Comma) {
                break;
            }

            self.next();
            self.skip_newlines();
        }
        let r_brace = self.expect(TokenKind::RBrace)?;
        Ok(Ast::new(
            AstKind::ObjectLiteral(key_value_pairs),
            l_brace_span.until(r_brace.span),
        ))
    }

    fn parse_list_literal(&mut self) -> Result<Ast, ParseError> {
        // [ <val1>, <val2>, ... ]
        let l_bracket_span = self.expect(TokenKind::LBracket)?.span;
        self.skip_newlines();
        let mut values = Vec::new();
        while self.peek_kind() != Some(&TokenKind::RBracket) {
            let value = self.parse_expression()?;
            values.push(value);
            self.skip_newlines();

            if self.peek_kind() != Some(&TokenKind::Comma) {
                break;
            }

            self.next();
            self.skip_newlines();
        }
        let r_bracket = self.expect(TokenKind::RBracket)?;
        Ok(Ast::new(
            AstKind::ListLiteral(values),
            l_bracket_span.until(r_bracket.span),
        ))
    }

    fn parse_function_definition(&mut self, named: bool) -> Result<Ast, ParseError> {
        // fn <function_definition_without_fn>
        let fn_keyword_span = self.expect(TokenKind::Keyword(Keyword::Fn))?.span;
        self.skip_newlines();
        self.parse_function_definition_without_fn(named, fn_keyword_span)
    }

    fn parse_function_definition_without_fn(
        &mut self,
        named: bool,
        start_span: Span,
    ) -> Result<Ast, ParseError> {
        // [ <name> ] (<arg1>, <arg2>, ...) { <body> }
        let fn_name = if named {
            let ident = self.expect_identifier()?.0.to_string();
            self.skip_newlines();
            Some(ident)
        } else {
            None
        };
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();

        let mut arg_names = Vec::new();
        while let Some(TokenKind::Identifier(arg_name)) = self.peek_kind() {
            arg_names.push(arg_name.to_string());
            self.next();
            self.skip_newlines();

            if self.peek_kind() != Some(&TokenKind::Comma) {
                break;
            }

            self.next();
            self.skip_newlines();
        }

        self.expect(TokenKind::RParen)?;
        self.skip_newlines();
        let l_brace_span = self.expect(TokenKind::LBrace)?.span;
        let body = self.parse_block(l_brace_span)?;
        let r_brace = self.expect(TokenKind::RBrace)?;

        let span = start_span.until(r_brace.span);
        if let Some(name) = fn_name {
            Ok(Ast::new(
                AstKind::FunctionDefinition {
                    name,
                    arg_names,
                    body: Box::new(body),
                },
                span,
            ))
        } else {
            Ok(Ast::new(
                AstKind::UnnamedFunction {
                    arg_names,
                    body: Box::new(body),
                },
                span,
            ))
        }
    }

    fn parse_if_statement(&mut self) -> Result<Ast, ParseError> {
        // if ( <expr> ) { <body> } [ else if ( <expr> ) { <body> } [ ... ] ] [ else { <body> } ]
        let if_keyword_span = self.expect(TokenKind::Keyword(Keyword::If))?.span;
        self.skip_newlines();
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let condition = self.parse_expression()?;
        self.skip_newlines();
        self.expect(TokenKind::RParen)?;
        self.skip_newlines();
        let l_brace_span = self.expect(TokenKind::LBrace)?.span;
        let if_body = self.parse_block(l_brace_span)?;
        self.skip_newlines();
        let if_r_brace_span = self.expect(TokenKind::RBrace)?.span;

        let (else_body, end_span) = if self.peek_next_non_newline().map(|t| &t.kind)
            == Some(&TokenKind::Keyword(Keyword::Else))
        {
            self.skip_newlines();
            self.next();
            if self.peek_kind() == Some(&TokenKind::Keyword(Keyword::If)) {
                let else_if_statement = self.parse_if_statement()?;
                let else_if_statement_span = else_if_statement.span;
                (Some(Box::new(else_if_statement)), else_if_statement_span)
            } else {
                self.skip_newlines();
                let l_brace_span = self.expect(TokenKind::LBrace)?.span;
                let else_body = self.parse_block(l_brace_span)?;
                self.skip_newlines();
                let else_r_brace = self.expect(TokenKind::RBrace)?;
                (Some(Box::new(else_body)), else_r_brace.span)
            }
        } else {
            (None, if_r_brace_span)
        };

        Ok(Ast::new(
            AstKind::IfStatement {
                condition: Box::new(condition),
                if_body: Box::new(if_body),
                else_body,
            },
            if_keyword_span.until(end_span),
        ))
    }

    fn parse_while_loop(&mut self) -> Result<Ast, ParseError> {
        // while ( <expr> ) { <body> }
        let while_keyword_span = self.expect(TokenKind::Keyword(Keyword::While))?.span;
        self.skip_newlines();
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let condition = self.parse_expression()?;
        self.skip_newlines();
        self.expect(TokenKind::RParen)?;
        self.skip_newlines();

        let l_brace_span = self.expect(TokenKind::LBrace)?.span;
        let body = self.parse_block(l_brace_span)?;
        let r_brace = self.expect(TokenKind::RBrace)?;

        Ok(Ast::new(
            AstKind::WhileLoop {
                condition: Box::new(condition),
                body: Box::new(body),
            },
            while_keyword_span.until(r_brace.span),
        ))
    }

    fn parse_for_loop(&mut self) -> Result<Ast, ParseError> {
        // for ( <name> in <expr> ) { <body> }
        let for_keyword_span = self.expect(TokenKind::Keyword(Keyword::For))?.span;
        self.skip_newlines();
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let variable = self.expect_identifier()?.0.to_string();
        self.skip_newlines();
        self.expect(TokenKind::Keyword(Keyword::In))?;
        self.skip_newlines();
        let iterable = self.parse_expression()?;
        self.skip_newlines();
        self.expect(TokenKind::RParen)?;
        self.skip_newlines();

        let l_brace_span = self.expect(TokenKind::LBrace)?.span;
        let body = self.parse_block(l_brace_span)?;
        let r_brace = self.expect(TokenKind::RBrace)?;

        Ok(Ast::new(
            AstKind::ForLoop {
                variable,
                iterable: Box::new(iterable),
                body: Box::new(body),
            },
            for_keyword_span.until(r_brace.span),
        ))
    }

    fn parse_return(&mut self) -> Result<Ast, ParseError> {
        // return [ <expr> ]
        let return_keyword_span = self.expect(TokenKind::Keyword(Keyword::Return))?.span;
        let (expr, span) = match self.peek_kind() {
            Some(&TokenKind::Newline | &TokenKind::RBrace | &TokenKind::Eof) => {
                (None, return_keyword_span)
            }
            _ => {
                let expr = self.parse_expression()?;
                let expr_span = expr.span;
                (Some(Box::new(expr)), return_keyword_span.until(expr_span))
            }
        };
        Ok(Ast::new(AstKind::Return(expr), span))
    }

    /// Takes the next token, behaving like `next` of an iterator.
    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.idx);
        self.idx += 1;
        token
    }

    /// Peeks the nth token.
    ///
    /// Peek with n = 1 behaves like `peek` of an iterator, peeking the next available token.
    fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.idx + n - 1)
    }

    /// Peeks the next token, behaving like `peek` of an iterator.
    fn peek(&self) -> Option<&Token> {
        self.peek_nth(1)
    }

    /// Peeks the kind of the nth token.
    fn peek_nth_kind(&self, n: usize) -> Option<&TokenKind> {
        self.peek_nth(n).map(|t| &t.kind)
    }

    /// Peeks the kind of the next token.
    fn peek_kind(&self) -> Option<&TokenKind> {
        self.peek_nth_kind(1)
    }

    /// Peeks the next token that isn't a newline.
    ///
    /// [`Self::skip_newlines`] and [`Self::next`] can be used to advance the position to the next non-newline
    /// token.
    fn peek_next_non_newline(&self) -> Option<&Token> {
        self.peek_next_non_newline_from_offset(1)
    }

    /// Peeks the kind of the next token that isn't a newline.
    ///
    /// See [`Self::peek_next_non_newline`].
    fn peek_next_non_newline_kind(&self) -> Option<&TokenKind> {
        self.peek_next_non_newline_from_offset(1).map(|t| &t.kind)
    }

    /// Peeks the next token that isn't a newline from one-based offset of `offset`.
    ///
    /// `offset` must be greater than or equal to 1.
    fn peek_next_non_newline_from_offset(&self, offset: usize) -> Option<&Token> {
        let mut peek_idx = offset.max(1);
        while let Some(TokenKind::Newline) = self.peek_nth_kind(peek_idx) {
            peek_idx += 1;
        }

        self.peek_nth(peek_idx)
    }

    /// Asserts that `expected` is the next token, while also advancing the position.
    fn expect(&mut self, expected: TokenKind) -> Result<&Token, ParseError> {
        match self.next() {
            Some(actual) => {
                if actual.kind != expected {
                    Err(ParseError::UnexpectedToken {
                        token: actual.clone(),
                        expected: Some(expected),
                    })
                } else {
                    Ok(actual)
                }
            }
            None => panic_skipped_eof(),
        }
    }

    /// Asserts that the next token is an identifier, returning the inner string slice of the
    /// identifier and advancing the position.
    fn expect_identifier(&mut self) -> Result<(String, Span), ParseError> {
        match self.next() {
            Some(Token {
                kind: TokenKind::Identifier(ref name),
                span,
            }) => Ok((name.to_string(), *span)),
            Some(Token { span, .. }) => Err(ParseError::ExpectedIdentifier { pos: span.start }),
            None => panic_skipped_eof(),
        }
    }

    /// Advanced the position until the next token is not a newline.
    fn skip_newlines(&mut self) {
        while self.peek_kind() == Some(&TokenKind::Newline) {
            self.next();
        }
    }
}

fn panic_skipped_eof() -> ! {
    panic!("Tokens didn't contain EOF")
}
