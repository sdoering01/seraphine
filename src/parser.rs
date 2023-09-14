use crate::{
    common::Pos,
    error::{OperatorKind, ParseError},
    tokenizer::{Keyword, Operator, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum Ast {
    Lines(Vec<Ast>),
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
    Continue,
    Break,
    Return(Option<Box<Ast>>),
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
    let combined = match op {
        Operator::Plus => Ast::Add(Box::new(lhs), Box::new(rhs)),
        Operator::Minus => Ast::Subtract(Box::new(lhs), Box::new(rhs)),
        Operator::Star => Ast::Multiply(Box::new(lhs), Box::new(rhs)),
        Operator::Slash => Ast::Divide(Box::new(lhs), Box::new(rhs)),
        Operator::Percent => Ast::Modulo(Box::new(lhs), Box::new(rhs)),
        Operator::Caret => Ast::Power(Box::new(lhs), Box::new(rhs)),
        Operator::Equal => Ast::Equality(Box::new(lhs), Box::new(rhs)),
        Operator::Unequal => Ast::Inequality(Box::new(lhs), Box::new(rhs)),
        Operator::LessThan => Ast::LessThan(Box::new(lhs), Box::new(rhs)),
        Operator::GreaterThan => Ast::GreaterThan(Box::new(lhs), Box::new(rhs)),
        Operator::LessThanOrEqual => Ast::LessThanOrEqual(Box::new(lhs), Box::new(rhs)),
        Operator::GreaterThanOrEqual => Ast::GreaterThanOrEqual(Box::new(lhs), Box::new(rhs)),
        Operator::And => Ast::And(Box::new(lhs), Box::new(rhs)),
        Operator::Or => Ast::Or(Box::new(lhs), Box::new(rhs)),
        Operator::Exclamation => unreachable!(),
    };
    Ok(combined)
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
        let ast = self.parse_block()?;
        // If some function stopped parsing for some reason and we haven't parsed all tokens, the
        // token at the current position is unexpected.
        //
        // For example: A `}`, where the function stops parsing to let the caller decide whether
        // the token makes sense at this place.
        if self.idx < self.tokens.len() {
            return Err(ParseError::UnexpectedToken {
                token: self.tokens[self.idx].clone(),
                expected: None,
            });
        }
        Ok(ast)
    }

    fn parse_block(&mut self) -> Result<Ast, ParseError> {
        let mut lines = Vec::new();
        let mut want_newline_this_iteration = false;
        while let Some(token) = self.peek() {
            let (line, want_newline_next_iteration) = match &token.kind {
                TokenKind::Eof | TokenKind::Newline => {
                    self.next();
                    (None, false)
                }
                TokenKind::RBrace => break,
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
                TokenKind::Keyword(Keyword::Continue) => {
                    self.next();
                    (Some(Ast::Continue), true)
                }
                TokenKind::Keyword(Keyword::Break) => {
                    self.next();
                    (Some(Ast::Break), true)
                }
                TokenKind::Keyword(Keyword::Return) => (Some(self.parse_return()?), true),
                _ => (Some(self.parse_expression_or_assignment()?), true),
            };

            if let Some(token) = line {
                lines.push(token);
            }

            want_newline_this_iteration = want_newline_next_iteration;
        }
        Ok(Ast::Lines(lines))
    }

    fn parse_expression_or_assignment(&mut self) -> Result<Ast, ParseError> {
        let is_potential_assignment = matches!(self.peek_kind(), Some(TokenKind::Identifier(_)));
        let lhs = self.parse_expression()?;
        if is_potential_assignment && self.peek_next_non_newline_kind() == Some(&TokenKind::Equal) {
            match lhs {
                Ast::Variable(var) => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let rhs = self.parse_expression()?;
                    return Ok(Ast::Assign(var, Box::new(rhs)));
                }
                Ast::Indexing { value, index } => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let rhs = self.parse_expression()?;
                    return Ok(Ast::IndexingAssign {
                        value,
                        index,
                        rhs: Box::new(rhs),
                    });
                }
                Ast::MemberAccess { value, member } => {
                    self.skip_newlines();
                    self.next();
                    self.skip_newlines();
                    let rhs = self.parse_expression()?;
                    return Ok(Ast::MemberAssign {
                        value,
                        member,
                        rhs: Box::new(rhs),
                    });
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
            pos,
        }) = self.peek_next_non_newline()
        {
            let op = *op;
            let pos = *pos;
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
    ///              +
    ///            1         *
    ///                  ^     4
    ///                2   3
    ///
    /// Or in another notation: `Add(1, Multiply(Power(2, 3), 4)`
    fn parse_expression_with_min_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Ast, ParseError> {
        match self.peek() {
            Some(token) => {
                match token.kind {
                    TokenKind::Operator(Operator::Minus) => {
                        let pos = token.pos;
                        self.next();
                        self.skip_newlines();
                        let unary_minus_precedence = op_precedence(Operator::Minus, false, pos)?;
                        // Not `+ 1` like in the other cases so we can take multiple unary minus operators
                        // after each other
                        let rhs =
                            self.parse_expression_with_min_precedence(unary_minus_precedence)?;
                        Ok(Ast::UnaryMinus(Box::new(rhs)))
                    }
                    TokenKind::Operator(Operator::Exclamation) => {
                        let pos = token.pos;
                        self.next();
                        self.skip_newlines();
                        let boolean_negate_precedence =
                            op_precedence(Operator::Exclamation, false, pos)?;
                        // Not `+ 1` like in the other cases so we can take multiple boolean negate operators
                        // after each other
                        let rhs =
                            self.parse_expression_with_min_precedence(boolean_negate_precedence)?;
                        Ok(Ast::BooleanNegate(Box::new(rhs)))
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
                            pos,
                        }) = self.peek_next_non_newline()
                        {
                            let op = *op;
                            let precedence = op_precedence(op, true, *pos)?;
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
            None => Err(ParseError::NoTokensLeft),
        }
    }

    fn parse_identifier_or_value(&mut self) -> Result<Ast, ParseError> {
        let mut ast = match self.next() {
            Some(token) => match &token.kind {
                TokenKind::LParen => {
                    self.skip_newlines();
                    let inner = self.parse_expression()?;
                    self.skip_newlines();
                    self.expect(TokenKind::RParen)?;
                    Ast::Brackets(Box::new(inner))
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
                TokenKind::Identifier(name) => Ast::Variable(name.clone()),
                TokenKind::Number(num) => Ast::NumberLiteral(*num),
                TokenKind::String(str) => Ast::StringLiteral(str.clone()),
                TokenKind::Keyword(Keyword::True) => Ast::BooleanLiteral(true),
                TokenKind::Keyword(Keyword::False) => Ast::BooleanLiteral(false),
                TokenKind::Keyword(Keyword::Null) => Ast::Null,
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
            },
            None => return Err(ParseError::NoTokensLeft),
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
                    let member = self.expect_identifier()?;
                    ast = Ast::MemberAccess {
                        value: Box::new(ast),
                        member: member.to_string(),
                    };
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
        self.expect(TokenKind::RParen)?;
        Ok(Ast::FunctionCall {
            value: Box::new(called_value),
            args,
        })
    }

    fn parse_indexing(&mut self, value: Ast) -> Result<Ast, ParseError> {
        // <value>[<index>]
        self.expect(TokenKind::LBracket)?;
        self.skip_newlines();
        let index = self.parse_expression()?;
        self.skip_newlines();
        self.expect(TokenKind::RBracket)?;
        Ok(Ast::Indexing {
            value: Box::new(value),
            index: Box::new(index),
        })
    }

    fn parse_object_literal(&mut self) -> Result<Ast, ParseError> {
        // { <key1>[: <val1> | <unnamed_function_definition_without_fn>], ... }
        self.expect(TokenKind::LBrace)?;
        let mut key_value_pairs = Vec::new();
        while self.peek_kind() != Some(&TokenKind::RBrace) {
            self.skip_newlines();
            let key = self.expect_identifier()?.to_string();
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
                }) => self.parse_function_definition_without_fn(false)?,
                Some(Token {
                    kind: TokenKind::Comma | TokenKind::RBrace,
                    ..
                }) => Ast::Variable(key.clone()),
                Some(t) => {
                    return Err(ParseError::UnexpectedToken {
                        token: t.clone(),
                        expected: Some(TokenKind::Colon),
                    })
                }
                None => return Err(ParseError::NoTokensLeft),
            };
            key_value_pairs.push((key, value));

            self.skip_newlines();
            if self.peek_kind() != Some(&TokenKind::Comma) {
                break;
            }

            self.next();
            self.skip_newlines();
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Ast::ObjectLiteral(key_value_pairs))
    }

    fn parse_list_literal(&mut self) -> Result<Ast, ParseError> {
        // [ <val1>, <val2>, ... ]
        self.expect(TokenKind::LBracket)?;
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
        self.expect(TokenKind::RBracket)?;
        Ok(Ast::ListLiteral(values))
    }

    fn parse_function_definition(&mut self, named: bool) -> Result<Ast, ParseError> {
        // fn <function_definition_without_fn>
        self.expect(TokenKind::Keyword(Keyword::Fn))?;
        self.skip_newlines();
        self.parse_function_definition_without_fn(named)
    }

    fn parse_function_definition_without_fn(&mut self, named: bool) -> Result<Ast, ParseError> {
        // [ <name> ] (<arg1>, <arg2>, ...) { <body> }
        let fn_name = if named {
            let ident = self.expect_identifier()?.to_string();
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
        self.expect(TokenKind::LBrace)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::RBrace)?;

        if let Some(name) = fn_name {
            Ok(Ast::FunctionDefinition {
                name,
                arg_names,
                body: Box::new(body),
            })
        } else {
            Ok(Ast::UnnamedFunction {
                arg_names,
                body: Box::new(body),
            })
        }
    }

    fn parse_if_statement(&mut self) -> Result<Ast, ParseError> {
        // if ( <expr> ) { <body> } [ else if ( <expr> ) { <body> } [ ... ] ] [ else { <body> } ]
        self.expect(TokenKind::Keyword(Keyword::If))?;
        self.expect(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(TokenKind::RParen)?;
        self.skip_newlines();
        self.expect(TokenKind::LBrace)?;
        let if_body = self.parse_block()?;
        self.skip_newlines();
        self.expect(TokenKind::RBrace)?;

        let else_body = if self.peek_next_non_newline().map(|t| &t.kind)
            == Some(&TokenKind::Keyword(Keyword::Else))
        {
            self.skip_newlines();
            self.next();
            if self.peek_kind() == Some(&TokenKind::Keyword(Keyword::If)) {
                let else_if_statement = self.parse_if_statement()?;
                Some(Box::new(else_if_statement))
            } else {
                self.skip_newlines();
                self.expect(TokenKind::LBrace)?;
                let else_body = self.parse_block()?;
                self.skip_newlines();
                self.expect(TokenKind::RBrace)?;
                Some(Box::new(else_body))
            }
        } else {
            None
        };

        Ok(Ast::IfStatement {
            condition: Box::new(condition),
            if_body: Box::new(if_body),
            else_body,
        })
    }

    fn parse_while_loop(&mut self) -> Result<Ast, ParseError> {
        // while ( <expr> ) { <body> }
        self.expect(TokenKind::Keyword(Keyword::While))?;
        self.skip_newlines();
        self.expect(TokenKind::LParen)?;
        self.skip_newlines();
        let condition = self.parse_expression()?;
        self.skip_newlines();
        self.expect(TokenKind::RParen)?;
        self.skip_newlines();

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::RBrace)?;

        Ok(Ast::WhileLoop {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_return(&mut self) -> Result<Ast, ParseError> {
        // return [ <expr> ]
        self.expect(TokenKind::Keyword(Keyword::Return))?;
        let expr = match self.peek_kind() {
            Some(&TokenKind::Newline | &TokenKind::RBrace) => None,
            _ => Some(Box::new(self.parse_expression()?)),
        };
        Ok(Ast::Return(expr))
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
    fn expect(&mut self, expected: TokenKind) -> Result<(), ParseError> {
        match self.next() {
            Some(actual) => {
                if actual.kind != expected {
                    Err(ParseError::UnexpectedToken {
                        token: actual.clone(),
                        expected: Some(expected),
                    })
                } else {
                    Ok(())
                }
            }
            None => Err(ParseError::NoTokensLeft),
        }
    }

    /// Asserts that the next token is an identifier, returning the inner string slice of the
    /// identifier and advancing the position.
    fn expect_identifier(&mut self) -> Result<&str, ParseError> {
        match self.next() {
            Some(Token {
                kind: TokenKind::Identifier(ref name),
                ..
            }) => Ok(name),
            Some(Token { pos, .. }) => Err(ParseError::ExpectedIdentifier { pos: *pos }),
            None => Err(ParseError::NoTokensLeft),
        }
    }

    /// Advanced the position until the next token is not a newline.
    fn skip_newlines(&mut self) {
        while self.peek_kind() == Some(&TokenKind::Newline) {
            self.next();
        }
    }
}
