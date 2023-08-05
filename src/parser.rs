use crate::{
    common::Pos,
    error::{OperatorKind, ParseError},
    tokenizer::{Keyword, Operator, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub enum AST {
    Lines(Vec<AST>),
    NumberLiteral(f64),
    BooleanLiteral(bool),
    Variable(String),
    Add(Box<AST>, Box<AST>),
    Subtract(Box<AST>, Box<AST>),
    Multiply(Box<AST>, Box<AST>),
    Divide(Box<AST>, Box<AST>),
    Modulo(Box<AST>, Box<AST>),
    Power(Box<AST>, Box<AST>),
    UnaryMinus(Box<AST>),
    BooleanNegate(Box<AST>),
    Equality(Box<AST>, Box<AST>),
    Inequality(Box<AST>, Box<AST>),
    LessThan(Box<AST>, Box<AST>),
    GreaterThan(Box<AST>, Box<AST>),
    LessThanOrEqual(Box<AST>, Box<AST>),
    GreaterThanOrEqual(Box<AST>, Box<AST>),
    And(Box<AST>, Box<AST>),
    Or(Box<AST>, Box<AST>),
    Brackets(Box<AST>),
    Assign(String, Box<AST>),
    FunctionCall(String, Vec<AST>),
    FunctionDefinition {
        name: String,
        arg_names: Vec<String>,
        body: Box<AST>,
    },
    IfStatement {
        condition: Box<AST>,
        if_body: Box<AST>,
        else_body: Option<Box<AST>>,
    },
    WhileLoop {
        condition: Box<AST>,
        body: Box<AST>,
    },
    Return(Option<Box<AST>>),
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

fn combine_lhs_rhs(op: Operator, lhs: AST, rhs: AST) -> Result<AST, ParseError> {
    let combined = match op {
        Operator::Plus => AST::Add(Box::new(lhs), Box::new(rhs)),
        Operator::Minus => AST::Subtract(Box::new(lhs), Box::new(rhs)),
        Operator::Star => AST::Multiply(Box::new(lhs), Box::new(rhs)),
        Operator::Slash => AST::Divide(Box::new(lhs), Box::new(rhs)),
        Operator::Percent => AST::Modulo(Box::new(lhs), Box::new(rhs)),
        Operator::Caret => AST::Power(Box::new(lhs), Box::new(rhs)),
        Operator::Equal => AST::Equality(Box::new(lhs), Box::new(rhs)),
        Operator::Unequal => AST::Inequality(Box::new(lhs), Box::new(rhs)),
        Operator::LessThan => AST::LessThan(Box::new(lhs), Box::new(rhs)),
        Operator::GreaterThan => AST::GreaterThan(Box::new(lhs), Box::new(rhs)),
        Operator::LessThanOrEqual => AST::LessThanOrEqual(Box::new(lhs), Box::new(rhs)),
        Operator::GreaterThanOrEqual => AST::GreaterThanOrEqual(Box::new(lhs), Box::new(rhs)),
        Operator::And => AST::And(Box::new(lhs), Box::new(rhs)),
        Operator::Or => AST::Or(Box::new(lhs), Box::new(rhs)),
        Operator::Exclamation => unreachable!(),
    };
    Ok(combined)
}

pub fn parse(tokens: &[Token]) -> Result<AST, ParseError> {
    Parser::new(tokens).parse()
}

struct Parser<'a> {
    tokens: &'a [Token],
    idx: usize,
}

// TODO: Allow newlines in more places (e.g. argument list of function definition)
// TODO: After that, allow optional comma at the end of argument lists
impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, idx: 0 }
    }

    /// Entrypoint to the parser
    fn parse(&mut self) -> Result<AST, ParseError> {
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

    fn parse_block(&mut self) -> Result<AST, ParseError> {
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
                TokenKind::Keyword(Keyword::Fn) => (Some(self.parse_function_definition()?), true),
                TokenKind::Keyword(Keyword::If) => (Some(self.parse_if_statement()?), true),
                TokenKind::Keyword(Keyword::While) => (Some(self.parse_while_loop()?), true),
                TokenKind::Keyword(Keyword::Return) => (Some(self.parse_return()?), true),
                TokenKind::Identifier(_) if self.peek_nth_kind(2) == Some(&TokenKind::Equal) => {
                    (Some(self.parse_assignment()?), true)
                }
                _ => (Some(self.parse_expression()?), true),
            };

            if let Some(token) = line {
                lines.push(token);
            }

            want_newline_this_iteration = want_newline_next_iteration;
        }
        Ok(AST::Lines(lines))
    }

    /// Parses an expression.
    ///
    /// This works by calling a helper function that parses a part of an expression. Once that
    /// helper function returned the AST of the expression snipper, this function reads the next
    /// operator and creates a new AST node. The currently parsed AST becomes the left hand side of
    /// the new node and the right hand side is once again determined by the helper function.
    fn parse_expression(&mut self) -> Result<AST, ParseError> {
        let mut lhs = self.parse_expression_with_min_precedence(0)?;
        while let Some(Token {
            kind: TokenKind::Operator(op),
            pos,
        }) = self.peek()
        {
            let op = *op;
            let pos = *pos;
            self.next();
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
    ) -> Result<AST, ParseError> {
        match self.peek() {
            Some(token) => {
                match token.kind {
                    TokenKind::Operator(Operator::Minus) => {
                        let pos = token.pos;
                        self.next();
                        let unary_minus_precedence = op_precedence(Operator::Minus, false, pos)?;
                        // Not `+ 1` like in the other cases so we can take multiple unary minus operators
                        // after each other
                        let rhs =
                            self.parse_expression_with_min_precedence(unary_minus_precedence)?;
                        Ok(AST::UnaryMinus(Box::new(rhs)))
                    }
                    TokenKind::Operator(Operator::Exclamation) => {
                        let pos = token.pos;
                        self.next();
                        let boolean_negate_precedence =
                            op_precedence(Operator::Exclamation, false, pos)?;
                        // Not `+ 1` like in the other cases so we can take multiple boolean negate operators
                        // after each other
                        let rhs =
                            self.parse_expression_with_min_precedence(boolean_negate_precedence)?;
                        Ok(AST::BooleanNegate(Box::new(rhs)))
                    }
                    TokenKind::LParen => {
                        self.next();
                        let inner = self.parse_expression()?;
                        self.expect(TokenKind::RParen)?;
                        Ok(AST::Brackets(Box::new(inner)))
                    }
                    TokenKind::Identifier(_)
                        if self.peek_nth_kind(2) == Some(&TokenKind::LParen) =>
                    {
                        self.parse_function_call()
                    }
                    TokenKind::Identifier(_)
                    | TokenKind::Number(_)
                    | TokenKind::Keyword(Keyword::True | Keyword::False) => {
                        let mut lhs = self.parse_identifier_or_value()?;
                        while let Some(Token {
                            kind: TokenKind::Operator(op),
                            pos,
                        }) = self.peek()
                        {
                            let op = *op;
                            let precedence = op_precedence(op, true, *pos)?;
                            if precedence >= min_precedence {
                                self.next();
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

    fn parse_identifier_or_value(&mut self) -> Result<AST, ParseError> {
        match self.next() {
            Some(token) => match &token.kind {
                TokenKind::Identifier(name) => Ok(AST::Variable(name.clone())),
                TokenKind::Number(num) => Ok(AST::NumberLiteral(*num)),
                TokenKind::Keyword(Keyword::True) => Ok(AST::BooleanLiteral(true)),
                TokenKind::Keyword(Keyword::False) => Ok(AST::BooleanLiteral(false)),
                _ => Err(ParseError::UnexpectedToken {
                    token: token.clone(),
                    expected: None,
                }),
            },
            None => Err(ParseError::NoTokensLeft),
        }
    }

    fn parse_function_call(&mut self) -> Result<AST, ParseError> {
        // <name>(<val1>, <val2>, ...)
        let fn_name = self.expect_identifier()?.to_string();
        self.expect(TokenKind::LParen)?;
        let mut args = Vec::new();
        while self.peek_kind() != Some(&TokenKind::RParen) {
            let arg = self.parse_expression()?;
            args.push(arg);

            match self.peek_kind() {
                // TODO: Remove guard once trailing commas are allowed
                Some(TokenKind::Comma) if self.peek_nth_kind(2) != Some(&TokenKind::RParen) => {
                    self.next();
                }
                // Let `expect` after loop handle the error
                _ => break,
            }
        }
        self.expect(TokenKind::RParen)?;
        Ok(AST::FunctionCall(fn_name, args))
    }

    fn parse_assignment(&mut self) -> Result<AST, ParseError> {
        let var_name = self.expect_identifier()?.to_string();
        self.expect(TokenKind::Equal)?;
        let rhs = self.parse_expression()?;
        Ok(AST::Assign(var_name, Box::new(rhs)))
    }

    fn parse_function_definition(&mut self) -> Result<AST, ParseError> {
        // fn <name> (<arg1>, <arg2>, ...) { <body> }
        self.expect(TokenKind::Keyword(Keyword::Fn))?;
        let fn_name = self.expect_identifier()?.to_string();
        self.expect(TokenKind::LParen)?;

        let mut arg_names = Vec::new();
        while let Some(TokenKind::Identifier(arg_name)) = self.peek_kind() {
            arg_names.push(arg_name.to_string());
            self.next();

            match self.peek_kind() {
                // TODO: Remove guard when trailing commas are allowed
                Some(TokenKind::Comma) if self.peek_nth_kind(2) != Some(&TokenKind::RParen) => {
                    self.next();
                }
                _ => break,
            }
        }

        self.expect(TokenKind::RParen)?;
        self.skip_newlines();
        self.expect(TokenKind::LBrace)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::RBrace)?;
        Ok(AST::FunctionDefinition {
            name: fn_name,
            arg_names,
            body: Box::new(body),
        })
    }

    fn parse_if_statement(&mut self) -> Result<AST, ParseError> {
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

        Ok(AST::IfStatement {
            condition: Box::new(condition),
            if_body: Box::new(if_body),
            else_body,
        })
    }

    fn parse_while_loop(&mut self) -> Result<AST, ParseError> {
        // while ( <expr> ) { <body> }
        self.expect(TokenKind::Keyword(Keyword::While))?;
        self.expect(TokenKind::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(TokenKind::RParen)?;
        self.skip_newlines();

        self.expect(TokenKind::LBrace)?;
        let body = self.parse_block()?;
        self.expect(TokenKind::RBrace)?;

        Ok(AST::WhileLoop {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_return(&mut self) -> Result<AST, ParseError> {
        // return [ <expr> ]
        self.expect(TokenKind::Keyword(Keyword::Return))?;
        let expr = match self.peek_kind() {
            Some(&TokenKind::Newline | &TokenKind::RBrace) => None,
            _ => Some(Box::new(self.parse_expression()?)),
        };
        Ok(AST::Return(expr))
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
        let mut peek_idx = 1;
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
