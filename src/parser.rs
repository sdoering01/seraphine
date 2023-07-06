use crate::{
    error::{OperatorKind, ParseError},
    tokenizer::{Keyword, Operator, Token},
};

#[derive(Debug, Clone)]
pub enum AST {
    Lines(Vec<AST>),
    Number(String),
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
}

/// Returns the precedence of the operator.
///
/// Higher precedence means that the operator is calculated first (e.g. multiplication has higher
/// precedence than addition).
/// `is_binary` provides information about the operator being used as a
/// unary or binary operator (i.e. if `is_binary` is false, the operator is unary).
fn op_precedence(op: Operator, is_binary: bool) -> Result<u8, ParseError> {
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
            return Err(ParseError::InvalidOperator(op, kind));
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
    pos: usize,
}

// TODO: Allow newlines in more places (e.g. argument list of function definition)
// TODO: After that, allow optional comma at the end of argument lists
impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, pos: 0 }
    }

    /// Entrypoint to the parser
    fn parse(&mut self) -> Result<AST, ParseError> {
        let ast = self.parse_block()?;
        // If some function stopped parsing for some reason and we haven't parsed all tokens, the
        // token at the current position is unexpected.
        //
        // For example: A `}`, where the function stops parsing to let the caller decide whether
        // the token makes sense at this place.
        if self.pos < self.tokens.len() {
            return Err(ParseError::UnexpectedToken(self.tokens[self.pos].clone()));
        }
        Ok(ast)
    }

    fn parse_block(&mut self) -> Result<AST, ParseError> {
        let mut lines = Vec::new();
        let mut want_newline_this_iteration = false;
        while let Some(token) = self.peek() {
            let (line, want_newline_next_iteration) = match token {
                Token::Newline => {
                    self.next();
                    (None, false)
                }
                Token::RBrace => break,
                // All following constructs can only appear at the beginning of a line
                _ if want_newline_this_iteration => {
                    return Err(ParseError::ExpectedToken(Token::Newline));
                }
                Token::Keyword(Keyword::Fn) => (Some(self.parse_function_definition()?), true),
                Token::Keyword(Keyword::If) => (Some(self.parse_if_statement()?), true),
                Token::Keyword(Keyword::While) => (Some(self.parse_while_loop()?), true),
                Token::Identifier(_) if self.peek_nth(2) == Some(&Token::Equal) => {
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
    /// This works by calling another function that attaches expressions with operators of higher
    /// precedence to the right hand side of the current operator. Once there are no operators of
    /// higher precedence, it reads the next operator and creates a new AST node. The currently
    /// parsed AST becomes the left hand side of the new node and the right hand side is once again
    /// determined by the other function.
    fn parse_expression(&mut self) -> Result<AST, ParseError> {
        let mut lhs = self.parse_expression_with_min_precedence(0)?;
        while let Some(Token::Operator(op)) = self.peek() {
            let op = *op;
            self.next();
            let precedence = op_precedence(op, true)?;
            let rhs = self.parse_expression_with_min_precedence(precedence + 1)?;
            lhs = combine_lhs_rhs(op, lhs, rhs)?;
        }
        Ok(lhs)
    }

    /// Helper function for `parse_expression` that parses an expression that includes operators of
    /// equal or higher precedence than `min_precedence`.
    ///
    /// This function recursively calls itself to build up a chain of operators of increasing
    /// precedence. The base case of the recursion is reached when the next operator has smaller or
    /// equal precedence than the previous one. This will return the current chain.
    ///
    /// ## Example
    ///
    /// Calling the function with the input `1 + 2 * 3 ^ 4 + 5` would stop at the last `+` and
    /// would produce the following AST:
    ///
    ///              +
    ///            1   *
    ///              2   ^
    ///                3   4
    ///
    /// Or in another notation: Add(1, Multiply(2, Power(3, 4))
    fn parse_expression_with_min_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<AST, ParseError> {
        match self.peek() {
            // Parse logic for unary operators could be combined if we add more of them
            Some(Token::Operator(Operator::Minus)) => {
                self.next();
                let unary_minus_precedence = op_precedence(Operator::Minus, false)?;
                // Not `+ 1` like in the other cases so we can take multiple unary minus operators
                // after each other
                let rhs = self.parse_expression_with_min_precedence(unary_minus_precedence)?;
                Ok(AST::UnaryMinus(Box::new(rhs)))
            }
            Some(Token::Operator(Operator::Exclamation)) => {
                self.next();
                let boolean_negate_precedence = op_precedence(Operator::Exclamation, false)?;
                // Not `+ 1` like in the other cases so we can take multiple boolean negate operators
                // after each other
                let rhs = self.parse_expression_with_min_precedence(boolean_negate_precedence)?;
                Ok(AST::BooleanNegate(Box::new(rhs)))
            }
            Some(Token::LParen) => {
                self.next();
                let inner = self.parse_expression()?;
                self.expect(Token::RParen)?;
                Ok(AST::Brackets(Box::new(inner)))
            }
            Some(Token::Identifier(_) | Token::Number(_)) => {
                if self.peek_nth(2) == Some(&Token::LParen) {
                    self.parse_function_call()
                } else {
                    let lhs = self.parse_identifier_or_value()?;
                    match self.peek() {
                        Some(Token::Operator(op)) => {
                            let precedence = op_precedence(*op, true)?;
                            if precedence >= min_precedence {
                                let op = op.clone();
                                self.next();
                                let rhs =
                                    self.parse_expression_with_min_precedence(precedence + 1)?;
                                combine_lhs_rhs(op, lhs, rhs)
                            } else {
                                Ok(lhs)
                            }
                        }
                        _ => Ok(lhs),
                    }
                }
            }
            Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
            None => Err(ParseError::NoTokensLeft),
        }
    }

    fn parse_identifier_or_value(&mut self) -> Result<AST, ParseError> {
        match self.next() {
            Some(Token::Identifier(name)) => Ok(AST::Variable(name.clone())),
            Some(Token::Number(num)) => Ok(AST::Number(num.clone())),
            Some(token) => Err(ParseError::UnexpectedToken(token.clone())),
            None => Err(ParseError::NoTokensLeft),
        }
    }

    fn parse_function_call(&mut self) -> Result<AST, ParseError> {
        // <name>(<val1>, <val2>, ...)
        let fn_name = self.expect_identifier()?.to_string();
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        while self.peek() != Some(&Token::RParen) {
            let arg = self.parse_expression()?;
            args.push(arg);

            match self.peek() {
                // TODO: Remove guard once trailing commas are allowed
                Some(Token::Comma) if self.peek_nth(2) != Some(&Token::RParen) => {
                    self.next();
                }
                // Let `expect` after loop handle the error
                _ => break,
            }
        }
        self.expect(Token::RParen)?;
        Ok(AST::FunctionCall(fn_name, args))
    }

    fn parse_assignment(&mut self) -> Result<AST, ParseError> {
        let var_name = self.expect_identifier()?.to_string();
        self.expect(Token::Equal)?;
        let rhs = self.parse_expression()?;
        Ok(AST::Assign(var_name, Box::new(rhs)))
    }

    fn parse_function_definition(&mut self) -> Result<AST, ParseError> {
        // fn <name> (<arg1>, <arg2>, ...) { <body> }
        self.expect(Token::Keyword(Keyword::Fn))?;
        let fn_name = self.expect_identifier()?.to_string();
        self.expect(Token::LParen)?;

        let mut arg_names = Vec::new();
        while let Some(Token::Identifier(arg_name)) = self.peek() {
            arg_names.push(arg_name.to_string());
            self.next();

            match self.peek() {
                // TODO: Remove guard when trailing commas are allowed
                Some(Token::Comma) if self.peek_nth(2) != Some(&Token::RParen) => {
                    self.next();
                }
                _ => break,
            }
        }

        self.expect(Token::RParen)?;
        self.skip_newlines();
        self.expect(Token::LBrace)?;
        let body = self.parse_block()?;
        self.expect(Token::RBrace)?;
        Ok(AST::FunctionDefinition {
            name: fn_name,
            arg_names,
            body: Box::new(body),
        })
    }

    fn parse_if_statement(&mut self) -> Result<AST, ParseError> {
        // if ( <expr> ) { <body> } [ else if ( <expr> ) { <body> } [ ... ] ] [ else { <body> } ]
        self.expect(Token::Keyword(Keyword::If))?;
        self.expect(Token::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::RParen)?;
        self.skip_newlines();
        self.expect(Token::LBrace)?;
        let if_body = self.parse_block()?;
        self.skip_newlines();
        self.expect(Token::RBrace)?;

        let else_body = if self.peek_next_non_newline() == Some(&Token::Keyword(Keyword::Else)) {
            self.skip_newlines();
            self.next();
            if self.peek() == Some(&Token::Keyword(Keyword::If)) {
                let else_if_statement = self.parse_if_statement()?;
                Some(Box::new(else_if_statement))
            } else {
                self.skip_newlines();
                self.expect(Token::LBrace)?;
                let else_body = self.parse_block()?;
                self.skip_newlines();
                self.expect(Token::RBrace)?;
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
        self.expect(Token::Keyword(Keyword::While))?;
        self.expect(Token::LParen)?;
        let condition = self.parse_expression()?;
        self.expect(Token::RParen)?;
        self.skip_newlines();

        self.expect(Token::LBrace)?;
        let body = self.parse_block()?;
        self.expect(Token::RBrace)?;

        Ok(AST::WhileLoop {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    /// Takes the next token, behaving like `next` of an iterator.
    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    /// Peeks the nth token.
    ///
    /// Peek with n = 1 behaves like `peek` of an iterator, peeking the next available token.
    fn peek_nth(&self, n: usize) -> Option<&Token> {
        self.tokens.get(self.pos + n - 1)
    }

    /// Peeks the next token, behaving like `peek` of an iterator.
    fn peek(&self) -> Option<&Token> {
        self.peek_nth(1)
    }

    /// Peeks the next token that isn't a newline.
    ///
    /// [`Self::skip_newlines`] and [`Self::next`] can be used to advance the position to the next non-newline
    /// token.
    fn peek_next_non_newline(&self) -> Option<&Token> {
        let mut peek_idx = 1;
        while let Some(Token::Newline) = self.peek_nth(peek_idx) {
            peek_idx += 1;
        }

        self.peek_nth(peek_idx)
    }

    /// Asserts that `expected` is the next token, while also advancing the position.
    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        let actual = self.next();
        if actual != Some(&expected) {
            return Err(ParseError::ExpectedToken(expected));
        }
        Ok(())
    }

    /// Asserts that the next token is an identifier, returning the inner string slice of the
    /// identifier and advancing the position.
    fn expect_identifier(&mut self) -> Result<&str, ParseError> {
        match self.next() {
            Some(Token::Identifier(ref name)) => Ok(name),
            _ => Err(ParseError::ExpectedIdentifier),
        }
    }

    /// Advanced the position until the next token is not a newline.
    fn skip_newlines(&mut self) {
        while self.peek() == Some(&Token::Newline) {
            self.next();
        }
    }
}
