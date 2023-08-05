use std::{iter::Peekable, str::Chars};

use crate::{error::TokenizeError, common::Pos};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    If,
    Else,
    While,
    Return,
    True,
    False,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `^`
    Caret,
    /// `!`
    Exclamation,
    /// `==`
    Equal,
    /// `!=`
    Unequal,
    /// `<`
    LessThan,
    /// `>`
    GreaterThan,
    /// `<=`
    LessThanOrEqual,
    /// `>=`
    GreaterThanOrEqual,
    /// `&&`
    And,
    /// `||`
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword(Keyword),
    Identifier(String),
    Number(f64),
    Operator(Operator),
    /// `,`
    Comma,
    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `=`
    Equal,
    Newline,
    /// End of file
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub pos: Pos,
    pub kind: TokenKind,
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenizeError> {
    Tokenizer::new(s).tokenize()
}

struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
    idx: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            chars: s.chars().peekable(),
            idx: 0,
        }
    }

    fn next(&mut self) -> Option<char> {
        self.idx += 1;
        self.chars.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Checks whether the next token is equal to `c`. If that is the case, advance the underlying
    /// iterator and returns true, else returns false.
    fn take(&mut self, c: char) -> bool {
        if self.peek() == Some(&c) {
            self.next();
            true
        } else {
            false
        }
    }

    fn tokenize(&mut self) -> Result<Vec<Token>, TokenizeError> {
        let mut tokens = vec![];

        while let Some(c) = self.next() {
            let token_start_pos = self.idx - 1;
            let token_kind = match c {
                '/' if self.take('/') => {
                    while let Some(c) = self.peek() {
                        if *c == '\n' {
                            break;
                        }
                        self.next();
                    }
                    continue
                }
                '!' if self.take('=') => TokenKind::Operator(Operator::Unequal),
                '=' if self.take('=') => TokenKind::Operator(Operator::Equal),
                '<' if self.take('=') => TokenKind::Operator(Operator::LessThanOrEqual),
                '>' if self.take('=') => TokenKind::Operator(Operator::GreaterThanOrEqual),
                '&' if self.take('&') => TokenKind::Operator(Operator::And),
                '|' if self.take('|') => TokenKind::Operator(Operator::Or),
                '!' => TokenKind::Operator(Operator::Exclamation),
                '<' => TokenKind::Operator(Operator::LessThan),
                '>' => TokenKind::Operator(Operator::GreaterThan),
                '+' => TokenKind::Operator(Operator::Plus),
                '-' => TokenKind::Operator(Operator::Minus),
                '*' => TokenKind::Operator(Operator::Star),
                '/' => TokenKind::Operator(Operator::Slash),
                '^' => TokenKind::Operator(Operator::Caret),
                '%' => TokenKind::Operator(Operator::Percent),
                ',' => TokenKind::Comma,
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '=' => TokenKind::Equal,
                c @ ('0'..='9' | '.') => {
                    let mut has_dot = c == '.';

                    let mut num = String::new();
                    num.push(c);
                    while let Some(c) = self.peek() {
                        match c {
                            '.' => {
                                if has_dot {
                                    return Err(TokenizeError::UnexpectedChar{got: '.', pos: self.idx});
                                }
                                has_dot = true;
                            }
                            '0'..='9' => (),
                            _ => break,
                        }
                        let c = self.next().unwrap();
                        num.push(c);
                    }

                    if num == "." {
                        return Err(TokenizeError::UnexpectedChar{ got: '.', pos: self.idx - 1 });
                    }

                    let Ok(n) = num.parse() else {
                        return Err(TokenizeError::MalformedNumber { number_str: num, pos: token_start_pos })
                    };
                    TokenKind::Number(n)
                }
                c @ ('a'..='z' | 'A'..='Z' | '_') => {
                    let mut ident = String::new();
                    ident.push(c);
                    while let Some(c) = self.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                                let c = self.next().unwrap();
                                ident.push(c);
                            }
                            _ => break,
                        }
                    }

                    match ident.as_str() {
                        "fn" => TokenKind::Keyword(Keyword::Fn),
                        "if" => TokenKind::Keyword(Keyword::If),
                        "else" => TokenKind::Keyword(Keyword::Else),
                        "while" => TokenKind::Keyword(Keyword::While),
                        "return" => TokenKind::Keyword(Keyword::Return),
                        "true" => TokenKind::Keyword(Keyword::True),
                        "false" => TokenKind::Keyword(Keyword::False),
                        _ => TokenKind::Identifier(ident),
                    }
                }
                // TODO: Account for \r\n
                '\n' => TokenKind::Newline,
                c if c.is_ascii_whitespace() => continue,
                c => return Err(TokenizeError::UnexpectedChar{ got: c, pos: self.idx - 1 }),
            };

            let token = Token {
                kind: token_kind,
                pos: token_start_pos,
            };
            tokens.push(token);
        }

        let eof_token = Token {
            kind: TokenKind::Eof,
            pos: self.idx - 1,
        };
        tokens.push(eof_token);

        Ok(tokens)
    }
}
