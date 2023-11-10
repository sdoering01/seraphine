use std::{iter::Peekable, str::Chars};

use crate::{
    common::Span,
    error::TokenizeError,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    If,
    Else,
    While,
    For,
    In,
    Continue,
    Break,
    Return,
    True,
    False,
    Null,
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
    String(String),
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
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `=`
    Equal,
    /// '.'
    Dot,
    /// ':'
    Colon,
    Newline,
    /// End of file
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub span: Span,
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

    fn tokenize(&mut self) -> Result<Vec<Token>, TokenizeError> {
        let mut tokens = vec![];

        while let Some(c) = self.next() {
            let token_start_pos = self.idx - 1;
            let token_kind = match (c, self.peek()) {
                ('/', Some('/')) => {
                    self.next();
                    while let Some(c) = self.peek() {
                        if *c == '\n' {
                            break;
                        }
                        self.next();
                    }
                    continue;
                }
                ('!', Some('=')) => {
                    self.next();
                    TokenKind::Operator(Operator::Unequal)
                }
                ('=', Some('=')) => {
                    self.next();
                    TokenKind::Operator(Operator::Equal)
                }
                ('<', Some('=')) => {
                    self.next();
                    TokenKind::Operator(Operator::LessThanOrEqual)
                }
                ('>', Some('=')) => {
                    self.next();
                    TokenKind::Operator(Operator::GreaterThanOrEqual)
                }
                ('&', Some('&')) => {
                    self.next();
                    TokenKind::Operator(Operator::And)
                }
                ('|', Some('|')) => {
                    self.next();
                    TokenKind::Operator(Operator::Or)
                }
                ('!', _) => TokenKind::Operator(Operator::Exclamation),
                ('<', _) => TokenKind::Operator(Operator::LessThan),
                ('>', _) => TokenKind::Operator(Operator::GreaterThan),
                ('+', _) => TokenKind::Operator(Operator::Plus),
                ('-', _) => TokenKind::Operator(Operator::Minus),
                ('*', _) => TokenKind::Operator(Operator::Star),
                ('/', _) => TokenKind::Operator(Operator::Slash),
                ('^', _) => TokenKind::Operator(Operator::Caret),
                ('%', _) => TokenKind::Operator(Operator::Percent),
                (',', _) => TokenKind::Comma,
                ('(', _) => TokenKind::LParen,
                (')', _) => TokenKind::RParen,
                ('{', _) => TokenKind::LBrace,
                ('}', _) => TokenKind::RBrace,
                ('[', _) => TokenKind::LBracket,
                (']', _) => TokenKind::RBracket,
                ('=', _) => TokenKind::Equal,
                (c @ ('0'..='9'), _) | (c @ '.', Some('0'..='9')) => {
                    let mut has_dot = c == '.';
                    let mut has_e = false;

                    let mut num = String::new();
                    num.push(c);
                    while let Some(c) = self.peek() {
                        match c {
                            '.' => {
                                if has_dot || has_e {
                                    return Err(TokenizeError::UnexpectedChar {
                                        got: '.',
                                        pos: self.idx,
                                    });
                                }
                                has_dot = true;
                            }
                            'e' => {
                                if has_e {
                                    return Err(TokenizeError::UnexpectedChar {
                                        got: 'e',
                                        pos: self.idx,
                                    });
                                }
                                has_e = true;
                            }
                            '0'..='9' => (),
                            _ => break,
                        }
                        let c = self.next().unwrap();
                        num.push(c);

                        if c == 'e' && self.peek() == Some(&'-') {
                            num.push(self.next().unwrap());
                        }
                    }

                    if num == "." {
                        return Err(TokenizeError::UnexpectedChar {
                            got: '.',
                            pos: self.idx - 1,
                        });
                    }

                    let Ok(n) = num.parse() else {
                        return Err(TokenizeError::MalformedNumber {
                            number_str: num,
                            pos: token_start_pos,
                        });
                    };
                    TokenKind::Number(n)
                }
                ('.', _) => TokenKind::Dot,
                (':', _) => TokenKind::Colon,
                ('"', _) => {
                    let mut str = String::new();
                    let mut terminated = false;
                    while let Some(c) = self.next() {
                        match c {
                            '"' => {
                                terminated = true;
                                break;
                            }
                            '\n' => break,
                            '\\' => match self.next() {
                                Some('"') => str.push('"'),
                                Some('n') => str.push('\n'),
                                Some('r') => str.push('\r'),
                                Some('t') => str.push('\t'),
                                Some('\\') => str.push('\\'),
                                Some('0') => str.push('\0'),
                                Some(c) => {
                                    return Err(TokenizeError::UnexpectedChar {
                                        got: c,
                                        pos: self.idx - 1,
                                    })
                                }
                                None => break,
                            },
                            _ => str.push(c),
                        }
                    }

                    if !terminated {
                        return Err(TokenizeError::UnterminatedString {
                            pos: token_start_pos,
                        });
                    }

                    TokenKind::String(str)
                }
                (c @ ('a'..='z' | 'A'..='Z' | '_'), _) => {
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
                        "for" => TokenKind::Keyword(Keyword::For),
                        "in" => TokenKind::Keyword(Keyword::In),
                        "continue" => TokenKind::Keyword(Keyword::Continue),
                        "break" => TokenKind::Keyword(Keyword::Break),
                        "return" => TokenKind::Keyword(Keyword::Return),
                        "true" => TokenKind::Keyword(Keyword::True),
                        "false" => TokenKind::Keyword(Keyword::False),
                        "null" => TokenKind::Keyword(Keyword::Null),
                        _ => TokenKind::Identifier(ident),
                    }
                }
                // TODO: Account for \r\n
                ('\n', _) => TokenKind::Newline,
                (c, _) if c.is_ascii_whitespace() => continue,
                (c, _) => {
                    return Err(TokenizeError::UnexpectedChar {
                        got: c,
                        pos: self.idx - 1,
                    })
                }
            };

            let token_len = self.idx - token_start_pos;
            let token = Token {
                kind: token_kind,
                span: Span::new(token_start_pos, token_len),
            };
            tokens.push(token);
        }

        let eof_token = Token {
            kind: TokenKind::Eof,
            span: Span::new(self.idx - 1, 1),
        };
        tokens.push(eof_token);

        Ok(tokens)
    }
}
