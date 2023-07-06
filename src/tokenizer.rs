use crate::error::TokenizeError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Fn,
    If,
    Else,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Number(String),
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
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = vec![];

    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        let token = match c {
            '!' if chars.peek() == Some(&'=') => {
                chars.next();
                Token::Operator(Operator::Unequal)
            }
            '=' if chars.peek() == Some(&'=') => {
                chars.next();
                Token::Operator(Operator::Equal)
            }
            '<' if chars.peek() == Some(&'=') => {
                chars.next();
                Token::Operator(Operator::LessThanOrEqual)
            }
            '>' if chars.peek() == Some(&'=') => {
                chars.next();
                Token::Operator(Operator::GreaterThanOrEqual)
            }
            '!' => Token::Operator(Operator::Exclamation),
            '<' => Token::Operator(Operator::LessThan),
            '>' => Token::Operator(Operator::GreaterThan),
            '+' => Token::Operator(Operator::Plus),
            '-' => Token::Operator(Operator::Minus),
            '*' => Token::Operator(Operator::Star),
            '/' => Token::Operator(Operator::Slash),
            '^' => Token::Operator(Operator::Caret),
            '%' => Token::Operator(Operator::Percent),
            ',' => Token::Comma,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '=' => Token::Equal,
            c @ ('0'..='9' | '.') => {
                let mut has_dot = c == '.';

                let mut num = String::new();
                num.push(c);
                while let Some(c) = chars.peek() {
                    match c {
                        '.' => {
                            if has_dot {
                                return Err(TokenizeError::UnexpectedChar('.'));
                            }
                            has_dot = true;
                        }
                        '0'..='9' => (),
                        _ => break,
                    }
                    let c = chars.next().unwrap();
                    num.push(c);
                }

                if num == "." {
                    return Err(TokenizeError::UnexpectedChar('.'));
                }

                Token::Number(num)
            }
            c @ ('a'..='z' | 'A'..='Z' | '_') => {
                let mut ident = String::new();
                ident.push(c);
                while let Some(c) = chars.peek() {
                    match c {
                        'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                            let c = chars.next().unwrap();
                            ident.push(c);
                        }
                        _ => break,
                    }
                }

                match ident.as_str() {
                    "fn" => Token::Keyword(Keyword::Fn),
                    "if" => Token::Keyword(Keyword::If),
                    "else" => Token::Keyword(Keyword::Else),
                    _ => Token::Identifier(ident),
                }
            }
            // TODO: Account for \r\n
            '\n' => Token::Newline,
            c if c.is_ascii_whitespace() => continue,
            c => return Err(TokenizeError::UnexpectedChar(c)),
        };
        tokens.push(token);
    }

    Ok(tokens)
}
