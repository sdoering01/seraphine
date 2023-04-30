use crate::error::TokenizeError;

#[derive(Debug, Clone)]
pub enum Keyword {
    Fn,
    If,
}

#[derive(Debug, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Number(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Equal,
    Newline,
}

pub fn tokenize(s: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = vec![];

    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        let token = match c {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '^' => Token::Caret,
            ',' => Token::Comma,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '%' => Token::Percent,
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
