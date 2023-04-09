// TODO: Add unary + and - Ops
// TODO: Build AST in correct order (e.g. * and / before + and +)

use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
enum TokenizeError {
    UnexpectedChar(char),
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use TokenizeError::*;
        match self {
            UnexpectedChar(c) => write!(f, "Unexpected char {}", c),
        }
    }
}

#[derive(Debug)]
enum ParseError {
    NoTokensLeft,
    UnexpectedToken(Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ParseError::*;
        match self {
            NoTokensLeft => write!(f, "No tokens left to parse"),
            UnexpectedToken(t) => write!(f, "Unexpected token {:?}", t),
        }
    }
}

#[derive(Debug)]
enum EvalError {
    DivideByZero,
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            DivideByZero => write!(f, "Divide by zero"),
        }
    }
}

#[derive(Debug, Clone)]
enum Token {
    Number(i64),
    Plus,
    Minus,
    Times,
    Divide,
}

#[derive(Debug)]
enum AST {
    Number(i64),
    Add(Box<AST>, Box<AST>),
    Subtract(Box<AST>, Box<AST>),
    Multiply(Box<AST>, Box<AST>),
    Divide(Box<AST>, Box<AST>),
}

fn tokenize(s: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = vec![];

    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        let token = match c {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Times,
            '/' => Token::Divide,
            num_char @ '0'..='9' => {
                let mut num = num_char as i64 - '0' as i64;
                while let Some(c) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            let num_char = chars.next().unwrap();
                            num = 10 * num + (num_char as i64 - '0' as i64);
                        }
                        _ => break,
                    }
                }
                Token::Number(num)
            }
            c if c.is_ascii_whitespace() => continue,
            c => return Err(TokenizeError::UnexpectedChar(c)),
        };
        tokens.push(token);
    }

    Ok(tokens)
}

fn parse(mut tokens: Vec<Token>) -> Result<AST, ParseError> {
    tokens.reverse();
    parse_inner(&tokens)
}

fn parse_inner(tokens: &[Token]) -> Result<AST, ParseError> {
    if tokens.len() == 0 {
        return Err(ParseError::NoTokensLeft);
    }

    // Simple parse from left to right
    // NOTE: This is not the correct order!
    let ast = match (tokens.len(), &tokens[0]) {
        // Checked above
        (0, _) => unreachable!(),
        (1, Token::Number(n)) => AST::Number(*n),
        (_, Token::Number(n)) => match &tokens[1] {
            t @ Token::Number(_) => return Err(ParseError::UnexpectedToken(t.clone())),
            Token::Plus => AST::Add(
                Box::new(AST::Number(*n)),
                Box::new(parse_inner(&tokens[2..])?),
            ),
            Token::Minus => AST::Subtract(
                Box::new(AST::Number(*n)),
                Box::new(parse_inner(&tokens[2..])?),
            ),
            Token::Times => AST::Multiply(
                Box::new(AST::Number(*n)),
                Box::new(parse_inner(&tokens[2..])?),
            ),
            Token::Divide => AST::Divide(
                Box::new(AST::Number(*n)),
                Box::new(parse_inner(&tokens[2..])?),
            ),
        },
        (_, token @ (Token::Plus | Token::Minus | Token::Times | Token::Divide)) => {
            return Err(ParseError::UnexpectedToken(token.clone()))
        }
    };

    Ok(ast)
}

fn evaluate(ast: &AST) -> Result<i64, EvalError> {
    let result = match ast {
        AST::Number(n) => *n,
        AST::Add(lhs, rhs) => evaluate(lhs)? + evaluate(rhs)?,
        AST::Subtract(lhs, rhs) => evaluate(lhs)? - evaluate(rhs)?,
        AST::Multiply(lhs, rhs) => evaluate(lhs)? * evaluate(rhs)?,
        AST::Divide(lhs, rhs) => {
            let lval = evaluate(lhs)?;
            let rval = evaluate(rhs)?;
            if rval == 0 {
                return Err(EvalError::DivideByZero);
            }
            lval / rval
        }
    };

    Ok(result)
}

fn main() {
    let expression = "2 + 2 * 5 + 2";

    println!("Expression: {}", expression);

    let tokens = match tokenize(expression) {
        Ok(tokens) => {
            println!("Tokens: {:?}", tokens);
            tokens
        }
        Err(e) => {
            println!("Tokenize error: {}", e);
            return;
        }
    };

    let ast = match parse(tokens) {
        Ok(ast) => {
            println!("AST: {:?}", ast);
            ast
        }
        Err(e) => {
            println!("Parse error: {}", e);
            return;
        }
    };

    match evaluate(&ast) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Evaluate error: {}", e),
    }
}
