// TODO: Add unary + and - Ops
// TODO: Add brackets

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

fn parse(tokens: &[Token]) -> Result<AST, ParseError> {
    if tokens.len() == 0 {
        return Err(ParseError::NoTokensLeft);
    }

    if tokens.len() == 1 {
        match &tokens[0] {
            Token::Number(num) => return Ok(AST::Number(*num)),
            token @ (Token::Plus | Token::Minus | Token::Times | Token::Divide) => {
                return Err(ParseError::UnexpectedToken(token.clone()))
            }
        }
    }

    let mut last_pls_mns_idx = None;
    let mut last_tim_div_idx = None;

    for (idx, token) in tokens.iter().enumerate() {
        match token {
            Token::Plus | Token::Minus => last_pls_mns_idx = Some(idx),
            Token::Times | Token::Divide => last_tim_div_idx = Some(idx),
            _ => (),
        }
    }

    // Start building AST from the operators of lowest precedence so that those operators are
    // applied last
    if let Some(idx) = last_pls_mns_idx {
        let l_ast = Box::new(parse(&tokens[..idx])?);
        let r_ast = Box::new(parse(&tokens[(idx + 1)..])?);
        let ast = match tokens[idx] {
            Token::Plus => AST::Add(l_ast, r_ast),
            Token::Minus => AST::Subtract(l_ast, r_ast),
            _ => unreachable!(),
        };
        return Ok(ast);
    }

    if let Some(idx) = last_tim_div_idx {
        let l_ast = Box::new(parse(&tokens[..idx])?);
        let r_ast = Box::new(parse(&tokens[(idx + 1)..])?);
        let ast = match tokens[idx] {
            Token::Times => AST::Multiply(l_ast, r_ast),
            Token::Divide => AST::Divide(l_ast, r_ast),
            _ => unreachable!(),
        };
        return Ok(ast);
    }

    // If we checked for a slice that conatains a single number and haven't found any operators it
    // means that we have multiple numbers. The second number is an unexpected token.
    Err(ParseError::UnexpectedToken(tokens[1].clone()))
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
    let expression = "3 * 2 * 5 + 10 / 5 - 8";

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

    let ast = match parse(&tokens) {
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
