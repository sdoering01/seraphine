// TODO: Add brackets

use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
enum CalcError {
    TokenizeError(TokenizeError),
    ParseError(ParseError),
    EvalError(EvalError),
}

impl Display for CalcError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use CalcError::*;
        match self {
            TokenizeError(e) => write!(f, "Tokenize error: {}", e),
            ParseError(e) => write!(f, "Parse error: {}", e),
            EvalError(e) => write!(f, "Eval error: {}", e),
        }
    }
}

impl From<TokenizeError> for CalcError {
    fn from(e: TokenizeError) -> Self {
        Self::TokenizeError(e)
    }
}

impl From<ParseError> for CalcError {
    fn from(e: ParseError) -> Self {
        Self::ParseError(e)
    }
}

impl From<EvalError> for CalcError {
    fn from(e: EvalError) -> Self {
        Self::EvalError(e)
    }
}

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
    UnaryPlus(Box<AST>),
    UnaryMinus(Box<AST>),
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

    if tokens.len() == 2 {
        match (&tokens[0], &tokens[1]) {
            (Token::Plus, Token::Number(num)) => return Ok(AST::UnaryPlus(Box::new(AST::Number(*num)))),
            (Token::Minus, Token::Number(num)) => return Ok(AST::UnaryMinus(Box::new(AST::Number(*num)))),
            (t, _) => return Err(ParseError::UnexpectedToken(t.clone())),
        }
    }

    let mut last_pls_mns_idx = None;
    let mut last_tim_div_idx = None;

    for (prev_idx, token_window) in tokens.windows(2).enumerate() {
        let idx = prev_idx + 1;
        let prev_token = &token_window[0];
        let token = &token_window[1];
        match (prev_token, token) {
            // Only take plus or minus if it isn't unary
            (Token::Number(_), Token::Plus | Token::Minus) => last_pls_mns_idx = Some(idx),
            (_, Token::Times | Token::Divide) => last_tim_div_idx = Some(idx),
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
        AST::UnaryPlus(rhs) => evaluate(rhs)?,
        AST::UnaryMinus(rhs) => -evaluate(rhs)?,
    };

    Ok(result)
}

fn main() -> Result<(), CalcError> {
    let expression = "3 * 2 * 5 + 10 / 5 - 8";

    println!("Expression: {}", expression);

    let tokens = tokenize(expression)?;
    println!("Tokens: {:?}", tokens);

    let ast = parse(&tokens)?;
    println!("AST: {:?}", ast);

    let result = evaluate(&ast)?;
    println!("Result: {}", result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval_str(s: &str) -> Result<i64, CalcError> {
        let tokens = tokenize(s)?;
        let ast = parse(&tokens)?;
        let result = evaluate(&ast)?;
        Ok(result)
    }

    #[test]
    fn test_eval_str() {
        assert!(eval_str("").is_err());
        assert!(eval_str("-").is_err());
        assert!(eval_str("* 2").is_err());
        assert!(eval_str("2 +").is_err());
        assert_eq!(eval_str("2").unwrap(), 2);
        assert_eq!(eval_str("2 - 3").unwrap(), -1);
        assert_eq!(eval_str("2-3").unwrap(), -1);
        assert_eq!(eval_str("2 + 2 * 2").unwrap(), 6);
        assert_eq!(eval_str("3 * 2 * 5 + 10 / 5 - 8").unwrap(), 24);
    }

    #[test]
    fn test_unary_plus() {
        assert_eq!(eval_str("+2").unwrap(), 2);
        assert_eq!(eval_str("2-+2").unwrap(), 0);
        assert_eq!(eval_str("2++2").unwrap(), 4);
        assert_eq!(eval_str("+2++2").unwrap(), 4);
        assert!(eval_str("2+++2").is_err());
        assert!(eval_str("2*-+2").is_err());
    }

    #[test]
    fn test_unary_minus() {
        assert_eq!(eval_str("-2").unwrap(), -2);
        assert_eq!(eval_str("2--2").unwrap(), 4);
        assert_eq!(eval_str("2+-2").unwrap(), 0);
        assert_eq!(eval_str("-2+-2").unwrap(), -4);
        assert!(eval_str("2---2").is_err());
        assert!(eval_str("2*+-2").is_err());
    }
}
