use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

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
    ExpectedToken(Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ParseError::*;
        match self {
            NoTokensLeft => write!(f, "No tokens left to parse"),
            UnexpectedToken(t) => write!(f, "Unexpected token {:?}", t),
            ExpectedToken(t) => write!(f, "Expected token {:?}", t),
        }
    }
}

#[derive(Debug)]
enum EvalError {
    DivideByZero,
    Overflow,
    VariableNotDefined(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use EvalError::*;
        match self {
            DivideByZero => write!(f, "Divide by zero"),
            Overflow => write!(f, "Overflow"),
            VariableNotDefined(name) => write!(f, "Variable with name '{}' is not defined", name),
        }
    }
}

#[derive(Debug, Clone)]
enum Token {
    Identifier(String),
    Number(String),
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    LBracket,
    RBracket,
    Equal,
}

#[derive(Debug)]
enum AST {
    Number(String),
    Variable(String),
    Add(Box<AST>, Box<AST>),
    Subtract(Box<AST>, Box<AST>),
    Multiply(Box<AST>, Box<AST>),
    Divide(Box<AST>, Box<AST>),
    Modulo(Box<AST>, Box<AST>),
    Power(Box<AST>, Box<AST>),
    UnaryPlus(Box<AST>),
    UnaryMinus(Box<AST>),
    Brackets(Box<AST>),
    Assign(String, Box<AST>),
}

#[derive(Debug)]
struct Context {
    variables: HashMap<String, i64>,
}

impl Context {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn get_var(&self, name: &str) -> Option<i64> {
        self.variables.get(name).copied()
    }

    fn set_var(&mut self, name: impl Into<String>, val: i64) {
        self.variables.insert(name.into(), val);
    }
}

fn tokenize(s: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = vec![];

    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        let token = match c {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '^' => Token::Caret,
            '(' => Token::LBracket,
            ')' => Token::RBracket,
            '%' => Token::Percent,
            '=' => Token::Equal,
            c @ '0'..='9' => {
                let mut num = String::new();
                num.push(c);
                while let Some(c) = chars.peek() {
                    match c {
                        '0'..='9' => {
                            let c = chars.next().unwrap();
                            num.push(c);
                        }
                        _ => break,
                    }
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
                Token::Identifier(ident)
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
            Token::Number(num) => return Ok(AST::Number(num.clone())),
            Token::Identifier(name) => return Ok(AST::Variable(name.clone())),
            token => return Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    let mut last_pls_mns_idx = None;
    let mut last_tim_div_mod_idx = None;
    let mut last_caret_idx = None;
    let mut first_eq_idx = None;
    let mut last_rbracket_idx = None;

    let mut bracket_depth = 0;
    for (prev_idx, token_window) in tokens.windows(2).enumerate() {
        let idx = prev_idx + 1;
        let prev_token = &token_window[0];
        let token = &token_window[1];

        if let Token::LBracket = prev_token {
            bracket_depth += 1;
        }
        if let Token::RBracket = token {
            last_rbracket_idx = Some(idx);
            bracket_depth -= 1;
            if bracket_depth < 0 {
                return Err(ParseError::UnexpectedToken(token.clone()));
            }
        }

        // Only take operators if they aren't inside of brackets, since brackets have higher
        // precedence
        if bracket_depth == 0 {
            match (prev_token, token) {
                (_, Token::Equal) if first_eq_idx.is_none() => first_eq_idx = Some(idx),
                // Only take plus or minus if they aren't unary
                (Token::Number(_) | Token::Identifier(_) | Token::RBracket, Token::Plus | Token::Minus) => last_pls_mns_idx = Some(idx),
                (_, Token::Star | Token::Slash | Token::Percent) => {
                    last_tim_div_mod_idx = Some(idx)
                }
                (_, Token::Caret) => last_caret_idx = Some(idx),
                _ => (),
            }
        }
    }

    if bracket_depth != 0 {
        return Err(ParseError::ExpectedToken(Token::RBracket));
    }
    let has_brackets = last_rbracket_idx.is_some();

    // Start building AST from the operators of lowest precedence so that those operators are
    // applied last
    if let Some(idx) = first_eq_idx {
        match (&tokens[0], idx) {
            (Token::Identifier(name), 1) => {
                let inner_ast = Box::new(parse(&tokens[2..])?);
                return Ok(AST::Assign(name.clone(), inner_ast));
            }
            _ => return Err(ParseError::UnexpectedToken(tokens[1].clone())),
        }
    }

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

    if let Some(idx) = last_tim_div_mod_idx {
        let l_ast = Box::new(parse(&tokens[..idx])?);
        let r_ast = Box::new(parse(&tokens[(idx + 1)..])?);
        let ast = match tokens[idx] {
            Token::Star => AST::Multiply(l_ast, r_ast),
            Token::Slash => AST::Divide(l_ast, r_ast),
            Token::Percent => AST::Modulo(l_ast, r_ast),
            _ => unreachable!(),
        };
        return Ok(ast);
    }

    if let Some(idx) = last_caret_idx {
        let l_ast = Box::new(parse(&tokens[..idx])?);
        let r_ast = Box::new(parse(&tokens[(idx + 1)..])?);
        return Ok(AST::Power(l_ast, r_ast));
    }

    // We checked for all operations outside of brackets, so if the token stream starts with a plus
    // or minus, it has to be unary.
    match &tokens[0] {
        Token::Plus => match &tokens[1] {
            token @ (Token::Plus | Token::Minus) => {
                return Err(ParseError::UnexpectedToken(token.clone()))
            }
            _ => {
                let inner_ast = Box::new(parse(&tokens[1..])?);
                return Ok(AST::UnaryPlus(inner_ast));
            }
        },
        Token::Minus => match &tokens[1] {
            token @ (Token::Plus | Token::Minus) => {
                return Err(ParseError::UnexpectedToken(token.clone()))
            }
            _ => {
                let inner_ast = Box::new(parse(&tokens[1..])?);
                return Ok(AST::UnaryMinus(inner_ast));
            }
        },
        _ => (),
    }

    if has_brackets {
        match (tokens.first(), tokens.last(), last_rbracket_idx) {
            (Some(Token::LBracket), Some(Token::RBracket), _) => {
                let inner_ast = Box::new(parse(&tokens[1..tokens.len() - 1])?);
                return Ok(AST::Brackets(inner_ast));
            }
            // Return correct error when a token that is not an operator follows the brackets
            (Some(Token::LBracket), _, Some(idx)) => {
                // SAFETY: The next index exists since all brackets are closed properly at this
                // point and the last index is not the last closing bracket.
                return Err(ParseError::UnexpectedToken(tokens[idx + 1].clone()));
            }
            _ => (),
        }
    }

    // If we checked for a slice that contains a single number and haven't found any operators, it
    // means that the token at the second index is unexpected.
    Err(ParseError::UnexpectedToken(tokens[1].clone()))
}

fn evaluate(ast: &AST, ctx: &mut Context) -> Result<i64, EvalError> {
    let result = match ast {
        AST::Number(n) => n.parse().map_err(|_| EvalError::Overflow)?,
        AST::Variable(name) => ctx
            .get_var(name)
            .ok_or_else(|| EvalError::VariableNotDefined(name.clone()))?,
        AST::Add(lhs, rhs) => evaluate(lhs, ctx)?
            .checked_add(evaluate(rhs, ctx)?)
            .ok_or(EvalError::Overflow)?,
        AST::Subtract(lhs, rhs) => evaluate(lhs, ctx)?
            .checked_sub(evaluate(rhs, ctx)?)
            .ok_or(EvalError::Overflow)?,
        AST::Multiply(lhs, rhs) => evaluate(lhs, ctx)?
            .checked_mul(evaluate(rhs, ctx)?)
            .ok_or(EvalError::Overflow)?,
        AST::Divide(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            if rval == 0 {
                return Err(EvalError::DivideByZero);
            }
            lval / rval
        }
        AST::Modulo(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            if rval == 0 {
                return Err(EvalError::DivideByZero);
            }
            lval % rval
        }
        AST::Power(lhs, rhs) => {
            let lval = evaluate(lhs, ctx)?;
            let rval = evaluate(rhs, ctx)?;
            // a ^ -b is defined as 1 / (a ^ b)
            if rval < 0 {
                match lval {
                    0 => return Err(EvalError::DivideByZero),
                    // 1 / (1 ^ N) = 1
                    1 => 1,
                    // 1 / (-1 ^ N) = { 1 if N even, -1 if N uneven }
                    -1 => {
                        if rval % 2 == 0 {
                            1
                        } else {
                            -1
                        }
                    }
                    // 1 / (n ^ N) = 0 where abs(n) >= 2
                    _ => 0,
                }
            } else {
                let rval = u32::try_from(rval).map_err(|_| EvalError::Overflow)?;
                lval.checked_pow(rval).ok_or(EvalError::Overflow)?
            }
        }
        AST::UnaryPlus(rhs) => evaluate(rhs, ctx)?,
        AST::UnaryMinus(rhs) => evaluate(rhs, ctx)?
            .checked_neg()
            .ok_or(EvalError::Overflow)?,
        AST::Brackets(inner) => evaluate(inner, ctx)?,
        AST::Assign(name, rhs) => {
            let rval = evaluate(rhs, ctx)?;
            ctx.set_var(name, rval);
            rval
        }
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

    let result = evaluate(&ast, &mut Context::new())?;
    println!("Result: {}", result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval_str(s: &str) -> Result<i64, CalcError> {
        let tokens = tokenize(s)?;
        let ast = parse(&tokens)?;
        let result = evaluate(&ast, &mut Context::new())?;
        Ok(result)
    }

    fn eval_str_ctx(s: &str, ctx: &mut Context) -> Result<i64, CalcError> {
        let tokens = tokenize(s)?;
        let ast = parse(&tokens)?;
        let result = evaluate(&ast, ctx)?;
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

    #[test]
    fn test_brackets() {
        assert_eq!(eval_str("4 * (5 - 1)").unwrap(), 16);
        assert_eq!(eval_str("(2 + 2) * (3 + 3)").unwrap(), 24);
        assert_eq!(eval_str("(2 + 2)").unwrap(), 4);
        assert_eq!(eval_str("-(2 + 2)").unwrap(), -4);
        assert_eq!(eval_str("-((2 + 3) * 4)").unwrap(), -20);
        assert_eq!(eval_str("-((2 + -4) * 5) / 2").unwrap(), 5);
        assert_eq!(eval_str("(1 + 2) + 3").unwrap(), 6);
        assert!(eval_str("-2 + 2)").is_err());
        assert!(eval_str("-(2 + 2").is_err());
        assert!(eval_str("()").is_err());
    }

    #[test]
    fn test_power() {
        assert!(eval_str("4 ^").is_err());
        assert!(eval_str("^ 3").is_err());
        assert_eq!(eval_str("1 ^ -3").unwrap(), 1);
        assert_eq!(eval_str("(-1) ^ -3").unwrap(), -1);
        assert_eq!(eval_str("(-1) ^ -4").unwrap(), 1);
        assert_eq!(eval_str("2 ^ -3").unwrap(), 0);
        assert_eq!(eval_str("2 ^ 0").unwrap(), 1);
        assert_eq!(eval_str("3 ^ 5").unwrap(), 243);
        assert_eq!(eval_str("-1 ^ 4").unwrap(), 1);
        assert_eq!(eval_str("-1 ^ 5").unwrap(), -1);
        assert_eq!(eval_str("-1 ^ -5").unwrap(), -1);
        assert_eq!(eval_str("(1 + 1) ^ (4 * 2)").unwrap(), 256);
    }

    #[test]
    fn test_overflow() {
        assert!(eval_str("2^63").is_err());
        assert!(eval_str("2^32 * 2^31").is_err());
        // 2^62 = 4611686018427387904
        assert!(eval_str("2^62 + 4611686018427387904").is_err());
        assert_eq!(eval_str("-2^63").unwrap(), (-2i64).pow(63));
        assert!(eval_str("-(2^62 * -2)").is_err());
        assert!(eval_str("-1 * (2^62 * -2)").is_err());
    }

    #[test]
    fn test_mod() {
        assert!(eval_str("2 %").is_err());
        assert!(eval_str("% 3").is_err());
        assert!(eval_str("100 % 0").is_err());
        assert_eq!(eval_str("7 % 3").unwrap(), 1);
        assert_eq!(eval_str("7 % -3").unwrap(), 1);
        assert_eq!(eval_str("-7 % 3").unwrap(), -1);
        assert_eq!(eval_str("-9 % -3").unwrap(), 0);
        assert_eq!(eval_str("42 % 1337").unwrap(), 42);
        assert_eq!(eval_str("2 + 3 * 4 % 5").unwrap(), 4);
    }

    #[test]
    fn test_variables() {
        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("a = 2", &mut ctx).unwrap(), 2);
        assert_eq!(eval_str_ctx("b = a + 1", &mut ctx).unwrap(), 3);
        assert_eq!(eval_str_ctx("c = a + b", &mut ctx).unwrap(), 5);
        assert_eq!(ctx.get_var("a"), Some(2));
        assert_eq!(ctx.get_var("b"), Some(3));
        assert_eq!(ctx.get_var("c"), Some(5));

        assert!(eval_str("not_defined").is_err());

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("a = -(b = ((c = -8) * 5) - 2)", &mut ctx).unwrap(), 42);
        assert_eq!(ctx.get_var("a"), Some(42));
        assert_eq!(ctx.get_var("b"), Some(-42));
        assert_eq!(ctx.get_var("c"), Some(-8));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("some_longer_name = 2", &mut ctx).unwrap(), 2);
        assert_eq!(ctx.get_var("some_longer_name"), Some(2));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("(a = 2)", &mut ctx).unwrap(), 2);
        assert_eq!(ctx.get_var("a"), Some(2));

        let mut ctx = Context::new();
        assert_eq!(eval_str_ctx("(a = 2) * a", &mut ctx).unwrap(), 4);
        assert_eq!(ctx.get_var("a"), Some(2));

        assert!(eval_str("a * (a = 2)").is_err());

        assert!(eval_str("a b = 2").is_err());
        assert!(eval_str("2 = 2").is_err());
        assert!(eval_str("* = 2").is_err());
        assert!(eval_str("() = 2").is_err());
    }
}
