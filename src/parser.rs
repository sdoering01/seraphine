use crate::{error::ParseError, tokenizer::Token};

#[derive(Debug)]
pub enum AST {
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
    FunctionCall(String, Vec<AST>),
}

pub fn parse(tokens: &[Token]) -> Result<AST, ParseError> {
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
    let mut first_lbracket_idx = None;
    let mut last_rbracket_idx = None;

    let mut bracket_depth = 0;
    for (prev_idx, token_window) in tokens.windows(2).enumerate() {
        let idx = prev_idx + 1;
        let prev_token = &token_window[0];
        let token = &token_window[1];

        if let Token::LBracket = prev_token {
            if first_lbracket_idx.is_none() {
                first_lbracket_idx = Some(prev_idx);
            }
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
                (
                    Token::Number(_) | Token::Identifier(_) | Token::RBracket,
                    Token::Plus | Token::Minus,
                ) => last_pls_mns_idx = Some(idx),
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
        match (
            tokens.first(),
            tokens.last(),
            first_lbracket_idx,
            last_rbracket_idx,
        ) {
            (Some(Token::LBracket), Some(Token::RBracket), _, _) => {
                let inner_ast = Box::new(parse(&tokens[1..tokens.len() - 1])?);
                return Ok(AST::Brackets(inner_ast));
            }
            // Return correct error when a token that is not an operator follows the brackets
            (Some(Token::LBracket), _, _, Some(idx)) => {
                // SAFETY: The next index exists since all brackets are closed properly at this
                // point and the last index is not the last closing bracket.
                return Err(ParseError::UnexpectedToken(tokens[idx + 1].clone()));
            }
            (Some(Token::Identifier(name)), Some(Token::RBracket), Some(1), _) => {
                let mut args = Vec::new();
                let mut arg_start = 2;
                let mut arg_end;
                let mut inner_bracket_depth = 0;
                for (idx, token) in tokens.iter().enumerate().skip(arg_start + 1) {
                    match token {
                        Token::Comma if inner_bracket_depth == 0 => {
                            arg_end = idx;
                            let arg = parse(&tokens[arg_start..arg_end])?;
                            arg_start = arg_end + 1;
                            args.push(arg);
                        }
                        Token::LBracket => inner_bracket_depth += 1,
                        Token::RBracket => inner_bracket_depth -= 1,
                        _ => (),
                    }
                }
                let last_arg = parse(&tokens[arg_start..tokens.len() - 1])?;
                args.push(last_arg);
                return Ok(AST::FunctionCall(name.clone(), args));
            }
            _ => (),
        }
    }

    // If we checked for a slice that contains a single number and haven't found any operators, it
    // means that the token at the second index is unexpected.
    Err(ParseError::UnexpectedToken(tokens[1].clone()))
}
