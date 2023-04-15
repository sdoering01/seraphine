use crate::{error::ParseError, tokenizer::Token};

#[derive(Debug, Clone)]
pub enum AST {
    Lines(Vec<Option<AST>>),
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
    FunctionDefinition {
        name: String,
        arg_names: Vec<String>,
        body: Box<AST>,
    },
}

pub fn parse(tokens: &[Token]) -> Result<AST, ParseError> {
    inner_parse(tokens, true)
}

fn inner_parse(tokens: &[Token], statements_allowed: bool) -> Result<AST, ParseError> {
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

    let mut line_indices = vec![];
    // Explicitly handle the first token being a newline, since the loop below starts at the second
    if let Token::Newline = tokens[0] {
        line_indices.push(0);
    }

    let mut last_pls_mns_idx = None;
    let mut last_tim_div_mod_idx = None;
    let mut last_caret_idx = None;
    let mut first_eq_idx = None;

    let mut first_lparen_idx = None;
    let mut last_rparen_idx = None;
    let mut first_lbrace_idx = None;
    let mut last_rbrace_idx = None;

    // TODO: Clean this up
    let mut bracket_stack = Vec::new();
    match tokens[0] {
        Token::LParen => {
            if first_lparen_idx.is_none() && bracket_stack.is_empty() {
                first_lparen_idx = Some(0);
            }
            bracket_stack.push(tokens[0].clone());
        }
        Token::LBrace => {
            if first_lbrace_idx.is_none() && bracket_stack.is_empty() {
                first_lbrace_idx = Some(0);
            }
            bracket_stack.push(tokens[0].clone());
        }
        Token::RParen => {
            match bracket_stack.pop() {
                Some(Token::LParen) => (),
                _ => return Err(ParseError::UnexpectedToken(tokens[0].clone())),
            }
            if bracket_stack.is_empty() {
                last_rparen_idx = Some(0);
            }
        }
        Token::RBrace => {
            match bracket_stack.pop() {
                Some(Token::LBrace) => (),
                _ => return Err(ParseError::UnexpectedToken(tokens[0].clone())),
            }
            if bracket_stack.is_empty() {
                last_rbrace_idx = Some(0);
            }
        }
        _ => (),
    }

    for (prev_idx, token_window) in tokens.windows(2).enumerate() {
        let idx = prev_idx + 1;
        let prev_token = &token_window[0];
        let token = &token_window[1];

        match token {
            Token::LParen => {
                if first_lparen_idx.is_none() && bracket_stack.is_empty() {
                    first_lparen_idx = Some(idx);
                }
                bracket_stack.push(token.clone());
            }
            Token::LBrace => {
                if first_lbrace_idx.is_none() && bracket_stack.is_empty() {
                    first_lbrace_idx = Some(idx);
                }
                bracket_stack.push(token.clone());
            }
            Token::RParen => {
                match bracket_stack.pop() {
                    Some(Token::LParen) => (),
                    _ => return Err(ParseError::UnexpectedToken(token.clone())),
                }
                if bracket_stack.is_empty() {
                    last_rparen_idx = Some(idx);
                }
            }
            Token::RBrace => {
                match bracket_stack.pop() {
                    Some(Token::LBrace) => (),
                    _ => return Err(ParseError::UnexpectedToken(token.clone())),
                }
                if bracket_stack.is_empty() {
                    last_rbrace_idx = Some(idx);
                }
            }
            _ => (),
        }

        // Only take operators if they aren't inside of brackets, since brackets have higher
        // precedence
        if bracket_stack.is_empty() {
            match (prev_token, token) {
                (_, Token::Newline) => line_indices.push(idx),
                (_, Token::Equal) if first_eq_idx.is_none() => first_eq_idx = Some(idx),
                // Only take plus or minus if they aren't unary
                (
                    Token::Number(_) | Token::Identifier(_) | Token::RParen,
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

    match bracket_stack.last() {
        Some(token @ Token::LBrace) | Some(token @ Token::LParen) => {
            return Err(ParseError::UnmatchedBracket((*token).clone()))
        }
        None => (),
        _ => unreachable!(),
    };
    let has_brackets = last_rparen_idx.is_some();

    if line_indices.len() > 0 {
        if !statements_allowed {
            return Err(ParseError::UnexpectedToken(tokens[line_indices[0]].clone()));
        }

        line_indices.push(tokens.len());
        let mut line_asts = vec![];
        let mut prev_idx = 0;
        for idx in line_indices {
            let maybe_ast = if idx == prev_idx {
                prev_idx += 1;
                None
            } else {
                let line_ast = inner_parse(&tokens[prev_idx..idx], statements_allowed)?;
                prev_idx = idx + 1;
                Some(line_ast)
            };

            line_asts.push(maybe_ast);
        }

        return Ok(AST::Lines(line_asts));
    }

    match tokens[0] {
        Token::Identifier(ref ident) if ident == "fn" => {
            // fn <name> (<arg1>, <arg2>, ...) { <body> }
            let Some(Token::Identifier(name)) = tokens.get(1).cloned() else {
                return Err(ParseError::ExpectedIdentifier);
            };

            if first_lparen_idx != Some(2) {
                return Err(ParseError::ExpectedToken(Token::LBrace));
            }

            let mut arg_names = vec![];
            let mut arg_idx = 3;
            let last_rparen_idx = last_rparen_idx.unwrap();
            while arg_idx < last_rparen_idx {
                match tokens[arg_idx] {
                    Token::Identifier(ref name) => arg_names.push(name.clone()),
                    _ => return Err(ParseError::ExpectedIdentifier),
                }
                arg_idx += 1;

                if arg_idx < last_rparen_idx {
                    if let Token::Comma = tokens[arg_idx] {
                        if arg_idx < last_rparen_idx - 1 {
                            arg_idx += 1;
                        } else {
                            return Err(ParseError::UnexpectedToken(tokens[arg_idx].clone()));
                        }
                    } else {
                        return Err(ParseError::ExpectedToken(Token::Comma));
                    }
                }
            }

            if first_lbrace_idx != Some(last_rparen_idx + 1) {
                return Err(ParseError::ExpectedToken(Token::LBrace));
            }
            let last_rbrace_idx = last_rbrace_idx.unwrap();
            if last_rbrace_idx != tokens.len() - 1 {
                return Err(ParseError::UnexpectedToken(
                    tokens[last_rbrace_idx + 1].clone(),
                ));
            }

            let body_ast = Box::new(inner_parse(
                &tokens[(first_lbrace_idx.unwrap() + 1)..last_rbrace_idx],
                true,
            )?);
            return Ok(AST::FunctionDefinition {
                name,
                arg_names,
                body: body_ast,
            });
        }
        _ => (),
    }

    // Start building AST from the operators of lowest precedence so that those operators are
    // applied last
    if let Some(idx) = first_eq_idx {
        match (&tokens[0], idx) {
            (Token::Identifier(name), 1) => {
                let inner_ast = Box::new(inner_parse(&tokens[2..], false)?);
                return Ok(AST::Assign(name.clone(), inner_ast));
            }
            _ => return Err(ParseError::UnexpectedToken(tokens[1].clone())),
        }
    }

    if let Some(idx) = last_pls_mns_idx {
        let l_ast = Box::new(inner_parse(&tokens[..idx], false)?);
        let r_ast = Box::new(inner_parse(&tokens[(idx + 1)..], false)?);
        let ast = match tokens[idx] {
            Token::Plus => AST::Add(l_ast, r_ast),
            Token::Minus => AST::Subtract(l_ast, r_ast),
            _ => unreachable!(),
        };
        return Ok(ast);
    }

    if let Some(idx) = last_tim_div_mod_idx {
        let l_ast = Box::new(inner_parse(&tokens[..idx], false)?);
        let r_ast = Box::new(inner_parse(&tokens[(idx + 1)..], false)?);
        let ast = match tokens[idx] {
            Token::Star => AST::Multiply(l_ast, r_ast),
            Token::Slash => AST::Divide(l_ast, r_ast),
            Token::Percent => AST::Modulo(l_ast, r_ast),
            _ => unreachable!(),
        };
        return Ok(ast);
    }

    if let Some(idx) = last_caret_idx {
        let l_ast = Box::new(inner_parse(&tokens[..idx], false)?);
        let r_ast = Box::new(inner_parse(&tokens[(idx + 1)..], false)?);
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
                let inner_ast = Box::new(inner_parse(&tokens[1..], false)?);
                return Ok(AST::UnaryPlus(inner_ast));
            }
        },
        Token::Minus => match &tokens[1] {
            token @ (Token::Plus | Token::Minus) => {
                return Err(ParseError::UnexpectedToken(token.clone()))
            }
            _ => {
                let inner_ast = Box::new(inner_parse(&tokens[1..], false)?);
                return Ok(AST::UnaryMinus(inner_ast));
            }
        },
        _ => (),
    }

    if has_brackets {
        match (
            tokens.first(),
            tokens.last(),
            first_lparen_idx,
            last_rparen_idx,
        ) {
            (Some(Token::LParen), Some(Token::RParen), _, _) => {
                let inner_ast = Box::new(inner_parse(&tokens[1..tokens.len() - 1], false)?);
                return Ok(AST::Brackets(inner_ast));
            }
            // Return correct error when a token that is not an operator follows the brackets
            (Some(Token::LParen), _, _, Some(idx)) => {
                // SAFETY: The next index exists since all brackets are closed properly at this
                // point and the last index is not the last closing bracket.
                return Err(ParseError::UnexpectedToken(tokens[idx + 1].clone()));
            }
            (Some(Token::Identifier(name)), Some(Token::RParen), Some(1), _) => {
                let mut args = Vec::new();
                let mut arg_start = 2;
                let mut arg_end;
                let mut inner_bracket_depth = 0;
                for (idx, token) in tokens.iter().enumerate().skip(arg_start + 1) {
                    match token {
                        Token::Comma if inner_bracket_depth == 0 => {
                            arg_end = idx;
                            let arg = inner_parse(&tokens[arg_start..arg_end], false)?;
                            arg_start = arg_end + 1;
                            args.push(arg);
                        }
                        Token::LParen => inner_bracket_depth += 1,
                        Token::RParen => inner_bracket_depth -= 1,
                        _ => (),
                    }
                }
                let last_arg = inner_parse(&tokens[arg_start..tokens.len() - 1], false)?;
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
