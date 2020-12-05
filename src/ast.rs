use crate::tokenizer;

use std::{iter::Peekable, option::NoneError, vec::IntoIter};
use tokenizer::Keyword;
use tokenizer::SpecialToken;
use tokenizer::Token;

type Tokenizer = Peekable<IntoIter<Token>>;

pub fn build_ast(mut tokenizer: Tokenizer) -> Result<(), ParseError> {
    match tokenizer.peek() {
        Some(Token::Keyword(Keyword::Fn)) => parse_fn_header(&mut tokenizer)?,
        _ => todo!(),
    };

    Ok(())
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEOF,
    FnHeader,
}

impl From<NoneError> for ParseError {
    fn from(_: NoneError) -> Self {
        Self::UnexpectedEOF
    }
}

pub fn parse_fn_header(tokenizer: &mut Tokenizer) -> Result<(), ParseError> {
    tokenizer
        .next()?
        .assert_keyword(Keyword::Fn)
        .map_err(|_| ParseError::FnHeader)?;

    let fn_name = tokenizer
        .next()?
        .assert_ident()
        .map_err(|_| ParseError::FnHeader)?;

    tokenizer
        .next()?
        .assert_special(SpecialToken::LParen)
        .map_err(|_| ParseError::FnHeader)?;

    let mut args: Vec<(String, String)> = Vec::new();
    loop {
        if matches!(tokenizer.peek()?, Token::Special(SpecialToken::RParen)) {
            tokenizer.next();
            break;
        }

        let arg_name = tokenizer
            .next()?
            .assert_ident()
            .map_err(|_| ParseError::FnHeader)?;

        tokenizer
            .next()?
            .assert_special(SpecialToken::Colon)
            .map_err(|_| ParseError::FnHeader)?;

        let arg_type = tokenizer
            .next()?
            .assert_ident()
            .map_err(|_| ParseError::FnHeader)?;

        args.push((arg_name, arg_type));

        match tokenizer.peek()? {
            Token::Special(SpecialToken::Comma) => {
                tokenizer.next();
                continue;
            }
            Token::Special(SpecialToken::RParen) => continue,
            _ => return Result::Err(ParseError::FnHeader),
        }
    }

    let mut returns = None;
    if let Token::Special(SpecialToken::Returns) = tokenizer.peek()? {
        tokenizer.next();
        returns = Some(
            tokenizer
                .next()?
                .assert_ident()
                .map_err(|_| ParseError::FnHeader)?,
        )
    }

    dbg!(fn_name);
    dbg!(args);
    dbg!(returns);

    parse_block_expr(tokenizer);

    Ok(())
}

pub fn parse_block_expr(tokenizer: &mut Tokenizer) {}
