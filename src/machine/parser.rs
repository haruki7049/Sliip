//! # lll parser

use crate::machine::lexer;
use chumsky::prelude::*;
use chumsky::{Parser, error::Rich, extra};
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum SExpression {
    As((Atom, Type)),
    Define((String, Rc<SExpression>)),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    String,
}

pub fn parse(source: &str) -> Result<Vec<SExpression>, ParseError> {
    let tokens: Vec<lexer::Token> = lexer::tokenize(source)?;

    todo!("tokens: {:?}", tokens)
}

#[derive(Debug, Error, PartialEq, Clone)]
pub enum ParseError {
    #[error("from lexer: {0:?}")]
    Lexing(#[from] lexer::LexingError),
}

#[cfg(test)]
mod tests {
    // use crate::machine::parser::{Atom, SExpression, Type, parse};
    // use std::rc::Rc;

    // #[test]
    // fn sexp_invalid() -> anyhow::Result<()> {
    //     let invalid = "(hoge foo \"NO_CLOSING_QUOTE";
    //     let actual = parse(invalid);

    //     assert_eq!(
    //         actual,
    //         Err(ParseError::Lexing(
    //             crate::machine::lexer::LexingError::UnknownParseError
    //         ))
    //     );
    //     Ok(())
    // }

    // #[test]
    // fn sexp_define() -> anyhow::Result<()> {
    //     let invalid = "(define foo (as \"Hoge\" String))";
    //     let actual = parse(invalid)?;

    //     assert_eq!(
    //         actual,
    //         vec![SExpression::Define((
    //             "foo".to_string(),
    //             Rc::from(SExpression::As((
    //                 Atom::String("Hoge".to_string()),
    //                 Type::String
    //             )))
    //         ))]
    //     );
    //     Ok(())
    // }
}
