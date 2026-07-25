//! # lll parser

mod types;

use chumsky::prelude::*;
use thiserror::Error;
use types::*;

pub fn parse() -> Result<Programs, ParseError> {
    todo!()
}

fn parse_number<'src>() -> impl Parser<'src, &'src str, AST, extra::Err<Rich<'src, char>>> {
    text::int(10)
        .map(|s: &str| AST::Number(s.parse().unwrap()))
        .padded()
}

// fn keywords<'src>() -> impl Parser<'src, &'src str, AST, extra::Err<Rich<'src, char>>> {
//     // choice((keywords::DEFINE,))
//     todo!()
// }

// fn ast<'src>() -> impl Parser<'src, &'src str, AST, extra::Err<Rich<'src, char>>> {
//     todo!()
// }

// fn define<'src>() -> impl Parser<'src, &'src str, AST, extra::Err<Rich<'src, char>>> {
//     let define = just(keywords::DEFINE);
//     let name = just(text::ident());
//     let ast = just(ast);
// }

#[derive(Debug, Error)]
pub enum ParseError {}

#[cfg(test)]
mod tests {
    use crate::evaluator::parser::{parse_number, types::AST};
    use chumsky::prelude::*;

    #[test]
    fn number_parser_parses_number() {
        assert_eq!(parse_number().parse("1").into_result(), Ok(AST::Number(1)));
        assert_eq!(
            parse_number().parse("100000").into_result(),
            Ok(AST::Number(100000))
        );
        assert!(parse_number().parse("1.0").into_result().is_err());
    }
}
