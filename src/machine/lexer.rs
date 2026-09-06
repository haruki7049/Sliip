//! # lll lexer

use itertools::Itertools;
use logos::Logos;
use std::rc::Rc;
use thiserror::Error;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexingError)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip(r";[^\r\n]*", allow_greedy = true))]
pub enum Token {
    /// Integer number
    #[regex("-?[0-9]+", |lex| lex.slice().parse())]
    Integer(i64),

    /// Float number
    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().parse())]
    Float(f64),

    /// String
    #[regex(r#""([^"\\\x00-\x1F]|\\(["\\bnfrt/]|u[a-fA-F0-9]{4}))*""#, |lex| Rc::from(&lex.slice()[1..lex.slice().len()-1]))]
    Str(Rc<str>),

    /// Opened parenthesis
    #[token("(")]
    ParenOpen,

    /// Closed parenthesis
    #[token(")")]
    ParenClose,

    /// Right arrow
    #[token("->")]
    RightArrow,

    /// Lambda keyword
    #[token("lambda")]
    Lambda,

    /// Define keyword
    #[token("define")]
    Define,

    /// As keyword
    #[token("as")]
    As,

    /// Ident
    #[regex(r#"([[:alpha:]]|_)([[:alnum:]]|[\-\_]|_)*|`[^`\\\x00-\x1F\s]*`|\\(["\\bnfrt/]|u[a-fA-F0-9]{4})*"#, |lex| Rc::from(lex.slice()))]
    Ident(Rc<str>),
}

impl From<std::num::ParseIntError> for LexingError {
    fn from(_: std::num::ParseIntError) -> Self {
        LexingError::IntegerParseError
    }
}

impl From<std::num::ParseFloatError> for LexingError {
    fn from(_: std::num::ParseFloatError) -> Self {
        LexingError::FloatParseError
    }
}

// TODO: Create Unterminated error
// #[error("Unterminated litetal / S expresion: {0}")]
// Unterminated(char),
#[derive(Debug, Error, PartialEq, Clone, Default)]
pub enum LexingError {
    // TODO: Rename FloatParseError to Float(f64)
    #[error("Float parse error")]
    FloatParseError,

    // TODO: Rename IntegerParseError to Integer(i64)
    #[error("Integer parse error")]
    IntegerParseError,

    #[default]
    #[error("Unknown parse error")]
    UnknownParseError,
}

pub fn tokenize(source: &str) -> Result<Vec<Token>, LexingError> {
    Token::lexer(source)
        .try_collect()
        .map_err(|_| LexingError::UnknownParseError)
}

#[cfg(test)]
mod tests {
    use super::{LexingError, Token, tokenize};
    use std::rc::Rc;

    #[test]
    fn sexp_invalid() -> anyhow::Result<()> {
        let invalid = "(hoge foo \"NO_CLOSING_QUOTE";
        let actual = tokenize(invalid);

        assert_eq!(actual, Err(LexingError::UnknownParseError));
        Ok(())
    }

    #[test]
    fn sexp_common() -> anyhow::Result<()> {
        let common = "(hoge foo \"This is a string\")";
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                Token::ParenOpen,
                Token::Ident(Rc::from("hoge")),
                Token::Ident(Rc::from("foo")),
                Token::Str(Rc::from("This is a string")),
                Token::ParenClose,
            ]
        );

        Ok(())
    }

    /// No backslash before the quote
    #[test]
    fn only_quote() -> anyhow::Result<()> {
        let only_quote = "\"";
        let actual = tokenize(only_quote);
        assert_eq!(actual, Err(LexingError::UnknownParseError));

        Ok(())
    }

    /// A backslash before the quote
    #[test]
    fn backslash_and_quote() -> anyhow::Result<()> {
        let backslash_and_quote = "\\\"";
        let actual = tokenize(backslash_and_quote)?;
        assert_eq!(actual, vec![Token::Ident(Rc::from("\\\""))]);

        Ok(())
    }

    #[test]
    fn integer() -> anyhow::Result<()> {
        let common = "1";
        let actual = tokenize(common)?;
        assert_eq!(actual, vec![Token::Integer(1)]);

        Ok(())
    }

    #[test]
    fn minus_integer() -> anyhow::Result<()> {
        let common = "-1";
        let actual = tokenize(common)?;
        assert_eq!(actual, vec![Token::Integer(-1)]);

        Ok(())
    }

    #[test]
    fn float() -> anyhow::Result<()> {
        let common = "1.0";
        let actual = tokenize(common)?;
        assert_eq!(actual, vec![Token::Float(1.0)]);

        let pi = std::f64::consts::PI.to_string();
        let actual = tokenize(&pi)?;
        assert_eq!(actual, vec![Token::Float(3.141592653589793)]);

        Ok(())
    }

    #[test]
    fn empty_string() -> anyhow::Result<()> {
        let program = "\"\"";
        let actual = tokenize(program)?;
        assert_eq!(actual, vec![Token::Str(Rc::from(""))]);

        Ok(())
    }

    #[test]
    fn some_string() -> anyhow::Result<()> {
        let program = "\"SOME_STRING_DATA\"";
        let actual = tokenize(program)?;
        assert_eq!(actual, vec![Token::Str(Rc::from("SOME_STRING_DATA"))]);

        Ok(())
    }

    #[test]
    fn sexp_common_multiline() -> anyhow::Result<()> {
        let common = "(hoge foo \"This is a string\")\n(this is the second line)";
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                Token::ParenOpen,
                Token::Ident(Rc::from("hoge")),
                Token::Ident(Rc::from("foo")),
                Token::Str(Rc::from("This is a string")),
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("this")),
                Token::Ident(Rc::from("is")),
                Token::Ident(Rc::from("the")),
                Token::Ident(Rc::from("second")),
                Token::Ident(Rc::from("line")),
                Token::ParenClose
            ]
        );

        Ok(())
    }

    #[test]
    fn sexp_comment_line() -> anyhow::Result<()> {
        let common = "; This is a comment line";
        let actual = tokenize(common)?;
        assert_eq!(actual, vec![]);

        Ok(())
    }

    #[test]
    fn sexp_comment_with_two_semicolomn() -> anyhow::Result<()> {
        let common = ";; This is also a comment line";
        let actual = tokenize(common)?;
        assert_eq!(actual, vec![]);

        Ok(())
    }

    #[test]
    fn sexp_comment_multiline() -> anyhow::Result<()> {
        let common = "; This is a comment line\n;This is the second line";
        let actual = tokenize(common)?;
        assert_eq!(actual, vec![]);

        Ok(())
    }

    #[test]
    fn sexp_comments_with_program() -> anyhow::Result<()> {
        let common = "; This is a comment line\n;This is the second line\n\n(define s (as \"EXAMPLE_DATA\" String))";
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("s")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("EXAMPLE_DATA")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
            ]
        );

        Ok(())
    }

    #[test]
    fn lll_arithmetic() -> anyhow::Result<()> {
        let common = include_str!("../../examples/arithmetic.lll");
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                // (define pi
                //   (as 3.14159 Float))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("pi")),
                Token::ParenOpen,
                Token::As,
                Token::Float(3.14159),
                Token::Ident(Rc::from("Float")),
                Token::ParenClose,
                Token::ParenClose,
                // (define radius
                //   (as 5 Integer))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("radius")),
                Token::ParenOpen,
                Token::As,
                Token::Integer(5),
                Token::Ident(Rc::from("Integer")),
                Token::ParenClose,
                Token::ParenClose,
                // (define greeting
                //   (as "Arithmetic operations demonstration" String))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("greeting")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("Arithmetic operations demonstration")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                // (define main
                //   (as
                //     (lambda ()
                //       (write-line greeting))
                //     (-> Void))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("main")),
                Token::ParenOpen,
                Token::As,
                Token::ParenOpen,
                Token::Lambda,
                Token::ParenOpen,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("write-line")),
                Token::Ident(Rc::from("greeting")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::RightArrow,
                Token::Ident(Rc::from("Void")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenClose
            ]
        );

        Ok(())
    }

    #[test]
    fn lll_hello() -> anyhow::Result<()> {
        let common = include_str!("../../examples/hello-lll.lll");
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                // (define s
                //   (as "Hello, lll!" String))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("s")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("Hello, lll!")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                // (define main
                //   (as
                //     (lambda ()
                //       (write-line s))
                //     (-> Void)))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("main")),
                Token::ParenOpen,
                Token::As,
                Token::ParenOpen,
                Token::Lambda,
                Token::ParenOpen,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("write-line")),
                Token::Ident(Rc::from("s")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::RightArrow,
                Token::Ident(Rc::from("Void")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenClose
            ]
        );

        Ok(())
    }

    #[test]
    fn lll_variables() -> anyhow::Result<()> {
        let common = include_str!("../../examples/variables.lll");
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                // (define firstName
                //   (as "Alice" String))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("firstName")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("Alice")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                // (define lastName
                //   (as "Wonderland" String))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("lastName")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("Wonderland")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                // (define greeting
                //   (as "Hello from variables!" String))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("greeting")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("Hello from variables!")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                // (define main
                //   (as
                //     (lambda ()
                //       (write-line greeting)
                //       (write-line firstName)
                //       (write-line lastName))
                //     (-> Void)))
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("main")),
                Token::ParenOpen,
                Token::As,
                Token::ParenOpen,
                Token::Lambda,
                Token::ParenOpen,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("write-line")),
                Token::Ident(Rc::from("greeting")),
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("write-line")),
                Token::Ident(Rc::from("firstName")),
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("write-line")),
                Token::Ident(Rc::from("lastName")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::RightArrow,
                Token::Ident(Rc::from("Void")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenClose
            ]
        );

        Ok(())
    }

    #[test]
    fn lll_type_annotations() -> anyhow::Result<()> {
        let common = include_str!("../../examples/type-annotations.lll");
        let actual = tokenize(common)?;
        assert_eq!(
            actual,
            vec![
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("typedString")),
                Token::ParenOpen,
                Token::As,
                Token::Str(Rc::from("This is a typed string")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("typedNumber")),
                Token::ParenOpen,
                Token::As,
                Token::Integer(42),
                Token::Ident(Rc::from("Integer")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("typedFloat")),
                Token::ParenOpen,
                Token::As,
                Token::Float(3.14),
                Token::Ident(Rc::from("Float")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("typedBool")),
                Token::ParenOpen,
                Token::As,
                Token::Ident(Rc::from("true")),
                Token::Ident(Rc::from("Bool")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("typedFunction")),
                Token::ParenOpen,
                Token::As,
                Token::ParenOpen,
                Token::Lambda,
                Token::ParenOpen,
                Token::ParenOpen,
                Token::Ident(Rc::from("x")),
                Token::Ident(Rc::from("String")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("write-line")),
                Token::Ident(Rc::from("x")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::RightArrow,
                Token::Ident(Rc::from("String")),
                Token::Ident(Rc::from("Void")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Define,
                Token::Ident(Rc::from("main")),
                Token::ParenOpen,
                Token::As,
                Token::ParenOpen,
                Token::Lambda,
                Token::ParenOpen,
                Token::ParenClose,
                Token::ParenOpen,
                Token::Ident(Rc::from("typedFunction")),
                Token::Ident(Rc::from("typedString")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenOpen,
                Token::RightArrow,
                Token::Ident(Rc::from("Void")),
                Token::ParenClose,
                Token::ParenClose,
                Token::ParenClose
            ]
        );

        Ok(())
    }
}
