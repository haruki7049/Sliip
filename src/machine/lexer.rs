use itertools::Itertools;
use logos::{Lexer, Logos};
use std::rc::Rc;
use thiserror::Error;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexingError)]
#[logos(skip r"\s+")]
#[logos(skip(r";[^\n]*", allow_greedy = true))]
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
    #[regex(r"([[:alpha:]]|_)([[:alnum:]]|[\-\_]|_)*|`[^`\\\x00-\x1F\s]*`", |lex| Rc::from(lex.slice()))]
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

#[derive(Debug, Error, PartialEq, Clone, Default)]
pub enum LexingError {
    #[error("Float parse error")]
    FloatParseError,

    #[error("Integer parse error")]
    IntegerParseError,

    #[error("TODO")]
    #[default]
    Todo,
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexingError> {
    Token::lexer(src)
        .try_collect()
        .map_err(|_| LexingError::Todo)
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::machine::lexer::{Token, tokenize};

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
}
