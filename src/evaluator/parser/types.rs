//! # lll parser types

pub type Programs = Vec<AST>;

#[derive(Debug, PartialEq)]
pub enum AST {
    Number(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Symbol(String),
    List(Vec<AST>),
    Define((String, Box<AST>)),
    Lambda((Vec<Parameter>, Vec<AST>)),
    If((Box<AST>, Box<AST>, Box<AST>)),
    Let((Vec<(String, AST)>, Vec<AST>)),
    LetStar((Vec<(String, AST)>, Vec<AST>)),
    LetRec((Vec<(String, AST)>, Vec<AST>)),
    Begin(Vec<AST>),
    Quote(Box<AST>),
    Ascription((Box<AST>, TypeExpression)),
    DefType((String, Vec<String>, Vec<Constructor>)),
    Match((Box<AST>, Vec<(Pattern, Vec<AST>)>)),
    App((Box<AST>, Vec<AST>)),
}

#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub ast: Option<TypeExpression>,
}

#[derive(Debug, PartialEq)]
pub enum TypeExpression {
    Name(String),
    App((String, Vec<TypeExpression>)),
    Arrow(Vec<TypeExpression>),
}

#[derive(Debug, PartialEq)]
pub struct Constructor {
    pub name: String,
    pub ast: Vec<TypeExpression>,
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Variable(String),
    WildCard,
    Unit,
    Constructor((String, Vec<TypeExpression>)),
}
