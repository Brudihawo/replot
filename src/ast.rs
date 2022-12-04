mod assignment;
mod expression;
mod known;
mod simple_nodes;

pub(crate) use assignment::*;
pub(crate) use expression::*;
pub(crate) use known::*;
pub(crate) use simple_nodes::*;

use crate::tokenize::{Location, OperatorType, Token, TokenType};
use std::fmt::Debug;

#[derive(Debug, PartialEq, Eq)]
pub enum SyntaxError {
    NonterminatedParen(Location),
    EtraneousParen(Location),
    MisplacedEquals(Location),
    EmptyParenScope(Location, Location),
    NonEvaluatableScope(Location, Location),
    CannotEvaluate(Location),
    InvalidBinaryOperand(Location),
    InvalidBinaryOperator(Location),
    MultipleEquals(Vec<Location>),
    InvalidAssignmentRHS(Location),
    InvalidAssignmentLHS(Location),
    EmptyAssignmentLHS,
    ExpectedComma(Location),
    ExpectedName(Location),
    UnexpectedToken(Token),
    ExpectedFunctionDef(Location),
}

#[derive(Debug)]
pub enum AstBuildError {
    WrongOperator,
}

pub struct NameError {
    msg: String,
    loc: Location,
}

pub trait EvalASTNode: Debug {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError>;
    fn simple_print(&self) -> String;
}
