mod assignment;
mod expression;
mod known;
mod seq;
mod simple_nodes;

pub(crate) use assignment::*;
pub(crate) use expression::*;
pub(crate) use known::*;
pub(crate) use seq::*;
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
    MultipleKeywords(Vec<Location>),
    InvalidKeywordPosition(Location),
    InvalidAssignmentRHS(Location),
    InvalidAssignmentLHS(Location),
    EmptyAssignmentLHS,
    ExpectedComma(Location),
    ExpectedName(Location),
    UnexpectedToken(Token),
    ExpectedFunctionDef(Location),
    NotEnoughArguments(Location),
    TooManyArguments(Location),
    ExpectedFuncAssign(Location),
    ExpectedNameAssign(Location),
}

#[derive(Debug)]
pub enum AstBuildError {
    WrongOperator,
}

#[derive(Debug)]
pub struct NameError {
    msg: String,
    loc: Location,
}

pub enum EvalResult {
    Single(f64),
    Multiple(Vec<f64>),
}

impl std::ops::Add for EvalResult {
    type Output = EvalResult;
    fn add(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs + rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs + r).collect()),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| rhs + l).collect()),
                Self::Multiple(rhs) => {
                    // TODO(Hawo): Make this a nice error that we can use, probably check before
                    assert!(rhs.len() == lhs.len());
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l + r).collect())
                }
            },
        }
    }
}

impl std::ops::Sub for EvalResult {
    type Output = EvalResult;
    fn sub(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs - rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs - r).collect()),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| l - rhs).collect()),
                Self::Multiple(rhs) => {
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l - r).collect())
                }
            },
        }
    }
}

impl std::ops::Mul for EvalResult {
    type Output = EvalResult;
    fn mul(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs * rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|x| x * lhs).collect()),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|x| x * rhs).collect()),
                Self::Multiple(rhs) => {
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l * r).collect())
                }
            },
        }
    }
}

impl std::ops::Div for EvalResult {
    type Output = EvalResult;
    fn div(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs / rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs / r).collect()),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| l / rhs).collect()),
                Self::Multiple(rhs) => {
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l / r).collect())
                }
            },
        }
    }
}

impl std::ops::Neg for EvalResult {
    type Output = EvalResult;
    fn neg(self) -> Self::Output {
        match &self {
            Self::Single(val) => Self::Single(-val),
            Self::Multiple(vals) => Self::Multiple(vals.iter().map(|val| -val).collect()),
        }
    }
}

impl EvalResult {
    fn pow(self, rhs: Self) -> Self {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs.powf(rhs)),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs.powf(*r)).collect()),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| l.powf(rhs)).collect()),
                Self::Multiple(rhs) => Self::Multiple(
                    lhs.iter()
                        .zip(rhs.iter())
                        .map(|(l, r)| l.powf(*r))
                        .collect(),
                ),
            },
        }
    }
}

pub trait EvalASTNode: Debug + std::fmt::Display {
    fn eval(&self, known_values: &KnownValues) -> Result<EvalResult, NameError>;
}
