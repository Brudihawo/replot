mod expression;
mod simple_nodes;
pub(crate) use expression::*;
pub(crate) use simple_nodes::*;

use crate::tokenize::{Location, OperatorType, Token, TokenType};
use std::collections::HashMap;
use std::fmt::Debug;

pub struct KnownValues {
    singles: HashMap<String, f64>,
    functions: HashMap<String, Box<dyn EvalASTNode>>,
    // multiples: HashMap<String, Vec<f64>>,
}

impl KnownValues {
    fn get(&self, name: &String) -> Option<f64> {
        if let Some(val) = self.singles.get(name) {
            Some(*val)
        } else {
            None
        }
    }

    fn get_func(&self, name: &String) -> Option<&Box<dyn EvalASTNode>> {
        if let Some(val) = self.functions.get(name) {
            Some(val)
        } else {
            None
        }
    }
}

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
