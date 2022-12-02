use crate::tokenize::{Location, OperatorType, Token, TokenType};

use itertools::Itertools;
use itertools::MultiPeek;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt::Debug;
use std::option::Iter;

pub struct KnownValues {
    singles: HashMap<String, f64>,
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

#[derive(Debug)]
enum BinXprType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
}

#[derive(Debug)]
pub enum UnXprType {
    Negate,
    RedundantPlus,
}

#[derive(Debug)]
pub enum NameType {
    Variable(String),
    Function(String, Vec<String>),
}

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<dyn EvalASTNode>,
    right: Box<dyn EvalASTNode>,
    tipe: BinXprType,
}

impl BinaryExpression {
    pub fn new(
        tipe: OperatorType,
        left: Box<dyn EvalASTNode>,
        right: Box<dyn EvalASTNode>,
    ) -> Result<Self, AstBuildError> {
        Ok(BinaryExpression {
            tipe: match tipe {
                OperatorType::Plus => BinXprType::Add,
                OperatorType::Minus => BinXprType::Subtract,
                OperatorType::Asterisk => BinXprType::Multiply,
                OperatorType::Slash => BinXprType::Divide,
                OperatorType::Power => BinXprType::Power,
                OperatorType::Equals => return Err(AstBuildError::WrongOperator),
            },
            left,
            right,
        })
    }

    pub fn new_box(
        tipe: OperatorType,
        left: Box<dyn EvalASTNode>,
        right: Box<dyn EvalASTNode>,
    ) -> Result<Box<Self>, AstBuildError> {
        Ok(Box::new(Self::new(tipe, left, right)?))
    }
}

#[derive(Debug)]
pub struct UnaryExpression {
    child: Box<dyn EvalASTNode>,
    tipe: UnXprType,
}

impl UnaryExpression {
    pub fn try_from(operator: &Token, operand: &Token) -> Result<Self, ()> {
        Ok(UnaryExpression {
            child: match &operand.token_type {
                TokenType::Literal(value) => Box::new(Literal::new(*value, operand.location)),
                TokenType::Name(name) => Box::new(Name::new(name.clone(), operand.location)),
                _ => return Err(()),
            },
            tipe: match &operator.token_type {
                TokenType::Operator(op) => match op {
                    OperatorType::Plus => UnXprType::RedundantPlus,
                    OperatorType::Minus => UnXprType::Negate,
                    _ => return Err(()),
                },
                _ => return Err(()),
            },
        })
    }
}

impl EvalASTNode for BinaryExpression {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        let lval = self.left.eval(known_values)?;
        let rval = self.right.eval(known_values)?;
        match &self.tipe {
            BinXprType::Add => Ok(lval + rval),
            BinXprType::Subtract => Ok(lval - rval),
            BinXprType::Multiply => Ok(lval * rval),
            BinXprType::Divide => Ok(lval / rval),
            BinXprType::Power => Ok(lval.powf(rval)),
        }
    }
    fn simple_print(&self) -> String {
        format!(
            "({} {} {})",
            self.left.simple_print(),
            match &self.tipe {
                BinXprType::Add => "+",
                BinXprType::Subtract => "-",
                BinXprType::Multiply => "*",
                BinXprType::Divide => "/",
                BinXprType::Power => "^",
            },
            self.right.simple_print()
        )
    }
}

impl EvalASTNode for UnaryExpression {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        let cval = self.child.eval(known_values)?;
        match &self.tipe {
            UnXprType::Negate => Ok(-cval),
            UnXprType::RedundantPlus => Ok(cval),
        }
    }

    fn simple_print(&self) -> String {
        format!(
            "({} {})",
            match &self.tipe {
                UnXprType::Negate => "-",
                UnXprType::RedundantPlus => "+",
            },
            self.child.simple_print()
        )
    }
}

#[derive(Debug)]
pub struct Literal {
    value: f64,
    loc: Location,
}

impl Literal {
    pub fn new(value: f64, loc: Location) -> Self {
        Self { value, loc }
    }

    pub fn new_box(value: f64, loc: Location) -> Box<Self> {
        Box::new(Self::new(value, loc))
    }
}

impl EvalASTNode for Literal {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        Ok(self.value)
    }
    fn simple_print(&self) -> String {
        format!("{}", self.value)
    }
}

#[derive(Debug)]
pub struct Name {
    name: String,
    loc: Location,
}

impl Name {
    pub fn new(name: String, loc: Location) -> Self {
        Self { name, loc }
    }

    pub fn new_box(name: String, loc: Location) -> Box<Self> {
        Box::new(Self::new(name, loc))
    }
}

impl EvalASTNode for Name {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        if let Some(val) = known_values.get(&self.name) {
            Ok(val)
        } else {
            Err(NameError {
                loc: self.loc,
                msg: format!("Name {} is not known", self.name),
            })
        }
    }

    fn simple_print(&self) -> String {
        self.name.clone()
    }
}

struct VariableList {
    // (x, y, z)
    names: Vec<Name>,
}
