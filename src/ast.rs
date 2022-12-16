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
use std::fmt::{self, Debug};

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

// Evaluation input generated from name nodes
pub struct EvalInput<'a> {
    pub name: &'a String,
    pub value: EvalResult,
}

pub struct Eval<'a> {
    pub result: EvalResult,
    pub inputs: Vec<EvalInput<'a>>,
}

impl<'a> std::ops::Neg for Eval<'a> {
    type Output = Eval<'a>;

    fn neg(self) -> Self::Output {
        Self::Output {
            result: -self.result,
            inputs: self.inputs,
        }
    }
}

impl<'a> std::ops::Add for Eval<'a> {
    type Output = Eval<'a>;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result + rhs.result,
            inputs: Vec::<EvalInput>::new(),
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> std::ops::Sub for Eval<'a> {
    type Output = Eval<'a>;

    fn sub(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result - rhs.result,
            inputs: Vec::new(),
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> std::ops::Mul for Eval<'a> {
    type Output = Eval<'a>;

    fn mul(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result * rhs.result,
            inputs: Vec::new(),
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> std::ops::Div for Eval<'a> {
    type Output = Eval<'a>;

    fn div(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result / rhs.result,
            inputs: Vec::new(),
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> Eval<'a> {
    const RESULT_STR: &str = "result";
    const INDEX_STR: &str = "index";

    fn new(result: EvalResult, input: EvalInput<'a>) -> Self {
        match result {
            EvalResult::Single(_) => {
                assert!(matches!(input.value, EvalResult::Single(_)))
            }
            EvalResult::Multiple(ref res) => {
                match input.value {
                    EvalResult::Single(_) => {}
                    // TODO(Hawo): This has to change if we implement multiple values in functions
                    EvalResult::Multiple(ref seq) => assert!(seq.len() == res.len()),
                }
            }
        }

        Self {
            result,
            inputs: vec![input],
        }
    }

    fn pow(mut self, mut rhs: Self) -> Self {
        let mut out = Self {
            result: self.result.pow(rhs.result),
            inputs: Vec::new(),
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl std::fmt::Display for Eval<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // line with variable names
        write!(f, "{}", Self::INDEX_STR)?;
        for input in self.inputs.iter() {
            write!(f, ", {}", input.name)?
        }
        write!(f, ", {}\n", Self::RESULT_STR)?;

        match &self.result {
            EvalResult::Single(val) => {
                // this is going to be a 2-line result like:
                // index, x, y, z, result
                // 0, 2, 3, 4, 68
                write!(f, "{}", 0)?;
                for input in self.inputs.iter() {
                    match input.value {
                        EvalResult::Single(in_val) => write!(f, ", {}", in_val)?,
                        EvalResult::Multiple(_) => {
                            unreachable!("Single value EvalResults cannot have multiples as input!")
                        }
                    }
                }
                // TODO: Do we want this newline to be here?
                write!(f, ", {}\n", val)
            }
            EvalResult::Multiple(vals) => {
                let mut index = 0;
                while index < vals.len() {
                    write!(f, "{}", index)?;
                    for input in self.inputs.iter() {
                        match &input.value {
                            EvalResult::Single(in_val) => write!(f, ", {}", in_val)?,
                            EvalResult::Multiple(in_vals) => write!(
                                f,
                                ", {}",
                                in_vals
                                    .get(index)
                                    .expect("We are inside the range by definition")
                            )?,
                        }
                    }
                    write!(f, ", {}\n", vals[index])?;
                    index += 1;
                }
                Ok(())
            }
        }
    }
}

impl From<&Literal> for Eval<'_> {
    fn from(lit: &Literal) -> Self {
        Self {
            result: EvalResult::Single(lit.value),
            inputs: Vec::new(),
        }
    }
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

pub trait EvalASTNode: Debug + fmt::Display {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval<'a>, NameError>;
}
