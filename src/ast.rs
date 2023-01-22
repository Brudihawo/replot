mod assignment;
mod evaluatable;
mod expression;
mod function;
mod known;
mod seq;
mod simple_nodes;

pub(crate) use assignment::*;
pub(crate) use evaluatable::*;
pub(crate) use expression::*;
pub(crate) use function::*;
pub(crate) use known::*;
pub(crate) use seq::*;
pub(crate) use simple_nodes::*;

use crate::tokenize::{Location, OperatorType, Token, TokenType};
use std::fmt::{self, Debug};

#[derive(Debug)]
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
    ExpectedNameAssign(Location, assignment::AssignmentRHS),
    InvalidFunctionName(Token),
    InvalidArgument(crate::parser::Argument),
}

#[derive(Debug)]
pub enum AstBuildError {
    WrongOperator,
}

#[derive(Debug)]
pub enum EvalError {
    UnknownName(String, Location),
    InvalidArgumentNum(String, usize),
    InvalidArgumentType(String),
}

// Evaluation input generated from name nodes
pub struct EvalInput {
    pub name: String,
    pub value: EvalData,
}

pub struct Eval {
    pub result: EvalData,
    pub inputs: Vec<EvalInput>,
    locals: Option<KnownValues>,
}

impl<'a> std::ops::Neg for Eval {
    type Output = Eval;

    fn neg(self) -> Self::Output {
        Self::Output {
            result: -self.result,
            inputs: self.inputs,
            locals: None,
        }
    }
}

impl<'a> std::ops::Add for Eval {
    type Output = Eval;

    fn add(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result + rhs.result,
            inputs: Vec::<EvalInput>::new(),
            locals: None,
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> std::ops::Sub for Eval {
    type Output = Eval;

    fn sub(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result - rhs.result,
            inputs: Vec::new(),
            locals: None,
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> std::ops::Mul for Eval {
    type Output = Eval;

    fn mul(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result * rhs.result,
            inputs: Vec::new(),
            locals: None,
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl<'a> std::ops::Div for Eval {
    type Output = Eval;

    fn div(mut self, mut rhs: Self) -> Self::Output {
        let mut out = Self::Output {
            result: self.result / rhs.result,
            inputs: Vec::new(),
            locals: None,
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }
}

impl Eval {
    const RESULT_STR: &str = "result";
    const INDEX_STR: &str = "index";

    fn new(result: EvalData, input: EvalInput) -> Self {
        match result {
            EvalData::Single(_) => {
                assert!(matches!(input.value, EvalData::Single(_)))
            }
            EvalData::Multiple(ref res) => {
                match input.value {
                    EvalData::None => {}
                    EvalData::Single(_) => {}
                    // TODO(Hawo): This has to change if we implement multiple values in functions
                    EvalData::Multiple(ref seq) => assert!(seq.len() == res.len()),
                }
            }
            EvalData::None => unreachable!("None should never be a result type"),
        }

        Self {
            result,
            inputs: vec![input],
            locals: None,
        }
    }

    fn add_knowns(mut self: Eval, knowns: &'_ KnownValues) -> Self {
        self.locals = Some(knowns.clone());
        self
    }

    fn pow(mut self, mut rhs: Self) -> Self {
        let mut out = Self {
            result: self.result.pow(rhs.result),
            inputs: Vec::new(),
            locals: None,
        };
        out.inputs.append(&mut self.inputs);
        out.inputs.append(&mut rhs.inputs);
        out
    }

    fn scale(self, rhs: f64) -> Self {
        let out = Self {
            result: match self.result {
                EvalData::None => unreachable!("Empty Eval should never be scaled"),
                EvalData::Single(val) => EvalData::Single(val * rhs),
                EvalData::Multiple(vals) => {
                    EvalData::Multiple(vals.iter().map(|&x| x * rhs).collect())
                }
            },
            inputs: self.inputs,
            locals: None,
        };
        out
    }
}

impl std::fmt::Display for Eval {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // line with variable names
        write!(f, "{}", Self::INDEX_STR)?;
        for input in self
            .inputs
            .iter()
            .filter(|&x| !(matches![x.value, EvalData::None]))
        {
            write!(f, ", {}", input.name)?
        }
        write!(f, ", {}\n", Self::RESULT_STR)?;

        match &self.result {
            EvalData::None => unreachable!("EvalResult cannot be of type None"),
            EvalData::Single(val) => {
                // this is going to be a 2-line result like:
                // index, x, y, z, result
                // 0, 2, 3, 4, 68
                write!(f, "{}", 0)?;
                for input in self.inputs.iter() {
                    match input.value {
                        EvalData::None => {}
                        EvalData::Single(in_val) => write!(f, ", {}", in_val)?,
                        EvalData::Multiple(_) => {
                            unreachable!("Single value EvalResults cannot have multiples as input!")
                        }
                    }
                }
                // TODO: Do we want this newline to be here?
                write!(f, ", {}\n", val)
            }
            EvalData::Multiple(vals) => {
                let mut index = 0;
                while index < vals.len() {
                    write!(f, "{}", index)?;
                    for input in self.inputs.iter() {
                        match &input.value {
                            EvalData::Single(in_val) => write!(f, ", {}", in_val)?,
                            EvalData::Multiple(in_vals) => write!(
                                f,
                                ", {}",
                                in_vals
                                    .get(index)
                                    .expect("We are inside the range by definition")
                            )?,
                            EvalData::None => {}
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

impl From<&Literal> for Eval {
    fn from(lit: &Literal) -> Self {
        Self {
            result: EvalData::Single(lit.value),
            inputs: Vec::new(),
            locals: None,
        }
    }
}

#[derive(Clone)]
pub enum EvalData {
    None,
    Single(f64),
    Multiple(Vec<f64>),
}

impl std::ops::Add for EvalData {
    type Output = EvalData;
    fn add(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs + rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs + r).collect()),
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| rhs + l).collect()),
                Self::Multiple(rhs) => {
                    // TODO(Hawo): Make this a nice error that we can use, probably check before
                    assert!(rhs.len() == lhs.len());
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l + r).collect())
                }
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::None => panic!("This is a placeholder Type"),
        }
    }
}

impl std::ops::Sub for EvalData {
    type Output = EvalData;
    fn sub(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs - rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs - r).collect()),
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| l - rhs).collect()),
                Self::Multiple(rhs) => {
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l - r).collect())
                }
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::None => panic!("This is a placeholder Type"),
        }
    }
}

impl std::ops::Mul for EvalData {
    type Output = EvalData;
    fn mul(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs * rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|x| x * lhs).collect()),
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|x| x * rhs).collect()),
                Self::Multiple(rhs) => {
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l * r).collect())
                }
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::None => panic!("This is a placeholder Type"),
        }
    }
}

impl std::ops::Div for EvalData {
    type Output = EvalData;
    fn div(self, rhs: Self) -> Self::Output {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs / rhs),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs / r).collect()),
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| l / rhs).collect()),
                Self::Multiple(rhs) => {
                    Self::Multiple(lhs.iter().zip(rhs.iter()).map(|(l, r)| l / r).collect())
                }
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::None => panic!("This is a placeholder Type"),
        }
    }
}

impl std::ops::Neg for EvalData {
    type Output = EvalData;
    fn neg(self) -> Self::Output {
        match &self {
            Self::Single(val) => Self::Single(-val),
            Self::Multiple(vals) => Self::Multiple(vals.iter().map(|val| -val).collect()),
            Self::None => panic!("This is a placeholder Type"),
        }
    }
}

impl EvalData {
    fn pow(self, rhs: Self) -> Self {
        match &self {
            Self::Single(lhs) => match rhs {
                Self::Single(rhs) => Self::Single(lhs.powf(rhs)),
                Self::Multiple(rhs) => Self::Multiple(rhs.iter().map(|r| lhs.powf(*r)).collect()),
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::Multiple(lhs) => match rhs {
                Self::Single(rhs) => Self::Multiple(lhs.iter().map(|l| l.powf(rhs)).collect()),
                Self::Multiple(rhs) => Self::Multiple(
                    lhs.iter()
                        .zip(rhs.iter())
                        .map(|(l, r)| l.powf(*r))
                        .collect(),
                ),
                Self::None => panic!("This is a placeholder Type"),
            },
            Self::None => panic!("This is a placeholder Type"),
        }
    }
}

pub trait EvalASTNode: Debug + fmt::Display {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, EvalError>;
}
