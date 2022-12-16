use crate::ast::{
    Eval, EvalASTNode, EvalInput, EvalResult, KnownValue, KnownValues, Location, NameError,
};

#[derive(Debug)]
pub struct Literal {
    pub value: f64,
    pub loc: Location,
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
    fn eval(&self, known_values: &KnownValues) -> Result<Eval, NameError> {
        Ok(Eval::from(self))
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct Name {
    pub name: String,
    pub loc: Location,
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
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, NameError> {
        if let Some(known) = known_values.get(&self.name) {
            match known.value {
                KnownValue::Single(val) => Ok(Eval::new(
                    EvalResult::Single(val),
                    EvalInput {
                        name: known.name,
                        value: EvalResult::Single(val),
                    },
                )),
                KnownValue::Multiple(seq) => Ok(Eval::new(
                    seq.eval()?,
                    EvalInput {
                        name: known.name,
                        value: seq.eval()?,
                    },
                )),
                KnownValue::Expression(_) => {
                    unreachable!("This is a Name and should never resolve to a function")
                }
            }
        } else {
            Err(NameError {
                loc: self.loc,
                msg: format!("Name {} is not known", self.name),
            })
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub struct Function {
    pub loc: Location,
    pub name: String,
    // (x, y, z)
    pub dependents: Vec<Name>,
}

impl EvalASTNode for Function {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval<'a>, NameError> {
        if let Some(ast) = known_values.functions.get(&self.name) {
            ast.eval(known_values)
        } else {
            Err(NameError {
                loc: self.loc,
                msg: format!("Name {} is not known", self.name),
            })
        }
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var_list = if self.dependents.len() > 1 {
            self.dependents
                .iter()
                .skip(1)
                .fold(self.dependents.first().unwrap().name.clone(), |acc, x| {
                    format!("{}, {}", acc, x.name)
                })
        } else {
            "".to_string()
        };
        write!(f, "{}({})", self.name, var_list)
    }
}
