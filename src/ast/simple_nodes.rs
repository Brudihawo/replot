use crate::ast::{EvalASTNode, EvalResult, KnownValue, KnownValues, Location, NameError};

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
    fn eval(&self, known_values: &KnownValues) -> Result<EvalResult, NameError> {
        Ok(EvalResult::Single(self.value))
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
    fn eval(&self, known_values: &KnownValues) -> Result<EvalResult, NameError> {
        if let Some(known_value) = known_values.get(&self.name) {
            match known_value {
                KnownValue::Single(val) => Ok(EvalResult::Single(val)),
                KnownValue::Multiple(seq) => Ok(seq.eval()?),
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
    fn eval(&self, known_values: &KnownValues) -> Result<EvalResult, NameError> {
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
