use crate::ast::{NameError, KnownValues, EvalASTNode, Location};

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

struct Function {
    name: String,
    // (x, y, z)
    dependents: Vec<Name>,
}
