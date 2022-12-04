use crate::ast::{EvalASTNode, KnownValues, Location, NameError};

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
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        if let Some(val) = known_values.singles.get(&self.name) {
            Ok(*val)
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

#[derive(Debug)]
pub struct Function {
    pub loc: Location,
    pub name: String,
    // (x, y, z)
    pub dependents: Vec<Name>,
}

impl EvalASTNode for Function {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        if let Some(ast) = known_values.functions.get(&self.name) {
            ast.eval(known_values)
        } else {
            Err(NameError {
                loc: self.loc,
                msg: format!("Name {} is not known", self.name),
            })
        }
    }

    fn simple_print(&self) -> String {
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
        format!("{}({})", self.name.clone(), var_list)
    }
}
