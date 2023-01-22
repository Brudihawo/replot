use crate::ast::{
    Eval, EvalASTNode, EvalData, EvalError, EvalInput, KnownValue, KnownValues, Location,
};

#[derive(Debug, Clone)]
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
    fn eval(&self, _known_values: &KnownValues) -> Result<Eval, EvalError> {
        Ok(Eval::from(self))
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
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
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, EvalError> {
        if let Some(known) = known_values.get(&self.name) {
            match known.value {
                KnownValue::Single(val) => Ok(Eval::new(
                    EvalData::Single(val),
                    EvalInput {
                        name: known.name.clone(),
                        value: EvalData::Single(val),
                    },
                )),
                KnownValue::Multiple(seq) => {
                    let val = seq.eval(known_values)?.result;
                    Ok(Eval::new(
                        val.clone(),
                        EvalInput {
                            name: known.name.clone(),
                            value: val,
                        },
                    ))
                }
                KnownValue::Function(func) => func.eval(known_values),
            }
        } else {
            Err(EvalError::UnknownName(self.name.clone(), self.loc))
        }
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}
