use super::{Evaluatable, FunctionNode, Known, KnownValues, Location, Name, OwnedKnownValue};

#[derive(Debug)]
pub enum AssignmentRHS {
    Single(f64),
    Eval(Evaluatable),
}

#[derive(Debug)]
pub enum AssignmentLHS {
    Name(Name),
    Function(FunctionNode),
}

pub enum AssignmentResult<'a> {
    Set(Known<'a>),
    Update(Known<'a>),
}

pub struct Assignment {
    lhs: AssignmentLHS,
    rhs: AssignmentRHS,
}

#[derive(Debug)]
pub enum AssignmentError {
    InvalidFunctionParameter(String),
    ExpectedFunc(Location),
    ExpectedName(Location),
    InvalidName(Name),
    CannotDepend(Name),
}

impl Assignment {
    pub fn new(
        lhs: AssignmentLHS,
        rhs: AssignmentRHS,
        loc: Location,
    ) -> Result<Self, AssignmentError> {
        match lhs {
            AssignmentLHS::Name(_) => {
                if let AssignmentRHS::Eval(Evaluatable {
                    dependents: Some(_),
                    ..
                }) = &rhs
                {
                    Err(AssignmentError::ExpectedName(loc))
                } else {
                    Ok(Self { lhs, rhs })
                }
            }
            AssignmentLHS::Function(_) => {
                if matches!(rhs, AssignmentRHS::Single(_)) {
                    Err(AssignmentError::ExpectedFunc(loc))
                } else {
                    Ok(Self { lhs, rhs })
                }
            }
        }
    }

    /// Execute Consumes the Assignment
    pub fn execute(
        self,
        known_values: &mut KnownValues,
    ) -> Result<AssignmentResult, AssignmentError> {
        // TODO: Do i want to allow replacement of values without permission?
        let name_str = match &self.lhs {
            AssignmentLHS::Name(name) => name.name.clone(),
            AssignmentLHS::Function(func) => func.name.clone(),
        };
        let target = match self.rhs {
            AssignmentRHS::Single(val) => OwnedKnownValue::Single(val),
            AssignmentRHS::Eval(mut ev) => {
                match self.lhs {
                    AssignmentLHS::Name(n) => {
                        if ev.dependents.is_some() {
                            // Cannot assign evaluatable with dependents to name
                            return Err(AssignmentError::CannotDepend(n));
                        }
                    }
                    AssignmentLHS::Function(func) => {
                        ev.dependents = Some(func.args);
                        ev.name = Some(func.name);
                    }
                }
                OwnedKnownValue::Eval(ev)
            }
        };

        Ok(known_values.set(name_str, target))
    }
}
impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rhs_str = match &self.rhs {
            AssignmentRHS::Single(val) => format!("{}", val),
            AssignmentRHS::Eval(ev) => format!("{}", ev),
        };

        let name_str = match &self.lhs {
            AssignmentLHS::Name(name) => format!("{}", name),
            AssignmentLHS::Function(func) => format!("{}", func),
        };

        write!(f, "{} = {}", name_str, rhs_str)
    }
}
