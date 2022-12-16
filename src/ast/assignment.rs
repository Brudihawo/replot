use super::{
    EvalASTNode, Function, Known, KnownValues, Location, Name, NameError, OwnedKnownValue, Seq,
    SyntaxError,
};

pub enum AssignmentRHS {
    Single(f64),
    Multiple(Seq),
    Expression(Box<dyn EvalASTNode>),

    Name(Name),
    Function(Function),
}

pub enum AssignmentLHS {
    Name(Name),
    Function(Function),
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
pub enum AssignmentError {}

impl Assignment {
    pub fn from(
        lhs: AssignmentLHS,
        rhs: AssignmentRHS,
        loc: Location,
    ) -> Result<Self, SyntaxError> {
        match lhs {
            AssignmentLHS::Name(_) => {
                if !matches!(
                    rhs,
                    AssignmentRHS::Name(_) | AssignmentRHS::Single(_) | AssignmentRHS::Multiple(_)
                ) {
                    Err(SyntaxError::ExpectedNameAssign(loc))
                } else {
                    Ok(Self { lhs, rhs })
                }
            }
            AssignmentLHS::Function(_) => {
                if !matches!(
                    rhs,
                    AssignmentRHS::Function(_) | AssignmentRHS::Expression(_)
                ) {
                    Err(SyntaxError::ExpectedFuncAssign(loc))
                } else {
                    Ok(Self { lhs, rhs })
                }
            }
        }
    }

    /// Execute Consumes the Assignment
    pub fn execute(self, known_values: &mut KnownValues) -> Result<AssignmentResult, NameError> {
        // TODO: Do i want to allow replacement of values without permission?
        let name_str = match self.lhs {
            AssignmentLHS::Name(name) => name.name,
            AssignmentLHS::Function(func) => func.name,
        };

        Ok(known_values.set(
            name_str,
            match self.rhs {
                AssignmentRHS::Single(val) => OwnedKnownValue::Single(val),
                AssignmentRHS::Multiple(vals) => OwnedKnownValue::Multiple(vals),
                AssignmentRHS::Expression(expr) => OwnedKnownValue::Expression(expr),
                AssignmentRHS::Name(name) => {
                    if let Some(known) = known_values.get(&name.name) {
                        OwnedKnownValue::Single(known.value.into())
                    } else {
                        return Err(NameError {
                            msg: format!("Name '{}' not found", name.name),
                            loc: name.loc,
                        });
                    }
                }
                AssignmentRHS::Function(func) => {
                    if let Some(known) = known_values.get(&func.name) {
                        OwnedKnownValue::Single(known.value.into())
                    } else {
                        return Err(NameError {
                            msg: format!("Name '{}' not found", func.name),
                            loc: func.loc,
                        });
                    }
                }
            },
        ))
    }
}
impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rhs_str = match &self.rhs {
            AssignmentRHS::Single(val) => format!("{}", val),
            AssignmentRHS::Multiple(seq) => format!("{}", seq),
            AssignmentRHS::Name(name) => format!("{}", name),
            AssignmentRHS::Function(fun) => format!("{}", fun),
            AssignmentRHS::Expression(expr) => format!("{}", expr),
        };

        let name_str = match &self.lhs {
            AssignmentLHS::Name(name) => format!("{}", name),
            AssignmentLHS::Function(func) => format!("{}", func),
        };

        write!(f, "{} = {}", name_str, rhs_str)
    }
}
