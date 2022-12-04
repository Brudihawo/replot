use super::{
    EvalASTNode, Function, KnownValue, KnownValues, Location, Name, NameError, OwnedKnownValue,
};

pub enum AssignmentRHS {
    Single(f64),
    Multiple(Vec<f64>),
    Expression(Box<dyn EvalASTNode>),

    Name(Name),
    Function(Function),
}

pub enum AssignmentLHS {
    Name(Name),
    Function(Function),
}

pub enum AssignmentResult<'a> {
    Set(KnownValue<'a>),
    Update(KnownValue<'a>),
}

pub struct Assignment {
    lhs: AssignmentLHS,
    rhs: AssignmentRHS,
}

#[derive(Debug)]
pub enum AssignmentError {
    ExpectedFuncAssign(Location),
    ExpectedNameAssign(Location),
}

impl Assignment {
    pub fn from(
        lhs: AssignmentLHS,
        rhs: AssignmentRHS,
        loc: Location,
    ) -> Result<Self, AssignmentError> {
        match lhs {
            AssignmentLHS::Name(_) => {
                if !matches!(
                    rhs,
                    AssignmentRHS::Name(_) | AssignmentRHS::Single(_) | AssignmentRHS::Multiple(_)
                ) {
                    Err(AssignmentError::ExpectedNameAssign(loc))
                } else {
                    Ok(Self { lhs, rhs })
                }
            }
            AssignmentLHS::Function(_) => {
                if !matches!(
                    rhs,
                    AssignmentRHS::Function(_) | AssignmentRHS::Expression(_)
                ) {
                    Err(AssignmentError::ExpectedFuncAssign(loc))
                } else {
                    Ok(Self { lhs, rhs })
                }
            }
        }
    }

    pub fn simple_print(&self) -> String {
        let rhs_str = match &self.rhs {
            AssignmentRHS::Single(val) => format!("{}", val),
            AssignmentRHS::Multiple(vals) => {
                assert!(vals.len() > 1);
                if vals.len() < 6 {
                    format!(
                        "{}",
                        vals.iter()
                            .skip(1)
                            .fold(format!("{}", vals[0]), |acc, x| format!("{}, {}", acc, x))
                    )
                } else {
                    format!(
                        "[{}, {}, ... {}, {}]",
                        vals[0],
                        vals[1],
                        vals[vals.len() - 2],
                        vals[vals.len() - 1]
                    )
                }
            }
            AssignmentRHS::Name(name) => name.simple_print(),
            AssignmentRHS::Function(fun) => fun.simple_print(),
            AssignmentRHS::Expression(expr) => expr.simple_print(),
        };

        let name_str = match &self.lhs {
            AssignmentLHS::Name(name) => name.simple_print(),
            AssignmentLHS::Function(func) => func.simple_print(),
        };

        format!("{} = {}", name_str, rhs_str)
    }

    /// Execute Consumes the Assignment
    fn execute(self, known_values: &mut KnownValues) -> Result<AssignmentResult, NameError> {
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
                        OwnedKnownValue::Single(known.into())
                    } else {
                        return Err(NameError {
                            msg: format!("Name '{}' not found", name.name),
                            loc: name.loc,
                        });
                    }
                }
                AssignmentRHS::Function(func) => {
                    if let Some(known) = known_values.get(&func.name) {
                        OwnedKnownValue::Single(known.into())
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
