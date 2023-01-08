use super::{
    EvalASTNode, Function, FunctionNode, Known, KnownValues, Location, Name, NameError,
    OwnedKnownValue, Seq, SyntaxError,
};

pub enum AssignmentRHS {
    Single(f64),
    Multiple(Seq),
    Expression(Box<dyn EvalASTNode>),

    Name(Name),
    Function(FunctionNode),
}

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
    InvalidName(NameError),
}

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
    pub fn execute(
        self,
        known_values: &mut KnownValues,
    ) -> Result<AssignmentResult, AssignmentError> {
        // TODO: Do i want to allow replacement of values without permission?
        let name_str = match &self.lhs {
            AssignmentLHS::Name(name) => name.name.clone(),
            AssignmentLHS::Function(func) => func.name.clone(),
        };

        Ok(known_values.set(
            name_str,
            match self.rhs {
                AssignmentRHS::Single(val) => OwnedKnownValue::Single(val),
                AssignmentRHS::Multiple(vals) => OwnedKnownValue::Multiple(vals),
                AssignmentRHS::Expression(expr) => {
                    let args = match self.lhs {
                        AssignmentLHS::Name(_) => unreachable!("Trying to set expression to a name. This is disallowed for now and should fail elsewhere!"),
                        AssignmentLHS::Function(func) => func.args
                    };
                    let first_invalid = args.iter().find(|x| !matches![x, crate::parser::Argument::Name(_)]);
                    if let Some(arg) = first_invalid {
                        // this cannot be a declaration, because the arguments to the function need
                        // to be specified as names
                        return Err(AssignmentError::InvalidFunctionParameter(format!("{}", arg)));
                    }
                    OwnedKnownValue::Function(Function::new(args.iter().map(|x| match x {
                        crate::parser::Argument::Name(name) => name.name.clone(),
                        _ => unreachable!()
                    }).collect(),
                    expr))
                },
                AssignmentRHS::Name(name) => {
                    if let Some(known) = known_values.get(&name.name) {
                        OwnedKnownValue::Single(known.value.into())
                    } else {
                        return Err(AssignmentError::InvalidName(NameError {
                            msg: format!("Name '{}' not found", name.name),
                            loc: name.loc,
                        }));
                    }
                }
                AssignmentRHS::Function(func) => {
                    if let Some(known) = known_values.get(&func.name) {
                        OwnedKnownValue::Single(known.value.into())
                    } else {
                        return Err(AssignmentError::InvalidName(NameError {
                            msg: format!("Name '{}' not found", func.name),
                            loc: func.loc,
                        }));
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
