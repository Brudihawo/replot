use super::{EvalASTNode, Function, KnownValue, KnownValues, Name, NameError, OwnedKnownValue};

enum AssignmentTargetType {
    Single(f64),
    Multiple(Vec<f64>),
    Expression(Box<dyn EvalASTNode>),

    Name(Name),
    Function(Function),
}

enum AssignmentName {
    Name(Name),
    Function(Function),
}

pub enum AssignmentResult<'a> {
    Set(KnownValue<'a>),
    Update(KnownValue<'a>),
}

pub struct Assignment {
    name: AssignmentName,
    target: AssignmentTargetType,
}

impl Assignment {
    fn simple_print(&self) -> String {
        let target_str = match &self.target {
            AssignmentTargetType::Single(val) => format!("{}", val),
            AssignmentTargetType::Multiple(vals) => {
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
            AssignmentTargetType::Name(name) => name.name.clone(),
            AssignmentTargetType::Function(fun) => fun.simple_print(),
            AssignmentTargetType::Expression(expr) => expr.simple_print(),
        };
        let name_str = match &self.name {
            AssignmentName::Name(name) => &name.name,
            AssignmentName::Function(func) => &func.name,
        };

        format!("{} = {}", name_str, target_str)
    }

    /// Execute Consumes the Assignment
    fn execute(self, known_values: &mut KnownValues) -> Result<AssignmentResult, NameError> {
        // TODO: Do i want to allow replacement of values without permission?
        let name_str = match self.name {
            AssignmentName::Name(name) => name.name,
            AssignmentName::Function(func) => func.name,
        };

        Ok(known_values.set(
            name_str,
            match self.target {
                AssignmentTargetType::Single(val) => OwnedKnownValue::Single(val),
                AssignmentTargetType::Multiple(vals) => OwnedKnownValue::Multiple(vals),
                AssignmentTargetType::Expression(expr) => OwnedKnownValue::Expression(expr),
                AssignmentTargetType::Name(name) => {
                    if let Some(known) = known_values.get(&name.name) {
                        OwnedKnownValue::Single(known.into())
                    } else {
                        return Err(NameError {
                            msg: format!("Name '{}' not found", name.name),
                            loc: name.loc,
                        });
                    }
                }
                AssignmentTargetType::Function(func) => {
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
