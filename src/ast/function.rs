use crate::ast::{Assignment, AssignmentLHS, AssignmentRHS, KnownValues};
use crate::tokenize::Location;

use super::{Eval, EvalASTNode, EvalError};

#[derive(Debug, Clone)]
pub struct FunctionNode {
    pub name: String,
    pub loc: Location,
    pub args: Vec<crate::parser::Argument>,
}

impl FunctionNode {
    pub fn new(name: String, loc: Location, args: Vec<crate::parser::Argument>) -> Self {
        Self { name, loc, args }
    }
}

impl EvalASTNode for FunctionNode {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, EvalError> {
        // TODO: Do i want to move handling local variables outside of this call?
        let maybe_func = known_values.functions.get(&self.name);
        if maybe_func.is_none() {
            return Err(EvalError::UnknownName(self.name.clone(), self.loc));
        }

        let ev = maybe_func.unwrap();
        if ev.dependents.is_none() {
            unreachable!("Function must have parameters");
        }

        let deps = ev.dependents.as_ref().unwrap();
        let mut locals = known_values.clone();
        if deps.len() != self.args.len() {
            return Err(EvalError::InvalidArgumentNum(self.name.clone(), deps.len()));
        }

        for (dep, arg) in deps.iter().zip(self.args.iter()) {
            let dep_name = match dep {
                crate::parser::Argument::Name(n) => n,
                _ => unreachable!("For Now, functions can only have names as arguments"),
            };
            let lhs = AssignmentLHS::Name(dep_name.clone());

            let rhs = match arg {
                crate::parser::Argument::Name(n) => AssignmentRHS::Eval(n.clone().into()),
                crate::parser::Argument::Lit(lit) => AssignmentRHS::Single(lit.value),
                crate::parser::Argument::Eval(ev) => AssignmentRHS::Eval(ev.clone()),
            };

            let ass = Assignment::new(lhs, rhs, 0).expect("This assignment cannot fail");
            // TODO(Hawo): Do something with the assignment result
            println!("{}", ass);
            ass.execute(&mut locals).map_err(|x| match x {
                super::AssignmentError::InvalidFunctionParameter(n) => {
                    EvalError::InvalidArgumentType(n)
                }
                super::AssignmentError::InvalidName(crate::ast::simple_nodes::Name {
                    name,
                    loc,
                }) => EvalError::UnknownName(name, loc),
                super::AssignmentError::CannotDepend(_)
                | super::AssignmentError::ExpectedFunc(_)
                | super::AssignmentError::ExpectedName(_) => unreachable!(),
            })?;
        }

        // TODO: handle overriding of function arguments in known values for function call
        Ok(ev.eval(&locals)?.add_knowns(&locals))
    }
}

impl std::fmt::Display for FunctionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var_list = if self.args.len() > 1 {
            self.args.iter().skip(1).fold(
                format!("{}", self.args.first().unwrap().clone()),
                |acc, x| format!("{}, {}", acc, x.clone()),
            )
        } else if self.args.len() == 1 {
            format!("{}", self.args[0])
        } else {
            "".to_string()
        };
        write!(f, "{}({})", self.name, var_list)
    }
}
