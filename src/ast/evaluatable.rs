use super::{simple_nodes, EvalASTNode, KnownValues};
use crate::parser::Argument;

#[derive(Debug, Clone)]
pub struct Evaluatable {
    pub name: Option<String>,
    pub dependents: Option<Vec<Argument>>,
    pub ast: std::rc::Rc<dyn EvalASTNode>,
}

impl std::fmt::Display for Evaluatable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(n) = &self.name {
            write!(f, "{}: ", n)?;
        }
        if let Some(args) = &self.dependents {
            if args.len() > 0 {
                write!(f, "({}", args[0])?;
                for arg in args.iter().skip(1) {
                    write!(f, ", {}", arg)?;
                }
                write!(f, ") -> ")?;
            }
        }
        write!(f, "{}", self.ast)
    }
}

impl EvalASTNode for Evaluatable {
    fn eval<'a>(
        &'a self,
        known_values: &'a KnownValues,
    ) -> Result<super::Eval, super::EvalError> {
        self.ast.eval(known_values)
    }
}

impl Evaluatable {
    pub fn with_name(
        name: String,
        dependents: Vec<Argument>,
        ast: std::rc::Rc<dyn EvalASTNode>,
    ) -> Self {
        Self {
            name: Some(name),
            dependents: Some(dependents),
            ast,
        }
    }

    pub fn new(dependents: Vec<Argument>, ast: std::rc::Rc<dyn EvalASTNode>) -> Self {
        Self {
            name: None,
            dependents: Some(dependents),
            ast,
        }
    }

    pub fn new_nodeps(ast: std::rc::Rc<dyn EvalASTNode>) -> Self {
        Self {
            name: None,
            dependents: None,
            ast,
        }
    }
}

macro_rules! into_eval_nodeps {
    ($t:ty) => {
        impl Into<Evaluatable> for $t {
            fn into(self) -> Evaluatable {
                Evaluatable::new_nodeps(std::rc::Rc::new(self))
            }
        }
    };
}

into_eval_nodeps!(simple_nodes::Name);
into_eval_nodeps!(simple_nodes::Literal);
into_eval_nodeps!(crate::ast::UnaryExpression);
into_eval_nodeps!(crate::ast::BinaryExpression);
into_eval_nodeps!(crate::ast::Seq);

impl Into<Evaluatable> for crate::ast::function::FunctionNode {
    fn into(self) -> Evaluatable {
        Evaluatable {
            name: Some(self.name.clone()),
            dependents: Some(self.args.clone()),
            ast: std::rc::Rc::new(self),
        }
    }
}
