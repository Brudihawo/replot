use crate::ast::*;

#[derive(Debug)]
pub struct BinaryExpression {
    left: Evaluatable,
    right: Evaluatable,
    tipe: BinXprType,
}

impl BinaryExpression {
    pub fn new(
        tipe: OperatorType,
        left: Evaluatable,
        right: Evaluatable,
    ) -> Result<Self, AstBuildError> {
        Ok(BinaryExpression {
            tipe: match tipe {
                OperatorType::Plus => BinXprType::Add,
                OperatorType::Minus => BinXprType::Subtract,
                OperatorType::Asterisk => BinXprType::Multiply,
                OperatorType::Slash => BinXprType::Divide,
                OperatorType::Power => BinXprType::Power,
                OperatorType::Equals => return Err(AstBuildError::WrongOperator),
            },
            left,
            right,
        })
    }
}

#[derive(Debug)]
pub struct UnaryExpression {
    child: Box<dyn EvalASTNode>,
    tipe: UnXprType,
}

impl UnaryExpression {
    pub fn try_from(operator: &Token, operand: &Token) -> Result<Self, ()> {
        Ok(UnaryExpression {
            child: match &operand.token_type {
                TokenType::Literal(value) => Box::new(Literal::new(*value, operand.loc)),
                TokenType::Name(name) => Box::new(Name::new(name.clone(), operand.loc)),
                _ => return Err(()),
            },
            tipe: match &operator.token_type {
                TokenType::Operator(op) => match op {
                    OperatorType::Plus => UnXprType::RedundantPlus,
                    OperatorType::Minus => UnXprType::Negate,
                    _ => return Err(()),
                },
                _ => return Err(()),
            },
        })
    }
}

impl EvalASTNode for BinaryExpression {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, EvalError> {
        let lval = self.left.ast.eval(known_values)?;
        let rval = self.right.ast.eval(known_values)?;
        match &self.tipe {
            BinXprType::Add => Ok(lval + rval),
            BinXprType::Subtract => Ok(lval - rval),
            BinXprType::Multiply => Ok(lval * rval),
            BinXprType::Divide => Ok(lval / rval),
            BinXprType::Power => Ok(lval.pow(rval)),
        }
    }
}

impl std::fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {} {})",
            self.left,
            match &self.tipe {
                BinXprType::Add => "+",
                BinXprType::Subtract => "-",
                BinXprType::Multiply => "*",
                BinXprType::Divide => "/",
                BinXprType::Power => "^",
            },
            self.right,
        )
    }
}

impl EvalASTNode for UnaryExpression {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, EvalError> {
        let cval = self.child.eval(known_values)?;
        match &self.tipe {
            UnXprType::Negate => Ok(-cval),
            UnXprType::RedundantPlus => Ok(cval),
        }
    }
}

impl std::fmt::Display for UnaryExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({} {})",
            match &self.tipe {
                UnXprType::Negate => "-",
                UnXprType::RedundantPlus => "+",
            },
            self.child,
        )
    }
}

#[derive(Debug)]
enum BinXprType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
}

#[derive(Debug)]
pub enum UnXprType {
    Negate,
    RedundantPlus,
}
