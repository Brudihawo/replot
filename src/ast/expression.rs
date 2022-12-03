use crate::ast::*;

#[derive(Debug)]
pub struct BinaryExpression {
    left: Box<dyn EvalASTNode>,
    right: Box<dyn EvalASTNode>,
    tipe: BinXprType,
}

impl BinaryExpression {
    pub fn new(
        tipe: OperatorType,
        left: Box<dyn EvalASTNode>,
        right: Box<dyn EvalASTNode>,
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

    pub fn new_box(
        tipe: OperatorType,
        left: Box<dyn EvalASTNode>,
        right: Box<dyn EvalASTNode>,
    ) -> Result<Box<Self>, AstBuildError> {
        Ok(Box::new(Self::new(tipe, left, right)?))
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
                TokenType::Literal(value) => Box::new(Literal::new(*value, operand.location)),
                TokenType::Name(name) => Box::new(Name::new(name.clone(), operand.location)),
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
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        let lval = self.left.eval(known_values)?;
        let rval = self.right.eval(known_values)?;
        match &self.tipe {
            BinXprType::Add => Ok(lval + rval),
            BinXprType::Subtract => Ok(lval - rval),
            BinXprType::Multiply => Ok(lval * rval),
            BinXprType::Divide => Ok(lval / rval),
            BinXprType::Power => Ok(lval.powf(rval)),
        }
    }
    fn simple_print(&self) -> String {
        format!(
            "({} {} {})",
            self.left.simple_print(),
            match &self.tipe {
                BinXprType::Add => "+",
                BinXprType::Subtract => "-",
                BinXprType::Multiply => "*",
                BinXprType::Divide => "/",
                BinXprType::Power => "^",
            },
            self.right.simple_print()
        )
    }
}

impl EvalASTNode for UnaryExpression {
    fn eval(&self, known_values: &KnownValues) -> Result<f64, NameError> {
        let cval = self.child.eval(known_values)?;
        match &self.tipe {
            UnXprType::Negate => Ok(-cval),
            UnXprType::RedundantPlus => Ok(cval),
        }
    }

    fn simple_print(&self) -> String {
        format!(
            "({} {})",
            match &self.tipe {
                UnXprType::Negate => "-",
                UnXprType::RedundantPlus => "+",
            },
            self.child.simple_print()
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
