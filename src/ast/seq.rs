use crate::{
    ast::{Eval, EvalASTNode, EvalData, EvalError, Evaluatable, KnownValues},
};

#[derive(Debug, Clone)]
pub struct Seq {
    start: Evaluatable,
    end: Evaluatable,
    n: usize,
}

#[derive(Debug)]
pub struct OutOfBounds {
    maxval: usize,
    minval: usize,
    actual: usize,
}

impl std::fmt::Display for OutOfBounds {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Out of Bounds in seq: Expected index in [{}, {}], got {}",
            self.minval, self.maxval, self.actual
        )
    }
}

impl Seq {
    pub fn new(start: Evaluatable, end: Evaluatable, n: usize) -> Self {
        Self {
            start: start.into(),
            end: end.into(),
            n,
        }
    }

    pub fn size(&self) -> usize {
        self.n
    }
}

const SEQ_VAR_NAME: &str = "seq_idx";

impl EvalASTNode for Seq {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval, EvalError> {
        let start = if let EvalData::Single(val) = self.start.eval(known_values)?.result {
            val
        } else {
            // TODO(Hawo): Improve these errors
            return Err(EvalError::InvalidArgumentType(format!(
                "{}",
                self.start.ast
            )));
        };
        let end = if let EvalData::Single(val) = self.end.eval(known_values)?.result {
            val
        } else {
            return Err(EvalError::InvalidArgumentType(format!("{}", self.end.ast)));
        };
        let vals = (0..self.n)
            .map(|i| start + (end - start) / (self.n as f64 - 1.0) * i as f64)
            .collect();
        Ok(Eval::new(
            EvalData::Multiple(vals),
            super::EvalInput {
                name: SEQ_VAR_NAME.to_string(),
                value: EvalData::None,
            },
        ))
    }
}

impl std::fmt::Display for Seq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "seq({}, {}, {})", self.start, self.end, self.n)
    }
}
