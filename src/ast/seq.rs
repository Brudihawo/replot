use crate::ast::{Eval, EvalASTNode, EvalData, KnownValues, NameError};

#[derive(Debug)]
pub struct Seq {
    start: f64,
    end: f64,
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
    pub fn new(start: f64, end: f64, n: usize) -> Self {
        Self { start, end, n }
    }

    pub fn all_vals(&self) -> Vec<f64> {
        (0..self.n)
            .map(|i| self.start + (self.end - self.start) / (self.n as f64 - 1.0) * i as f64)
            .collect()
    }

    pub fn size(&self) -> usize {
        self.n
    }

    pub fn at(&self, pos: usize) -> Result<f64, OutOfBounds> {
        if pos < self.n {
            Err(OutOfBounds {
                maxval: self.n,
                minval: 0,
                actual: pos,
            })
        } else {
            Ok(self.start + (self.end - self.start) / (self.n as f64 - 1.0) * pos as f64)
        }
    }
}

const SEQ_VAR_NAME: &str = "seq_idx";

impl EvalASTNode for Seq {
    fn eval<'a>(&'a self, known_values: &'a KnownValues) -> Result<Eval<'a>, NameError> {
        let vals = (0..self.n)
            .map(|i| self.start + (self.end - self.start) / (self.n as f64 - 1.0) * i as f64)
            .collect();
        Ok(Eval::new(
            EvalData::Multiple(vals),
            super::EvalInput {
                name: SEQ_VAR_NAME,
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
