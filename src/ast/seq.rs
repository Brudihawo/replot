use crate::ast::{EvalResult, NameError};

#[derive(Debug)]
pub struct Seq {
    start: f64,
    end: f64,
    n: usize,
}

impl Seq {
    pub fn new(start: f64, end: f64, n: usize) -> Self {
        Self { start, end, n }
    }

    pub fn eval(&self) -> Result<EvalResult, NameError> {
        let vals = (0..self.n)
            .map(|i| self.start + (self.end - self.start) / (self.n as f64 - 1.0) * i as f64)
            .collect();
        Ok(EvalResult::Multiple(vals))
    }

    pub fn simple_print(&self) -> String {
        format!("seq({}, {}, {})", self.start, self.end, self.n)
    }
}