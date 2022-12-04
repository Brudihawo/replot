use super::{AssignmentResult, EvalASTNode, Seq};

use std::collections::HashMap;

pub struct KnownValues {
    pub singles: HashMap<String, f64>,
    pub functions: HashMap<String, Box<dyn EvalASTNode>>,
    pub multiples: HashMap<String, Seq>,
}

pub enum KnownValue<'a> {
    Single(f64),
    Multiple(&'a Seq),
    Expression(&'a Box<dyn EvalASTNode>),
}

impl<'a> Into<f64> for KnownValue<'a> {
    fn into(self) -> f64 {
        if let KnownValue::Single(val) = self {
            return val;
        }
        panic!("KnownValue is not of variant Single, so it cannot be cast to f64");
    }
}

impl<'a> Into<&'a Box<dyn EvalASTNode>> for KnownValue<'a> {
    fn into(self) -> &'a Box<dyn EvalASTNode> {
        if let KnownValue::Expression(expr) = self {
            return expr;
        }
        panic!("KnownValue is not of variant Multiple, so it cannot be cast to f64");
    }
}

pub enum OwnedKnownValue {
    Single(f64),
    Multiple(Seq),
    Expression(Box<dyn EvalASTNode>),
}

impl Into<f64> for OwnedKnownValue {
    fn into(self) -> f64 {
        if let OwnedKnownValue::Single(val) = self {
            return val;
        }
        panic!("OwnedKnownValue is not of variant Single, so it cannot be cast to f64");
    }
}

impl Into<Box<dyn EvalASTNode>> for OwnedKnownValue {
    fn into(self) -> Box<dyn EvalASTNode> {
        if let OwnedKnownValue::Expression(expr) = self {
            return expr;
        }
        panic!("OwnedKnownValue is not of variant Multiple, so it cannot be cast to f64");
    }
}

impl KnownValues {
    pub fn get(&self, name: &String) -> Option<KnownValue> {
        if let Some(val) = self.singles.get(name) {
            return Some(KnownValue::Single(*val));
        }

        if let Some(func) = self.functions.get(name) {
            return Some(KnownValue::Expression(&func));
        }

        if let Some(multiples) = self.multiples.get(name) {
            return Some(KnownValue::Multiple(multiples.clone()));
        }

        None
    }

    pub fn set(&mut self, name: String, target: OwnedKnownValue) -> AssignmentResult {
        if let Some(_) = match target {
            OwnedKnownValue::Single(val) => self.singles.insert(name.clone(), val).map(|_| ()),
            OwnedKnownValue::Multiple(vals) => {
                self.multiples.insert(name.clone(), vals).map(|_| ())
            }
            OwnedKnownValue::Expression(expr) => {
                self.functions.insert(name.clone(), expr).map(|_| ())
            }
        } {
            AssignmentResult::Update(self.get(&name).unwrap())
        } else {
            AssignmentResult::Set(self.get(&name).unwrap())
        }
    }
}
