use super::{AssignmentResult, Evaluatable, Seq};

use std::collections::HashMap;

#[derive(Clone)]
pub struct KnownValues {
    pub singles: HashMap<String, f64>,
    pub functions: HashMap<String, Evaluatable>,
    pub multiples: HashMap<String, Seq>,
}

pub struct Known<'a> {
    pub name: &'a String,
    pub value: KnownValue<'a>,
}

impl<'a> From<(f64, &'a String)> for Known<'a> {
    fn from(i: (f64, &'a String)) -> Self {
        Self {
            name: i.1,
            value: KnownValue::Single(i.0),
        }
    }
}

impl<'a> From<(&'a Seq, &'a String)> for Known<'a> {
    fn from(i: (&'a Seq, &'a String)) -> Self {
        Self {
            name: i.1,
            value: KnownValue::Multiple(i.0),
        }
    }
}

impl<'a> From<(&'a Evaluatable, &'a String)> for Known<'a> {
    fn from(i: (&'a Evaluatable, &'a String)) -> Self {
        Self {
            name: i.1,
            value: KnownValue::Function(i.0),
        }
    }
}

pub enum KnownValue<'a> {
    Single(f64),
    Multiple(&'a Seq),
    Function(&'a Evaluatable),
}

impl<'a> Into<f64> for KnownValue<'a> {
    fn into(self) -> f64 {
        if let KnownValue::Single(val) = self {
            return val;
        }
        panic!("KnownValue is not of variant Single, so it cannot be cast to f64");
    }
}

impl<'a> Into<&'a Evaluatable> for KnownValue<'a> {
    fn into(self) -> &'a Evaluatable {
        if let KnownValue::Function(expr) = self {
            return expr;
        }
        panic!("KnownValue is not of variant Multiple, so it cannot be cast to f64");
    }
}

#[derive(Debug)]
pub enum OwnedKnownValue {
    Single(f64),
    Multiple(Seq),
    Eval(Evaluatable),
}

impl Into<f64> for OwnedKnownValue {
    fn into(self) -> f64 {
        if let OwnedKnownValue::Single(val) = self {
            return val;
        }
        panic!("OwnedKnownValue is not of variant Single, so it cannot be cast to f64");
    }
}

impl Into<Evaluatable> for OwnedKnownValue {
    fn into(self) -> Evaluatable {
        if let OwnedKnownValue::Eval(ev) = self {
            return ev;
        }
        panic!("OwnedKnownValue is not of variant Multiple, so it cannot be cast to f64");
    }
}

impl KnownValues {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            singles: HashMap::new(),
            multiples: HashMap::new(),
        }
    }

    pub fn get<'a>(&'a self, name: &String) -> Option<Known> {
        if let Some((key, val)) = self.singles.get_key_value(name) {
            return Some(Known::from((*val, key)));
        }

        if let Some((key, func)) = self.functions.get_key_value(name) {
            return Some(Known::from((func, key)));
        }

        if let Some((key, multiples)) = self.multiples.get_key_value(name) {
            return Some(Known::from((multiples, key)));
        }

        None
    }

    pub fn set(&mut self, name: String, target: OwnedKnownValue) -> AssignmentResult {
        if let Some(_) = match target {
            // TODO(Hawo): What is going on with the .map here
            OwnedKnownValue::Single(val) => self.singles.insert(name.clone(), val).map(|_| ()),
            OwnedKnownValue::Multiple(vals) => {
                self.multiples.insert(name.clone(), vals).map(|_| ())
            }
            OwnedKnownValue::Eval(ev) => self.functions.insert(name.clone(), ev).map(|_| ()),
        } {
            AssignmentResult::Update(self.get(&name).unwrap())
        } else {
            AssignmentResult::Set(self.get(&name).unwrap())
        }
    }
}
