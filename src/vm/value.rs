use std::ops;

use crate::compiler::parser::ast::Literal;

#[derive(Debug, PartialEq, PartialOrd)]
pub(crate) enum Value {
    Int(i32),
    Float(f32),
    Char(char),
    Bool(bool),
}
impl From<Literal> for Value {
    fn from(literal: Literal) -> Self {
        match literal {
            Literal::Int(i) => Self::Int(i),
            Literal::Float(f) => Self::Float(f),
            Literal::Char(c) => Self::Char(c),
            Literal::Bool(b) => Self::Bool(b),
            Literal::String(_) => unimplemented!(),
        }
    }
}

impl Value {
    pub(crate) fn to_bool(&self) -> bool {
        match self {
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::Char(c) => *c != '\0',
            Value::Bool(b) => *b,
        }
    }
}

impl ops::Add<Value> for Value {
    type Output = Value;

    fn add(self, _rhs: Value) -> Value {
        match self {
            Value::Int(i) => match _rhs {
                Value::Int(j) => Value::Int(i + j),
                Value::Float(j) => Value::Float(i as f32 + j),
                _ => panic!("Cannot add {:?} to {:?}", _rhs, self),
            },
            Value::Float(i) => match _rhs {
                Value::Int(j) => Value::Float(i + j as f32),
                Value::Float(j) => Value::Float(i + j),
                _ => panic!("Cannot add {:?} to {:?}", _rhs, self),
            },
            _ => panic!("Cannot add {:?} to {:?}", _rhs, self),
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    fn sub(self, _rhs: Value) -> Value {
        match self {
            Value::Int(i) => match _rhs {
                Value::Int(j) => Value::Int(i - j),
                Value::Float(j) => Value::Float(i as f32 - j),
                _ => panic!("Cannot subtract {:?} from {:?}", _rhs, self),
            },
            Value::Float(i) => match _rhs {
                Value::Int(j) => Value::Float(i - j as f32),
                Value::Float(j) => Value::Float(i - j),
                _ => panic!("Cannot subtract {:?} from {:?}", _rhs, self),
            },
            _ => panic!("Cannot subtract {:?} from {:?}", _rhs, self),
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = Value;

    fn mul(self, _rhs: Value) -> Value {
        match self {
            Value::Int(i) => match _rhs {
                Value::Int(j) => Value::Int(i * j),
                Value::Float(j) => Value::Float(i as f32 * j),
                _ => panic!("Cannot multiply {:?} with {:?}", _rhs, self),
            },
            Value::Float(i) => match _rhs {
                Value::Int(j) => Value::Float(i * j as f32),
                Value::Float(j) => Value::Float(i * j),
                _ => panic!("Cannot multiply {:?} with {:?}", _rhs, self),
            },
            _ => panic!("Cannot multiply {:?} with {:?}", _rhs, self),
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = Value;

    fn div(self, _rhs: Value) -> Value {
        match self {
            Value::Int(i) => match _rhs {
                Value::Int(j) => Value::Int(i / j),
                Value::Float(j) => Value::Float(i as f32 / j),
                _ => panic!("Cannot divide {:?} by {:?}", _rhs, self),
            },
            Value::Float(i) => match _rhs {
                Value::Int(j) => Value::Float(i / j as f32),
                Value::Float(j) => Value::Float(i / j),
                _ => panic!("Cannot divide {:?} by {:?}", _rhs, self),
            },
            _ => panic!("Cannot divide {:?} by {:?}", _rhs, self),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(Value::Int(1) + Value::Int(2), Value::Int(3));
        assert_eq!(Value::Int(1) + Value::Float(2.0), Value::Float(3.0));
        assert_eq!(Value::Float(1.0) + Value::Int(2), Value::Float(3.0));
        assert_eq!(Value::Float(1.0) + Value::Float(2.0), Value::Float(3.0));
    }

    #[test]
    fn test_sub() {
        assert_eq!(Value::Int(1) - Value::Int(2), Value::Int(-1));
        assert_eq!(Value::Int(1) - Value::Float(2.0), Value::Float(-1.0));
        assert_eq!(Value::Float(1.0) - Value::Int(2), Value::Float(-1.0));
        assert_eq!(Value::Float(1.0) - Value::Float(2.0), Value::Float(-1.0));
    }

    #[test]
    fn test_mul() {
        assert_eq!(Value::Int(1) * Value::Int(2), Value::Int(2));
        assert_eq!(Value::Int(1) * Value::Float(2.0), Value::Float(2.0));
        assert_eq!(Value::Float(1.0) * Value::Int(2), Value::Float(2.0));
        assert_eq!(Value::Float(1.0) * Value::Float(2.0), Value::Float(2.0));
    }

    #[test]
    fn test_div() {
        assert_eq!(Value::Int(1) / Value::Int(2), Value::Float(0.5));
        assert_eq!(Value::Int(1) / Value::Float(2.0), Value::Float(0.5));
        assert_eq!(Value::Float(1.0) / Value::Int(2), Value::Float(0.5));
        assert_eq!(Value::Float(1.0) / Value::Float(2.0), Value::Float(0.5));
    }

    #[test]
    fn test_and() {
        todo!();
    }

    #[test]
    fn test_or() {
        todo!();
    }

    #[test]
    fn test_not() {
        todo!();
    }

    #[test]
    fn test_compare() {
        todo!();
    }
}

