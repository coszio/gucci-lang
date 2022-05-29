use std::ops;

use crate::compiler::parser::ast::Literal;

#[derive(Debug, PartialEq)]
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
// impl Value {
//     fn to_char(val: Value) -> Self {
//         match val {
//             Value::Int(i) => Self::Char(i as u8 as char),
//             Value::Char(c) => Self::Char(c),
//             _ => panic!("Cannot convert {:?} to char", val),
//         }
//     }
//     fn to_int(val: Value) -> Self {
//         match val {
//             Value::Int(i) => Self::Int(i),
//             Value::Char(c) => Self::Int(c as i32),
//             _ => panic!("Cannot convert {:?} to int", val),
//         }
//     }
//     fn to_float(val: Value) -> Self {
//         match val {
//             Value::Int(i) => Self::Float(i as f32),
//             Value::Float(f) => Self::Float(f),
//             _ => panic!("Cannot convert {:?} to float", val),
//         }
//     }
//     fn to_bool(val: Value) -> Self {
//         match val {
//             Value::Int(i) => Self::Bool(i != 0),
//             Value::Float(f) => Self::Bool(f != 0.0),
//             Value::Char(c) => Self::Bool(c != '\0'),
//             Value::Bool(b) => Self::Bool(b),
//         }
//     }
// }
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

impl ops::BitAnd<Value> for Value {
    type Output = Value;

    fn bitand(self, _rhs: Value) -> Value {
        match self {
            Value::Int(i) => match _rhs {
                Value::Int(j) => Value::Int(i & j),
                _ => panic!("Cannot bitwise and {:?} with {:?}", _rhs, self),
            },
            _ => panic!("Cannot bitwise and {:?} with {:?}", _rhs, self),
        }
    }
}

impl ops::BitOr<Value> for Value {
    type Output = Value;

    fn bitor(self, _rhs: Value) -> Value {
        match self {
            Value::Int(i) => match _rhs {
                Value::Int(j) => Value::Int(i | j),
                _ => panic!("Cannot bitwise or {:?} with {:?}", _rhs, self),
            },
            _ => panic!("Cannot bitwise or {:?} with {:?}", _rhs, self),
        }
    }
}