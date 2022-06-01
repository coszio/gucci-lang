use std::fmt::Display;

use crate::compiler::parser::ast::{BinOp, Type};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TypeError {
    lhs: Type,
    rhs: Type,
    op: BinOp,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Invalid operation {} {:?} {} ",
            self.lhs, self.op, self.rhs
        )
    }
}

pub(crate) fn resolve(lhs: &Type, op: &BinOp, rhs: &Type) -> std::result::Result<Type, ()> {
    if let BinOp::Chain = op {
        return Ok(rhs.clone());
    }
    match op {
        BinOp::Assign => {
            if lhs == rhs {
                return Ok(lhs.clone());
            } else {
                return Err(());
            }
        }
        BinOp::Chain => return Ok(rhs.clone()),
        _ => (),
    };
    let return_type = match lhs {
        Type::Int => match rhs {
            Type::Int => match op {
                BinOp::Add => Type::Int,
                BinOp::Sub => Type::Int,
                BinOp::Mul => Type::Int,
                BinOp::Div => Type::Int,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Float => match op {
                BinOp::Add => Type::Float,
                BinOp::Sub => Type::Float,
                BinOp::Mul => Type::Float,
                BinOp::Div => Type::Float,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Bool => match op {
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Char => match op {
                BinOp::Add => Type::Char,
                BinOp::Sub => Type::Char,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Array(_) => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::String => match op {
                BinOp::Mul => Type::String,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            _ => Type::Error,
        },
        Type::Float => match rhs {
            Type::Int => match op {
                BinOp::Add => Type::Float,
                BinOp::Sub => Type::Float,
                BinOp::Mul => Type::Float,
                BinOp::Div => Type::Float,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Float => match op {
                BinOp::Add => Type::Float,
                BinOp::Sub => Type::Float,
                BinOp::Mul => Type::Float,
                BinOp::Div => Type::Float,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Bool => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Char => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Array(_) => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::String => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            _ => Type::Error,
        },
        Type::Bool => match rhs {
            Type::Int => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Float => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Bool => match op {
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Char => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Array(_) => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::String => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            _ => Type::Error,
        },
        Type::Char => match rhs {
            Type::Int => match op {
                BinOp::Add => Type::Char,
                BinOp::Sub => Type::Char,
                BinOp::Mul => Type::String,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Float => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Bool => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Char => match op {
                BinOp::Add => Type::String,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::Lt => Type::Bool,
                BinOp::Gt => Type::Bool,
                BinOp::Lte => Type::Bool,
                BinOp::Gte => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Array(_) => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::String => match op {
                BinOp::Add => Type::String,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            _ => Type::Error,
        },
        Type::Array(_) => match rhs {
            Type::Int => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Float => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Bool => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Char => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Array(_) => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::String => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            _ => Type::Error,
        },
        Type::String => match rhs {
            Type::Int => match op {
                BinOp::Mul => Type::String,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Float => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Bool => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Char => match op {
                BinOp::Add => Type::String,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::Array(_) => match op {
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            Type::String => match op {
                BinOp::Add => Type::String,
                BinOp::Eq => Type::Bool,
                BinOp::Ne => Type::Bool,
                BinOp::And => Type::Bool,
                BinOp::Or => Type::Bool,
                _ => Type::Error,
            },
            _ => Type::Error,
        },
        Type::Custom(_) => match rhs {
            _ => Type::Error,
        },
        Type::Error => match rhs {
            _ => Type::Error,
        },
    };
    if return_type == Type::Error {
        return Err(());
    }
    Ok(return_type)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_add_int() {
        let lhs = Type::Int;
        let op = BinOp::Add;
        let rhs = Type::Int;

        let result = resolve(&lhs, &op, &rhs);

        assert_eq!(result, Ok(Type::Int));
    }

    #[test]
    fn test_int_add_string() {
        let lhs = Type::Int;
        let op = BinOp::Add;
        let rhs = Type::String;

        let result = resolve(&lhs, &op, &rhs);

        assert!(result.is_err());
    }

    #[test]
    fn test_high_load() {
        let lhs = Type::Int;
        let op = BinOp::Add;
        let rhs = Type::Float;

        // test 10 million times, sequentially... should take less than a second
        (0..10_000_000).for_each(|_| {
            let result = resolve(&lhs, &op, &rhs);
            assert!(result.is_ok());
        });
    }
}
