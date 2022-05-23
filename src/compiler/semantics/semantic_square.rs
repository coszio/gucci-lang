use std::fmt::Display;

use crate::compiler::parser::ast::{UnOp, Type};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct TypeError {
    op: UnOp,
    oper: Type,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid operation {:?} {} ", self.op, self.oper)
    }
}

pub(crate) fn resolve(op: &UnOp, rhs: &Type) -> std::result::Result<Type, ()> {
    let return_type = match op {
        UnOp::Neg => match rhs {
            Type::Int => Type::Int,
            Type::Float => Type::Float,
            _ => Type::Error,
        },
        UnOp::Not => match rhs {
            Type::Bool => Type::Bool,
            _ => Type::Error,
        },
    };
    if return_type == Type::Error {
        return Err(())
    }
    Ok(return_type)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neg_int() {
        let op = UnOp::Neg;
        let oper = Type::Int;
        
        let result = resolve(&op, &oper);

        assert_eq!(result, Ok(Type::Int));
    }

    #[test]
    fn test_neg_bool() {
        let op = UnOp::Neg;
        let oper = Type::Bool;

        let result = resolve(&op, &oper);

        assert!(result.is_err());
    }
}

