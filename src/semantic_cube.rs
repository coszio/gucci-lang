use crate::parser::{Type, BinOp};

pub(crate) fn resolve(lhs: &Type, op: &BinOp, rhs: &Type) -> Type {
  let returns_type = match lhs {
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
            _ => Type::Error
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
            _ => Type::Error
        },
        Type::Bool => match op {
            BinOp::Eq => Type::Bool,
            BinOp::Ne => Type::Bool,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
        },
        Type::Char => match op {
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
            _ => Type::Error
        },
        Type::Array(_) => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
        },
        Type::String => match op {
            BinOp::Mul => Type::String,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
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
            _ => Type::Error
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
            _ => Type::Error
      },
      Type::Bool => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Char => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Array(_) => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::String => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      _ => Type::Error,
    },
    Type::Bool => match rhs {
      Type::Int => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Float => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Bool => match op {
            BinOp::Eq => Type::Bool,
            BinOp::Ne => Type::Bool,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Char => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Array(_) => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::String => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
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
            _ => Type::Error
      },
      Type::Float => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Bool => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
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
            _ => Type::Error
      },
      Type::Array(_) => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::String => match op {
            BinOp::Add => Type::String,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      _ => Type::Error,
    },
    Type::Array(_) => match rhs {
      Type::Int => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Float => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Bool => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Char => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      }
      Type::Array(_) => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::String => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      _ => Type::Error
    },
    Type::String => match rhs {
      Type::Int => match op {
            BinOp::Mul => Type::String,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Float => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Bool => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Char => match op {
            BinOp::Add => Type::String,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      },
      Type::Array(_) => match op {
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
      }
      Type::String => match op {
            BinOp::Add => Type::String,
            BinOp::Eq => Type::Bool,
            BinOp::Ne => Type::Bool,
            BinOp::And => Type::Bool,
            BinOp::Or => Type::Bool,
            _ => Type::Error
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
  if returns_type == Type::Error {
    // Handle error
  }
  returns_type
}