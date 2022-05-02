use crate::parser::{Type, BinOp};

pub(crate) fn resolve(lhs: &Type, op: &BinOp, rhs: &Type) -> Type {
  let returns_type = match lhs {
    Type::Int => match rhs {
        Type::Int => match op {
            BinOp::Add => todo!(),
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Gt => todo!(),
            BinOp::Lte => todo!(),
            BinOp::Gte => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            _ => Type::Error
        },
        Type::Float => match op {
            BinOp::Chain => todo!(),
            BinOp::Add => todo!(),
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Gt => todo!(),
            BinOp::Lte => todo!(),
            BinOp::Gte => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
            _ => Type::Error
        },
        Type::Bool => match op {
            BinOp::Chain => todo!(),
            BinOp::Add => todo!(),
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Eq => todo!(),
            BinOp::Ne => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Gt => todo!(),
            BinOp::Lte => todo!(),
            BinOp::Gte => todo!(),
            BinOp::And => todo!(),
            BinOp::Or => todo!(),
        },
        Type::Char => todo!(),
        Type::Array(_) => todo!(),
        Type::String => todo!(),
        Type::Custom(_) => todo!(),
        Type::Error => todo!(),
    },
    Type::Float => todo!(),
    Type::Bool => todo!(),
    Type::Char => todo!(),
    Type::Array(_) => todo!(),
    Type::String => todo!(),
    Type::Custom(_) => todo!(),
    Type::Error => todo!(),
  };
  if returns_type == Type::Error {
    // Handle error
  }
  returns_type
}