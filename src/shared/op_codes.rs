use std::fmt::{Display, Error, Formatter};

use crate::compiler::parser::ast::{BinOp, UnOp};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpCode {
    BinOp(BinOp),
    UnOp(UnOp),
    Era,
    Param,
    BeginBlock,
    EndBlock,
    Goto,
    GotoF,
    GotoT,
    Alloc,
    GoSub,
    End,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            OpCode::BinOp(op) => write!(f, "{}", op),
            OpCode::UnOp(op) => write!(f, "{}", op),
            OpCode::Era => write!(f, "ERA"),
            OpCode::BeginBlock => write!(f, "BEGINBLOCK"),
            OpCode::EndBlock => write!(f, "ENDBLOCK"),
            OpCode::Goto => write!(f, "GOTO"),
            OpCode::GotoF => write!(f, "GOTOF"),
            OpCode::GotoT => write!(f, "GOTOT"),
            OpCode::Alloc => write!(f, "ALLOC"),
            OpCode::GoSub => write!(f, "GOSUB"),
            OpCode::Param => write!(f, "PARAM"),
            OpCode::End => write!(f, "END"),
        }
    }
}
