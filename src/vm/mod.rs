
mod memory;
mod reader;
mod program;
mod value;

use crate::shared::quad::Quad;
use crate::shared::op_code::{OpCode, self};

// load program

// create mega match

// fn execute(quad: &Quad) -> Result<()> {
//     match quad.op_code {
//         OpCode::Assign => {
//             memory::get_val(quad.arg1);
//             memory::set_val(quad.arg2, memory::get_val(quad.arg1))
//         }
//         OpCode::Add => {
//             memory::get_val(quad.arg1);
//             memory::get_val(quad.arg2);
//             memory::set_val(quad.result, memory::get_val(quad.arg1) + memory::get_val(quad.arg2))
//         }
//     }
// }

