pub mod compiler;
pub mod utils;
pub(crate) mod shared;
pub mod vm;

use std::env;

pub use crate::{compiler::compile, vm::program::Program};


fn main() {
    let filename = env::args().nth(1).unwrap_or("examples/simple.gu".to_string());

    let output = compiler::compile(&filename);

    if let Some(output) = output {
        println!("guccic successfully compiled {} to {}", filename, output);
        vm::program::Program::new().load(&output).run();
    }
    
}
