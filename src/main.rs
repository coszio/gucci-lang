pub mod compiler;
pub mod utils;
pub(crate) mod shared;
pub mod vm;

use std::env;

pub use crate::{compiler::compile, vm::program::Program};


fn main() {
    let action = env::args().nth(1).unwrap_or("compile_run".to_string());
    let filename = env::args().nth(2).expect("no filename provided!");

    match action.as_str() {
        "c" | "compile" => {
            let output = compiler::compile(&filename);
            if let Some(output) = output {
                println!("Successfully compiled {} to {}", filename, output);
            }
        },
        "r" | "run" => {
            vm::program::Program::new().load(&filename).run();
        }
        "cr" | "compile_run" => {
            let output = compiler::compile(&filename);

            if let Some(output) = output {
                println!("Successfully compiled {} to {}", filename, output);
                vm::program::Program::new().load(&output).run();
            }
        }
        _ => panic!("Unknown action: {}", action),
    }
    
    
}
