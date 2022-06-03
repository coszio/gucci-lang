pub mod compiler;
pub mod utils;
pub(crate) mod shared;
pub mod vm;

pub use crate::{compiler::compile, vm::program::Program};


fn main() {
    // let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
    //     .expect("Failed to read file");

    // let filename = "examples/simple.gu";
    // let src = fs::read_to_string(filename.clone())
    //     .expect("Failed to read file");

    let filename = "examples/simple.gu";
    let output = compiler::compile(filename);

    if let Some(output) = output {
        println!("guccic successfully compiled {} to {}", filename, output);
        vm::program::Program::new().load(&output).run();
    }
    
}
