pub mod x64;
mod stack_allocator;

use crate::byte_generator;
use std::rc::Rc;

pub struct CodeGenerator {
    bytecode_functions: Vec<byte_generator::Function>
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String, // FIXME Make string table thread safe so that string ref can be shared
    pub start: usize,
    pub length: usize,
}

pub struct Code {
    pub code: Vec<u8>,
    pub functions: Vec<Function>,
}

impl CodeGenerator {
    pub fn new(bytecode_functions: Vec<byte_generator::Function>) -> CodeGenerator {
        CodeGenerator {
            bytecode_functions
        }
    }

    pub fn generate_code(self) -> Code {
        // TODO - remove hard coded architecture
        let bytecode_with_allocations = stack_allocator::allocate(self.bytecode_functions);

        let (functions, code) = x64::generate_code(bytecode_with_allocations);

        Code {
            code,
            functions
        }
    }
}

