pub mod x64;
mod stack_allocator;

use crate::byte_generator;
use crate::function_attributes::FunctionAttribute;

pub struct CodeGenerator {
    bytecode_functions: Vec<byte_generator::Function>
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String, // FIXME Make string table thread safe so that string ref can be shared
    pub start: usize,
    pub length: usize,
    pub attributes: Vec<FunctionAttribute>
}

impl Function {
    pub fn contains_attribute(&self, attribute: FunctionAttribute) -> bool {
        self.attributes.contains(&attribute)
    }
}

pub struct Code {
    pub code: Vec<u8>,
    pub functions: Vec<Function>,
    pub relocations: Vec<(String, usize)>,
}

impl CodeGenerator {
    pub fn new(bytecode_functions: Vec<byte_generator::Function>) -> CodeGenerator {
        CodeGenerator {
            bytecode_functions
        }
    }

    pub fn generate_code(self, print_bytecode: bool) -> Code {
        // TODO - remove hard coded architecture
        let bytecode_with_allocations = stack_allocator::allocate(self.bytecode_functions);

        if print_bytecode {
            for (f, _) in bytecode_with_allocations.iter() {
                f.print_bytecode();
            }
        }

        x64::generate_code(bytecode_with_allocations)
    }
}

