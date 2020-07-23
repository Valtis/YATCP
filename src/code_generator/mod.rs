pub mod x64; // FIXME: Should be internal implementation detail

use crate::byte_generator;
use crate::function_attributes::FunctionAttribute;

use self::x64::stack_allocator::allocate;

use took::Timer;

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

    pub fn generate_code(self, print_bytecode: bool, print_stack_map: bool, print_timings: bool) -> Code {

        // TODO - remove hard coded architecture
        let timer = Timer::new();
        let bytecode_with_allocations = allocate(self.bytecode_functions, print_stack_map);
        if print_timings {
            println!("Bytecode register allocation took {}", timer.took());
        }

        if print_bytecode {
            for (f, _) in bytecode_with_allocations.iter() {
                f.print_bytecode();
            }
        }

        let timer = Timer::new();
        let code = x64::generate_code(bytecode_with_allocations);
        if print_timings {
            println!("Machine code generation took {}", timer.took());
        }
        code
    }
}

