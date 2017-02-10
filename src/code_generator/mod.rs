mod x64;
use byte_generator;

use self::x64::X64CodeGen;

use std::rc::Rc;

pub struct CodeGenerator {
    bytecode_functions: Vec<byte_generator::Function>
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Rc<String>,
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
            bytecode_functions: bytecode_functions,
        }
    }

    pub fn generate_code(self) -> Code {
        // TODO - remove hard coded architecture
        let mut generator = X64CodeGen::new(self.bytecode_functions);

        generator.generate_code();
        Code {
            code: generator.get_code(),
            functions: generator.get_functions(),
        }
    }
}

