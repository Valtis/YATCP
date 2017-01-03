mod x64;

use byte_generator::ByteCode; 
use self::x64::X64CodeGen;

pub struct CodeGenerator {
    bytecode: Vec<ByteCode>
}

impl CodeGenerator {
    pub fn new(bytecode: Vec<ByteCode>) -> CodeGenerator {
        CodeGenerator {
            bytecode: bytecode,
        }
    }   

    pub fn generate_code(self) -> Vec<u8> {
        // TODO - remove hard coded architecture
        let mut generator = X64CodeGen::new(self.bytecode.clone());

        generator.generate_code();
        generator.get_code()
    }
}

