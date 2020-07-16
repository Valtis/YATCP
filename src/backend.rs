use crate::tac_generator::Function;
use crate::byte_generator::ByteGenerator;
use crate::code_generator::CodeGenerator;
use crate::obj_generator::{ObjectType, Architecture, generate_object_file};


pub fn run_backend(
    output: String,
    functions: Vec<Function>,
    print_bytecode: bool,
    print_bytecode_after_register_allocation: bool,
    print_stack_map: bool) {

    let mut byte_gen = ByteGenerator::new(functions.clone());
    byte_gen.generate_bytecode();

    if print_bytecode {
        byte_gen.print_bytecode();
    }

    let bytecode = byte_gen.bytecode_functions;

    let code_gen = CodeGenerator::new(bytecode);
    let asm_code = code_gen.generate_code(print_bytecode_after_register_allocation, print_stack_map);

    generate_object_file(ObjectType::Elf(Architecture::X64), output, asm_code);
}

