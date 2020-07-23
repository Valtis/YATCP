use crate::tac_generator::tac_code::Function;
use crate::byte_generator::ByteGenerator;
use crate::code_generator::CodeGenerator;
use crate::obj_generator::{ObjectType, Architecture, generate_object_file};

use took::Timer;

pub fn run_backend(
    output: String,
    functions: Vec<Function>,
    print_bytecode: bool,
    print_bytecode_after_register_allocation: bool,
    print_stack_map: bool,
    print_timings: bool) {


    let timer = Timer::new();
    let mut byte_gen = ByteGenerator::new(functions.clone());
    byte_gen.generate_bytecode();

    if print_timings {
        println!("Bytecode generation took {}", timer.took());
    }

    if print_bytecode {
        byte_gen.print_bytecode();
    }

    let bytecode = byte_gen.bytecode_functions;

    let code_gen = CodeGenerator::new(bytecode);

    let asm_code = code_gen.generate_code(print_bytecode_after_register_allocation, print_stack_map, print_timings);

    let timer = Timer::new();
    generate_object_file(ObjectType::Elf(Architecture::X64), output, asm_code);

    if print_timings {
        println!("Object file creation took {}", timer.took());
    }
}

