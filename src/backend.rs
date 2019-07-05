use crate::tac_generator::Function;
use crate::byte_generator::ByteGenerator;
use crate::code_generator::CodeGenerator;
use crate::obj_generator::{ObjectType, Architecture, generate_object_file};


pub fn run_backend(output: String, functions: Vec<Function>, print_bytecode: bool) {

    let mut byte_gen = ByteGenerator::new(functions.clone());
    byte_gen.generate_bytecode();

    if print_bytecode {
        print_bytecode_functions(&byte_gen);
    }
    let bytecode = byte_gen.bytecode_functions;

    let code_gen = CodeGenerator::new(bytecode);
    let asm_code = code_gen.generate_code();

    generate_object_file(ObjectType::Elf(Architecture::X64), output, asm_code);
}

fn print_bytecode_functions(byte_gen: &ByteGenerator) {
    println!();
    let mut counter = 1;
    for f in byte_gen.bytecode_functions.iter() {
        println!("Function '{}'\n", f.name);
        for c in &f.code {
            println!("    {}: {:?}", counter, c);
            counter += 1;
        }
    }
    println!();
}