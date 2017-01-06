mod elf_obj;

use code_generator::Code;

use self::elf_obj::ElfGenerator;

#[derive(Clone, Copy)]
pub enum ObjectType {
    Elf(Architecture),
}

#[derive(Clone, Copy)]
pub enum Architecture {
    X64,
}

pub fn generate_object_file(obj_type: ObjectType, input_file: String, output_file: String, code: Code) {
    match obj_type {
        ObjectType::Elf(arch) => { let generator = ElfGenerator::new(arch, input_file, output_file, code); generator.generate(); },
    };
}

