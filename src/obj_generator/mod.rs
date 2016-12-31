mod elf_obj;

use self::elf_obj::ElfGenerator;

#[derive(Clone, Copy)]
pub enum ObjectType {
    Elf(Architecture),
}

#[derive(Clone, Copy)]
pub enum Architecture {
    X64,
}

pub fn generate_object_file(obj_type: ObjectType, output_file: String) {
    match obj_type {
        ObjectType::Elf(arch) => { let generator = ElfGenerator::new(arch, output_file); generator.generate(); },
    };
}

