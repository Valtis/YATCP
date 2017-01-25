#![allow(dead_code)]
use obj_generator::Architecture;

use code_generator::Code;

use byteorder::{ByteOrder, LittleEndian };

use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::Error;

// elf file magic number
const MAGIC_NUMBER : &'static str  ="\u{007f}ELF";

// 32 and 64 bit architecture definitions
const ELFCLASSNONE : u8 = 0; 
const ELFCLASS32 : u8 = 1;
const ELFCLASS64 : u8 = 2;

// lsb/msb architecture definitions
const ELFDATANONE : u8 = 0;
const ELFDATA2LSB : u8 = 1;
const ELFDATA2MSB : u8 = 2;

// elf file version
const EV_NONE : u8 = 0;
const EV_CURRENT : u8 = 1;

// os api definitions.
const ELFOSABI_NONE : u8 = 0;
const ELFOSABI_HPUX : u8 = 1;
const ELFOSABI_NETBSD : u8 = 2;
const ELFOSABI_LINUX : u8 = 3;
const ELFOSABI_SOLARIS : u8 = 6;
const ELFOSABI_AIX : u8 = 7;
const ELFOSABI_IRIX : u8 = 8;
const ELFOSABI_FREEBSD : u8 = 9;
const ELFOSABI_TRU64 : u8 = 10;
const ELFOSABI_MODESTO : u8 = 11;
const ELFOSABI_OPENBSD : u8 = 12;
const ELFOSABI_OPENVMS : u8 = 13;
const ELFOSABI_NSK : u8 = 14;

// api version
const ELFOSABIVERSION_NONE : u8 = 0;

// padding byte used in elf header
const PADDING_BYTE : u8 = 0;

// object file type
const ET_NONE : u16 = 0;
const ET_REL : u16 = 1; 
const ET_EXEC : u16 = 2;
const ET_DYN : u16 = 3; 
const ET_CORE : u16 = 4;
const ET_LOOS : u16 = 0xfe00;
const ET_HIOS : u16 = 0xfeff;
const ET_LOPROC : u16 = 0xff00;
const ET_HIPROC : u16 = 0xffff;  


// cpu architecture
// rest of the values omitted, as there are quite a few of them
const EM_X86_64 : u16 = 0x3E;

// header size in bytes for 32 and 64 bit systems
const ELF_HEADER_SIZE_32 : u16 = 52;
const ELF_HEADER_SIZE_64 : u16 = 64;


// section header size in bytes for 32 and 64 bit systems
const ELF_SECTION_HEADER_SIZE_32 : u16 = 40;
const ELF_SECTION_HEADER_SIZE_64 : u16 = 64;

// reserved section indices
const SHN_UNDEF : u32 = 0;
const SHN_LORESERVE : u32 = 0xff00;
const SHN_LOPROC : u32 = 0xff00;
const SHN_HIPROC : u32 = 0xff1f;
const SHN_ABS : u32 = 0xfff1;
const SHN_COMMON : u32 = 0xfff2;
const SHN_HIRESERVE : u32 = 0xffff;

// section header types
const SHT_NULL : u32 = 0;
const SHT_PROGBITS : u32 = 1;
const SHT_SYMTAB : u32 = 2;
const SHT_STRTAB : u32 = 3;
const SHT_RELA : u32 = 4;
const SHT_HASH : u32 = 5;
const SHT_DYNAMIC : u32 = 6;
const SHT_NOTE : u32 = 7;
const SHT_NOBITS : u32 = 8;
const SHT_REL : u32 = 9;
const SHT_SHLIB : u32 = 10;
const SHT_DYNSYM : u32 = 11;
const SHT_LOPROC : u32 = 0x70000000;
const SHT_HIPROC : u32 = 0x7fffffff;
const SHT_LOUSER : u32 = 0x80000000;
const SHT_HIUSER : u32 = 0xffffffff;

// section header flags
const SHF_WRITE : u32 = 0x1;
const SHF_ALLOC : u32 = 0x2;
const SHF_EXECINSTR : u32 = 0x4;
const SHF_MASKPROC : u32 = 0xf0000000;


// symbol bindings
const STB_LOCAL : u8 = 0;
const STB_GLOBAL : u8 = 1;
const STB_WEAK : u8 = 2;
const STB_LOPROC : u8 = 13;
const STB_HIPROC: u8 = 15;  

// symbol types
const STT_NOTYPE : u8 = 0;
const STT_OBJECT : u8 = 1;
const STT_FUNC : u8 = 2;
const STT_SECTION : u8 = 3;
const STT_FILE : u8 = 4;
const STT_LOPROC : u8 = 13;
const STT_HIPROC : u8 = 15;

pub struct ElfHeader {
    magic_number: &'static str,
    format: u8,
    endianess: u8,
    ident_version : u8,
    os_abi : u8,
    os_abi_version : u8,
    file_type: u16,
    machine: u16,
    version: u32,
    entry_point: ElfSize,
    program_header_table_offset: ElfSize,
    section_header_table_offset: ElfSize,
    flags: u32,
    header_size: u16,
    program_header_entry_size: u16,
    program_header_entry_count: u16,
    section_header_entry_size: u16,
    section_header_entry_count: u16,
    section_header_string_table_index: u16,
}


struct ProgramHeader {
    // todo - implement
}

#[derive(Clone)]
struct SectionHeader {
    string_index: u32,
    header_type: u32,
    flags: ElfSize,
    virtual_address: ElfSize,
    offset: ElfSize,
    size: ElfSize,
    link: u32,
    info: u32,
    address_alignment: ElfSize,
    entry_size: ElfSize,
}

trait Section {
    fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error>;
}

struct StringSection {
    strings: Vec<String>,
}

struct TextSection {
    code: Vec<u8>,
}

struct SymbolSection {
    entries: Vec<SymbolEntry>,
}

struct SymbolEntry {
    string_index: u32,
    info: u8,
    other: u8,
    section_header_index : u16,
    value: ElfSize,
    size: ElfSize, 
}

#[derive(Clone, Copy, Debug)]
enum ElfSize {
    Elf32(u32),
    Elf64(u64),
}

impl ElfHeader {
    pub fn new(architecture: Architecture) -> ElfHeader {
        let (format, endianess, machine, elf_0, header_size, section_header_size) = match architecture {
            Architecture::X64 => (ELFCLASS64, ELFDATA2LSB, EM_X86_64, ElfSize::Elf64(0), ELF_HEADER_SIZE_64, ELF_SECTION_HEADER_SIZE_64),
        };

        ElfHeader {
            magic_number: MAGIC_NUMBER,
            format: format, 
            endianess: endianess,
            ident_version: EV_CURRENT,
            os_abi: ELFOSABI_NONE,
            os_abi_version: ELFOSABIVERSION_NONE,
            file_type: ET_REL,
            machine: machine,
            version: EV_CURRENT as u32,
            entry_point: elf_0,
            program_header_table_offset: elf_0,
            section_header_table_offset: elf_0,
            flags: 0,
            header_size: header_size,
            program_header_entry_size: 0,
            program_header_entry_count: 0,
            section_header_entry_size: section_header_size,
            section_header_entry_count: 0,
            section_header_string_table_index: 0,

        }
    }

    fn add_section(&mut self) {
        self.section_header_entry_count += 1;
    }

    fn update_offsets(&mut self) {

        if self.program_header_entry_count != 0 {
            self.program_header_table_offset = if self.format == ELFCLASS64 {
                ElfSize::Elf64(self.header_size as u64) 
            } else {
                ElfSize::Elf32(self.header_size as u32)
            };
        }

        self.section_header_table_offset = if self.format == ELFCLASS64 {
            let mut offset = self.header_size as u64;
            offset += (self.program_header_entry_size*self.program_header_entry_count) as u64;
            ElfSize::Elf64(offset)
        } else {
            let mut offset = self.header_size as u32;
            offset += (self.program_header_entry_size*self.program_header_entry_count) as u32;
            ElfSize::Elf32(offset)
        };
    }

    fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error> {
        file.write(self.magic_number.as_bytes())?;
        file.write(&[
            self.format, 
            self.endianess,
            self.ident_version,
            self.os_abi,
            self.os_abi_version,
            PADDING_BYTE,
            PADDING_BYTE,
            PADDING_BYTE,
            PADDING_BYTE,
            PADDING_BYTE,
            PADDING_BYTE,
            PADDING_BYTE
            ])?;

        write_u16(file, self.file_type,architecture)?;
        write_u16(file, self.machine, architecture)?;
        write_u32(file, self.version, architecture)?;

        write_elfsize(file, self.entry_point, architecture)?;
        write_elfsize(file, self.program_header_table_offset, architecture)?;
        write_elfsize(file, self.section_header_table_offset, architecture)?;
        write_u32(file, self.flags, architecture)?;
        write_u16(file, self.header_size, architecture)?;
        write_u16(file, self.program_header_entry_size, architecture)?;
        write_u16(file, self.program_header_entry_count, architecture)?;
        write_u16(file, self.section_header_entry_size, architecture)?;
        write_u16(file, self.section_header_entry_count, architecture)?;
        write_u16(file, self.section_header_string_table_index, architecture)?;

        Ok(())
    }
}

impl SectionHeader {
    fn new(architecture: Architecture) -> SectionHeader {
        let zero = match architecture {
            Architecture::X64 => ElfSize::Elf64(0),
        };

        SectionHeader {
            string_index: 0,
            header_type: SHT_NULL,
            flags: zero,
            virtual_address: zero,
            offset: zero,
            size: zero,
            link: SHN_UNDEF,
            info: 0,
            address_alignment: zero,
            entry_size: zero,
        }
    }

    fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error> {
        write_u32(file, self.string_index, architecture)?;
        write_u32(file, self.header_type, architecture)?;
        write_elfsize(file, self.flags, architecture)?;
        write_elfsize(file, self.virtual_address, architecture)?;
        write_elfsize(file, self.offset, architecture)?;
        write_elfsize(file, self.size, architecture)?;
        write_u32(file, self.link, architecture)?; 
        write_u32(file, self.info, architecture)?;  
        write_elfsize(file, self.address_alignment, architecture)?;
        write_elfsize(file, self.entry_size, architecture)?;             
        Ok(())
    }
} 

impl Section for StringSection {
    
    fn write(&self, file: &mut File, _architecture: Architecture) -> Result<(), Error> {
        for str in self.strings.iter() {
            file.write(str.as_bytes())?;
            file.write("\0".as_bytes())?;
        }
        Ok(())
    }    
}

impl Section for TextSection {

    fn write(&self, file: &mut File, _architecture: Architecture) -> Result<(), Error> {
        file.write(self.code.as_slice())?;
        Ok(())
    }     
}

impl Section for SymbolSection {
     fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error> {
        for v in self.entries.iter() {
            v.write(file, architecture)?;
        }

        Ok(())
    }  
}

impl SymbolEntry {
    fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error> {
        write_u32(file, self.string_index, architecture)?;
        file.write(&[self.info, self.other])?;
        write_u16(file, self.section_header_index, architecture)?;
        write_elfsize(file, self.value, architecture)?;
        write_elfsize(file, self.size, architecture)?;
        Ok(())
    }
}

pub struct ElfGenerator {
    architecture: Architecture,
    input_file: String,
    output_file: String,
    elf_header: ElfHeader,
    section_headers: Vec<SectionHeader>,
    sections: Vec<Box<Section>>,
    string_pos: HashMap<String, u32>,  
    code: Code, 
}

impl ElfGenerator {
    pub fn new(
        architecture: Architecture, 
        input_file: String, 
        output_file: String, 
        code: Code) -> ElfGenerator {
        ElfGenerator {
            architecture: architecture,
            input_file: input_file,
            output_file: output_file,
            elf_header: ElfHeader::new(architecture),
            section_headers: vec![],
            sections: vec![],
            string_pos: HashMap::new(),
            code: code
        }
    }

    pub fn generate(mut self) {
        let mut file = File::create(&self.output_file).expect("An IO error occured when creating object file");
       
        self.add_undefined_header();

        self.add_section_string_table();
        self.add_string_table();
        self.add_symbol_table();
        self.add_text_table();

        self.update_offsets();


        // TODO: Better error handling (retain/show the IO error)
        self.elf_header.write(&mut file, self.architecture).expect("An IO error occured during ELF header write");
        self.write_section_headers(&mut file).expect("An IO error occured during ELF section header write");
        self.write_sections(&mut file).expect("An IO error occured during ELF section write");        
    }

    fn add_undefined_header(&mut self) {
        let empty_header = SectionHeader::new(self.architecture);
        self.add_section_header(empty_header);

    }

    fn add_section_string_table(&mut self) {

        let strings = vec![
            "".to_string(), 
            ".shstrtab".to_string(), 
            ".strtab".to_string(), 
            ".symtab".to_string(), 
            ".data".to_string(), 
            ".text".to_string()];

        // string.len() -> null terminator count
        let string_size = strings.len() + strings.iter().fold(0, |sum, x| sum + x.len());
        println!("Size: {}", string_size); 

        let section = Box::new(StringSection {
            strings: strings,                     
        });

        let mut header = SectionHeader::new(self.architecture);
        header.string_index = 1;
        header.header_type = SHT_STRTAB;
        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(string_size as u64),
        };
       
        header.address_alignment = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(1),
        };

        self.elf_header.section_header_string_table_index = self.elf_header.section_header_entry_count;

        self.add_section_header(header);
        self.add_section(section);
    }

    fn add_string_table(&mut self) {
        // hard coding for now for testing
        let mut strings = vec![
            "".to_string(), 
            self.input_file.clone(), 
            ];

        // string.len() -> null terminator count
        let mut string_size = strings.len() + strings.iter().fold(0, |sum, x| sum + x.len());
        
        for f in self.code.functions.iter() {
            strings.push(f.name.clone());
            self.string_pos.insert(f.name.clone(), string_size as u32);
            string_size += f.name.len() + 1; // +1 for null terminatoe
        } 

      
        println!("Size: {}", string_size); 

        let section = Box::new(StringSection {
            strings: strings,                     
        });

        let mut header = SectionHeader::new(self.architecture);
        header.string_index = 11; // HARDCODED ASSUMPTION IN REGARDS OF SECTION STRING TABLE!
        header.header_type = SHT_STRTAB;
        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(string_size as u64),
        };
       
        header.address_alignment = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(1),
        };

        self.add_section_header(header);
        self.add_section(section);
    }

    fn add_symbol_table(&mut self) {
        let mut entries = vec![];
        let zero = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(0),
        };


        entries.push(SymbolEntry {
            string_index: 0,
            info: 0,
            other: 0,
            section_header_index: 0,
            value: zero,
            size: zero,
        });

        entries.push(SymbolEntry {
            string_index: 1,
            info: (((STB_LOCAL) << 4) + ((STT_FILE) & 0xf)),
            other: 0,
            section_header_index: SHN_ABS as u16,
            value: zero,
            size: zero,
        });

        for f in self.code.functions.iter() {
            println!("Function '{}' start: {} length: {}", f.name, f.start, f.length);
            entries.push(SymbolEntry {
                string_index: self.string_pos[&f.name],
                info: (((STB_GLOBAL) << 4) + ((STT_FUNC) & 0xf)),
                other: 0,
                section_header_index: 4, // text section index
                value: match self.architecture {
                    Architecture::X64 => ElfSize::Elf64(f.start as u64),
                },
                size: match self.architecture {
                    Architecture::X64 => ElfSize::Elf64(f.length as u64),
                }
            });
        }

        let entries_len = entries.len();

        let section = Box::new(SymbolSection {
            entries: entries,                     
        });

        let mut header = SectionHeader::new(self.architecture);
        header.string_index = 19; // HARDCODED ASSUMPTION IN REGARDS OF SECTION STRING TABLE!
        header.header_type = SHT_SYMTAB;
        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64((entries_len*24) as u64),
        };
        
        header.entry_size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(24),
        };

        header.link = 2; // dirty hardcoded value for the string table
        header.info = 2; // one larger than the index of the last local binding

        header.address_alignment = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(8),
        };

        self.add_section_header(header);
        self.add_section(section);
    }

    fn add_text_table(&mut self) {
      
        let len = self.code.code.len();

        let section = Box::new(TextSection {
            code: self.code.code.clone(),                     
        });

        let mut header = SectionHeader::new(self.architecture);

        header.string_index = 33; // HARDCODED ASSUMPTION IN REGARDS OF SECTION STRING TABLE!
        header.header_type = SHT_PROGBITS;
        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(len as u64),
        };

        println!("Text table header size: {:?}", header.size);
       
        header.address_alignment = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(1),
        };

        header.flags = match self.architecture {
            Architecture::X64 => ElfSize::Elf64((SHF_ALLOC | SHF_EXECINSTR) as u64),
        };

        self.add_section_header(header);
        self.add_section(section);
    }

    fn add_section_header(&mut self, header: SectionHeader) {
        self.section_headers.push(header);
        self.elf_header.add_section();
    }

    fn add_section(&mut self, section: Box<Section>) {
        self.sections.push(section);
    }

    fn update_offsets(&mut self) {
        self.elf_header.update_offsets();
        self.update_section_string_table_offset();
        self.update_remaining_section_offsets();
    }

    fn update_section_string_table_offset(&mut self) {
        // HARDCODED ASSUMPTION! Assumes that section string table header is the second entity in section_header vector
        let string_header = &mut self.section_headers[1];
        string_header.offset = match self.architecture {
            Architecture::X64 => {
                if let ElfSize::Elf64(val) = self.elf_header.section_header_table_offset {

                    let section_headers_size = 
                        (self.elf_header.section_header_entry_size*self.elf_header.section_header_entry_count) as u64;
                    println!("section string offset: {}", val + section_headers_size);
                    ElfSize::Elf64(val + section_headers_size) 
                } else {
                    panic!("Internal compiler error: Invalid variable size for architecture in ELF header");
                }
            } 
        };
    }

    fn update_remaining_section_offsets(&mut self) {
        for i in 2..self.section_headers.len() {
            println!("Current header index: {} prev header index {}", i, i-1);
            let prev_header = &self.section_headers[i-1].clone();
            let current_header = &mut self.section_headers[i];
            println!("Prev header: {:?} {:?}", prev_header.offset, prev_header.size);

            current_header.offset = match self.architecture {
                Architecture::X64 => {
                    if let (ElfSize::Elf64(val), ElfSize::Elf64(val2)) = (prev_header.offset, prev_header.size) {
                        ElfSize::Elf64(val + val2) 
                    } else {
                        panic!("Internal compiler error: Invalid variable size for architecture in ELF header");
                    }
                } 
            };

            println!("Current header: {:?} {:?}", current_header.offset, current_header.size);
        }
           

    }

    fn write_section_headers(&mut self, file: &mut File) -> Result<(), Error> {
        for header in self.section_headers.iter() {
            println!("Writing section header with offset {:?}", header.offset);
            header.write(file, self.architecture)?;
        }

        Ok(())
    }

    fn write_sections(&mut self, file: &mut File) -> Result<(), Error> {
        for section in self.sections.iter() {
            section.write(file, self.architecture)?;
        }
        Ok(())
    }
}


fn write_u16(file: &mut File, value: u16, architecture: Architecture) -> Result<(), Error> {
    let mut buffer = [0;2];
    match architecture {
        Architecture::X64 => LittleEndian::write_u16(&mut buffer, value),
    };
   
    file.write(&buffer)?;            
    Ok(())
}

fn write_u32(file: &mut File, value: u32, architecture: Architecture) -> Result<(), Error> {
    let mut buffer = [0;4];
    match architecture {
        Architecture::X64 => LittleEndian::write_u32(&mut buffer, value),
    };

    file.write(&buffer)?;            
    Ok(())
}


fn write_u64(file: &mut File, value: u64, architecture: Architecture) -> Result<(), Error> {
    let mut buffer = [0;8];
    match architecture {
        Architecture::X64 => LittleEndian::write_u64(&mut buffer, value),
    };

    file.write(&buffer)?;            
    Ok(())
}

fn write_elfsize(file: &mut File, elfval: ElfSize, architecture: Architecture) -> Result<(), Error> {
    match elfval {
        ElfSize::Elf32(val) => write_u32(file, val, architecture)?,
        ElfSize::Elf64(val) => write_u64(file, val, architecture)?,
    };

    Ok(())
}