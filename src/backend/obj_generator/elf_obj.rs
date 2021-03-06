#![allow(dead_code)]
use super::Architecture;
use super::super::code_generator::Code;

use byteorder::{ByteOrder, LittleEndian };

use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::Error;

use std::rc::Rc;
use crate::common::function_attributes::FunctionAttribute;

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
const SHN_UNDEF : u16 = 0;
const SHN_LORESERVE : u16 = 0xff00;
const SHN_LOPROC : u16 = 0xff00;
const SHN_HIPROC : u16 = 0xff1f;
const SHN_ABS : u16 = 0xfff1;
const SHN_COMMON : u16 = 0xfff2;
const SHN_HIRESERVE : u16 = 0xffff;

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
const SHF_INFO_LINKS: u32 = 0x40;
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


// Relocations, partial

const RELOCATION_AMD64_PLT32: u32 = 4;

// FIXME: Relies on the ordering of sections, should be dynamic
const SECTION_STRING_TABLE_INDEX: u16 = 1;
const STRING_TABLE_INDEX: u16 = 2;
const SYMBOL_TABLE_INDEX: u16 = 3;
const TEXT_SECTION_INDEX: u16 = 4;


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
    strings: Vec<Rc<String>>,
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


struct RelocationSection {
    entries: Vec<RelocationEntry>
}

struct RelocationEntry {
    offset: ElfSize,
    info: ElfSize,
    addend: ElfSize,
}



#[derive(Clone, Copy, Debug)]
enum ElfSize {
    Elf32(u32),
    Elf64(u64),
    Elf64Signed(i64),
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
            link: SHN_UNDEF as u32,
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

impl Section for RelocationSection {
    fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error> {
        for v in self.entries.iter() {
            v.write(file, architecture)?;
        }

        Ok(())
    }
}

impl RelocationEntry {
    fn write(&self, file: &mut File, architecture: Architecture) -> Result<(), Error> {
        write_elfsize(file, self.offset, architecture)?;
        write_elfsize(file, self.info, architecture)?;
        write_elfsize(file, self.addend, architecture)?;
        Ok(())
    }
}

pub struct ElfGenerator {
    architecture: Architecture,
    output_file: String,
    elf_header: ElfHeader,
    section_headers: Vec<SectionHeader>,
    sections: Vec<Box<dyn Section>>,
    string_pos: HashMap<String, u32>,
    section_string_pos: HashMap<String, u32>,
    symbol_table_entries: HashMap<String, usize>,
    code: Code,
}

impl ElfGenerator {
    pub fn new(
        architecture: Architecture,
        output_file: String,
        code: Code) -> ElfGenerator {
        ElfGenerator {
            architecture: architecture,
            output_file: output_file,
            elf_header: ElfHeader::new(architecture),
            section_headers: vec![],
            sections: vec![],
            string_pos: HashMap::new(),
            section_string_pos: HashMap::new(),
            symbol_table_entries: HashMap::new(),
            code
        }
    }

    pub fn generate(mut self) {
        let mut file = File::create(&self.output_file).expect("An IO error occured when creating object file");


        // FIXME: There are hardcoded references to section table index, based on this ordering here. Change to dynamic
        // In general, adding new table in the middle of the existing ones breaks things
        self.add_undefined_header();

        self.add_section_string_table();
        self.add_string_table();
        self.add_symbol_table();
        self.add_text_table();
        self.add_relocation_table();
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
            Rc::new("".to_string()),
            Rc::new(".shstrtab".to_string()),
            Rc::new(".strtab".to_string()),
            Rc::new(".symtab".to_string()),
            Rc::new(".data".to_string()),
            Rc::new(".text".to_string()),
            Rc::new(".rela.text".to_string()),
        ];

        // string table size, including null terminator bytes
        let mut string_size = 0;

        for s in strings.iter() {
            self.section_string_pos.insert((**s).clone(), string_size as u32);
            string_size += s.len() + 1; // +1 for null terminator
        }

        let section = Box::new(StringSection {
            strings: strings,
        });

        let mut header = SectionHeader::new(self.architecture);
        header.string_index = self.section_string_pos[".shstrtab"];
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
            Rc::new("".to_string()),
            Rc::new(self.output_file.clone()),
            ];

        // string table size, including null terminator bytes
        let mut string_size = strings.len() + strings.iter().fold(0, |sum, x| sum + x.len());

        for f in self.code.functions.iter() {
            strings.push(Rc::new(f.name.clone()));
            self.string_pos.insert(f.name.clone(), string_size as u32);
            string_size += f.name.len() + 1; // +1 for null terminatoe
        }

        let section = Box::new(StringSection {
            strings: strings,
        });

        let mut header = SectionHeader::new(self.architecture);
        header.string_index = self.section_string_pos[".strtab"];
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
            section_header_index: SHN_ABS,
            value: zero,
            size: zero,
        });

        for f in self.code.functions.iter() {
            let index = if f.contains_attribute(FunctionAttribute::External) {
                SHN_UNDEF
            } else {
                TEXT_SECTION_INDEX
            };

            self.symbol_table_entries.insert(f.name.clone(), entries.len());

            entries.push(SymbolEntry {
                string_index: self.string_pos[&f.name],
                info: (((STB_GLOBAL) << 4) + ((STT_FUNC) & 0xf)),
                other: 0,
                section_header_index: index,
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

        let entry_bytes = 24;

        let mut header = SectionHeader::new(self.architecture);
        header.string_index = self.section_string_pos[".symtab"];
        header.header_type = SHT_SYMTAB;
        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64((entries_len*entry_bytes) as u64),
        };

        header.entry_size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(entry_bytes as u64),
        };

        header.link = STRING_TABLE_INDEX as u32;
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

        header.string_index = self.section_string_pos[".text"];
        header.header_type = SHT_PROGBITS;
        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(len as u64),
        };

        header.address_alignment = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(1),
        };

        header.flags = match self.architecture {
            Architecture::X64 => ElfSize::Elf64((SHF_ALLOC | SHF_EXECINSTR) as u64),
        };

        self.add_section_header(header);
        self.add_section(section);
    }

    fn add_relocation_table(&mut self) {

        if self.code.relocations.is_empty() {
            return;
        }

        let mut header = SectionHeader::new(self.architecture);

        header.string_index = self.section_string_pos[".rela.text"];
        header.header_type = SHT_RELA;


        let entry_bytes = 24;
        let mut entries = vec![];

        for (name, offset) in self.code.relocations.iter() {

            let offset = match self.architecture {
                Architecture::X64 => ElfSize::Elf64(*offset as u64),
            };

            // symbol table index in high 32 bits, type in low 32 bits
            let info = match self.architecture {
                Architecture::X64 => ElfSize::Elf64(
                    (self.symbol_table_entries[name] as u64) << 32
                        | RELOCATION_AMD64_PLT32 as u64),
            };

            let addend = match self.architecture {
                Architecture::X64 => ElfSize::Elf64Signed(-4), // account for the field length in bytes we are patching
            };


            entries.push(
                RelocationEntry {
                    offset,
                    info,
                    addend,
                }
            );
        }


        header.size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64((entries.len()*entry_bytes) as u64),
        };

        header.entry_size = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(entry_bytes as u64),
        };

        header.address_alignment = match self.architecture {
            Architecture::X64 => ElfSize::Elf64(1),
        };

        header.flags = match self.architecture {
           Architecture::X64 => ElfSize::Elf64(SHF_INFO_LINKS as u64),
        };

        header.link = SYMBOL_TABLE_INDEX as u32;
        header.info = TEXT_SECTION_INDEX as u32; // SHF_INFO_LINKS flag must be set also


        let section = Box::new(
            RelocationSection {
            entries,
            }
        );

        self.add_section_header(header);
        self.add_section(section);
    }

    fn add_section_header(&mut self, header: SectionHeader) {
        self.section_headers.push(header);
        self.elf_header.add_section();
    }

    fn add_section(&mut self, section: Box<dyn Section>) {
        self.sections.push(section);
    }

    fn update_offsets(&mut self) {
        self.elf_header.update_offsets();
        self.update_section_string_table_offset();
        self.update_remaining_section_offsets();
    }


    fn update_section_string_table_offset(&mut self) {
        let string_header = &mut self.section_headers[SECTION_STRING_TABLE_INDEX as usize];
        string_header.offset = match self.architecture {
            Architecture::X64 => {
                if let ElfSize::Elf64(val) = self.elf_header.section_header_table_offset {

                    let section_headers_size =
                        (self.elf_header.section_header_entry_size*self.elf_header.section_header_entry_count) as u64;
                    ElfSize::Elf64(val + section_headers_size)
                } else {
                    panic!("Internal compiler error: Invalid variable size for architecture in ELF header");
                }
            }
        };
    }

    fn update_remaining_section_offsets(&mut self) {
        for i in 2..self.section_headers.len() {
            let prev_header = &self.section_headers[i-1].clone();
            let current_header = &mut self.section_headers[i];

            current_header.offset = match self.architecture {
                Architecture::X64 => {
                    if let (ElfSize::Elf64(val), ElfSize::Elf64(val2)) = (prev_header.offset, prev_header.size) {
                        ElfSize::Elf64(val + val2)
                    } else {
                        panic!("Internal compiler error: Invalid variable size for architecture in ELF header");
                    }
                }
            };

        }


    }

    fn write_section_headers(&mut self, file: &mut File) -> Result<(), Error> {
        for header in self.section_headers.iter() {
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

fn write_i64(file: &mut File, value: i64, architecture: Architecture) -> Result<(), Error> {
    let mut buffer = [0;8];
    match architecture {
        Architecture::X64 => LittleEndian::write_i64(&mut buffer, value),
    };

    file.write(&buffer)?;
    Ok(())
}

fn write_elfsize(file: &mut File, elfval: ElfSize, architecture: Architecture) -> Result<(), Error> {
    match elfval {
        ElfSize::Elf32(val) => write_u32(file, val, architecture)?,
        ElfSize::Elf64(val) => write_u64(file, val, architecture)?,
        ElfSize::Elf64Signed(val) => write_i64(file, val, architecture)?,
    };

    Ok(())
}