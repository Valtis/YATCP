#![allow(dead_code)]
use super::x64_register::X64Register;

use byteorder::{ByteOrder, LittleEndian };


#[derive(Debug, Clone, Copy)]
pub struct ModRM {
    pub addressing_mode: AddressingMode,
    pub reg_field: RegField,
    pub rm_field: RmField,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressingMode {
    IndirectAddressingNoDisplacement,
    IndirectAddressingOnlyDisplacement,
    IndirectAddressingOneByteDisplacement,
    IndirectAddressingFourByteDisplacement,
    DirectRegisterAddressing,
}

#[derive(Debug, Clone, Copy)]
pub enum RegField {
    OpcodeExtension(u8),
    Register(X64Register),
    Unused,
}

#[derive(Debug, Clone, Copy)]
pub enum RmField {
    Register(X64Register),
    Unused,
}

// scale, index, base, displacement
#[derive(Debug, Clone, Copy)]
pub struct Sib {
    pub scale: Option<Scale>,
    pub index: Option<X64Register>, // RSP-reg encoding used to denote this field is unused. RSP cannot be used as index reg as a result
    pub base: Option<X64Register>, // RBP is used to encode that base is unused, if no displacement variant is used. Must use displacement variants if this reg is used as a base
    pub displacement: Option<Displacement>
}

#[derive(Debug, Clone, Copy)]
pub enum Scale {
    One,
    Two,
    Four,
    Eight
}
#[derive(Debug, Clone, Copy)]
pub enum Displacement {
    FourByte(u32),
    OneByte(u8)
}

#[derive(Debug, Clone, Copy)]
pub enum Immediate {
    Byte(u8),
    ByteSigned(i8),
    FourByteSigned(i32),
    FourByte(u32),
    EightByteSigned(i64),
}

impl AddressingMode {
    pub fn get_addressing_encoding(&self) -> u8 {
        match self {
            AddressingMode::IndirectAddressingNoDisplacement => 0b0000_0100,
            AddressingMode::IndirectAddressingOnlyDisplacement => 0b0000_0101,
            AddressingMode::IndirectAddressingOneByteDisplacement => 0b0100_0000,
            AddressingMode::IndirectAddressingFourByteDisplacement => 0b1000_0000,
            AddressingMode::DirectRegisterAddressing => 0b1100_0000,
        }
    }
}

impl Scale {
    pub fn sib_bits(&self) -> u8 {
        match self {
            Scale::One => 0,
            Scale::Two => 0b0100_0000,
            Scale::Four => 0b1000_0000,
            Scale::Eight => 0b1100_0000,
        }
    }
}

impl From<u8> for Immediate {
    fn from(val: u8) -> Immediate {
        Immediate::Byte(val)
    }
}

impl From<i8> for Immediate {
    fn from(val: i8) -> Immediate {
        Immediate::ByteSigned(val)
    }
}

impl From<u32> for Immediate {
    fn from(val: u32) -> Immediate {
        Immediate::FourByte(val)
    }
}

impl From<i32> for Immediate {
    fn from(val: i32) -> Immediate {
        Immediate::FourByteSigned(val)
    }
}

impl From<i64> for Immediate {
    fn from(val: i64) -> Immediate {
        Immediate::EightByteSigned(val)
    }
}

impl Immediate {
    pub fn write_into_buffer(&self, write_buffer: &mut Vec<u8>) {
        match self {
            Immediate::Byte(val) => {
                write_buffer.push(*val);
            },
            Immediate::ByteSigned(val) => {
                write_buffer.push(*val as u8);
            },
            Immediate::FourByte(val) => {
                let mut buffer = [0; 4];
                LittleEndian::write_u32(&mut buffer, *val);
                for i in 0..buffer.len() {
                    write_buffer.push(buffer[i]);
                }
            },
            Immediate::FourByteSigned(val) => {
                let mut buffer = [0; 4];
                LittleEndian::write_i32(&mut buffer, *val);
                for i in 0..buffer.len() {
                    write_buffer.push(buffer[i]);
                }
            },
            Immediate::EightByteSigned(val ) => {
                let mut buffer = [0; 8];
                LittleEndian::write_i64(&mut buffer, *val);
                for i in 0..buffer.len() {
                    write_buffer.push(buffer[i]);
                }
            }
        }
    }
}
