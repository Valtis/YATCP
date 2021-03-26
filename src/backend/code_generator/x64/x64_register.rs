#![allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum X64Register {
    // 64 bit regs
    RAX,
    RCX,
    RDX,
    RBX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    // 32 bit regs
    EAX,
    ECX,
    EDX,
    EBX,
    ESP,
    EBP,
    ESI,
    EDI,
    R8d, // DWORDs
    R9d,
    // 8 bit registers
    AL,
    BL,
    CL,
    DL,
}



impl X64Register {
    pub fn encoding(&self) -> u8 {
        match self {
            X64Register::RAX | X64Register::R8 | X64Register::EAX | X64Register::R8d | X64Register::AL => 0x00,
            X64Register::RCX | X64Register::R9 | X64Register::ECX | X64Register::R9d | X64Register::CL => 0x01,
            X64Register::RDX | X64Register::R10 | X64Register::EDX | X64Register::DL => 0x02,
            X64Register::RBX | X64Register::R11 | X64Register::EBX | X64Register::BL => 0x03,
            X64Register::RSP | X64Register::ESP |  X64Register::R12 => 0x04,
            X64Register::RBP | X64Register::EBP | X64Register::R13 => 0x05,
            X64Register::RSI | X64Register::ESI | X64Register::R14 => 0x06,
            X64Register::RDI | X64Register::EDI | X64Register::R15 => 0x07,
        }
    }

    pub fn is_extended_reg(&self) -> bool {
        match self {
            X64Register::R8d |
            X64Register::R9d |
            X64Register::R8 |
            X64Register::R9 |
            X64Register::R10 |
            X64Register::R11 |
            X64Register::R12 |
            X64Register::R13 |
            X64Register::R14 |
            X64Register::R15 => true,
            _ => false
        }
    }


    // very barebones, implemented for idiv for now only
    pub fn get_alias_for_size(&self, size: u8) -> X64Register {
        match self {

            X64Register::AL |X64Register::EAX | X64Register::RAX => {
                match size {
                    1 => X64Register::AL,
                    4 => X64Register::EAX,
                    8 => X64Register::RAX,
                    _ => ice!("Invalid size {}", size),
                }
            },
            X64Register::BL | X64Register::EBX | X64Register::RBX => {
                match size {
                    1 => X64Register::BL,
                    4 => X64Register::EBX,
                    8 => X64Register::RBX,
                    _ => ice!("Invalid size {}", size),
                }
            },
            X64Register::CL | X64Register::ECX | X64Register::RCX => {
                match size {
                    1 => X64Register::CL,
                    4 => X64Register::ECX,
                    8 => X64Register::RCX,
                    _ => ice!("Invalid size {}", size),
                }
            },
            X64Register::DL | X64Register::EDX | X64Register::RDX => {
                match size {
                    1 => X64Register::DL,
                    4 => X64Register::EDX,
                    8 => X64Register::RDX,
                    _ => ice!("Invalid size {}", size),
                }
            }
            _ => todo!("Not implemented for register {:?}", self),
        }
    }

    pub fn is_64_bit_register(&self) -> bool {
        self.size() == 8
    }

    pub fn size(&self) -> u8 {
        match self {
            X64Register::RAX |
            X64Register::RCX |
            X64Register::RDX |
            X64Register::RBX |
            X64Register::RSP |
            X64Register::RBP |
            X64Register::RSI |
            X64Register::RDI |
            X64Register::R8 |
            X64Register::R9 |
            X64Register::R10 |
            X64Register::R11 |
            X64Register::R12 |
            X64Register::R13 |
            X64Register::R14 |
            X64Register::R15 => 8,

            X64Register::EAX |
            X64Register::ECX |
            X64Register::EDX |
            X64Register::EBX |
            X64Register::ESP |
            X64Register::EBP |
            X64Register::ESI |
            X64Register::EDI |
            X64Register::R8d |
            X64Register::R9d => 4,

            X64Register::AL |
            X64Register::BL |
            X64Register::CL |
            X64Register::DL => 1,
        }
    }
}
