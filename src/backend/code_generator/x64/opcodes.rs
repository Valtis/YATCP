#![allow(dead_code)]

pub const MOV_IMMEDIATE_32_BIT_TO_REG_BASE: u8 = 0xB8; // register encoding will be binary OR'ed into opcode
pub const MOV_IMMEDIATE_32_BIT_TO_RM: u8 = 0xC7;
pub const MOV_REG_TO_RM_32_BIT: u8 = 0x89;
pub const MOV_RM_TO_REG_32_BIT: u8 = 0x8B;

pub const MOV_IMMEDIATE_8_BIT_TO_REG_BASE: u8 = 0xB0; // register encoding will be binary OR'ed into opcode
pub const MOV_IMMEDIATE_8_BIT_TO_RM: u8 = 0xC6;
pub const MOV_REG_TO_RM_8_BIT: u8 = 0x88;
pub const MOV_RM_TO_REG_8_BIT: u8 = 0x8A;

pub const MOV_SIGN_EXTEND_8_BIT_TO_32_BIT: u16 = 0x0FBE;
pub const MOV_ZERO_EXTEND_8_BIT_TO_32_BIT: u16 = 0x0FB6;

pub const LEA_ADDR_TO_REG_64_BIT: u8 = 0x8D;

pub const ADD_IMMEDIATE_32_BIT_TO_RM: u8 = 0x81;
pub const ADD_REG_TO_RM_32_BIT: u8 = 0x01;
pub const ADD_RM_TO_REG_32_BIT: u8 = 0x03;

pub const ADD_IMMEDIATE_8_BIT_TO_RM: u8 = 0x83;
pub const ADD_REG_TO_RM_8_BIT: u8 = 0x00;
pub const ADD_RM_TO_REG_8_BIT: u8 = 0x02;

pub const ADD_OPCODE_EXT: u8 = 0;


pub const SUB_IMMEDIATE_32_BIT_FROM_RM: u8 = 0x81;
pub const SUB_RM_32_FROM_REG: u8 = 0x2B;
pub const SUB_REG_FROM_RM_32_BIT: u8 = 0x29;

pub const SUB_IMMEDIATE_8_BIT_FROM_RM: u8 = 0x83;
pub const SUB_RM_8_FROM_REG: u8 = 0x2A;
pub const SUB_REG_FROM_RM_8_BIT: u8 = 0x28;

pub const SUB_OPCODE_EXT: u8 = 0x05;

pub const SIGNED_MUL_RM_32_BIT_WITH_8_BIT_IMMEDIATE_: u8 = 0x6B;
pub const SIGNED_MUL_RM_32_BIT_WITH_32_BIT_IMMEDIATE: u8 = 0x69;
pub const SIGNED_MUL_REG_RM_32_BIT: u16 = 0x0FAF;

pub const SIGNED_DIV_RM_8_BIT: u8 = 0xF6;
pub const SIGNED_DIV_RM_32_BIT: u8 = 0xF7;
pub const DIV_OPCODE_EXT: u8 = 0x07;

pub const AND_RM_8_BIT_WITH_REG: u8 = 0x22;
pub const AND_RM_32_BIT_WITH_REG: u8 = 0x23;
pub const AND_REG_WITH_RM_32_BIT: u8 = 0x21;
pub const AND_REG_WITH_RM_8_BIT: u8 = 0x20;
pub const AND_IMMEDIATE_8_BIT_TO_RM_8_BIT: u8 = 0x80;
pub const AND_IMMEDIATE_8_BIT_SIGN_EXTENDED_TO_RM_32_BIT: u8 = 0x83;
pub const AND_IMMEDIATE_32_BIT_TO_RM_32_BIT: u8 = 0x81;

pub const AND_OPCODE_EXT: u8 = 0x04;

pub const OR_RM_8_BIT_WITH_REG: u8 = 0x0A;
pub const OR_RM_32_BIT_WITH_REG: u8 = 0x0B;
pub const OR_REG_WITH_RM_8_BIT: u8 = 0x08;
pub const OR_REG_WITH_RM_32_BIT: u8 = 0x09;
pub const OR_IMMEDIATE_8_BIT_TO_RM_8_BIT: u8 = 0x80;
pub const OR_IMMEDIATE_8_BIT_SIGN_EXTENDED_TO_RM_32_BIT: u8 = 0x83;
pub const OR_IMMEDIATE_32_BIT_TO_RM_32_BIT: u8 = 0x81;

pub const OR_OPCODE_EXT: u8 = 0x01;


pub const XOR_RM_8_BIT_WITH_REG: u8 = 0x32;
pub const XOR_RM_32_BIT_WITH_REG: u8 = 0x33;
pub const XOR_REG_WITH_RM_8_BIT: u8 = 0x30;
pub const XOR_REG_WITH_RM_32_BIT: u8 = 0x31;
pub const XOR_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0x80;
pub const XOR_IMMEDIATE_8_BIT_SIGN_EXTENDED_TO_RM_32_BIT: u8 = 0x83;
pub const XOR_RM_32_BIT_WITH_32_BIT_IMMEDIATE: u8 = 0x81;
pub const XOR_OPCODE_EXT: u8 = 0x06;


pub const NOT_RM_8_BIT: u8 = 0xF6;
pub const NOT_RM_32_BIT: u8 = 0xF7;

pub const NOT_OPCODE_EXT: u8 = 0x02;

pub const SIGN_EXTEND_ACCUMULATOR : u8 = 0x99;


pub const SHL_RM_32_BIT_ONCE: u8 = 0xD1;
pub const SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC1;
pub const SHL_RM_32_BIT: u8 = 0xD3;

pub const SHL_RM_8_BIT_ONCE: u8 = 0xD0;
pub const SHL_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC0;
pub const SHL_RM_8_BIT: u8 = 0xD2;

pub const LOGICAL_SHIFT_LEFT_OPCODE_EXT: u8 = 0x04;

pub const SAR_RM_32_BIT_ONCE: u8 = 0xD1;
pub const SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC1;
pub const SAR_RM_32_BIT: u8 = 0xD3;

pub const SAR_RM_8_BIT_ONCE: u8 = 0xD0;
pub const SAR_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC0;
pub const SAR_RM_8_BIT: u8 = 0xD2;

pub const ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT: u8 = 0x07;

pub const SHR_RM_32_BIT_ONCE: u8 = 0xD1;
pub const SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC1;
pub const SHR_RM_32_BIT: u8 = 0xD3;

pub const SHR_RM_8_BIT_ONCE: u8 = 0xD0;
pub const SHR_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC0;
pub const SHR_RM_8_BIT: u8 = 0xD2;

pub const LOGICAL_SHIFT_RIGHT_OPCODE_EXT: u8 = 0x05;


pub const NEGATE_RM_8_BIT: u8 = 0xF6;
pub const NEGATE_RM_32_BIT: u8 = 0xF7;

pub const NEGATE_OPCODE_EXT: u8 = 0x03;

pub const COMPARE_RM_32_BIT_WITH_32_BIT_IMMEDIATE: u8 = 0x81;
pub const COMPARE_REG_WITH_RM_32_BIT: u8 = 0x3B;

pub const COMPARE_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0x80;
pub const COMPARE_REG_WITH_RM_8_BIT: u8 = 0x3A;

pub const CMP_OPCODE_EXT: u8 = 0x07;


pub const CALL_32_BIT_NEAR_RELATIVE: u8 = 0xE8;
pub const JUMP_32BIT_NEAR_RELATIVE : u8 = 0xE9;

pub const JUMP_IF_LESS : u8 = 0x7C;
pub const JUMP_IF_LESS_OR_EQ : u8 = 0x7E;
pub const JUMP_IF_EQ : u8 = 0x74;
pub const JUMP_IF_NOT_EQ : u8 = 0x75;
pub const JUMP_IF_GREATER_OR_EQ : u8= 0x7D;
pub const JUMP_IF_GREATER : u8 = 0x7F;

pub const JUMP_IF_LESS_32_BIT: u16 = 0x0F8C;
pub const JUMP_IF_LESS_OR_EQ_32_BIT: u16 = 0x0F8E;
pub const JUMP_IF_EQ_32_BIT : u16 = 0x0F84;
pub const JUMP_IF_NOT_EQ_32_BIT : u16 = 0x0F85;
pub const JUMP_IF_GREATER_OR_EQ_32_BIT : u16= 0x0F8D;
pub const JUMP_IF_GREATER_32_BIT : u16 = 0x0F8F;

pub const SET_BYTE_IF_LESS : u16 = 0x0F9C;
pub const SET_BYTE_IF_LESS_OR_EQ : u16 = 0x0F9E;
pub const SET_BYTE_IF_EQ : u16 = 0x0F94;
pub const SET_BYTE_IF_NEQ : u16 = 0x0F95;
pub const SET_BYTE_IF_GREATER_OR_EQ : u16 = 0x0F9D;
pub const SET_BYTE_IF_GREATER : u16 = 0x0F9F;
pub const SET_BYTE_OPCODE_EXT: u8 = 0x00;

pub const NOP : u8 = 0x90;


pub const NEAR_RETURN : u8 = 0xC3;
pub const LEAVE: u8 = 0xC9;


pub const POP_REG: u8 = 0x58;

pub const PUSH_REG: u8 = 0x50;
pub const PUSH_RM: u8 = 0xFF;
pub const PUSH_32_BIT_IMMEDIATE: u8 = 0x68;
pub const PUSH_OPCODE_EXTENSION: u8 = 0x06;




#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizedOpCode {
    OpCode8(u8),
    OpCode16(u16),
}

impl From<u8> for SizedOpCode {
    fn from(val: u8) -> SizedOpCode {
        SizedOpCode::OpCode8(val)
    }
}

impl From<u16> for SizedOpCode {
    fn from(val: u16) -> SizedOpCode {
        SizedOpCode::OpCode16(val)
    }
}