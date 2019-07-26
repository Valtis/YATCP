
use crate::byte_generator;
use crate::byte_generator::Value::*;
use crate::byte_generator::{ByteCode, UnaryOperation, BinaryOperation, ComparisonOperation, Value, ComparisonType};

use crate::code_generator;

use byteorder::{ByteOrder, LittleEndian };

use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use rayon::prelude::*;
use std::hash::Hash;
use crate::obj_generator::Architecture::X64;
use crate::code_generator::x64::X64Register::RBP;
use crate::code_generator::x64::RegField::{OpcodeExtension, Register};
use std::hint::unreachable_unchecked;

const INTEGER_SIZE: usize = 4;

const MOV_IMMEDIATE_32_BIT_TO_REG_BASE: u8 = 0xB8; // register encoding will be binary OR'ed into opcode
const MOV_IMMEDIATE_32_BIT_TO_RM: u8 = 0xC7;
const MOV_REG_TO_RM_32_BIT: u8 = 0x89;
const MOV_RM_TO_REG_32_BIT: u8 = 0x8B;

const ADD_IMMEDIATE_8_BIT_TO_RM: u8 = 0x83;
const ADD_IMMEDIATE_32_BIT_TO_RM: u8 = 0x81;
const ADD_REG_TO_RM_32_BIT: u8 = 0x01;
const ADD_RM_TO_REG_32_BIT: u8 = 0x03;
const ADD_OPCODE_EXT: u8 = 0;

const SUB_IMMEDIATE_8_BIT_FROM_RM: u8 = 0x83;
const SUB_IMMEDIATE_32_BIT_FROM_RM: u8 = 0x81;
const SUB_RM_32_FROM_REG: u8 = 0x2B;
const SUB_REG_FROM_RM_32_BIT: u8 = 0x29;
const SUB_OPCODE_EXT: u8 = 0x05;

const SIGNED_MUL_RM_32_BIT_WITH_8_BIT_IMMEDIATE_: u8 = 0x6B;
const SIGNED_MUL_RM_32_BIT_WITH_32_BIT_IMMEDIATE: u8 = 0x69;
const SIGNED_MUL_REG_RM_32_BIT: u16 = 0x0FAF;

const SIGNED_DIV_RM_32_BIT: u8 = 0xF7;
const DIV_OPCODE_EXT: u8 = 0x07;

const SIGN_EXTEND_ACCUMULATOR : u8 = 0x99;

const COMPARE_RM_32_BIT_WITH_32_BIT_IMMEDIATE: u8 = 0x81;
const COMPARE_REG_WITH_RM_32_BIT: u8 = 0x3B;
const CMP_OPCODE_EXT: u8 = 0x07;


const JUMP_32BIT_NEAR_RELATIVE : u8 = 0xE9;

const JUMP_IF_LESS : u8 = 0x7C;
const JUMP_IF_LESS_OR_EQ : u8 = 0x7E;
const JUMP_IF_EQ : u8 = 0x74;
const JUMP_IF_NOT_EQ : u8 = 0x75;
const JUMP_IF_GREATER_OR_EQ : u8= 0x7D;
const JUMP_IF_GREATER : u8 = 0x7F;

const SET_BYTE_IF_LESS : u16 = 0x0F9C;
const SET_BYTE_IF_LESS_OR_EQ : u16 = 0x0F9E;
const SET_BYTE_IF_EQ : u16 = 0x0F94;
const SET_BYTE_IF_GREATER_OR_EQ : u16 = 0x0F9D;
const SET_BYTE_IF_GREATER : u16 = 0x0F9F;

const NOP : u8 = 0x90;

const NEAR_RETURN : u8 = 0xC3;

const PUSH : u8 = 0x50;
const POP : u8 = 0x58;

#[derive(Debug, Clone, Copy, PartialEq)]
enum AddressingMode {
    IndirectAddressingNoDisplacement,
    IndirectAddressingOnlyDisplacement,
    IndirectAddressingOneByteDisplacement,
    IndirectAddressingFourByteDisplacement,
    DirectRegisterAddressing,
}

impl AddressingMode {
    fn get_addressing_encoding(&self) -> u8 {
        match self {
            AddressingMode::IndirectAddressingNoDisplacement => 0b0000_0100,
            AddressingMode::IndirectAddressingOnlyDisplacement => 0b0000_0101,
            AddressingMode::IndirectAddressingOneByteDisplacement => 0b0100_0000,
            AddressingMode::IndirectAddressingFourByteDisplacement => 0b1000_0000,
            AddressingMode::DirectRegisterAddressing => 0b1100_0000,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum RegField {
    OpcodeExtension(u8),
    Register(X64Register),
    Unused,
}

#[derive(Debug, Clone, Copy)]
enum RmField {
    Register(X64Register),
    Unused,
}

#[derive(Debug, Clone, Copy)]
struct ModRM {
    addressing_mode: AddressingMode,
    reg_field: RegField,
    rm_field: RmField,
}

// scale, index, base, displacement
#[derive(Debug, Clone, Copy)]
struct Sib {
    scale: Option<u8>,
    index: Option<X64Register>, // RSP-reg encoding used to denote this field is unused. RSP cannot be used as index reg as a result
    base: Option<X64Register>,
    displacement: Option<Displacement>
}

#[derive(Debug, Clone, Copy)]
enum Displacement {
    FourByte(u32),
    OneByte(u8)
}

#[derive(Debug, Clone, Copy)]
enum Immediate {
    Byte(u8),
    FourByteSigned(i32),
    FourByte(u32)
}

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
    EDX,
    EBX,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SizedOpCode {
    OpCode8(u8),
    OpCode16(u16),
}

// used to store jumps that need the target patched afterwards

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JumpPatch {
    Jump(u32, usize),
    ConditionalShortJump(u32, usize),
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

impl From<u8> for Immediate {
    fn from(val: u8) -> Immediate {
        Immediate::Byte(val)
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

impl Immediate {
    fn write_into_buffer(&self, write_buffer: &mut Vec<u8>) {
        match self {
            Immediate::Byte(val) => {
                write_buffer.push(*val);
            }
            Immediate::FourByte(val) => {
                let mut buffer = [0; 4];
                LittleEndian::write_u32(&mut buffer, *val);
                for i in 0..4 {
                    write_buffer.push(buffer[i]);
                }
            },
            Immediate::FourByteSigned(val) => {
                let mut buffer = [0; 4];
                LittleEndian::write_i32(&mut buffer, *val);
                for i in 0..4 {
                    write_buffer.push(buffer[i]);
                }
            },
        }
    }

}


impl X64Register {
    fn encoding(&self) -> u8 {
        match self {
            X64Register::RAX | X64Register::R8 | X64Register::EAX => 0x00,
            X64Register::RCX | X64Register::R9 => 0x01,
            X64Register::RDX | X64Register::R10 | X64Register::EDX=> 0x02,
            X64Register::RBX | X64Register::R11 | X64Register::EBX => 0x03,
            X64Register::RSP | X64Register::R12 => 0x04,
            X64Register::RBP | X64Register::R13 => 0x05,
            X64Register::RSI | X64Register::R14 => 0x06,
            X64Register::RDI | X64Register::R15 => 0x07,
        }
    }

    fn is_extended_reg(&self) -> bool {
       match self {
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

    fn is_64_bit_register(&self) -> bool {
       self.get_register_size() == 8
    }

    fn get_register_size(&self) -> u8 {
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
            X64Register::EDX |
            X64Register::EBX => 4,
        }
    }
}



pub fn generate_code(functions: Vec<(byte_generator::Function, u32)>) -> (Vec<code_generator::Function>, Vec<u8>) {
    let function_asm: Vec<(String, Vec<u8>)> = functions.par_iter()
        .map(|(function, stack_size)| generate_code_for_function(function, *stack_size))
        .collect();

    let mut codegen_functions = vec![];
    let mut combined_asm = vec![];

    for (name, mut asm) in function_asm {
        codegen_functions.push(code_generator::Function{
           name: name,
           start: combined_asm.len(),
           length: asm.len(),
        });

        combined_asm.append(&mut asm);
    }


    (codegen_functions, combined_asm)
}

fn generate_code_for_function(function: &byte_generator::Function, stack_size: u32) -> (String, Vec<u8>) {


    let mut asm = vec![];
    let mut label_pos: HashMap<u32, usize> = HashMap::new();
    let mut jumps_requiring_updates = vec![];

    emit_function_prologue(&mut asm, stack_size);
    for b in function.code.iter() {
        match b {
            ByteCode::Nop => emit_nop(&mut asm),
            ByteCode::Mov(operands) => emit_mov(operands, &mut asm),
            ByteCode::Add(operands) => emit_add(operands, &mut asm),
            ByteCode::Sub(operands) => emit_sub(operands, &mut asm),
            ByteCode::Mul(operands) => emit_mul(operands, &mut asm),
            ByteCode::Div(operands) => emit_div(operands, &mut asm),
            ByteCode::Ret(value) => emit_ret(value, stack_size, &mut asm),
            ByteCode::SignExtend(operands) => emit_sign_extension(operands, &mut asm),
            ByteCode::Compare(ref operands) => emit_comparison(operands, &mut asm),
            ByteCode::Label(id) => handle_label(*id, asm.len(), &mut label_pos),
            ByteCode::Jump(id) => emit_unconditional_jump(*id, &mut jumps_requiring_updates, &mut asm),
            ByteCode::JumpConditional(id,  jmp_type) =>
                emit_conditional_jump(*id, jmp_type, &mut jumps_requiring_updates,  &mut asm),
            _ => unimplemented!("{:#?}", b),
        }
    }

    update_jumps(&jumps_requiring_updates, &label_pos, &mut asm);

    (function.name.clone(), asm)
}

// Comment out pending rewrite/fixes, as ByteCode representation is undergoing large changes

fn emit_nop(asm: &mut Vec<u8>) {
    asm.push(NOP);
}

fn emit_sign_extension(operands: &UnaryOperation, asm: &mut Vec<u8>) {

    let (src, dest) = match operands {
        UnaryOperation{
            src: PhysicalRegister(ref src_reg),
            dest: PhysicalRegister(ref dest_reg)
        } => {
            // sign extension extend ax/eax/rax into dx/edx/rdx, we have no say over this.
            ice_if!(!(*src_reg == X64Register::EAX || *src_reg == X64Register::RAX) ||
                !(*dest_reg == X64Register::EDX || *dest_reg == X64Register::RDX),
                "Invalid operand encoding for sign extension: {:#?}", operands);

            (src_reg, dest_reg)
        },
        _ => ice!("Invalid operand encoding for sign extension: {:#?}", operands)
    };


    let rex = create_rex_prefix(src.is_64_bit_register() || dest.is_64_bit_register(), None, None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SIGN_EXTEND_ACCUMULATOR),
        None,
        None,
        None,
    );
}


fn emit_mov(operand: &UnaryOperation, asm: &mut Vec<u8>) {
    match operand {
        UnaryOperation{
            dest: StackOffset { offset, size},
            src: BooleanConstant(value),
        } => {

            emit_mov_integer_to_stack(
                *offset,
                 if *value { 1 } else { 0 },
                asm);
        },
        UnaryOperation{
            dest: StackOffset {offset, size} ,
            src: IntegerConstant(value)} => {
            emit_mov_integer_to_stack(*offset, *value, asm);
        },
        UnaryOperation{
            dest: PhysicalRegister(ref reg),
            src: StackOffset {offset, size},
        } => {
            emit_mov_from_stack_to_reg(*reg, *offset, *size, asm);
        },
        UnaryOperation {
            dest: StackOffset {offset, size},
            src: PhysicalRegister(ref reg)
        } => {
            emit_mov_from_reg_to_stack(*reg, *offset, *size, asm);
        },
        UnaryOperation {
            dest: PhysicalRegister(ref reg),
            src: IntegerConstant(value),
        } => {
            emit_mov_integer_to_register(*value, *reg, asm);
        },
        _ => ice!("Invalid MOV operation:\n{:#?}", operand),
    }
}
/*
    MOV DWORD PTR [RBP - offset], imm32

    Note: depending on offset, may use one byte displacement or four byte displacement

    Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, opcode extension in reg field, dst in r/m
    sib: not used. sib struct is used to store one or four byte displacement, depending if offset is less or equal to 128 or not
    immediate: the 32 bit immediate
*/

fn emit_mov_integer_to_stack(offset: u32, immediate: i32, asm: &mut Vec<u8>) {

    /*
        Optimizing the offset = 0 case does not really do anything, as we are using SBP, and
        the no displacement encoding requires SIB byte in this case - nothing is saved
    */
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(0),
        rm_field: RmField::Register(X64Register::RBP)
    };


    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_IMMEDIATE_32_BIT_TO_RM),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

fn get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset: u32) -> (AddressingMode, Option<Sib>) {
    let addressing_mode = if offset <= 128 {
        AddressingMode::IndirectAddressingOneByteDisplacement
    } else {
        AddressingMode::IndirectAddressingFourByteDisplacement
    };
    let sib = if addressing_mode == AddressingMode::IndirectAddressingOneByteDisplacement {
        Some(Sib {
            base: None,
            index: None,
            scale: None,
            // in this case the number is signed integer, but we use u8. Convert the u8 offset to twos complement form, so that it is negative
            displacement: Some(Displacement::OneByte(u8::max_value().wrapping_sub(offset as u8).wrapping_add(1))),
        })
    } else {
        Some(Sib {
            base: None,
            index: None,
            scale: None,
            // in this case the number is signed integer, but we use u32. Convert the u32 offset to twos complement form, so that it is negative
            displacement: Some(Displacement::FourByte(u32::max_value().wrapping_sub(offset).wrapping_add(1))),
        })
    };
    (addressing_mode, sib)
}

/* MOV r32, imm32
    Rex prefix: No
    opcode: 1 byte, dst reg encoded in low bits
    modrm: no
    sib: no
    immediate: the 32 bit immediate
*/
fn emit_mov_integer_to_register(immediate: i32, register: X64Register, asm: &mut Vec<u8>)  {
    // register encoded in the opcode itself
    emit_instruction(
        asm,
        None,
        SizedOpCode::from(MOV_IMMEDIATE_32_BIT_TO_REG_BASE | register.encoding()),
        None,
        None,
        Some(Immediate::from(immediate)),
    )

}
/*
    MOV r32, r32
    Rex prefix: If regs R8-R15 are used
    opcode: 1 byte
    modrm: direct register addressing, src in reg field, dest in rm fielddddddd
    sib: no
    immediate: no

*/
fn emit_mov_reg_to_reg(dest: X64Register, src: X64Register, asm: &mut Vec<u8>) {

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(dest),
    };

    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_REG_TO_RM_32_BIT),
        Some(modrm),
        None,
        None,
    );
}

/*
    MOV reg, <ptr_size> [RBP - offset]

    Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, rbp in rm field, dest reg in reg field
    sib: unused, sib struct used to pass displacement
    immediate: none
*/
fn emit_mov_from_stack_to_reg(dest: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {

    if size != 4 {
        unimplemented!();
    }

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_RM_TO_REG_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_from_reg_to_stack(src: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {

    if size != 4 {
        unimplemented!();
    }

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_REG_TO_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

/*
fn emit_set_byte(
    &mut self,
    cmp_type: &ComparisonType,
    operand: &UnaryOperation) {

    let opcode = match *cmp_type {
        ComparisonType::Less => SET_BYTE_IF_LESS,
        ComparisonType::LessOrEq => SET_BYTE_IF_LESS_OR_EQ,
        ComparisonType::Equals => SET_BYTE_IF_EQ,
        ComparisonType::GreaterOrEq => SET_BYTE_IF_GREATER_OR_EQ,
        ComparisonType::Greater => SET_BYTE_IF_GREATER,
    };

    self.emit_instruction(
        SizedOpCode::from(opcode),
        Some((
            Mode::Register,
            ModReg::OpCode(0),
            &operand.dest)),
        None,
        0);

}
*/
fn emit_add(operand: &BinaryOperation, asm: &mut Vec<u8>) {

    match operand {
        BinaryOperation{
            dest: StackOffset{offset: dest_offset, size: dest_size },
            src1: StackOffset{offset: src_offset, size: src_size },
            src2: IntegerConstant(value)
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            emit_add_immediate_to_stack(*dest_offset, *dest_size, *value, asm);
        },
        BinaryOperation{
            dest: StackOffset {offset: dest_offset, size: dest_size},
            src1: StackOffset {offset: src_offset, size: src_size},
            src2: PhysicalRegister(ref reg),
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            emit_add_reg_to_stack(*reg, *dest_offset, *dest_size, asm);
        },
        BinaryOperation{
            dest: PhysicalRegister(ref dest_register),
            src1: PhysicalRegister(ref src_register),
            src2: StackOffset {offset, size}
        } => {
            ice_if!(
                dest_register != src_register,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            emit_add_stack_to_reg(*dest_register, *offset, *size, asm);

        },
        _ => ice!("Invalid add operation encoding: {:#?}", operand)
    }


}

/*
    ADD reg32, imm

    REX: used if reg is rax - r15, otherwise not used
    opcode: 8 bit opcode
    modrm: direct addressing, reg field for opcode extension, reg in rm field
    sib: not used
    immediate: 32 bit immediate
*/
fn emit_add_immediate_to_register(register: X64Register, immediate: i32, asm: &mut Vec<u8>) {

    let (imm, opcode) = if immediate <= 127 && immediate >= -128 {
        (Immediate::from(immediate as u8), ADD_IMMEDIATE_8_BIT_TO_RM)
    } else {
        (Immediate::from(immediate), ADD_IMMEDIATE_32_BIT_TO_RM)
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(ADD_OPCODE_EXT),
        rm_field: RmField::Register(register),
    };

    let rex = create_rex_prefix(register.is_64_bit_register(), Some(modrm.clone()), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        Some(imm),
    );
}

/*
    ADD <ptr_size> PTR [RBP-offset], imm32

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128
    sib: byte not used, struct used to pass displacement
    immediate: the 32 bit immediate value

*/
fn emit_add_immediate_to_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(ADD_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP)
    };


    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(ADD_IMMEDIATE_32_BIT_TO_RM),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}
/*
    ADD reg, <ptr_size> PTR [RBP-offset]

    REX: yes, rbp used.
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination reg encoded
    sib: byte not used, struct used to pass displacement
    immediate: no


*/
fn emit_add_stack_to_reg(dest: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };


    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(ADD_RM_TO_REG_32_BIT),
        Some(modrm),
        sib,
        None
    );
}

/*
    ADD <ptr_size> PTR [RBP-offset], reg

    REX: yes, rbp used.
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    sib: byte not used, struct used to pass displacement
    immediate: no


*/
fn emit_add_reg_to_stack(src: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };


    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(ADD_REG_TO_RM_32_BIT),
        Some(modrm),
        sib,
        None
    );
}

fn emit_sub(operand: &BinaryOperation, asm: &mut Vec<u8>) {

    match operand {
        BinaryOperation{
                dest: StackOffset{offset: dest_offset, size: dest_size },
                src1: StackOffset{offset: src_offset, size: src_size },
                src2: IntegerConstant(immediate)
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            emit_sub_immediate_from_stack(*dest_offset, *dest_size, *immediate, asm)
        },
        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: StackOffset { offset, size},
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operand);
             emit_sub_stack_from_reg(*dest_reg, *offset, *size, asm);
        },
        BinaryOperation{
            dest: StackOffset {offset: dest_offset, size: dest_size},
            src1: StackOffset {offset: src_offset, size: src_size},
            src2: PhysicalRegister(ref src_reg),
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            emit_sub_reg_from_stack(*src_reg, *dest_offset, *dest_size, asm);

        },
        _ => ice!("Invalid sub operation encoding: {:#?}", operand),
    }
}
/*
    SUB <ptr_size> PTR [rbp-offset], imm32

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128
    sib: byte not used, struct used to pass displacement
    immediate: the 32 bit immediate value
*/

fn emit_sub_immediate_from_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(SUB_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_IMMEDIATE_32_BIT_FROM_RM),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

/*
    SUB reg32, imm

    REX: used if reg is r8-r15, otherwise not used
    opcode: 8 bit opcode
    modrm: direct addressing, reg field for opcode extension, reg in rm field
    sib: not used
    immediate: 32 bit immediate
*/
fn emit_sub_immediate_from_register(register: X64Register, immediate: i32, asm: &mut Vec<u8>) {


    let (imm, opcode) = if immediate <= 127 && immediate >= -128 {
        (Immediate::from(immediate as u8), SUB_IMMEDIATE_8_BIT_FROM_RM)
    } else {
        (Immediate::from(immediate), SUB_IMMEDIATE_32_BIT_FROM_RM)
    };


    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(SUB_OPCODE_EXT),
        rm_field: RmField::Register(register),
    };

    let rex = create_rex_prefix(register.is_64_bit_register(), Some(modrm.clone()), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        Some(imm),
    );
}

/*
    SUB reg, <PTR_SIZE> [rbp-offset]

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination register encoded
    sib: byte not used, struct used to pass displacement
    immediate: not used
*/
fn emit_sub_stack_from_reg(dest: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_RM_32_FROM_REG),
        Some(modrm),
        sib,
        None
    );
}


/*
    SUB <PTR_SIZE> [rbp-offset], reg

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    sib: byte not used, struct used to pass displacement
    immediate: not used
*/
fn emit_sub_reg_from_stack(src: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_REG_FROM_RM_32_BIT),
        Some(modrm),
        sib,
        None
    );
}

fn emit_mul(operand: &BinaryOperation, asm: &mut Vec<u8>) {
    match operand {

        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: IntegerConstant(immediate),
        } => {
            emit_mul_reg_with_immediate(*dest_reg, *src_reg, *immediate, asm);
        },
        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: StackOffset {offset, size},
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            emit_mul_reg_with_stack(*dest_reg, *offset, *size, asm);
        },
        _ => ice!("Invalid mul operation encoding: {:#?}", operand),
    }
}


/*
    IMUL dst_reg, src_reg, immediate32/immediate8

    REX: if 64 bit registers are used
    opcode: 8 bit opcode, either for signed multiplying with 8 bit or 32 bit immediate, depending on immediate value
    modrm: direct addressing, destination in reg, src in R/M
    SIB: Not used
    Immediate: 8/32 bit immediate, depending on if the value fits in 8 bits
*/

fn emit_mul_reg_with_immediate(dest_reg: X64Register, src_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {

    let (imm, opcode) = if immediate <= 127 && immediate >= -128 {
        (Immediate::from(immediate as u8), SIGNED_MUL_RM_32_BIT_WITH_8_BIT_IMMEDIATE_)
    } else {
        (Immediate::from(immediate), SIGNED_MUL_RM_32_BIT_WITH_32_BIT_IMMEDIATE)
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::Register(dest_reg),
        rm_field: RmField::Register(src_reg)
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        Some(imm),
    );
}


fn emit_mul_reg_with_stack(dest_reg: X64Register,  offset: u32, size: u32, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest_reg),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SIGNED_MUL_REG_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_div(operand: &BinaryOperation, asm: &mut Vec<u8>) {
    match operand {
        BinaryOperation{
            dest: _, // don't care, will be stored in eax,
            src1: _, // don't care, uses edx:eax
            src2: PhysicalRegister(ref reg), // don't care
        } => {
            emit_div_with_reg(*reg, asm);
        },
        BinaryOperation{
            dest: _,
            src1: _,
            src2: StackOffset {
                offset,
                size,
            }
        } => {
            emit_div_with_stack(*offset, *size, asm);
        },
        _ => ice!("Invalid div operation encoding: {:#?}", operand),
    }
}

fn emit_div_with_reg(divisor: X64Register, asm: &mut Vec<u8>) {


    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(DIV_OPCODE_EXT),
        rm_field: RmField::Register(divisor),
    };

    let rex = create_rex_prefix(divisor.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SIGNED_DIV_RM_32_BIT),
        Some(modrm),
        None,
        None,
    );
}

fn emit_div_with_stack(offset: u32, size: u32, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(DIV_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SIGNED_DIV_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_comparison(operands: &ComparisonOperation, asm: &mut Vec<u8>)  {
    match operands {
        ComparisonOperation {
            src1: PhysicalRegister(reg),
            src2: IntegerConstant(immediate),
        } => {
            emit_compare_immediate_with_register(*reg, *immediate, asm)
        },
        ComparisonOperation {
            src1: StackOffset { offset, size},
            src2: IntegerConstant(immediate),
        } => {
            emit_compare_immediate_with_stack(*offset, *size, *immediate, asm);
        },
        ComparisonOperation {
            src1: StackOffset { offset, size},
            src2: BooleanConstant(val),
        } => {
            emit_compare_immediate_with_stack(
                *offset,
                *size,
               if *val { 1 } else { 0 },
                asm );
        },
        ComparisonOperation {
            src1: PhysicalRegister(reg),
            src2: StackOffset {offset, size },
        } => {
            emit_compare_stack_with_register(*offset, *size, *reg, asm);
        },
        _ => ice!("Invalid operand encoding for CMP: {:#?}", operands),
    }
}

fn emit_compare_immediate_with_register(reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        rm_field:  RmField::Register(reg),
        reg_field: RegField::OpcodeExtension(CMP_OPCODE_EXT),
    };

    let rex = create_rex_prefix(reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_RM_32_BIT_WITH_32_BIT_IMMEDIATE),
        Some(modrm),
        None,
        Some(Immediate::from(immediate)),
    )
}

fn emit_compare_immediate_with_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::OpcodeExtension(CMP_OPCODE_EXT),
    };

   let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_RM_32_BIT_WITH_32_BIT_IMMEDIATE),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

fn emit_compare_stack_with_register(offset: u32, size: u32, register: X64Register, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::Register(register),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_REG_WITH_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_unconditional_jump(id: u32, jumps_requiring_updates: &mut Vec<JumpPatch>,  asm: &mut Vec<u8>) {

    jumps_requiring_updates.push(JumpPatch::Jump(id, asm.len()));
    let placeholder_target = 0u32;

    emit_instruction(
        asm,
        None,
        SizedOpCode::from(JUMP_32BIT_NEAR_RELATIVE),
        None,
        None,
        Some(Immediate::from(placeholder_target)), // will be replaced by actual target later on
    );

}

fn emit_conditional_jump(
    id: u32,
    jmp_type: &ComparisonType,
    jumps_requiring_updates: &mut Vec<JumpPatch>,
    asm: &mut Vec<u8>) {

    let jmp_code = match *jmp_type {
        ComparisonType::Less => JUMP_IF_LESS,
        ComparisonType::LessOrEq => JUMP_IF_LESS_OR_EQ,
        ComparisonType::Equals => JUMP_IF_EQ,
        ComparisonType::NotEquals => JUMP_IF_NOT_EQ,
        ComparisonType::GreaterOrEq => JUMP_IF_GREATER_OR_EQ,
        ComparisonType::Greater => JUMP_IF_GREATER,
    };

    jumps_requiring_updates.push(JumpPatch::ConditionalShortJump(id, asm.len()));
    let placeholder = 0u8;

    emit_instruction(
        asm,
        None,
        SizedOpCode::from(jmp_code),
        None,
        None,
        Some(Immediate::from(placeholder)),
    );

}

fn emit_instruction(
    asm: &mut Vec<u8>,
    rex: Option<u8>,
    opcode: SizedOpCode,
    modrm: Option<ModRM>,
    sib: Option<Sib>,
    immediate: Option<Immediate>) {

    if let Some(rex_val) = rex {
        asm.push(rex_val);
    }

    match opcode {
        SizedOpCode::OpCode8(opcode) => {
            asm.push(opcode)
        },
        SizedOpCode::OpCode16(opcode) => {
            let mut buffer= [0; 2];
            LittleEndian::write_u16(&mut buffer, opcode);
            asm.push(buffer[1]);
            asm.push(buffer[0]);
        },
    }

    if let Some(modrm_values) = modrm {
        let mut modrm_byte =
            modrm_values
                .addressing_mode.get_addressing_encoding();

        match modrm_values.reg_field {
            RegField::Register(ref reg) => {
                modrm_byte |= reg.encoding() << 3;
            },
            RegField::OpcodeExtension(ext) => {
                modrm_byte |= ext << 3;
            },
            RegField::Unused => (),
        }

        match modrm_values.rm_field {
            RmField::Register(ref reg ) => {
                modrm_byte |= reg.encoding();
            },
            RmField::Unused => (),
        }

        asm.push(modrm_byte);
    }

    if let Some(sib_values) = sib {
        let mut sib_byte = 0u8;

        let mut missing_fields = 0;

        if let Some(scale) = sib_values.scale {
            sib_byte |= scale << 6;
        } else {
            missing_fields += 1;
        }

        if let Some(index) = sib_values.index {
            sib_byte |= index.encoding();
        } else {
            missing_fields +=1;
            sib_byte |= 0b100 << 3; // RSP-encoding used to indicate field is unused
        }

        if let Some(base) = sib_values.base {
            sib_byte |= base.encoding();
        } else {
            missing_fields +=1;
        }
        // if all of scale, index, base were not set, don't emit sib byte
        if missing_fields < 3 {
            asm.push(sib_byte);
        }

        if let Some(disp) = sib_values.displacement {
            match disp {
                Displacement::OneByte(val) => {
                    asm.push(val);
                },
                Displacement::FourByte(val) => {
                    let mut buffer = [0; 4];
                    LittleEndian::write_u32(&mut buffer, val);
                    for i in 0..4 {
                        asm.push(buffer[i]);
                    }
                },

            };
        }
    }

    if let Some(imm) = immediate {
        imm.write_into_buffer(asm);
    }

}

fn emit_ret(value: &Option<Value>, stack_size: u32, asm: &mut Vec<u8>) {

    match value {
        Some(IntegerConstant(value)) => {
            emit_mov_integer_to_register(*value, X64Register::EAX, asm)
        }
        Some(StackOffset {offset, size}) => emit_mov_from_stack_to_reg(X64Register::RAX, *offset, *size, asm),
        None => (),
        _ =>  ice!("Invalid return value: {:#?}", value),
    }

    emit_function_epilogue(asm, stack_size);
    asm.push(NEAR_RETURN);
}


/*
    FIXME: Unnecessary stack pointer modifications under certain conditions

    System V AMD 64 ABI, which we follow, guarantees 128 byte stack space ("red zone") for
    leaf functions. If we need at most 128 bytes of stack space for leaf function, we do not need
    to modify the stack pointer.

    Need to implement:
        * Function attributes
        * Proper phase needs to tag function as leaf function
        * Function prologue/epilogue needs to check for this attribute and act accordingly
        * RSP can be used instead of RBP for stack addressing - update code gen to account for this
        * Implement a command line flag to skip this optimization - unlikely this is ever needed,
          as red zone seems to be mostly only an issue in kernel mode, but might as well
*/
fn emit_function_prologue(asm: &mut Vec<u8>, stack_size: u32) {
    push(X64Register::RBP, asm);
    emit_mov_reg_to_reg(X64Register::RBP, X64Register::RSP, asm);
    if stack_size != 0 {
        emit_sub_immediate_from_register(X64Register::RSP, stack_size as i32, asm);
    }
}

fn emit_function_epilogue(asm: &mut Vec<u8>, stack_size: u32) {
    if stack_size != 0 {
        emit_add_immediate_to_register(X64Register::RSP, stack_size as i32, asm);
    }
    pop(X64Register::RBP, asm);
}

fn push(register: X64Register, asm: &mut Vec<u8>) {
    if register.is_extended_reg() {
        asm.push(0x41);
    }
    asm.push(PUSH + register.encoding());
}

fn pop(register: X64Register, asm: &mut Vec<u8>) {
    if register.is_extended_reg() {
        asm.push(0x41);
    }
    asm.push(POP + register.encoding());
}



// REX prefix is used to encode things related to 64 bit instruction set
// REX prefix always starts with bit pattern 0100 (0x4)
// remaining bits are called WRXB, so the whole pattern is 0b0100_WRXB (or 0x4z in hexadecimal)
// the meaning of these bits is the following (from http://wiki.osdev.org/X86-64_Instruction_Encoding):
// W   1 bit   When 1, a 64-bit operand size is used. Otherwise, when 0, the default operand size is used (which is 32-bit for most but not all instructions).
//
// R   1 bit   This 1-bit value is an extension to the MODRM.reg field. See Registers.
// X   1 bit   This 1-bit value is an extension to the SIB.index field. See 64-bit addressing.
// B   1 bit   This 1-bit value is an extension to the MODRM.rm field or the SIB.base field. See 64-bit addressing.
fn create_rex_prefix(operand_64bit: bool, modrm: Option<ModRM>, sib: Option<Sib>) -> Option<u8> {
    let mut prefix = 0x40;
    if operand_64bit {
        prefix |= 0b000_1000;
    }

    if let Some(ModRM{ addressing_mode: _, reg_field: RegField::Register(ref reg), rm_field: _}) = modrm {
        if reg.is_extended_reg() {
            prefix |= 0b0000_0100;
        }
    }

    if let Some(Sib{scale: _, index: Some(ref reg),  base: _, displacement: _}) = sib {
        if reg.is_extended_reg() {
            prefix |= 0b0000_0010;
        }
    }

    if let Some(ModRM{ addressing_mode: _, reg_field: _, rm_field: RmField::Register(ref reg)}) = modrm {
        if reg.is_extended_reg() {
            prefix |= 0b0000_0001;
        }
    }

    if prefix == 0x40 {
        None
    } else {
        Some(prefix)
    }
}

fn handle_label(id: u32, asm_code_len: usize, label_pos: &mut HashMap<u32, usize>) {
    label_pos.insert(id, asm_code_len);
}

fn update_jumps(
    jumps_requiring_updates: &Vec<JumpPatch>,
    label_pos: &HashMap<u32, usize>,
    asm: &mut Vec<u8>) {
    for jump in jumps_requiring_updates.iter() {
        match jump {
            JumpPatch::Jump(label_id, jump_opcode_location) => {
                if label_pos.contains_key(&label_id) {
                    let jump_address = label_pos[&label_id];
                    // Note: Breaks if the offset is larger than i32::maxval
                    // (although jump of that size is hopefully unlikely)
                    let unconditional_jump_size = 5; // 1 for opcode, 4 for location
                    let offset : i32 = jump_address as i32 - *jump_opcode_location as i32 - unconditional_jump_size;

                    let mut buffer = [0; 4];
                    LittleEndian::write_i32(&mut buffer, offset);

                    for i in 0..4 {
                        // +1 so that the one-byte opcode is skipped
                        asm[jump_opcode_location + 1 + i] = buffer[i];
                    }
                } else {
                    ice!("No jump target for jump stored: {:#?}", jump);
                }
            },
            JumpPatch::ConditionalShortJump(label_id, jump_opcode_location) => {
                if label_pos.contains_key(&label_id) {
                    let target = label_pos[&label_id];
                    let op_size = 2i32; // size of the whole operation (opcode + operand)
                    let offset : i32 = target as i32 - *jump_opcode_location as i32 - op_size;
                    if offset < -128 || offset > 127 {
                        panic!("Not implemented for jumps larger than a byte");
                    }
                    // +1 so that the one-byte opcocde is skipped
                    asm[jump_opcode_location + 1] = (offset as i8) as u8;
                } else {
                    ice!("No jump target for conditional short jump stored: {:#?}", jump);
                }
            }
        }
    }
}


