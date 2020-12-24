pub mod stack_allocator;
pub mod x64_register; // FIXME: Make bytecode use some abstraction rather than this directly (move reg impl to trait?)
mod opcodes;
mod encoding;

use x64_register::X64Register;
use opcodes::*;
use encoding::*;

use super::super::byte_generator::byte_code::Value::*;
use super::super::byte_generator::byte_code::{ByteCode, UnaryOperation, BinaryOperation, ComparisonOperation, Value, ComparisonType, Function as ByteCodeFunction};
use super::super::code_generator;

use byteorder::{ByteOrder, LittleEndian };
use rayon::prelude::*;

use std::collections::HashMap;
use std::collections::HashSet;
use crate::common::function_attributes::FunctionAttribute;
use crate::backend::code_generator::x64::encoding::Immediate::Byte;


// used to store jumps that need the target patched afterwards

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JumpPatch {
    Jump(u32, usize),
    ConditionalShortJump(u32, usize),
}

// used to store function calls that need the target patched afterwards
#[derive(Debug, Clone, PartialEq, Eq)]
struct CallPatch {
    name: String,
    location: usize,
}


pub fn generate_code(functions: Vec<(ByteCodeFunction, u32)>) -> super::Code {
    let function_asm: Vec<(ByteCodeFunction, Vec<u8>, Vec<CallPatch>)> = functions.par_iter()
        .filter(|(function, _)| !function.has_attribute(FunctionAttribute::External))
        .map(|(function, stack_size)| generate_code_for_function(function, *stack_size))
        .collect();

    let mut codegen_functions = vec![];
    let mut combined_asm = vec![];

    let mut combined_call_patches = vec![];
    let mut function_positions = HashMap::new();
    // for external functions, just leave the call offset as a placeholder, linker deals with it

    let external_functions: HashSet<String> = functions.iter()
        .filter(|(function, _)| function.has_attribute(FunctionAttribute::External))
        .map(|(function, _)| function.name.clone()).collect();

      functions.iter()
        .filter(|(function, _)| function.has_attribute(FunctionAttribute::External))
        .for_each(|(function, _)| codegen_functions.push(
            code_generator::Function {
                name: function.name.clone(),
                start: 0,
                length: 0,
                attributes: function.attributes.clone(),
            }));


    for (function, mut asm, mut calls_requiring_updates) in function_asm {

        ice_if!(function_positions.contains_key(&function.name),
             "Function {} already present in function call location patch table", function.name);

        function_positions.insert(function.name.clone(), combined_asm.len());

        for patch in calls_requiring_updates.iter_mut() {
            patch.location = patch.location + combined_asm.len();
            combined_call_patches.push(patch.clone())
        }

        codegen_functions.push(code_generator::Function{
           name: function.name,
           start: combined_asm.len(),
           length: asm.len(),
           attributes: function.attributes,
        });

        combined_asm.append(&mut asm);
    }

    let relocations = update_calls(&combined_call_patches, &function_positions, external_functions, &mut combined_asm);

    super::Code {
        functions: codegen_functions,
        code: combined_asm,
        relocations,
    }
}

fn generate_code_for_function(function: &ByteCodeFunction, stack_size: u32) -> (ByteCodeFunction, Vec<u8>, Vec<CallPatch>) {


    let mut asm = vec![];
    let mut label_pos: HashMap<u32, usize> = HashMap::new();
    let mut jumps_requiring_updates = vec![];
    let mut calls_requiring_updates = vec![];

    emit_function_prologue(&mut asm, stack_size);
    for b in function.code.iter() {
        match b {
            ByteCode::Nop => emit_nop(&mut asm),
            ByteCode::Movzx(operands) => emit_mov_zero_extending(operands, &mut asm),
            ByteCode::Movsx(operands) => emit_mov_sign_extending(operands, &mut asm),
            ByteCode::Mov(operands) => emit_mov(operands, &mut asm),
            ByteCode::Lea(operands) => emit_lea(operands, &mut asm),
            ByteCode::Add(operands) => emit_add(operands, &mut asm),
            ByteCode::Sub(operands) => emit_sub(operands, &mut asm),
            ByteCode::Mul(operands) => emit_mul(operands, &mut asm),
            ByteCode::Div(operands) => emit_div(operands, &mut asm),
            ByteCode::Shl(operands) => emit_shl(operands, &mut asm),
            ByteCode::Sar(operands) => emit_sar(operands, &mut asm),
            ByteCode::Shr(operands) => emit_shr(operands, &mut asm),
            ByteCode::And(operands) => emit_and(operands, &mut asm),
            ByteCode::Xor(operands) => emit_xor(operands, &mut asm),
            ByteCode::Negate(operands) => emit_neg(operands, &mut asm),
            ByteCode::Ret(value) => emit_ret(value, stack_size, function.parameter_count, &mut asm),
            ByteCode::SignExtend(operands) => emit_sign_extension(operands, &mut asm),
            ByteCode::Compare(ref operands) => emit_comparison(operands, &mut asm),
            ByteCode::Label(id) => handle_label(*id, asm.len(), &mut label_pos),
            ByteCode::Jump(id) => emit_unconditional_jump(*id, &mut jumps_requiring_updates, &mut asm),
            ByteCode::JumpConditional(id,  jmp_type) =>
                emit_conditional_jump(*id, jmp_type, &mut jumps_requiring_updates,  &mut asm),
            ByteCode::Call(name) => emit_function_call(name, &mut calls_requiring_updates, &mut asm),
            ByteCode::Push(value) => emit_push(value, &mut asm),
            ByteCode::Pop(value) => emit_pop(value, &mut asm),
            _ => unimplemented!("{:#?}", b),
        }
    }

    // codegen thus far was optimistic and used 8 bit offsets. Update to 32 bit offsets for jumps
    // where this is not enough. Function patches also need to be adjusted, as ASM shifts around when
    // placeholders and opcodes are replaced with larger ones (opcode 1 -> 2 byte, offset placeholder 1 byte -> 4 byte)
    change_1_byte_jump_offsets_to_4_byte_offsets_where_needed(
        &mut calls_requiring_updates,
        &mut jumps_requiring_updates,
        &mut label_pos,
        &mut asm);

    update_jumps(&mut jumps_requiring_updates, &mut label_pos,  &mut asm);

    (function.clone(), asm, calls_requiring_updates)
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

fn emit_mov_sign_extending(operand: &UnaryOperation, asm: &mut Vec<u8>) {
    match operand {
        UnaryOperation {
            dest: PhysicalRegister(dest_reg),
            src: StackOffset { size, offset },
        } => {
            match (dest_reg.size(), size) {
                (4, 1) => emit_mov_sign_extend_byte_to_integer_stack(*dest_reg, *offset, *size, asm),
                _ => ice!("Invalid register {:?} and size {}", dest_reg, size),
            }
        }
        _ => ice!("Invalid MOVSX operation:\n{:#?}", operand),
    }
}

fn emit_mov_sign_extend_byte_to_integer_stack(dest: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register: {:?}", dest);
    ice_if!(size != 1, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_SIGN_EXTEND_8_BIT_TO_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_zero_extending(operand: &UnaryOperation, asm: &mut Vec<u8>) {
    match operand {
        UnaryOperation {
            dest: PhysicalRegister(dest_reg),
            src: StackOffset { size, offset },
        } => {
            match (dest_reg.size(), size) {
                (4, 1) => emit_mov_zero_extend_byte_to_integer_stack(*dest_reg, *offset, *size, asm),
                _ => ice!("Invalid register {:?} and size {}", dest_reg, size),
            }
        }
        _ => ice!("Invalid MOVSX operation:\n{:#?}", operand),
    }
}

fn emit_mov_zero_extend_byte_to_integer_stack(dest: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register: {:?}", dest);
    ice_if!(size != 1, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_ZERO_EXTEND_8_BIT_TO_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov(operand: &UnaryOperation, asm: &mut Vec<u8>) {
    match operand {
        UnaryOperation{
            dest: StackOffset { offset, size: _},
            src: ByteConstant(value),
        } => {
            emit_mov_byte_to_stack(
                *offset,
                *value,
                asm);
        },
        UnaryOperation{
            dest: StackOffset {offset, size: _ } ,
            src: IntegerConstant(value)} => {
            emit_mov_integer_to_stack(*offset, *value, asm);
        },
        UnaryOperation {
            dest: PhysicalRegister(dest_reg),
            src: PhysicalRegister(src_reg),
        } => {
            match dest_reg.size() {
                4 | 8 => emit_mov_integer_reg_to_reg(*dest_reg, *src_reg, asm),
                _ => ice!("Invalid register size {}", dest_reg.size()),
            }
        },
        UnaryOperation{
            dest: PhysicalRegister(ref reg),
            src: StackOffset {offset, size},
        } => {
            ice_if!(reg.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", reg.size(), size);
            match size {
                1 => emit_mov_byte_from_stack_to_reg(*reg, *offset, asm),
                4 | 8 => emit_mov_integer_from_stack_to_reg(*reg, *offset, asm),
                _ => ice!("Unexpected stack slot size {}", size),
            }
        },
        UnaryOperation {
            dest: StackOffset {offset, size},
            src: PhysicalRegister(ref reg)
        } => {
            ice_if!(reg.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", reg.size(), size);
            match *size {
                1 => emit_mov_byte_from_reg_to_stack(*reg, *offset, asm),
                4 | 8 => emit_mov_integer_from_reg_to_stack(*reg, *offset, asm),
                _ => ice!("Unexpected stack slot size {}", size),
            }
        },
        UnaryOperation {
            dest: PhysicalRegister(ref reg),
            src: IntegerConstant(value),
        } => {
            emit_mov_integer_to_register(*value, *reg, asm);
        },
        UnaryOperation {
            dest: PhysicalRegister(ref reg),
            src: ByteConstant(value),
        } => {
            emit_mov_byte_to_register(*value, *reg, asm);
        }
        UnaryOperation {
            dest: StackOffset {offset, size },
            src: ComparisonResult(comparison_type),
        } => {
            emit_mov_comp_result_into_stack(comparison_type, *offset, *size, asm);
        },
        UnaryOperation{
            dest: DynamicStackOffset { id: _, index, offset, size: _, },
            src: IntegerConstant(immediate)
        } => {
            match **index {
                PhysicalRegister(reg) => emit_mov_integer_to_stack_reg_indexed_with_offset(
                    reg,
                    *offset,
                    *immediate,
                    asm),
                _ => ice!("Invalid MOV operation:\n{:#?}", operand),
            }
        },
        UnaryOperation{
            dest: DynamicStackOffset { id: _, index, offset, size: _, },
            src: ByteConstant(immediate)
        } => {
            match **index {
                PhysicalRegister(reg) => emit_mov_byte_to_stack_reg_indexed_with_offset(
                    reg,
                    *offset,
                    *immediate,
                    asm),
                _ => ice!("Invalid MOV operation:\n{:#?}", operand),
            }
        },
        UnaryOperation{
            dest: DynamicStackOffset { id: _, index, offset, size, },
            src: PhysicalRegister(src)
        } => {
            ice_if!(src.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", src.size(), size);
            match (&**index, size) {
                (PhysicalRegister(reg), 4) => emit_mov_integer_register_to_stack_reg_indexed_with_offset(
                    reg.clone(),
                    *offset,
                    *src,
                    asm),
                (PhysicalRegister(reg), 1) => emit_mov_byte_register_to_stack_reg_indexed_with_offset(
                    reg.clone(),
                    *offset,
                    *src,
                    asm),
                _ => ice!("Invalid MOV operation:\n{:#?}", operand),
            }
        }
        UnaryOperation {
            dest: PhysicalRegister(dest),
            src: DynamicStackOffset { id: _, index, offset, size },
        } => {
            ice_if!(dest.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", dest.size(), size);
            match (&**index, *size) {
                (PhysicalRegister(reg), 4) => emit_mov_integer_stack_reg_indexed_with_offset_to_register(
                    reg.clone(),
                    *offset,
                    *dest,
                    asm),
                (PhysicalRegister(reg), 1) => emit_mov_byte_stack_reg_indexed_with_offset_to_register(
                    reg.clone(),
                    *offset,
                    *dest,
                    asm),
                _ => ice!("Invalid MOV operation:\n{:#?}", operand),
            }
        },
        UnaryOperation {
            dest: PhysicalRegister(dest),
            src: IndirectAddress {
                base, // FIXME replace with box pattern matching once it becomes available
                index: None,
                offset: Some(offset),
                size,
            }
        } => {
            ice_if!(dest.size()  as u32 != *size, "Register and stack slot sizes are different: {} vs {}", dest.size(), size);
            let base_reg = if let PhysicalRegister(reg) = **base {
                reg
            } else {
                ice!("Base pointer is not a register: {} ", base);
            };

            match *size {
                1 => emit_mov_indirect_byte_to_register(base_reg, *offset, dest.clone(), asm),
                4 | 8 => emit_mov_indirect_integer_to_register(base_reg, *offset, dest.clone(),  asm),
                _ => ice!("Unexpected stack slot size {}", size),
            }
        },
        UnaryOperation {
            dest: PhysicalRegister(dest),
            src: IndirectAddress {
                base, // FIXME replace with box pattern matching once it becomes available
                index: Some(index),
                offset: None,
                size,
            }
        } => {
            ice_if!(dest.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", dest.size(), size);
            let base_reg = if let PhysicalRegister(reg) = **base {
                reg
            } else {
                ice!("Base pointer is not a register: {} ", base);
            };

            match (&**index, *size) {
                (PhysicalRegister(index), 1) =>
                    emit_mov_indirect_byte_indexed_to_reg(base_reg, index.clone(), dest.clone(), asm),
                (PhysicalRegister(index), 4) =>
                    emit_mov_indirect_integer_indexed_to_reg(base_reg, index.clone(), dest.clone(), asm),
                _ => ice!("Invalid MOV operand size {}, with operand\n{:#?}", size, operand),
            }
        },
        UnaryOperation {
            dest: IndirectAddress {
                base, // FIXME replace with box pattern matching once it becomes available
                index: Some(index),
                offset: None,
                size,
            },
            src: PhysicalRegister(dest),
        } => {
            ice_if!(dest.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", dest.size(), size);
            let base_reg = if let PhysicalRegister(reg) = **base {
                reg
            } else {
                ice!("Base pointer is not a register: {} ", base);
            };

            match (&**index, *size) {
                (PhysicalRegister(index), 1) =>
                    emit_mov_reg_to_indirect_byte_indexed(base_reg, index.clone(), dest.clone(), asm),
                (PhysicalRegister(index), 4) =>
                    emit_mov_reg_to_indirect_integer_indexed(base_reg, index.clone(), dest.clone(), asm),
                _ => ice!("Invalid MOV size {} and operand \n{:#?}", size, operand),
            }
        },
        UnaryOperation {
            dest: IndirectAddress {
                base, // FIXME replace with box pattern matching once it becomes available
                index: None,
                offset: Some(offset),
                size,
            },
            src: PhysicalRegister(src),
        } => {
            ice_if!(src.size() as u32 != *size, "Register and stack slot sizes are different: {} vs {}", src.size(), size);
            let base_reg = if let PhysicalRegister(reg) = **base {
                reg
            } else {
                ice!("Base pointer is not a register: {} ", base);
            };

            match *size {
                1 => emit_mov_reg_to_indirect_byte(base_reg, *offset, src.clone(),  asm),
                4 | 8 => emit_mov_reg_to_indirect_integer(base_reg, *offset, src.clone(),  asm),
                _ => ice!("Unexpected stack slot size {}", size),
            }
        },
        _ => ice!("Invalid MOV operation:\n{:#?}", operand),
    }
}

fn emit_lea(operand: &UnaryOperation, asm: &mut Vec<u8>) {
    match operand {
        UnaryOperation {
            dest: PhysicalRegister(dest),
            src: StackOffset { offset, size, }
        } => {
            if *size != 8 {
                ice!("LEA not implemented for non-pointer types");
            }
            emit_lea_stack_address_to_reg(*offset, dest.clone(), asm);
        },
        _ => ice!("Invalid LEA operation:\n{:#?}", operand),
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

/*
    MOV BYTE PTR [RBP - offset], imm32

    Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, opcode extension in reg field, dst in r/m
    sib: not used. sib struct is used to store one or four byte displacement, depending if offset is less or equal to 128 or not
    immediate: the 32 bit immediate
*/

fn emit_mov_byte_to_stack(offset: u32, immediate: i8, asm: &mut Vec<u8>) {

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
        SizedOpCode::from(MOV_IMMEDIATE_8_BIT_TO_RM),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}




/*
    Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, opcode extension in reg field, dst in r/m
    sib: yes
    immediate: the 32 bit immediate
*/
fn emit_mov_integer_to_stack_reg_indexed_with_offset(index: X64Register, displacement: u32, immediate: i32, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::Four),
            Some(X64Register::RBP),
            Some(index),
            Some(displacement));


    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(0),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
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

/*
    Rex prefix: Not used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, opcode extension in reg field, dst in r/m
    sib: yes
    immediate: the 32 bit immediate
*/
fn emit_mov_byte_to_stack_reg_indexed_with_offset(index: X64Register, displacement: u32, immediate: i8, asm: &mut Vec<u8>) {

    ice_if!(index.is_extended_reg(), "REX encoding not used and as such index register {:?} not encodable", index);
    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::One),
            Some(X64Register::RBP),
            Some(index),
            Some(displacement));


    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(0),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };

    emit_instruction(
        asm,
        None,
        SizedOpCode::from(MOV_IMMEDIATE_8_BIT_TO_RM),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

/*
    Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: addressing mode depends on SIB byte, src reg in reg field, RM field contains RSP encoding to signify SIB byte is used
    sib: yes
    immediate: no
*/
fn emit_mov_integer_register_to_stack_reg_indexed_with_offset(index: X64Register, displacement: u32, src: X64Register, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::Four),
            Some(X64Register::RBP),
            Some(index),
            Some(displacement));


    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
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
    Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: addressing mode depends on SIB byte, src reg in reg field, RM field contains RSP encoding to signify SIB byte is used
    sib: yes
    immediate: no
*/
fn emit_mov_byte_register_to_stack_reg_indexed_with_offset(index: X64Register, displacement: u32, src: X64Register, asm: &mut Vec<u8>) {

    ice_if!(src.is_extended_reg(), "REX encoding not used and as such source register {:?} not encodable", src);
    ice_if!(index.is_extended_reg(), "REX encoding not used and as such index register {:?} not encodable", index);
    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::One),
            Some(X64Register::RBP),
            Some(index),
            Some(displacement));


    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };

    emit_instruction(
        asm,
        None,
        SizedOpCode::from(MOV_REG_TO_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


/*
Rex prefix: If 64 bit regs used
    opcode: 1 byte,
    modrm: addressing mode depends on SIB byte, dest reg in reg field, RM field contains RSP encoding to signify SIB byte is used
    sib: yes
    immediate: no
*/
fn emit_mov_integer_stack_reg_indexed_with_offset_to_register(index: X64Register, displacement: u32, dest: X64Register, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::Four),
            Some(X64Register::RBP),
            Some(index),
            Some(displacement));


    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
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
/*
    Rex prefix: None
    opcode: 1 byte,
    modrm: addressing mode depends SIB byte, dest reg in reg field, RM field contains RSP encoding to signify SIB byte is used
    sib: yes
    immediate: no
*/
fn emit_mov_byte_stack_reg_indexed_with_offset_to_register(index: X64Register, displacement: u32, dest: X64Register, asm: &mut Vec<u8>) {

    ice_if!(dest.is_extended_reg(), "REX encoding not used and as such register {:?} not encodable", dest);
    ice_if!(index.is_extended_reg(), "REX encoding not used and as such index register {:?} not encodable", index);

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::One),
            Some(X64Register::RBP),
            Some(index),
            Some(displacement));


    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };

    emit_instruction(
        asm,
        None,
        SizedOpCode::from(MOV_RM_TO_REG_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


/* MOV r32, imm32
    Rex prefix: No
    opcode: 1 byte, dst reg encoded in low bits
    modrm: no
    sib: no
    immediate: the 32 bit immediate
*/
fn emit_mov_integer_to_register(immediate: i32, register: X64Register, asm: &mut Vec<u8>)  {
    /*
        Modrm is not really used by this instruction, just used to get correct reg encoding in REX
        prefix
        */

    let rex_modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing, // not used
        reg_field: RegField::Unused,
        rm_field: RmField::Register(register),
    };

    let rex = create_rex_prefix(false, Some(rex_modrm), None);



    let immediate = Immediate::from(immediate);

    // register encoded in the opcode itself
    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_IMMEDIATE_32_BIT_TO_REG_BASE | register.encoding()),
        None,
        None,
        Some(immediate),
    )
}

/* MOV reg, imm8
    Rex prefix: No
    opcode: 1 byte, dst reg encoded in low bits
    modrm: no
    sib: no
    immediate: the 8 bit immediate
*/

fn emit_mov_byte_to_register(immediate: i8, register: X64Register, asm: &mut Vec<u8>) {
     /*
        Modrm is not really used by this instruction, just used to get correct reg encoding in REX
        prefix
    */

    let immediate = Immediate::from(immediate);

    // register encoded in the opcode itself
    emit_instruction(
        asm,
        None,
        SizedOpCode::from(MOV_IMMEDIATE_8_BIT_TO_REG_BASE | register.encoding()),
        None,
        None,
        Some(immediate),
    )
}


/*
    MOV r32, r32
    Rex prefix: if 64 bit registers used, if r8-r15 used
    opcode: 1 byte
    modrm: direct register addressing, src in reg field, dest in rm field
    sib: no
    immediate: no

*/
fn emit_mov_integer_reg_to_reg(dest: X64Register, src: X64Register, asm: &mut Vec<u8>) {

    ice_if!(dest.size() < 4, "Invalid register: {:?}", dest);
    ice_if!(dest.size() != src.size(), "Mismatching reg sizes: src: {:?} dest: {:?}",  src, dest);

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
    MOV reg, QWORD/DWORD PTR [RBP - offset]

    Rex prefix: If 64 bit regs used, if extended regs are used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, rbp in rm field, dest reg in reg field
    sib: unused, sib struct used to pass displacement
    immediate: none
*/
fn emit_mov_integer_from_stack_to_reg(dest: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register: {:?}", dest);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_RM_TO_REG_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}


/*
    MOV reg, BYTE PTR [RBP - offset]

    Rex prefix: if extended regs are used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, rbp in rm field, dest reg in reg field
    sib: unused, sib struct used to pass displacement
    immediate: none
*/
fn emit_mov_byte_from_stack_to_reg(dest: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() != 1, "Invalid register: {:?}", dest);

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
        SizedOpCode::from(MOV_RM_TO_REG_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


/*
    MOV QWORD/DWORD PTR [RBP - offset], reg

    Rex prefix: If 64 bit regs used, if extended regs are used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, rbp in rm field, dest reg in reg field
    sib: unused, sib struct used to pass displacement
    immediate: none
*/
fn emit_mov_integer_from_reg_to_stack(src: X64Register, offset: u32, asm: &mut Vec<u8>) {

    ice_if!(src.size() < 4, "Invalid register: {:?}", src);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

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
    MOV QWORD/DWORD PTR [RBP - offset], reg

    Rex prefix: if extended regs are used
    opcode: 1 byte,
    modrm: indirect addressing with one byte displacement, rbp in rm field, dest reg in reg field
    sib: unused, sib struct used to pass displacement
    immediate: none
*/
fn emit_mov_byte_from_reg_to_stack(src: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(src.size() != 1, "Invalid register: {:?}", src);

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
        SizedOpCode::from(MOV_REG_TO_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_mov_indirect_integer_to_register(base_reg: X64Register, offset: u32, dest: X64Register, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register {:?}", dest);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(base_reg)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_RM_TO_REG_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_indirect_byte_to_register(base_reg: X64Register, offset: u32, dest: X64Register, asm: &mut Vec<u8>) {
    ice_if!(dest.size() != 1, "Invalid register {:?}", dest);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(base_reg)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_RM_TO_REG_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_mov_indirect_integer_indexed_to_reg(base: X64Register, index: X64Register, dest: X64Register, asm: &mut Vec<u8>) {

    ice_if!(!dest.size() < 4, "Invalid register {:?}", dest);

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::Four),
            Some(base),
            Some(index),
            None);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };
    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_RM_TO_REG_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_mov_indirect_byte_indexed_to_reg(base: X64Register, index: X64Register, dest: X64Register, asm: &mut Vec<u8>) {

    ice_if!(dest.size() != 1, "Invalid register {:?}", dest);

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::One),
            Some(base),
            Some(index),
            None);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };
    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_RM_TO_REG_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_reg_to_indirect_integer(base_reg: X64Register, offset: u32, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(src.size() < 4, "Invalid register {:?}", src);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(base_reg)
    };

    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_REG_TO_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_reg_to_indirect_byte(base_reg: X64Register, offset: u32, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(src.size() != 1, "Invalid register {:?}", src);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(base_reg)
    };

    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_REG_TO_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_mov_reg_to_indirect_integer_indexed(base: X64Register, index: X64Register, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(src.size() < 4, "Invalid register {:?}", src);

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::Four),
            Some(base),
            Some(index),
            None);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };
    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_REG_TO_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_reg_to_indirect_byte_indexed(base: X64Register, index: X64Register, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(src.size() != 1, "Invalid register {:?}", src);

    let (addressing_mode, sib) =
        get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
            Some(Scale::One),
            Some(base),
            Some(index),
            None);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RSP), // signifies SIB byte is present
    };
    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(MOV_REG_TO_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}




fn emit_lea_stack_address_to_reg(offset: u32, dest: X64Register, asm: &mut Vec<u8>) {


    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(true, Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(LEA_ADDR_TO_REG_64_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_mov_comp_result_into_stack(comparison_type: &ComparisonType, offset: u32, _size: u32, asm: &mut Vec<u8>) {

    let opcode = match comparison_type {
        ComparisonType::Less => SET_BYTE_IF_LESS,
        ComparisonType::LessOrEq => SET_BYTE_IF_LESS_OR_EQ,
        ComparisonType::Equals => SET_BYTE_IF_EQ,
        ComparisonType::NotEquals => SET_BYTE_IF_NEQ,
        ComparisonType::GreaterOrEq => SET_BYTE_IF_GREATER_OR_EQ,
        ComparisonType::Greater => SET_BYTE_IF_GREATER,
    };


    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(SET_BYTE_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_mov_comp_result_into_reg(comparison_type: &ComparisonType, reg: X64Register, asm: &mut Vec<u8>) {

    let opcode = match comparison_type {
        ComparisonType::Less => SET_BYTE_IF_LESS,
        ComparisonType::LessOrEq => SET_BYTE_IF_LESS_OR_EQ,
        ComparisonType::Equals => SET_BYTE_IF_EQ,
        ComparisonType::NotEquals => SET_BYTE_IF_NEQ,
        ComparisonType::GreaterOrEq => SET_BYTE_IF_GREATER_OR_EQ,
        ComparisonType::Greater => SET_BYTE_IF_GREATER,
    };



    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(SET_BYTE_OPCODE_EXT),
        rm_field: RmField::Register(reg)
    };

    let rex = create_rex_prefix(false, Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        None,
    );
}
fn emit_add(operand: &BinaryOperation, asm: &mut Vec<u8>) {

    match operand {
        BinaryOperation{
            dest: PhysicalRegister(dest_reg),
            src1: PhysicalRegister(src_reg),
            src2: IntegerConstant(value)
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 registers are not in two address form: {:?} vs {:?}", dest_reg, src_reg);

            match dest_reg.size() {
                4 => emit_add_immediate_to_register(*dest_reg, *value, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation{
            dest: PhysicalRegister(dest_reg),
            src1: PhysicalRegister(src_reg),
            src2: PhysicalRegister(src2_reg)
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 registers are not in two address form: {:?} vs {:?}", dest_reg, src_reg);

            match dest_reg.size() {
                4 | 8 => emit_add_integer_reg_reg(*dest_reg, *src2_reg, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation{
            dest: StackOffset{offset: dest_offset, size: dest_size },
            src1: StackOffset{offset: src_offset, size: src_size },
            src2: IntegerConstant(value)
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            match src_size {
                4 => emit_add_integer_immediate_to_stack(*dest_offset, *dest_size, *value, asm),
                _ => ice!("Invalid size {}", src_size),
            }
        },
        BinaryOperation{
            dest: StackOffset{offset: dest_offset, size: dest_size },
            src1: StackOffset{offset: src_offset, size: src_size },
            src2: ByteConstant(value)
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            match src_size {
                1 => emit_add_byte_immediate_to_stack(*dest_offset, *dest_size, *value, asm),
                _ => ice!("Invalid size {}", src_size),
            }
        },
        BinaryOperation{
            dest: StackOffset {offset: dest_offset, size: dest_size},
            src1: StackOffset {offset: src_offset, size: src_size},
            src2: PhysicalRegister(ref reg),
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            match src_size {
                1 => emit_add_byte_reg_to_stack(*reg, *dest_offset, asm),
                4 | 8 => emit_add_integer_reg_to_stack(*reg, *dest_offset, asm),
                _ => ice!("Invalid size {}", src_size)
            }
        },
        BinaryOperation{
            dest: PhysicalRegister(ref dest_register),
            src1: PhysicalRegister(ref src_register),
            src2: StackOffset {offset, size}
        } => {
            ice_if!(
                dest_register != src_register,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            match size {
                1 => emit_add_stack_to_byte_reg(*dest_register, *offset,  asm),
                4 | 8 => emit_add_stack_to_integer_reg(*dest_register, *offset,  asm),
                _ => ice!("Invalid size {}", size),
            }

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

    let (imm, opcode) = if immediate_fits_in_8_bits(immediate) {
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
    ADD DWORD/QWORD PTR [RBP-offset], imm32

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128
    sib: byte not used, struct used to pass displacement
    immediate: the 32 bit immediate value

*/
fn emit_add_integer_immediate_to_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size != 4, "Invalid stack slot size {}", size);

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
    ADD BYTE PTR [RBP-offset], imm32

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128
    sib: byte not used, struct used to pass displacement
    immediate: the 8 bit immediate value

*/
fn emit_add_byte_immediate_to_stack(offset: u32, size: u32, immediate: i8, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid stack slot size {}", size);

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
        SizedOpCode::from(ADD_IMMEDIATE_8_BIT_TO_RM),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

/*
    ADD reg, DWORD/QWORD PTR [RBP-offset]

    REX: yes, rbp used.
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination reg encoded
    sib: byte not used, struct used to pass displacement
    immediate: no


*/
fn emit_add_stack_to_integer_reg(dest: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register {:?}", dest);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };


    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

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
    ADD reg32/64, rm/32/64 as reg

    REX: yes Extended regs may be used, may be 8 byte operand size
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination reg encoded
    sib: byte not used, struct used to pass displacement
    immediate: no


*/
fn emit_add_integer_reg_reg(dest: X64Register, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register {:?}", dest);
    ice_if!(src.size() < 4, "Invalid register {:?}", src);


    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(src)
    };


    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(ADD_RM_TO_REG_32_BIT),
        Some(modrm),
        None,
        None
    );
}


/*
    ADD reg, BYTE PTR [RBP-offset]

    REX: yes, rbp used.
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination reg encoded
    sib: byte not used, struct used to pass displacement
    immediate: no


*/
fn emit_add_stack_to_byte_reg(dest: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() != 1, "Invalid register {:?}", dest);
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
        SizedOpCode::from(ADD_RM_TO_REG_8_BIT),
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
fn emit_add_integer_reg_to_stack(src: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(src.size() < 4, "Invalid register {:?}", src);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };


    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(ADD_REG_TO_RM_32_BIT),
        Some(modrm),
        sib,
        None
    );
}

fn emit_add_byte_reg_to_stack(src: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(src.size() != 1, "Invalid register {:?}", src);

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
        SizedOpCode::from(ADD_REG_TO_RM_8_BIT),
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
                src2: ByteConstant(immediate)
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            ice_if!(*src_size != 1, "Invalid size {}", src_size);
            emit_sub_byte_immediate_from_stack(*dest_offset, *dest_size, *immediate, asm)
        },
        BinaryOperation{
                dest: StackOffset{offset: dest_offset, size: dest_size },
                src1: StackOffset{offset: src_offset, size: src_size },
                src2: IntegerConstant(immediate)
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            ice_if!(*src_size != 4, "Invalid size {}", src_size);
            emit_sub_integer_immediate_from_stack(*dest_offset, *dest_size, *immediate, asm)
        },
        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: StackOffset { offset, size},
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operand);

            match size {
               1 => emit_sub_byte_stack_from_reg(*dest_reg, *offset, asm),
               4 | 8 => emit_sub_integer_stack_from_reg(*dest_reg, *offset, asm),
               _ => ice!("Invalid size {}", size),
            }

        },
        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: PhysicalRegister( ref src2_reg ),
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            match dest_reg.size() {
               1 => emit_sub_byte_reg_reg(*dest_reg, *src2_reg, asm),
               4 | 8 => emit_sub_integer_reg_reg(*dest_reg, *src2_reg, asm),
               _ => ice!("Invalid size {}", dest_reg.size()),
            }

        },
        BinaryOperation{
            dest: StackOffset {offset: dest_offset, size: dest_size},
            src1: StackOffset {offset: src_offset, size: src_size},
            src2: PhysicalRegister(ref src_reg),
        } => {
            ice_if!(
                dest_offset != src_offset || dest_size != src_size,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            match src_size {
                1 => emit_sub_byte_reg_from_stack(*src_reg, *dest_offset, asm),
                4 | 8 => emit_sub_integer_reg_from_stack(*src_reg, *dest_offset, asm),
                _ => ice!("Invalid size {}", src_size),
            }
        },
        _ => ice!("Invalid sub operation encoding: {:#?}", operand),
    }
}
/*
    SUB QWORD/DWORD PTR [rbp-offset], imm32

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128
    sib: byte not used, struct used to pass displacement
    immediate: the 32 bit immediate value
*/

fn emit_sub_integer_immediate_from_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size != 4, "Invalid stack slot size {}", size);
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
    SUB BYTE PTR [rbp-offset], imm8

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128
    sib: byte not used, struct used to pass displacement
    immediate: the 8 bit immediate value
*/

fn emit_sub_byte_immediate_from_stack(offset: u32, size: u32, immediate: i8, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid stack slot size {}", size);
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
        SizedOpCode::from(SUB_IMMEDIATE_8_BIT_FROM_RM),
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


    let (imm, opcode) = if immediate_fits_in_8_bits(immediate) {
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
    SUB reg, QWORD/DWORD [rbp-offset]

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination register encoded
    sib: byte not used, struct used to pass displacement
    immediate: not used
*/
fn emit_sub_integer_stack_from_reg(dest: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register {:?}", dest);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), sib);

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
    SUB reg, BYTE [rbp-offset]

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Destination register encoded
    sib: byte not used, struct used to pass displacement
    immediate: not used
*/
fn emit_sub_byte_stack_from_reg(dest: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest.size() != 1, "Invalid register {:?}", dest);
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
        SizedOpCode::from(SUB_RM_8_FROM_REG),
        Some(modrm),
        sib,
        None
    );
}

/*
    SUB reg8, rm8 as reg

    REX: Yes, extended regs may be be used as src or dest
    opcode: 8 bits
    modrm: Direct register addressing
    sib: not used
    immediate: not used
*/
fn emit_sub_byte_reg_reg(dest: X64Register, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(dest.size() != 1, "Invalid register {:?}", dest);
    ice_if!(src.size() != 1, "Invalid register {:?}", src);

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(src)
    };

    let rex = create_rex_prefix(false, Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_RM_8_FROM_REG),
        Some(modrm),
        None,
        None
    );
}

/*
    SUB reg32/64, rm32/64 as reg

    REX: Yes, extended regs may be be used as src or dest, operand size may be 8 byte
    opcode: 8 bits
    modrm: Direct register addressing
    sib: not used
    immediate: not used
*/
fn emit_sub_integer_reg_reg(dest: X64Register, src: X64Register, asm: &mut Vec<u8>) {
    ice_if!(dest.size() < 4, "Invalid register {:?}", dest);
    ice_if!(src.size() < 4, "Invalid register {:?}", src);

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::Register(dest),
        rm_field: RmField::Register(src)
    };

    let rex = create_rex_prefix(dest.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_RM_32_FROM_REG),
        Some(modrm),
        None,
        None
    );
}


/*
    SUB QWORD/DWORD [rbp-offset], reg

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    sib: byte not used, struct used to pass displacement
    immediate: not used
*/
fn emit_sub_integer_reg_from_stack(src: X64Register, offset: u32,  asm: &mut Vec<u8>) {
    ice_if!(src.size() < 4, "Invalid register {:?}", src);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_REG_FROM_RM_32_BIT),
        Some(modrm),
        sib,
        None
    );
}

/*
    SUB BYTE [rbp-offset], reg

    REX: yes, RBP reg used
    opcode: 8 bits
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    sib: byte not used, struct used to pass displacement
    immediate: not used
*/
fn emit_sub_byte_reg_from_stack(src: X64Register, offset: u32,  asm: &mut Vec<u8>) {
    ice_if!(src.size() != 1, "Invalid register {:?}", src);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::Register(src),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(src.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SUB_REG_FROM_RM_8_BIT),
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
            ice_if!(dest_reg.size() != src_reg.size(), "Registers {:?} and {:?} have different sizes", dest_reg, src_reg);
            emit_mul_integer_reg_with_immediate(*dest_reg, *src_reg, *immediate, asm);
        },
        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: StackOffset {offset, size},
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            match size {
                4 | 8 => emit_mul_integer_reg_with_stack(*dest_reg, *offset, asm),
                _ => ice!("Invalid size {}", size),
            }
        },
        BinaryOperation {
            dest: PhysicalRegister(ref dest_reg),
            src1: PhysicalRegister(ref src_reg),
            src2: PhysicalRegister(ref src2_reg),
        } => {
            ice_if!(
                dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operand);
            match dest_reg.size() {
                4 | 8 => emit_mul_integer_reg_with_reg(*dest_reg, *src2_reg, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        _ => ice!("Invalid mul operation encoding: {:#?}", operand),
    }
}


/*
    IMUL dst_reg, src_reg, immediate32/immediate8

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in reg, src in R/M
    SIB: Not used
    Immediate: 8/32 bit immediate, depending on if the value fits in 8 bits
*/

fn emit_mul_integer_reg_with_immediate(dest_reg: X64Register, src_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid destination register {:?}", dest_reg);
    ice_if!(src_reg.size() < 4, "Invalid source register {:?}", src_reg);

    let (imm, opcode) = if immediate_fits_in_8_bits(immediate) {
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


/*
    IMUL dst_reg, src_reg, immediate32/immediate8

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in reg, src in R/M
    SIB: Not used
    Immediate: Not used
*/

fn emit_mul_integer_reg_with_stack(dest_reg: X64Register, offset: u32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid register {:?}", dest_reg);
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

fn emit_mul_integer_reg_with_reg(dest_reg: X64Register, src_reg: X64Register, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid destination register {:?}", dest_reg);
    ice_if!(src_reg.size() < 4, "Invalid source register {:?}", src_reg);


    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::Register(dest_reg),
        rm_field: RmField::Register(src_reg),
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SIGNED_MUL_REG_RM_32_BIT),
        Some(modrm),
        None,
        None,
    );
}


fn emit_div(operand: &BinaryOperation, asm: &mut Vec<u8>) {
    match operand {
        BinaryOperation{
            dest: _, // don't care, will be stored in eax,
            src1: _, // don't care, uses edx:eax
            src2: PhysicalRegister(ref reg),
        } => {
            match reg.size() {
                4 | 8 => emit_integer_div_with_reg(*reg, asm),
                _ => ice!("Invalid size {}", reg.size()),
            }
        },
        BinaryOperation{
            dest: _,
            src1: _,
            src2: StackOffset {
                offset,
                size,
            }
        } => {
            match size {
                4 | 8 => emit_integer_div_with_stack(*offset, *size, asm),
                _ => ice!("Invalid size {}", size),
            }
        },
        _ => ice!("Invalid div operation encoding: {:#?}", operand),
    }
}

fn emit_integer_div_with_reg(divisor: X64Register, asm: &mut Vec<u8>) {
    ice_if!(divisor.size() < 4, "Invalid register {:?}", divisor);

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

fn emit_integer_div_with_stack(offset: u32, size: u32, asm: &mut Vec<u8>) {

    ice_if!(size < 4, "Invalid size {}", size);

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

fn emit_neg(operands: &UnaryOperation, asm: &mut Vec<u8>) {
    match operands {
        UnaryOperation{
            src: StackOffset {
                size: src_size,
                offset: src_offset,
            },
            dest: StackOffset {
                size: dest_size,
                offset: dest_offset,
            }
        } if src_offset == dest_offset => {
            ice_if!(src_size != dest_size, "Source and destination sizes are different");
            match src_size {
                1 => emit_neg_byte_stack(*src_offset, *src_size, asm),
                4 | 8 => emit_neg_integer_stack(*src_offset, *src_size, asm),
                _ => ice!("Invalid size {}", src_size),
            }
        },
        _ => ice!("Invalid operand encoding for NEG: {:#?}", operands),
    }
}

fn emit_neg_integer_stack(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(NEGATE_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(NEGATE_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_neg_byte_stack(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(NEGATE_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP)
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(NEGATE_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_and(operands: &BinaryOperation, asm: &mut Vec<u8>) {
    match operands {
        BinaryOperation {
            dest: PhysicalRegister(dest_reg),
            src1: PhysicalRegister(src_reg),
            src2: StackOffset { offset, size },
        } => {
            ice_if!(dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operands);
            ice_if!(dest_reg.size() as u32 != *size, "Source and destination sizes are different: {:#?}", operands);

            match *size {
                4 | 8 => emit_and_integer_stack_with_reg(*dest_reg, *offset, *size, asm),
                _ => ice!("Invalid size {}", size),
            }
        },
        BinaryOperation {
            dest: PhysicalRegister(dest_reg),
            src1: PhysicalRegister(src_reg),
            src2: IntegerConstant(immediate),
        } => {
            ice_if!(dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operands);

            match dest_reg.size() {
                4 | 8 => emit_and_integer_immediate_with_reg(*dest_reg, *immediate, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation {
            dest: PhysicalRegister(dest_reg),
            src1: PhysicalRegister(src_reg),
            src2: ByteConstant(immediate),
        } => {
            ice_if!(dest_reg != src_reg,
                "Destination and src1 operands not in two address form: {:#?}", operands);

            match dest_reg.size() {
                1 => emit_and_byte_immediate_with_reg(*dest_reg, *immediate, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation {
            dest: StackOffset{offset: dest_offset, size: dest_size},
            src1: StackOffset{offset: src1_offset, size: src1_size},
            src2: PhysicalRegister(src_reg),
        } if dest_offset == src1_offset => {
            ice_if!(dest_size != src1_size,
                "Instruction not in two address form: {:#?}", operands);
            ice_if!(src_reg.size() as u32 != *dest_size,
                "Source and destination sizes are different: {:#?}", operands);

            match dest_size {
                4 | 8 => emit_and_integer_reg_with_stack(*src_reg, *dest_offset, *dest_size, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },
        BinaryOperation {
            dest: StackOffset{offset: dest_offset, size: dest_size},
            src1: StackOffset{offset: src1_offset, size: src1_size},
            src2: IntegerConstant(immediate),
        } if dest_offset == src1_offset => {
            ice_if!(dest_size != src1_size,
                "Instruction not in two address form: {:#?}", operands);

            match dest_size {
                4 | 8 => emit_and_integer_immediate_with_stack(*dest_offset, *dest_size, *immediate, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },
        BinaryOperation {
            dest: StackOffset{offset: dest_offset, size: dest_size},
            src1: StackOffset{offset: src1_offset, size: src1_size},
            src2: ByteConstant(immediate),
        } if dest_offset == src1_offset => {
            ice_if!(dest_size != src1_size,
                "Instruction not in two address form: {:#?}", operands);

            match dest_size {
                1 => emit_and_byte_immediate_with_stack(*dest_offset, *dest_size, *immediate, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },
        _ => ice!("Invalid operand encoding for AND: {:#?}", operands),
    }
}

/*
    AND dst_reg, DWORD/QWORD PTR [rbp - offset]

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    SIB: byte not used, struct used to pass displacement
    Immediate: Not used
*/

fn emit_and_integer_stack_with_reg(dest_reg: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid register '{:?}'", dest_reg);
    ice_if!(size < 4, "Invalid size {}", size);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::Register(dest_reg),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(AND_RM_32_BIT_WITH_REG),
        Some(modrm),
        sib,
        None,
    );
}

/*
    AND dst_reg 32/64 bit, immediate

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct register addressing
    SIB: not used.
    Immediate: 8 or 32 bit immediate, depending if immediate fits in 8 bit or not
*/
fn emit_and_integer_immediate_with_reg(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid destination register {:?}", dest_reg);

    let (imm, opcode) = if immediate_fits_in_8_bits(immediate) {
        (Immediate::from(immediate as u8), AND_IMMEDIATE_8_BIT_SIGN_EXTENDED_TO_RM_32_BIT)
    } else {
        (Immediate::from(immediate), AND_IMMEDIATE_32_BIT_TO_RM_32_BIT)
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(AND_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
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

/*
    AND dst_reg/8bit, immediate

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct register addressing
    SIB: not used.
    Immediate: 8 or 32 bit immediate, depending if immediate fits in 8 bit or not
*/
fn emit_and_byte_immediate_with_reg(dest_reg: X64Register, immediate: i8, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() != 1, "Invalid destination register {:?}", dest_reg);

    let imm = Immediate::from(immediate);

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(AND_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(AND_IMMEDIATE_8_BIT_TO_RM_8_BIT),
        Some(modrm),
        None,
        Some(imm),
    );
}



/*
    AND DWORD/QWORD PTR [rbp - offset], src_reg

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    SIB: byte not used, struct used to pass displacement
    Immediate: Not used
*/

fn emit_and_integer_reg_with_stack(src_reg: X64Register, offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(src_reg.size() < 4, "Invalid register '{:?}'", src_reg);
    ice_if!(size < 4, "Invalid size {}", size);
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::Register(src_reg),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(AND_REG_WITH_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

/*
    AND DWORD/QWORD PTR [rbp - offset], immediate

    REX: if 64 bit destination operand is used
    opcode: 8 bit opcode
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    SIB: byte not used, struct used to pass displacement
    Immediate: yes
*/

fn emit_and_integer_immediate_with_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid stack slot size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let (imm, opcode) = if immediate_fits_in_8_bits(immediate) {
        (Immediate::from(immediate as u8), AND_IMMEDIATE_8_BIT_SIGN_EXTENDED_TO_RM_32_BIT)
    } else {
        (Immediate::from(immediate), AND_IMMEDIATE_32_BIT_TO_RM_32_BIT)
    };

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(AND_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        Some(imm),
    );
}


/*
    AND BYTE PTR [rbp - offset], immediate

    REX: if 64 bit destination operand is used
    opcode: 8 bit opcode
    modrm: indirect register addressing with one or four byte displacement, depending if offset <= 128. Source register encoded
    SIB: byte not used, struct used to pass displacement
    Immediate: yes
*/

fn emit_and_byte_immediate_with_stack(offset: u32, size: u32, immediate: i8, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid stack slot size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let imm = Immediate::from(immediate);

    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(AND_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(AND_IMMEDIATE_8_BIT_TO_RM_8_BIT),
        Some(modrm),
        sib,
        Some(imm),
    );
}

fn emit_xor(operands: &BinaryOperation, asm: &mut Vec<u8>) {
    match operands {
        BinaryOperation {
            dest: StackOffset { offset: dest_offset, size: dest_size},
            src1: StackOffset { offset: src_offset, size: src_size},
            src2: IntegerConstant(immediate),
        } if src_offset == dest_offset => {
            ice_if!(src_size != dest_size, "Source and destination sizes are different: {:#?}", operands);
            emit_xor_immediate_with_stack(*dest_offset, *dest_size, *immediate, asm);
        },
        _ => ice!("Invalid operand encoding for XOR: {:#?}", operands),
    }
}

fn emit_xor_immediate_with_stack(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let (opcode, immediate) = if immediate_fits_in_8_bits(immediate)  {
        (XOR_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Immediate::from(immediate as u8))
    } else {
        (XOR_RM_32_BIT_WITH_32_BIT_IMMEDIATE, Immediate::from(immediate))
    };

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::OpcodeExtension(XOR_OPCODE_EXT),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        Some(immediate),
    );
}

fn emit_shl(operands: &BinaryOperation, asm: &mut Vec<u8>) {
    match operands {
        BinaryOperation {
            src1: PhysicalRegister(src_reg),
            src2: IntegerConstant(immediate),
            dest: PhysicalRegister(dest_reg),
        } if dest_reg == src_reg => {
            ice_if!(dest_reg.size() != src_reg.size(), "Source and destination sizes are different");
            match dest_reg.size() {
                1 => emit_shl_byte_reg_with_immediate(*dest_reg, *immediate, asm),
                4 | 8 => emit_shl_integer_reg_with_immediate(*dest_reg, *immediate, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation {
            src1: StackOffset{ size: src_size, offset: src_offset},
            src2: IntegerConstant(immediate),
            dest: StackOffset { size: dest_size, offset: dest_offset},
        } if src_offset == dest_offset => {
            ice_if!(dest_size != src_size, "Source and destination sizes are different");

            match dest_size {
                1 => emit_shl_byte_stack_with_immediate(*dest_offset, *dest_size, *immediate, asm),
                4 | 8 => emit_shl_integer_stack_with_immediate(*dest_offset, *dest_size, *immediate, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },
        BinaryOperation {
            src1: StackOffset{ size: src_size, offset: src_offset},
            src2: PhysicalRegister(reg),
            dest: StackOffset { size: dest_size, offset: dest_offset},
        } if src_offset == dest_offset => {
            ice_if!(dest_size != src_size, "Source and destination sizes are different");
            ice_if!(*reg != X64Register::CL, "Invalid count register {:?} for shifting", reg);

            match dest_size {
                1 => emit_shl_byte_stack_with_reg(*dest_offset, *dest_size, asm),
                4 | 8 => emit_shl_integer_stack_with_reg(*dest_offset, *dest_size, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },

        _ => ice!("Invalid operand encoding for SHL: {:#?}", operands),
    }
}
/*
    shl reg32, imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in r/m, opcode ext in reg
    SIB: Not used
    Immediate: Used if immediate != 1
*/
fn emit_shl_integer_reg_with_immediate(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid register '{:?}'", dest_reg);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHL_RM_32_BIT_ONCE, None)
    } else {
        (SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_LEFT_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        shift_count
    );
}


/*
    shl reg8, imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in r/m, opcode ext in reg
    SIB: Not used
    Immediate: Used if immediate != 1
*/
fn emit_shl_byte_reg_with_immediate(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() != 1, "Invalid register '{:?}'", dest_reg);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHL_RM_8_BIT_ONCE, None)
    } else {
        (SHL_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_LEFT_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(false, Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        shift_count
    );
}


/*
    shl DWORD/QWORD PTR [RBP - offset], imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Used if immediate != 1
*/

fn emit_shl_integer_stack_with_immediate(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHL_RM_32_BIT_ONCE, None)
    } else {
        (SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_LEFT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        shift_count
    );
}

/*
    shl BYTE PTR [RBP - offset], imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Used if immediate != 1
*/

fn emit_shl_byte_stack_with_immediate(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHL_RM_8_BIT_ONCE, None)
    } else {
        (SHL_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_LEFT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        shift_count
    );
}


/*
    shl DWORD/QWORD PTR [RBP - offset], CL

    REX: if 64 bit operands used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Not used
*/

fn emit_shl_integer_stack_with_reg(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_LEFT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SHL_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}


/*
    shl BYTE PTR [RBP - offset], CL

    REX: if 64 bit operands used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Not used
*/
fn emit_shl_byte_stack_with_reg(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_LEFT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SHL_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}

fn emit_sar(operands: &BinaryOperation, asm: &mut Vec<u8>) {
    match operands {
        BinaryOperation {
            src1: PhysicalRegister(src_reg),
            src2: IntegerConstant(immediate),
            dest: PhysicalRegister(dest_reg),
        } if dest_reg == src_reg => {
            ice_if!(dest_reg.size() != src_reg.size(), "Source and destination sizes are different");
            match dest_reg.size() {
                1 => emit_sar_byte_reg_with_immediate(*dest_reg, *immediate, asm),
                4 | 8 => emit_sar_integer_reg_with_immediate(*dest_reg, *immediate, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation {
            src1: StackOffset{ size: src_size, offset: src_offset},
            src2: IntegerConstant(immediate),
            dest: StackOffset { size: dest_size, offset: dest_offset},
        } if src_offset == dest_offset => {
            ice_if!(dest_size != src_size, "Source and destination sizes are different");

            match dest_size {
                1 => emit_sar_byte_stack_with_immediate(*dest_offset, *dest_size, *immediate, asm),
                4 | 8 => emit_sar_integer_stack_with_immediate(*dest_offset, *dest_size, *immediate, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },
        BinaryOperation {
            src1: StackOffset{ size: src_size, offset: src_offset},
            src2: PhysicalRegister(reg),
            dest: StackOffset { size: dest_size, offset: dest_offset},
        } if src_offset == dest_offset => {
            ice_if!(dest_size != src_size, "Source and destination sizes are different");
            ice_if!(*reg != X64Register::CL, "Invalid count register {:?} for shifting", reg);

            match dest_size {
                1 => emit_sar_byte_stack_with_reg(*dest_offset, *dest_size, asm),
                4 | 8 => emit_sar_integer_stack_with_reg(*dest_offset, *dest_size, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },

        _ => ice!("Invalid operand encoding for SHL: {:#?}", operands),
    }
}
/*
    sar reg32, imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in r/m, opcode ext in reg
    SIB: Not used
    Immediate: Used if immediate != 1
*/
fn emit_sar_integer_reg_with_immediate(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid register '{:?}'", dest_reg);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SAR_RM_32_BIT_ONCE, None)
    } else {
        (SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        shift_count
    );
}

/*
    sar reg8, imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in r/m, opcode ext in reg
    SIB: Not used
    Immediate: Used if immediate != 1
*/
fn emit_sar_byte_reg_with_immediate(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() != 1, "Invalid register '{:?}'", dest_reg);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SAR_RM_8_BIT_ONCE, None)
    } else {
        (SAR_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(false, Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        shift_count
    );
}

/*
    sar DWORD/QWORD PTR [RBP - offset], imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Used if immediate != 1
*/

fn emit_sar_integer_stack_with_immediate(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SAR_RM_32_BIT_ONCE, None)
    } else {
        (SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        shift_count
    );
}


/*
    sar BYTE PTR [RBP - offset], imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Used if immediate != 1
*/

fn emit_sar_byte_stack_with_immediate(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SAR_RM_8_BIT_ONCE, None)
    } else {
        (SAR_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        shift_count
    );
}

/*
    shl DWORD/QWORD PTR [RBP - offset], CL

    REX: if 64 bit operands used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Not used
*/

fn emit_sar_integer_stack_with_reg(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SAR_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

/*
    shl BYTE PTR [RBP - offset], CL

    REX: if 64 bit operands used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Not used
*/

fn emit_sar_byte_stack_with_reg(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(ARITHMETIC_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SAR_RM_8_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_shr(operands: &BinaryOperation, asm: &mut Vec<u8>) {
    match operands {
        BinaryOperation {
            src1: PhysicalRegister(src_reg),
            src2: IntegerConstant(immediate),
            dest: PhysicalRegister(dest_reg),
        } if dest_reg == src_reg => {
            ice_if!(dest_reg.size() != src_reg.size(), "Source and destination sizes are different");
            match dest_reg.size() {
                1 => emit_shr_byte_reg_with_immediate(*dest_reg, *immediate, asm),
                4 | 8 => emit_shr_integer_reg_with_immediate(*dest_reg, *immediate, asm),
                _ => ice!("Invalid size {}", dest_reg.size()),
            }
        },
        BinaryOperation {
            src1: StackOffset{ size: src_size, offset: src_offset},
            src2: IntegerConstant(immediate),
            dest: StackOffset { size: dest_size, offset: dest_offset},
        } if src_offset == dest_offset => {
            ice_if!(dest_size != src_size, "Source and destination sizes are different");

            match dest_size {
                1 => emit_shr_byte_stack_with_immediate(*dest_offset, *dest_size, *immediate, asm),
                4 | 8 => emit_shr_integer_stack_with_immediate(*dest_offset, *dest_size, *immediate, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },
        BinaryOperation {
            src1: StackOffset{ size: src_size, offset: src_offset},
            src2: PhysicalRegister(reg),
            dest: StackOffset { size: dest_size, offset: dest_offset},
        } if src_offset == dest_offset => {
            ice_if!(dest_size != src_size, "Source and destination sizes are different");
            ice_if!(*reg != X64Register::CL, "Invalid count register {:?} for shifting", reg);

            match dest_size {
                1 => emit_shr_byte_stack_with_reg(*dest_offset, *dest_size, asm),
                4 | 8 => emit_shr_integer_stack_with_reg(*dest_offset, *dest_size, asm),
                _ => ice!("Invalid size {}", dest_size),
            }
        },

        _ => ice!("Invalid operand encoding for SHL: {:#?}", operands),
    }
}
/*
    shr reg32, imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in r/m, opcode ext in reg
    SIB: Not used
    Immediate: Used if immediate != 1
*/
fn emit_shr_integer_reg_with_immediate(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() < 4, "Invalid register '{:?}'", dest_reg);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHR_RM_32_BIT_ONCE, None)
    } else {
        (SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(dest_reg.is_64_bit_register(), Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        shift_count
    );
}

/*
    shr reg8, imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: direct addressing, destination in r/m, opcode ext in reg
    SIB: Not used
    Immediate: Used if immediate != 1
*/
fn emit_shr_byte_reg_with_immediate(dest_reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(dest_reg.size() != 1, "Invalid register '{:?}'", dest_reg);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHR_RM_8_BIT_ONCE, None)
    } else {
        (SHR_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let modrm = ModRM {
        addressing_mode: AddressingMode::DirectRegisterAddressing,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(dest_reg),
    };

    let rex = create_rex_prefix(false, Some(modrm), None);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        None,
        shift_count
    );
}

/*
    shr DWORD/QWORD PTR [RBP - offset], imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Used if immediate != 1
*/

fn emit_shr_integer_stack_with_immediate(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHR_RM_32_BIT_ONCE, None)
    } else {
        (SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        shift_count
    );
}

/*
    shr BYTE PTR [RBP - offset], imm8

    Will be replaced with shift_once instruction if imm == 1

    REX: if 64 bit registers are used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Used if immediate != 1
*/

fn emit_shr_byte_stack_with_immediate(offset: u32, size: u32, immediate: i32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);
    ice_if!(immediate > 255 || immediate < 0, "Invalid shift count {}", immediate);

    let (opcode, shift_count)  = if immediate == 1 {
        (SHR_RM_8_BIT_ONCE, None)
    } else {
        (SHR_RM_8_BIT_WITH_8_BIT_IMMEDIATE, Some(Immediate::from(immediate as u8)))
    };

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(opcode),
        Some(modrm),
        sib,
        shift_count
    );
}


/*
    shr DWORD/QWORD PTR [RBP - offset], CL

    REX: if 64 bit operands used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Not used
*/

fn emit_shr_integer_stack_with_reg(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size < 4, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SHR_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}

/*
    shr BYTE PTR [RBP - offset], CL

    REX: if 64 bit operands used, if extended registers are used
    opcode: 8 bit opcode
    modrm: rbp in r/m opcode ext in reg. one or four byte displacement only addressing, depending on offset
    SIB: Yes, displacement only addressing
    Immediate: Not used
*/

fn emit_shr_byte_stack_with_reg(offset: u32, size: u32, asm: &mut Vec<u8>) {
    ice_if!(size != 1, "Invalid size {}", size);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);
    let modrm = ModRM {
        addressing_mode,
        reg_field: RegField::OpcodeExtension(LOGICAL_SHIFT_RIGHT_OPCODE_EXT),
        rm_field: RmField::Register(X64Register::RBP),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);


    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(SHR_RM_8_BIT),
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
            ice_if!(reg.size() != 4, "Invalid stack size for integer comparison!");
            emit_compare_immediate_with_register(*reg, *immediate, asm)
        },
        ComparisonOperation {
            src1: StackOffset { offset, size},
            src2: IntegerConstant(immediate),
        } => {
            ice_if!(*size != 4, "Invalid stack size for integer comparison!");
            emit_compare_integer_immediate_with_stack(*offset,  *immediate, asm);
        },
        ComparisonOperation {
            src1: StackOffset { offset, size},
            src2: ByteConstant(val),
        } => {
            ice_if!(*size != 1, "Invalid destination size {}", size);
            emit_compare_byte_immediate_with_stack(
                *offset,
                *val,
                asm );
        },
        ComparisonOperation {
            src1: PhysicalRegister(reg),
            src2: StackOffset {offset, size },
        } => {
            match *size {
                1 => emit_compare_byte_stack_with_register(*offset, *reg, asm),
                4 | 8 => emit_compare_integer_stack_with_register(*offset, *reg, asm),
                _ => ice!("Invalid operand size"),
            }
        },
        _ => ice!("Invalid operand encoding for CMP: {:#?}", operands),
    }
}

fn emit_compare_immediate_with_register(reg: X64Register, immediate: i32, asm: &mut Vec<u8>) {

    ice_if!(reg.size() < 4, "Invalid register {:?}", reg);

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

fn emit_compare_integer_immediate_with_stack(offset: u32,  immediate: i32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::OpcodeExtension(CMP_OPCODE_EXT),
    };

   let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_RM_32_BIT_WITH_32_BIT_IMMEDIATE),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

fn emit_compare_byte_immediate_with_stack(offset: u32, immediate: i8, asm: &mut Vec<u8>) {

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::OpcodeExtension(CMP_OPCODE_EXT),
    };

   let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_RM_8_BIT_WITH_8_BIT_IMMEDIATE),
        Some(modrm),
        sib,
        Some(Immediate::from(immediate)),
    );
}

fn emit_compare_integer_stack_with_register(offset: u32, register: X64Register, asm: &mut Vec<u8>) {
    ice_if!(register.size() < 4, "Invalid register {:?}", register);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::Register(register),
    };

    let rex = create_rex_prefix(register.is_64_bit_register(), Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_REG_WITH_RM_32_BIT),
        Some(modrm),
        sib,
        None,
    );
}


fn emit_compare_byte_stack_with_register(offset: u32, register: X64Register, asm: &mut Vec<u8>) {
    ice_if!(register.size() != 1, "Invalid register {:?}", register);

    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::Register(register),
    };

    let rex = create_rex_prefix(false, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(COMPARE_REG_WITH_RM_8_BIT),
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

fn emit_function_call(name: &str, calls_requiring_updates: &mut Vec<CallPatch>, asm: &mut Vec<u8> ) {

    calls_requiring_updates.push(CallPatch {
        name: name.to_owned(),
        location: asm.len(),
    });

    let placeholder = 0u32;
    emit_instruction(
        asm,
        None,
        SizedOpCode::from(CALL_32_BIT_NEAR_RELATIVE),
        None,
        None,
        Some(Immediate::from(placeholder))
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

        let mut used_fields = 0;

        if let Some(scale) = sib_values.scale {
            sib_byte |= scale.sib_bits();
            used_fields += 1;
        }

        if let Some(index) = sib_values.index {
            sib_byte |= index.encoding() << 3;
            used_fields +=1;
        } else {
            sib_byte |= 0b100 << 3; // RSP-encoding used to indicate field is unused
        }

        if let Some(base) = sib_values.base {
            sib_byte |= base.encoding();
            used_fields += 1;
        }

        // only emit the byte if at least of the fields is in use
        if used_fields != 0 {
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

fn emit_ret(value: &Option<Value>, stack_size: u32, args: u32, asm: &mut Vec<u8>) {

    match value {
        Some(IntegerConstant(value)) => {
            emit_mov_integer_to_register(*value, X64Register::EAX, asm)
        }
        Some(ByteConstant(value)) => {
            emit_mov_byte_to_register(*value, X64Register::EAX, asm)
        }
        Some(StackOffset {offset, size}) => {
            match *size {
                1 => emit_mov_byte_from_stack_to_reg(X64Register::AL, *offset, asm),
                4 => emit_mov_integer_from_stack_to_reg(X64Register::EAX, *offset, asm),
                8 => emit_mov_integer_from_stack_to_reg(X64Register::RAX, *offset, asm),
                _ => ice!("Not implemented for size {} !", size),
            }
        },
        Some(ComparisonResult(comp_type)) => {
            emit_mov_comp_result_into_reg(comp_type, X64Register::RAX, asm)
        },
        None => (),
        _ =>  ice!("Invalid return value: {:#?}", value),
    }

    emit_function_epilogue(asm, stack_size, args);
    asm.push(NEAR_RETURN);
}


fn emit_push(value: &Value, asm: &mut Vec<u8>) {
    match value {
        IntegerConstant(val) => push_integer(*val, asm),
        StackOffset{offset, size} => push_stack_offset(*offset, *size, asm),
        PhysicalRegister(reg) => push_register(*reg, asm),
        _ => unimplemented!("Not yet implemented for:\n{:#?}", value),
    }
}

fn emit_pop(value: &Value, asm: &mut Vec<u8>) {
    match value {
        PhysicalRegister(reg) => pop_register(*reg, asm),
        _ => unimplemented!("Not yet implemented for:\n{:#?}", value),
    }
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
    push_register(X64Register::RBP, asm);
    emit_mov_integer_reg_to_reg(X64Register::RBP, X64Register::RSP, asm);
    if stack_size != 0 {
        emit_sub_immediate_from_register(X64Register::RSP, stack_size as i32, asm);
    }


    // FIXME - preserve all non-scratch registers, based on calling convention, and if function uses them
    // (Probably this should be emitted by previous stage - this file should handle bytecode -> asm conversion only
    push_register(X64Register::RBX, asm);
    // FIXME: R12-R15 not preserved - not really used as as scratch regs so should not matter
}

// TODO: Clarify the function prologue/epilogue handling
fn emit_function_epilogue(asm: &mut Vec<u8>, _stack_size: u32, _args: u32) {
    pop_register(X64Register::RBX, asm);
    asm.push(LEAVE);
  /*  if stack_size != 0 {
        emit_add_immediate_to_register(X64Register::RSP, stack_size as i32, asm);
    }
    pop_register(X64Register::RBP, asm);
   */

}

fn push_register(register: X64Register, asm: &mut Vec<u8>) {
    if register.is_extended_reg() {
        asm.push(0x41);
    }
    asm.push(PUSH_REG + register.encoding());
}

// pushing a value from stack to stack sounds bit funny, but is necessary when using stack allocator
// and pushing function arguments
fn push_stack_offset(offset: u32, size: u32, asm: &mut Vec<u8>) {
    let (addressing_mode, sib) = get_addressing_mode_and_sib_data_for_displacement_only_addressing(offset);

    let modrm = ModRM {
        addressing_mode,
        rm_field: RmField::Register(X64Register::RBP),
        reg_field: RegField::OpcodeExtension(PUSH_OPCODE_EXTENSION),
    };

    let rex = create_rex_prefix(size == 8, Some(modrm), sib);

    emit_instruction(
        asm,
        rex,
        SizedOpCode::from(PUSH_RM),
        Some(modrm),
        sib,
        None,
    );
}

fn push_integer(val: i32, asm: &mut Vec<u8>) {

    emit_instruction(
        asm,
        None,
        SizedOpCode::from(PUSH_32_BIT_IMMEDIATE),
        None,
        None,
        Some(Immediate::from(val)),
    );
}

fn pop_register(register: X64Register, asm: &mut Vec<u8>) {
    if register.is_extended_reg() {
        asm.push(0x41);
    }
    asm.push(POP_REG + register.encoding());
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
        prefix |= 0b0000_1000;
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


// codegen optimistically uses the 1 byte offset variant for conditional jumps.
//
fn change_1_byte_jump_offsets_to_4_byte_offsets_where_needed(
    calls_requiring_updates: &mut Vec<CallPatch>,
    jumps_requiring_updates: &mut Vec<JumpPatch>,
    label_pos: &mut HashMap<u32, usize>,
    asm: &mut Vec<u8>) {


    let mut shifts = vec![]; // how many lines have been added before this line
    shifts.push(0); // first instruction is never shifted

    let mut shifted_bytes = 0;
    for jump in jumps_requiring_updates.iter_mut() {
        if let JumpPatch::ConditionalShortJump(label_id, jump_opcode_location) = jump {
            let target = label_pos[&label_id];
            let op_size = 2i32; // size of the whole operation (opcode + operand)
            let offset : i32 = target as i32 - *jump_opcode_location as i32 - op_size;

            // cannot borrow jump vec again, so create a shift table storing the shifts required for each line to remain in sync
            let cur_size = shifts.len();
            for _ in cur_size..*jump_opcode_location+1 {
                shifts.push(shifted_bytes);
            }

            ice_if!(shifts[*jump_opcode_location] != shifted_bytes, "Invalid shift table initialization");

            if offset > 127 || offset < -128 {
                update_conditional_jump_opcode_and_placeholder_to_32_bit_variant(*jump_opcode_location + shifted_bytes, asm);
                // everything after JUMP_OPCODE_LOCATION has now shifted by 4 bytes. Anything referring to any values after these
                // must be updated

                // update jump labels
                update_label_positions(label_pos, *jump_opcode_location + shifted_bytes);
                // update calls
                update_call_positions(calls_requiring_updates, *jump_opcode_location + shifted_bytes);

                shifted_bytes += 4; // how many bytes have been added into the ASM vec,
            }
            shifts.push(shifted_bytes);
            ice_if!(shifts[*jump_opcode_location+1] != shifted_bytes, "Invalid shift table initialization");
        }
    }

    // shift all jumps based on the shift table
    if shifted_bytes > 0 {
        for jump in jumps_requiring_updates.iter_mut() {
            match jump {
                JumpPatch::ConditionalShortJump(_, jump_opcode_location) => {
                    *jump_opcode_location += shifts[*jump_opcode_location];
                }
                JumpPatch::Jump(_, jump_opcode_location) => {
                    let max_len = shifts.len() - 1;
                    let index = std::cmp::min(*jump_opcode_location, max_len);
                    *jump_opcode_location += shifts[index];
                }
            }
        }
    }
}

fn update_conditional_jump_opcode_and_placeholder_to_32_bit_variant(location: usize, asm: &mut Vec<u8>) {
    // FIXME: Inserts are in general O(N^2) due to inserting into a middle of a vector.

    // extend the jump offset placeholder
    asm.insert(location+1, 0x0);
    asm.insert(location+1, 0x0);
    asm.insert(location+1, 0x0);

    let updated_opcode = match asm[location] {
        JUMP_IF_LESS => JUMP_IF_LESS_32_BIT,
        JUMP_IF_LESS_OR_EQ => JUMP_IF_LESS_OR_EQ_32_BIT,
        JUMP_IF_EQ => JUMP_IF_EQ_32_BIT,
        JUMP_IF_NOT_EQ => JUMP_IF_NOT_EQ_32_BIT,
        JUMP_IF_GREATER_OR_EQ => JUMP_IF_GREATER_OR_EQ_32_BIT,
        JUMP_IF_GREATER => JUMP_IF_GREATER_32_BIT,
        _ => ice!("Invalid opcode for 8bit->32bit conditional jump update: Opcode 0x{:x} is not a conditional jump opcode", asm[location])
    };

    let high_byte = (updated_opcode >> 8) as u8;
    let low_byte = updated_opcode as u8;
    asm[location] = high_byte;
    asm.insert(location+1, low_byte);
}

fn update_label_positions(
    label_pos: &mut HashMap<u32, usize>,
    last_valid_position: usize,) {

    for (_, position) in label_pos.iter_mut() {
        if *position > last_valid_position {
            *position +=4;
        }
    }
}

fn update_call_positions(
    calls_requiring_updates: &mut Vec<CallPatch>,
    last_valid_position: usize,) {

    for call in calls_requiring_updates.iter_mut() {
        if call.location > last_valid_position {
            call.location +=4;
        }
    }
}


fn update_jumps(
    jumps_requiring_updates: &mut Vec<JumpPatch>,
    label_pos: &mut HashMap<u32, usize>,
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
                        let opcode_offset = 1;
                        asm[jump_opcode_location + opcode_offset + i] = buffer[i];
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
                        // the 2 byte jump (opcode + one byte offset) has been replaced with 6 byte jump
                        // (2 byte opcode + 4 byte offset). Adjust the offset accordingly
                        let additional_opcode_size = 4;
                        let u32_offset = (offset - additional_opcode_size) as u32;

                        // +2 so that the two-byte opcocde is skipped
                        asm[jump_opcode_location + 2] = u32_offset  as u8;
                        asm[jump_opcode_location + 3] = (u32_offset >> 8)  as u8;
                        asm[jump_opcode_location + 4] = (u32_offset >> 16)  as u8;
                        asm[jump_opcode_location + 5] = (u32_offset >> 24)  as u8;
                    } else {
                        // +1 so that the one-byte opcocde is skipped
                        asm[jump_opcode_location + 1] = (offset as i8) as u8;
                    }
                } else {
                    ice!("No jump target for conditional short jump stored: {:#?}", jump);
                }
            }
        }
    }
}


fn update_calls(
    calls_requiring_updates: &Vec<CallPatch>,
    function_positions: &HashMap<String, usize>,
    external_functions: HashSet<String>,
    asm: &mut Vec<u8>) -> Vec<(String, usize)> {

    let mut relocations = vec![];
    for call in calls_requiring_updates.iter() {
        // +1 so that the one-byte opcode is skipped
        let opcode_offset = 1;
        let call_location = call.location + opcode_offset;

        if external_functions.contains(&call.name) {
            relocations.push((call.name.clone(), call_location));
            continue;
        }

        if function_positions.contains_key(&call.name) {
            let call_address = function_positions[&call.name];
            // Note: Breaks if the offset is larger than i32::maxval
            // (although call of that size is hopefully unlikely)
            let call_size = 5; // 1 for opcode, 4 for location
            let offset : i32 = call_address as i32 - call.location as i32 - call_size;

            let mut buffer = [0; 4];
            LittleEndian::write_i32(&mut buffer, offset);

            for i in 0..4 {
                asm[call_location + i] = buffer[i];
            }
        } else {
            ice!("No call target for call stored: {:#?}", call);
        }
    }

    relocations
}

fn immediate_fits_in_8_bits(immediate: i32) -> bool {
    immediate <= 127 && immediate >= -128
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


fn get_addressing_mode_and_sib_data_for_indexed_addressing_with_displacement(
    scale: Option<Scale>,
    base: Option<X64Register>,
    index: Option<X64Register>,
    displacement: Option<u32>) -> (AddressingMode, Option<Sib>) {

    let index = if let Some(index_reg) = index {
        ice_if!(index_reg == X64Register::RSP, "RSP cannot be used as an index register");
        ice_if!(scale.is_none(), "Scale must be set if index register is used");
        index_reg
    } else {
        X64Register::RSP // signifies unused register
    };

    let (addressing_mode, displacement) =
        match (displacement, base) {
            (None, Some(reg)) if reg != X64Register::RBP => {
                (AddressingMode::IndirectAddressingNoDisplacement, None)
            }
            (None, Some(_)) => { // encoding restriction, must encode using displacement
                (AddressingMode::IndirectAddressingOneByteDisplacement, Some(Displacement::OneByte(0)))
            }
            (Some(val), _) if val < 128 => {
                (AddressingMode::IndirectAddressingOneByteDisplacement, Some(Displacement::OneByte(u8::max_value().wrapping_sub(val as u8).wrapping_add(1))))
            }
            (Some(val), _) => {
                (AddressingMode::IndirectAddressingFourByteDisplacement, Some(Displacement::FourByte(u32::max_value().wrapping_sub(val).wrapping_add(1))))
            }
            (None, None) => {
                ice!("Not implemented")
            }
        };

    let sib = Sib {
            scale,
            index: Some(index),
            base,
            displacement,
        };

    (addressing_mode, Some(sib))
}