use crate::byte_generator::{Function, ByteCode, Value, UnaryOperation, VirtualRegisterData, BinaryOperation};
use crate::byte_generator::Value::{VirtualRegister, IntegerConstant, StackOffset, PhysicalRegister};
use crate::code_generator::stack_allocator::UpdateKind::{SwitchOperands, ConvertForm, NoUpdate};
use crate::semcheck::Type::Integer;

use super::x64::X64Register;

use rayon::prelude::*;
use std::collections::HashMap;

#[derive(Debug)]
struct StackSlot {
    offset: u32,
    size: u32,
}


#[derive(Debug)]
struct StackMap {
    reg_to_stack_slot: HashMap<u32, StackSlot>,
    stack_size: u32,
}



impl StackMap {
    fn new() -> StackMap {
        StackMap {
            reg_to_stack_slot: HashMap::new(),
            stack_size: 0,
        }
    }
}


pub fn allocate( bytecode_functions: Vec<Function>)  -> Vec<Function> {
   bytecode_functions.par_iter()
       .map(|function| allocate_function(function))
       .collect()
}

fn allocate_function(function: &Function) -> Function {

    let stack_map = allocate_variables_to_stack(function);
    update_instructions(function, &stack_map)

}

// create stack slots for each variable
fn allocate_variables_to_stack(function: &Function) -> StackMap {

    let mut stack_map = StackMap::new();

    for instr in function.code.iter() {
        match instr {
            ByteCode::Nop |
            ByteCode::Label(_) |
            ByteCode::Jump(_) => (), // do nothing

            ByteCode::Mov(ref unary_op) => handle_unary_op(unary_op, &mut stack_map),
            ByteCode::Add(ref binary_op) |
            ByteCode::Sub(ref binary_op) |
            ByteCode::Mul(ref binary_op) |
            ByteCode::Div(ref binary_op) => handle_binary_op(binary_op, &mut stack_map),

            ByteCode::Ret(Value::VirtualRegister(ref vrefdata)) => add_location(&mut stack_map, vrefdata),
            _ => unimplemented!("{:?}", *instr)
        }
    }

    // 16 byte align the stack

    stack_map.stack_size = if stack_map.stack_size & 0b1111 == 0 {
        stack_map.stack_size
    } else {
        (stack_map.stack_size | 0b1111) + 1
    };

    stack_map
}

fn handle_unary_op(unary_op: &UnaryOperation, stack_map: &mut StackMap) {

    if let Value::VirtualRegister(ref src) = unary_op.src {
        add_location(stack_map, src);
    }

    if let Value::VirtualRegister(ref dest) = unary_op.dest {
        add_location(stack_map, dest);
    }
}

fn handle_binary_op( binary_op: &BinaryOperation, stack_map: &mut StackMap) {

    if let Value::VirtualRegister(ref src) = binary_op.src1 {
       add_location(stack_map, src);
    }

    if let Value::VirtualRegister(ref src) = binary_op.src2 {
        add_location(stack_map, src);
    }

    if let Value::VirtualRegister(ref dest) = binary_op.dest {
        add_location(stack_map, dest);
    }
}

fn add_location(map: &mut StackMap, data: &VirtualRegisterData) {
    if !map.reg_to_stack_slot.contains_key(&data.id) {

        let slot_size = if data.size < 4 {
            4 // 4 byte align the variables
        } else {
            data.size
        };

        map.reg_to_stack_slot.insert(data.id, StackSlot{ offset: map.stack_size, size: slot_size} );
        map.stack_size += slot_size;
    }
}

// update instructions to use the stack allocated variables (use stack where possible, otherwise
// move from stack to register and then from register to stack.
//
// Also fix instructions to use the two-address code forms where necessary
// FIXME: Lots of unnecessary reallocations here, updating inplace could be smarter
fn update_instructions(function: &Function, stack_map: &StackMap) -> Function {
    let two_address_instr = convert_to_two_address_code(&function.code);
    let final_code = update_instructions_to_stack_form(&two_address_instr, stack_map);

    Function {
        name: function.name.clone(),
        code: final_code
    }
}

fn convert_to_two_address_code(code: &Vec<ByteCode>) -> Vec<ByteCode> {
    let mut updated_instructions = vec![];

    for instr in code.iter() {
        match instr {
            ByteCode::Add(ref binary_op) => update_add(binary_op, &mut updated_instructions),
            ByteCode::Mov(_) |
            ByteCode::Ret(_) => updated_instructions.push(instr.clone()), // instruction does not need update,
            _ => unimplemented!("Not implemented for\n {:#?}\n", instr),
        }
    }

    updated_instructions
}


enum UpdateKind {
    NoUpdate, // no update needed
    SwitchOperands, // switch from A = B op A to A = A op B; makes code gen phase simpler. May require other updates in case operand is not commutative
    ConvertForm // requires conversion from 3 address form to 2 address form; 3 unique operands
}

fn update_add(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>) {

    match requires_update(binary_op) {
        UpdateKind::NoUpdate => updated_instructions.push(ByteCode::Add(binary_op.clone())),
        UpdateKind::SwitchOperands => updated_instructions.push(ByteCode::Add(BinaryOperation {
            src1: binary_op.src2.clone(),
            src2: binary_op.src1.clone(),
            dest: binary_op.dest.clone(),
        })),
        UpdateKind::ConvertForm => {
            // break form A = B + C into:
            // A = B
            // A = A + C

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                dest: binary_op.dest.clone(),
                src: binary_op.src1.clone(),
            }));

            updated_instructions.push(ByteCode::Add(BinaryOperation {
                dest: binary_op.dest.clone(),
                src1: binary_op.dest.clone(),
                src2: binary_op.src2.clone()
            }));
        },
    }
}
// requires update dest reg is not also one of the source operands. Assumes commutativity
fn requires_update(binary_op: &BinaryOperation) -> UpdateKind {
    let dest_reg_id = if let VirtualRegister(ref vregdata) = binary_op.dest {
        vregdata.id
    } else {
        ice!("Destination operand is not a register");
    };

    if let VirtualRegister(ref vregdata) = binary_op.src1 {
        if vregdata.id == dest_reg_id {
            return NoUpdate;
        }
    }

    if let VirtualRegister(ref vregdata) = binary_op.src2 {
        if vregdata.id == dest_reg_id {
            return SwitchOperands;
        }
    }

    return ConvertForm;
}

fn update_instructions_to_stack_form(code: &Vec<ByteCode>, stack_map: &StackMap) -> Vec<ByteCode> {
    let mut updated_instructions = vec![];
    for instr in code.iter() {
        match instr {
            ByteCode::Mov(ref unary_op) => handle_mov_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Add(ref binary_op) => handle_add_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Ret(ref value) => handle_return_value_allocation(value, &mut updated_instructions, stack_map),
            _ => unimplemented!("Not implemented for:\n{:#?}\n", instr),
        }

    }

    updated_instructions
}

/*
    if the instruction is
        MOV virt_reg, imm
    replace with:
        MOV stack_address, imm:

    if the instruction is
        MOV virt_reg1_, virt_reg_2
    replace with:
        MOV real_tmp_reg, stack_address_2
        MOV stack_address_1, real_tmp_reg



*/
fn handle_mov_allocation(unary_op: &UnaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match unary_op {
        UnaryOperation{dest: VirtualRegister(ref vregdata), src: IntegerConstant(value)} => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
               dest: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
               src: IntegerConstant(*value),
            }));
        },
        UnaryOperation{dest: VirtualRegister(ref dest_vregdata), src: VirtualRegister(ref src_vregdata)} => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                dest: PhysicalRegister(get_register_for_size(dest_stack_slot.size)),
                src: StackOffset{offset: src_stack_slot.offset, size: src_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src: PhysicalRegister(get_register_for_size(dest_stack_slot.size)),
            }));


        },
        _ => unimplemented!("Not implemented for {:#?}", unary_op),
    }
}

/*
    if the instruction is
        ADD virt_reg, virt_reg, immm
    replace with:
        ADD stack_address, stack_address, imm

   if the instruction is
        ADD virt_reg1, virt_reg1, virt_reg2
   replace with:
        MOV real_tmp_reg, stack_address2,
        ADD stack_address1, stack_address1, real_tmp_reg2



*/

fn handle_add_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match binary_op {
        BinaryOperation{dest: VirtualRegister(ref dest_vregdata), src1: VirtualRegister(ref src_vregdata), src2: IntegerConstant(value)} => {
            if dest_vregdata.id != src_vregdata.id {
                ice!("Addition not in two address form: {:#?}", binary_op);
            }

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            updated_instructions.push(ByteCode::Add(BinaryOperation{
                dest: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src1: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src2: IntegerConstant(*value),
            }));
        },

        BinaryOperation{dest: VirtualRegister(ref dest_vregdata), src1: VirtualRegister(ref src1_vregdata), src2: VirtualRegister(ref src2_vregdata)} => {
            if dest_vregdata.id != src1_vregdata.id {
                ice!("Addition not in two address form: {:#?}", binary_op);
            }

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];

            let reg = get_register_for_size(src2_stack_slot.size);
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: PhysicalRegister(reg),
                src: StackOffset {offset: src2_stack_slot.offset, size: src2_stack_slot.size}
            }));

            updated_instructions.push(ByteCode::Add(BinaryOperation{
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src1: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src2: PhysicalRegister(reg),
            }));
        }
        _ => unimplemented!("Not implemented for {:#?}", binary_op),
    }
}

/*
    if the instruction is
        RET virt_reg
    replace with:
        RET stack_address (code gen will decide actual regs or stack position based on calling convention)


*/
fn handle_return_value_allocation(value: &Value, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match value {
        Value::VirtualRegister(ref vregdata) => {
            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            updated_instructions.push(ByteCode::Ret(
                Value::StackOffset{ offset: stack_slot.offset, size: stack_slot.size},
            ));
        },
        Value::IntegerConstant(value) => {
            updated_instructions.push(ByteCode::Ret(Value::IntegerConstant(*value)));
        },
        _ => unimplemented!("Not implemnented for {:#?}:", value),
    }
}

fn get_register_for_size(size: u32) -> X64Register {
    match size {
        4 => X64Register::EAX,
        _ => ice!("Invalid register size {}", size),
    }
}


#[cfg(test)]
mod tests {

    #[test]
    fn foo() {
        assert_eq!(4, 4);
    }

}