use crate::byte_generator::{Function, ByteCode, Value, UnaryOperation, VirtualRegisterData, BinaryOperation};
use crate::byte_generator::Value::{VirtualRegister, IntegerConstant, StackOffset, PhysicalRegister};
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


pub fn allocate( bytecode_functions: Vec<Function>)  -> Vec<(Function, u32)> {
   bytecode_functions.par_iter()
       .map(|function| allocate_function(function))
       .collect()
}

fn allocate_function(function: &Function) -> (Function, u32) {

    let stack_map = allocate_variables_to_stack(function);
    (update_instructions(function, &stack_map), stack_map.stack_size)
}

// create stack slots for each variable
fn allocate_variables_to_stack(function: &Function) -> StackMap {

    let mut stack_map = StackMap::new();

    for instr in function.code.iter() {
        match instr {
            ByteCode::Nop |
            ByteCode::Label(_) |
            ByteCode::Jump(_) |
            ByteCode::Ret(Some(IntegerConstant(_))) | // do nothing
            ByteCode::Ret(None) => (),

            ByteCode::Mov(ref unary_op) => handle_unary_op(unary_op, &mut stack_map),
            ByteCode::Add(ref binary_op) |
            ByteCode::Sub(ref binary_op) |
            ByteCode::Mul(ref binary_op) |
            ByteCode::Div(ref binary_op) => handle_binary_op(binary_op, &mut stack_map),

            ByteCode::Ret(Some(Value::VirtualRegister(ref vrefdata))) => add_location(&mut stack_map, vrefdata),
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
    let final_code = update_instructions_to_stack_form(&function.code, stack_map);

    Function {
        name: function.name.clone(),
        code: final_code
    }
}

fn update_instructions_to_stack_form(code: &Vec<ByteCode>, stack_map: &StackMap) -> Vec<ByteCode> {
    let mut updated_instructions = vec![];
    for instr in code.iter() {
        match instr {
            ByteCode::Mov(ref unary_op) => handle_mov_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Add(ref binary_op) => handle_add_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Sub(ref binary_op) => handle_sub_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Mul(ref binary_op) => handle_mul_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Div(ref binary_op) => handle_div_allocation(binary_op, &mut updated_instructions, stack_map),
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

        /*
            A = constant + constant2
            not directly encodable

            emit:

            MOV A, constant
            ADD A, constant2

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(src1_val),
            src2: IntegerConstant(src2_val),
        } => {
            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: IntegerConstant(*src1_val),
                    dest: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                })
            );

            updated_instructions.push(
                ByteCode::Add(
                    BinaryOperation{
                        dest: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                        src1: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                        src2: IntegerConstant(*src2_val),
                }));
        },
        /*
            A = A + constant
            encodable as is, just emit the instruction

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: IntegerConstant(src2_val)} if dest_vregdata.id == src_vregdata.id => {

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            updated_instructions.push(ByteCode::Add(BinaryOperation{
                dest: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src1: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src2: IntegerConstant(*src2_val),
            }));
        },

        /*
            A = constant + somereg

            swap constants around and call this function recursively

        */
        BinaryOperation{
            dest: VirtualRegister(_),
            src1: IntegerConstant(_),
            src2: VirtualRegister(_)} => {

            handle_add_allocation(&BinaryOperation {
                dest: binary_op.dest.clone(),
                src1: binary_op.src2.clone(),
                src2: binary_op.src1.clone(),
            },
            updated_instructions,
            stack_map);
        }
        /*
            A = B + constant
            cannot encode directly, so emit:

            mov tmp_reg, stack_slot_b
            mov stack_slot_a, tmp_reg
            add stack_slot, constant

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: IntegerConstant(src2_val)} if dest_vregdata.id != src_vregdata.id => {

            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg= get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: PhysicalRegister(reg),
                src: StackOffset{offset: src_stack_slot.offset, size: src_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src: PhysicalRegister(reg),
            }));

            updated_instructions.push(ByteCode::Add(BinaryOperation{
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src1: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src2: IntegerConstant(*src2_val),
            }));
        },

        /*
            A = A + B
            not directly encodable, as A and B are both memory operands, need to use tmp reg

            emit:

            MOV tmp_reg, b
            add A, tmp_reg


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata)} if dest_vregdata.id == src1_vregdata.id => {

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

        /*
            A = B + C
            not diretly encodable, three address form + max one memory operand per instruction

            mit:

            MOV tmp_reg, B
            ADD tmp_reg, C
            MOV A, tmp_reg


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata)} if dest_vregdata.id != src1_vregdata.id => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];

            let reg = get_register_for_size(src2_stack_slot.size);
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: PhysicalRegister(reg),
                src: StackOffset {offset: src1_stack_slot.offset, size: src1_stack_slot.size}
            }));

            updated_instructions.push(ByteCode::Add(BinaryOperation{
                dest: PhysicalRegister(reg),
                src1: PhysicalRegister(reg),
                src2: StackOffset{offset: src2_stack_slot.offset, size: src2_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: StackOffset {offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src: PhysicalRegister(reg),
            }));
        }
        _ => unimplemented!("Not implemented for {:#?}", binary_op),
    }
}

fn handle_sub_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match binary_op {

        /*
            A = constant - constant
            not directly encodable

            emit:

            MOV A, constant
            SUB A, constant

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(src1_val),
            src2: IntegerConstant(src2_val),
        } => {
            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: IntegerConstant(*src1_val),
                    dest: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                })
            );

            updated_instructions.push(
                ByteCode::Sub(
                    BinaryOperation{
                        dest: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                        src1: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                        src2: IntegerConstant(*src2_val),
                    }));
        },
        /*
            A = A - constant
            encodable as is, just emit the instruction

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: IntegerConstant(src2_val)} if dest_vregdata.id == src_vregdata.id => {

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            updated_instructions.push(ByteCode::Sub(BinaryOperation{
                dest: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src1: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src2: IntegerConstant(*src2_val),
            }));
        },
        /*
            A = B - constant
            not directly encodable

            emit:

            MOV tmp_reg, stack_slot_b
            MOV stack_slot_a, tmp_reg
            SUB stack_slot, constant

        */
        BinaryOperation {
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: IntegerConstant(src2_val),
        } if dest_vregdata.id != src_vregdata.id => {

            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg = get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: PhysicalRegister(reg),
                src: StackOffset{offset: src_stack_slot.offset, size: src_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src: PhysicalRegister(reg),
            }));

            updated_instructions.push(ByteCode::Sub(BinaryOperation{
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src1: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src2: IntegerConstant(*src2_val),
            }));
        },
        /*
            A = constant - B
            not directly encodable

            Emit:

            MOV tmp_reg, immediate
            SUB tmp_reg, B
            MOV A, tmp_reg

            Note: A is not clobbered during calculation, so safe for case when loc(A) == loc(B)
         */
        BinaryOperation {
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(value),
            src2: VirtualRegister(ref src_vregdata),
        } => {
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg = get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(*value),
                dest: PhysicalRegister(reg),
            }));

            updated_instructions.push(ByteCode::Sub(BinaryOperation{
                dest: PhysicalRegister(reg),
                src1: PhysicalRegister(reg),
                src2: StackOffset { offset: src_stack_slot.offset, size: src_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                src: PhysicalRegister(reg),
            })

            );
        },
        /*
            A = A - B
            not directly encodable, as A and B are both memory operands, need to use tmp reg

            emit:

            MOV tmp_reg, b
            SUB A, tmp_reg
        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata)} if dest_vregdata.id == src1_vregdata.id => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];

            let reg = get_register_for_size(src2_stack_slot.size);
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: PhysicalRegister(reg),
                src: StackOffset {offset: src2_stack_slot.offset, size: src2_stack_slot.size}
            }));

            updated_instructions.push(ByteCode::Sub(BinaryOperation{
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src1: StackOffset{offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src2: PhysicalRegister(reg),
            }));
        }


        /*
            A = B - C
            not diretly encodable, three address form + max one memory operand per instruction

            mit:

            MOV tmp_reg, B
            SUB tmp_reg, C
            MOV A, tmp_reg


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata)} if dest_vregdata.id != src1_vregdata.id => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];

            let reg = get_register_for_size(src2_stack_slot.size);
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: PhysicalRegister(reg),
                src: StackOffset {offset: src1_stack_slot.offset, size: src1_stack_slot.size}
            }));

            updated_instructions.push(ByteCode::Sub(BinaryOperation{
                dest: PhysicalRegister(reg),
                src1: PhysicalRegister(reg),
                src2: StackOffset{offset: src2_stack_slot.offset, size: src2_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: StackOffset {offset: dest_stack_slot.offset, size: dest_stack_slot.size},
                src: PhysicalRegister(reg),
            }));
        }
        _ => unimplemented!("Not implemented for {:#?}", binary_op),
    }
}

fn handle_mul_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match binary_op {
        /*
            A = constant * constant
            not directly encodable

            emit:

            MOV tmp_reg, constant
            IMUL tmp_reg, tmp_reg, constant
            MOV A, tmp_reg
        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(src1_val),
            src2: IntegerConstant(src2_val),
        } => {
            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg = get_register_for_size(stack_slot.size);
            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: IntegerConstant(*src1_val),
                    dest: PhysicalRegister(reg),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: PhysicalRegister(reg),
                        src1: PhysicalRegister(reg),
                        src2: IntegerConstant(*src2_val),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: PhysicalRegister(reg),
                    dest: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                })
            );
        },
        /*
            A = B * constant OR A = constant*B
            in case the latter, switch around to former

            directly encodable, as long as destination is a register. Need to add few moves from/to/stack

            emit:

            MOV tmp_register, B
            IMUL tmp_register, tmp_register, constant
            MOV A, tmp_register

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: IntegerConstant(constant),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(constant),
            src2: VirtualRegister(ref src_vregdata),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];

            let reg = get_register_for_size(dest_stack_slot.size);
            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: StackOffset {
                        offset: src_stack_slot.offset,
                        size: src_stack_slot.size
                    },
                    dest: PhysicalRegister(reg),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: PhysicalRegister(reg),
                        src1: PhysicalRegister(reg),
                        src2: IntegerConstant(*constant),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: PhysicalRegister(reg),
                    dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                })
            );
        },
        /*
            A = A*B
            not directly encodable in this case, as A is stack slot

            emit:

            MOV tmp_reg, A
            MUL tmp_reg, tmp_reg, B
            MOV A, tmp_reg
        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata),
        } => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
            let reg = get_register_for_size(dest_stack_slot.size);
            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: StackOffset {
                        offset: src1_stack_slot.offset,
                        size: src1_stack_slot.size,
                    },
                    dest: PhysicalRegister(reg),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: PhysicalRegister(reg),
                        src1: PhysicalRegister(reg),
                        src2: StackOffset {
                            offset: src2_stack_slot.offset,
                            size: src2_stack_slot.size
                        },
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: PhysicalRegister(reg),
                    dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                })
            );
        },

        _ => unimplemented!("Not implemented for {:#?}", binary_op),

    }
}

fn handle_div_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match binary_op {

        /*
            A = constant / constant

            emit:

            MOV EAX, dividend
            MOV TMP_REGISTER, divisor
            SIGN_EXTEND EAX
            IDIV TMP_REGISTER
            MOV A, EAX

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(dividend),
            src2: IntegerConstant(divisor),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let reg = get_register_for_size_for_division(stack_slot.size);

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*dividend),
                    dest: PhysicalRegister(X64Register::EAX)
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*divisor),
                    dest: PhysicalRegister(reg)
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation{
                    src: PhysicalRegister(X64Register::EAX),
                    dest: PhysicalRegister(X64Register::EDX),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation{
                    dest: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src1: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src2: PhysicalRegister(reg),
                })
            );


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: PhysicalRegister(X64Register::EAX),
                    dest: StackOffset { offset: stack_slot.offset, size: stack_slot.size },
                })
            );

        },
        /*
            A = B / constant

            emit:
            MOV EAX, B
            MOV TMP_REGISTER, divisor
            SIGN_EXTEND EAX
            IDIV TMP_REGISTER
            MOV A, EAX


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: IntegerConstant(divisor),
        } => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let reg = get_register_for_size_for_division(dest_stack_slot.size);

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: StackOffset{
                        offset: dividend_stack_slot.offset,
                        size: dividend_stack_slot.size,
                    },
                    dest: PhysicalRegister(X64Register::EAX)
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*divisor),
                    dest: PhysicalRegister(reg)
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation{
                    src: PhysicalRegister(X64Register::EAX),
                    dest: PhysicalRegister(X64Register::EDX),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation{
                    dest: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src1: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src2: PhysicalRegister(reg),
                })
            );


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: PhysicalRegister(X64Register::EAX),
                    dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                })
            );

        },

        /*
            A = constant / B

            emit:

            MOV EAX, constant
            SIGN_EXTEND EAX
            IDIV B
            MOV A, EAX


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: IntegerConstant(dividend),
            src2: VirtualRegister(ref divisor_vregdata),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&divisor_vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*dividend),
                    dest: PhysicalRegister(X64Register::EAX)
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: PhysicalRegister(X64Register::EAX),
                    dest: PhysicalRegister(X64Register::EDX),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src1: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src2: StackOffset {
                        offset: divisor_stack_slot.offset,
                        size: divisor_stack_slot.size,
                    },
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: PhysicalRegister(X64Register::EAX),
                    dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                })
            );
        },
        /*
            A = B / C (or A = A / B)

            emit:

            MOV EAX, B
            SIGN_EXTEND EAX
            IDIV C
            MOV A, EAX


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: VirtualRegister(ref divisor_vregdata),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&divisor_vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: StackOffset {
                        offset: dividend_stack_slot.offset,
                        size: dividend_stack_slot.size,
                    },
                    dest: PhysicalRegister(X64Register::EAX)
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: PhysicalRegister(X64Register::EAX),
                    dest: PhysicalRegister(X64Register::EDX),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src1: PhysicalRegister(X64Register::EAX), // Not really used, instruction hardcodes
                    src2: StackOffset {
                        offset: divisor_stack_slot.offset,
                        size: divisor_stack_slot.size,
                    },
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: PhysicalRegister(X64Register::EAX),
                    dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                })
            );
        },
        _ => unimplemented!("Not implemented for {:#?}", binary_op),
    }
}

/*
    if the instruction is
        RET virt_reg
    replace with:
        RET stack_address (code gen will decide actual regs or stack position based on calling convention)


*/
fn handle_return_value_allocation(value: &Option<Value>, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match value {
        Some(VirtualRegister(ref vregdata)) => {
            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            updated_instructions.push(ByteCode::Ret(
                Some(Value::StackOffset{ offset: stack_slot.offset, size: stack_slot.size}),
            ));
        },
        Some(IntegerConstant(value)) => {
            updated_instructions.push(ByteCode::Ret(Some(Value::IntegerConstant(*value))));
        },
        None => updated_instructions.push(ByteCode::Ret(None)),
        _ => unimplemented!("Not implemnented for {:#?}:", value),
    }
}

fn get_register_for_size(size: u32) -> X64Register {
    match size {
        4 => X64Register::EAX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size_for_division(size: u32) -> X64Register {
    match size {
        4 => X64Register::EBX,
        _ => ice!("Invalid register size {}", size),
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const TMP_REGISTER: X64Register = X64Register::EAX;
    const DIV_TMP_REGISTER: X64Register = X64Register::EBX;

    fn get_functions(bytecode: Vec<ByteCode>) -> Vec<Function> {
        vec![
            Function {
                name: "foo".to_owned(),
                code:  bytecode,
            }
        ]
    }

    #[test]
    fn should_allocate_constant_to_reg_move() {
        let functions = get_functions(
            vec![
                ByteCode::Mov(UnaryOperation{
                    src: IntegerConstant(4),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                    }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(4),
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
           allocated_code[0],
        );
    }

    #[test]
    fn should_allocate_reg_to_reg_move() {
        let functions = get_functions(
            vec![
                ByteCode::Mov(UnaryOperation{
                    src: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                },
            }),
            allocated_code[1],
        );
    }

    #[test]
    fn should_allocate_regs_for_two_constant_addition() {
        let functions = get_functions(
            vec![
                ByteCode::Add(BinaryOperation{
                    src2: IntegerConstant(8),
                    src1: IntegerConstant(9),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(9),
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(8),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[1],
        );
    }


    #[test]
    fn should_allocate_two_address_form_add_constant_to_reg() {
        let functions = get_functions(
            vec![
                ByteCode::Add(BinaryOperation{
                    src2: IntegerConstant(24),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(24),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[0],
        );
    }

    #[test]
    fn should_allocate_two_address_form_add_constant_to_reg_with_constant_as_first_argument() {
        let functions = get_functions(
            vec![
                ByteCode::Add(BinaryOperation{
                    src1: IntegerConstant(24),
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(24),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[0],
        );
    }

    #[test]
    fn should_allocate_two_address_form_add_reg_to_reg() {
        let functions = get_functions(
            vec![
                ByteCode::Add(BinaryOperation{
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 4,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: PhysicalRegister(TMP_REGISTER),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[1],
        );
    }

    #[test]
    fn should_break_three_address_constant_addition_to_two_address_form() {
        let functions = get_functions(
            vec![
                ByteCode::Add(BinaryOperation{
                    src2: IntegerConstant(7),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                },
            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(7),
                src1: StackOffset {
                    offset: 4,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                }
            }),
            allocated_code[2],
        );
    }

    #[test]
    fn should_break_three_address_reg_addition_to_two_address_form() {
        let functions = get_functions(
            vec![
                ByteCode::Add(BinaryOperation{
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 2,
                        }),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: StackOffset {
                    offset: 4,
                    size: 4,
                },
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 8,
                    size: 4,
                }
            }),
            allocated_code[2],
        );
    }

    #[test]
    fn should_allocate_constant_integer_return() {
        let functions = get_functions(
            vec![
                ByteCode::Ret(IntegerConstant(20)),
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Ret(IntegerConstant(20)),
            allocated_code[0],
        );
    }

    #[test]
    fn should_allocate_reg_return() {
        let functions = get_functions(
            vec![
                ByteCode::Ret(VirtualRegister(
                    VirtualRegisterData {
                        size: 4,
                        id: 0,
                    }
                )),
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Ret(
                StackOffset{
                    offset:0,
                    size: 4,
                }),
            allocated_code[0],
        );
    }

    #[test]
    fn should_allocate_regs_for_two_constant_subtraction() {
        let functions = get_functions(
            vec![
                ByteCode::Sub(BinaryOperation{
                    src2: IntegerConstant(8),
                    src1: IntegerConstant(9),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(9),
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: IntegerConstant(8),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[1],
        );
    }

    #[test]
    fn should_allocate_regs_for_sub_constant_from_reg() {
        let functions = get_functions(
            vec![
                ByteCode::Sub(BinaryOperation{
                    src2: IntegerConstant(8),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());


        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: IntegerConstant(8),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[0],
        );
    }


    #[test]
    fn should_allocate_regs_for_sub_reg_from_constant() {

        let functions = get_functions(
            vec![
                ByteCode::Sub(BinaryOperation{
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                    src1: IntegerConstant(678),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(678),
                dest: PhysicalRegister(TMP_REGISTER)
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: StackOffset { offset: 0 , size: 4},
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),

            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset { offset: 0 , size: 4},
            }),
            allocated_code[2],
        );
    }

    #[test]
    fn should_allocate_two_address_form_sub_reg_to_reg() {
        let functions = get_functions(
            vec![
                ByteCode::Sub(BinaryOperation{
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 4,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: PhysicalRegister(TMP_REGISTER),
                src1: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[1],
        );
    }

    #[test]
    fn should_break_three_address_constant_subtraction_to_two_address_form() {
        let functions = get_functions(
            vec![
                ByteCode::Sub(BinaryOperation{
                    src2: IntegerConstant(7),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                },
            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: IntegerConstant(7),
                src1: StackOffset {
                    offset: 4,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                }
            }),
            allocated_code[2],
        );
    }

    #[test]
    fn should_break_three_address_from_reg_subtraction_to_two_address_form() {
        let functions = get_functions(
            vec![
                ByteCode::Sub(BinaryOperation{
                    src2: VirtualRegister(
                        VirtualRegisterData{
                            size: 4,
                            id: 2
                        }
                    ),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 1,
                        }),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: StackOffset {
                    offset: 4,
                    size: 4,
                },
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 8,
                    size: 4,
                }
            }),
            allocated_code[2],
        );
    }


    #[test]
    fn should_allocate_constant_constant_multiplication() {
        let functions = get_functions(
            vec![
                ByteCode::Mul(BinaryOperation {
                    src2: IntegerConstant(2),
                    src1: IntegerConstant(30),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: IntegerConstant(30),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mul(BinaryOperation{
                src2: IntegerConstant(2),
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[2]
        );
    }

    #[test]
    fn should_allocate_reg_constant_multiplication() {
        let functions = get_functions(
            vec![
                ByteCode::Mul(BinaryOperation {
                    src2: IntegerConstant(2),
                    src1: VirtualRegister (
                        VirtualRegisterData {
                            id: 1,
                            size: 0
                       }
                    ),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset{
                    offset: 0,
                    size: 4
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mul(BinaryOperation{
                src2: IntegerConstant(2),
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                }
            }),
            allocated_code[2]
        );
    }

    #[test]
    fn should_allocate_constant_reg_multiplication() {
        let functions = get_functions(
            vec![
                ByteCode::Mul(BinaryOperation {
                    src2: VirtualRegister (
                        VirtualRegisterData {
                            id: 1,
                            size: 0
                        }
                    ),
                    src1: IntegerConstant(2),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            size: 4,
                            id: 0,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset{
                    offset: 0,
                    size: 4
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mul(BinaryOperation{
                src2: IntegerConstant(2),
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                }
            }),
            allocated_code[2]
        );
    }

    #[test]
    fn should_allocate_two_address_form_register_register_multiplication() {

        let functions = get_functions(
            vec![
                ByteCode::Mul(BinaryOperation {
                    src2: VirtualRegister (
                        VirtualRegisterData {
                            id: 1,
                            size: 4
                        }
                    ),
                    src1: VirtualRegister (
                        VirtualRegisterData {
                            id: 0,
                            size: 4,
                        }
                    ),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset{
                    offset: 0,
                    size: 4
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mul(BinaryOperation{
                src2: StackOffset {
                    offset: 4,
                    size: 4,
                },
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                }
            }),
            allocated_code[2]
        );
    }

    #[test]
    fn should_allocate_regs_for_constant_constant_division() {
        let functions = get_functions(
            vec![
                ByteCode::Div(BinaryOperation {
                    src2: IntegerConstant(30),
                    src1: IntegerConstant(2),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(5, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: IntegerConstant(2),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(30),
                dest: PhysicalRegister(DIV_TMP_REGISTER),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::SignExtend(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EDX),
            }),
            allocated_code[2]
        );

        assert_eq!(
            ByteCode::Div(BinaryOperation{
                src2: PhysicalRegister(DIV_TMP_REGISTER),
                src1: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[3]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: StackOffset {
                    offset: 0,
                    size: 4,
                },
            }),
            allocated_code[4]
        );

    }

    #[test]
    fn should_allocate_regs_for_reg_constant_division() {
        let functions = get_functions(
            vec![
                ByteCode::Div(BinaryOperation {
                    src2: IntegerConstant(30),
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            id: 1,
                            size: 4
                        }
                    ) ,
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(5, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset {
                    offset: 0,
                    size: 4,
                },
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(30),
                dest: PhysicalRegister(DIV_TMP_REGISTER),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::SignExtend(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EDX),
            }),
            allocated_code[2]
        );

        assert_eq!(
            ByteCode::Div(BinaryOperation{
                src2: PhysicalRegister(DIV_TMP_REGISTER),
                src1: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[3]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                },
            }),
            allocated_code[4]
        );
    }

    #[test]
    fn should_allocate_regs_for_constant_reg_division() {
        let functions = get_functions(
            vec![
                ByteCode::Div(BinaryOperation {
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            id: 1,
                            size: 4
                        }
                    ) ,
                    src1: IntegerConstant(30),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(4, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(30),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::SignExtend(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EDX),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::Div(BinaryOperation{
                src2: StackOffset {
                    offset: 0,
                size: 4,
                },
                src1: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[2]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: StackOffset {
                    offset: 4,
                    size: 4,
                },
            }),
            allocated_code[3]
        );
    }

    #[test]
    fn should_allocate_regs_for_reg_reg_division() {
        let functions = get_functions(
            vec![
                ByteCode::Div(BinaryOperation {
                    src2: VirtualRegister(
                        VirtualRegisterData {
                            id: 1,
                            size: 4
                        }
                    ) ,
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            id: 2,
                            size: 4,
                        },
                    ),
                    dest: VirtualRegister(
                        VirtualRegisterData {
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(4, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset{
                    offset: 0,
                    size: 4,
                } ,
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::SignExtend(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EDX),
            }),
            allocated_code[1]
        );

        assert_eq!(
            ByteCode::Div(BinaryOperation{
                src2: StackOffset {
                    offset: 4,
                    size: 4,
                },
                src1: PhysicalRegister(X64Register::EAX),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[2]
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(X64Register::EAX),
                dest: StackOffset {
                    offset: 8,
                    size: 4,
                },
            }),
            allocated_code[3]
        );
    }
}