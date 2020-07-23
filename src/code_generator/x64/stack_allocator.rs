use crate::byte_generator::byte_code::{Function, ByteCode, Value, UnaryOperation, VirtualRegisterData, BinaryOperation, ComparisonOperation};
use crate::byte_generator::byte_code::Value::*;
use crate::semcheck::Type;

use super::x64_register::X64Register;

const RETURN_VALUE_REGISTER: X64Register = X64Register::EAX; // TODO: Abstract calling convention
const PTR_SIZE: u32 =  8;

use rayon::prelude::*;
use std::collections::HashMap;
use crate::function_attributes::FunctionAttribute;
use crate::tac_generator::ARRAY_LENGTH_SLOT_SIZE;

#[derive(Debug, Clone)]
struct StackSlot {
    offset: u32,
    size: u32,
}


#[derive(Debug)]
struct StackMap {
    reg_to_stack_slot: HashMap<u32, StackSlot>,
    array_to_stack_slot: HashMap<u32, StackSlot>, // reg and array can be same
    stack_size: u32, // stack size, size chosen so that stack is aligned to 16 byte boundary (adjusted for arg pushes)
    stack_space_used: u32, // stack size, unaligned.
}



impl StackMap {
    fn new() -> StackMap {
        StackMap {
            reg_to_stack_slot: HashMap::new(),
            array_to_stack_slot: HashMap::new(),
            stack_size: 4, // start stack at 4 bytes, so we do not overwrite the old RBP value we push
            stack_space_used: 4,
        }
    }
}


pub fn allocate(bytecode_functions: Vec<Function>, print_stack_map: bool)  -> Vec<(Function, u32)> {
   bytecode_functions.par_iter()
       .map(|function| allocate_function(function, print_stack_map))
       .collect()
}

fn allocate_function(function: &Function, print_stack_allocation: bool) -> (Function, u32) {
    let mut stack_map = allocate_variables_to_stack(function);

    if print_stack_allocation {
        print_stack_map(function, &stack_map);
    }
    (update_instructions(function, &mut stack_map), stack_map.stack_size)
}

// create stack slots for each variable
fn allocate_variables_to_stack(function: &Function) -> StackMap {

    let mut stack_map = StackMap::new();

    for instr in function.code.iter() {
        match instr {
            ByteCode::Nop |
            ByteCode::Label(_) |
            ByteCode::Jump(_) |
            ByteCode::Ret(Some(IntegerConstant(_))) |
            ByteCode::Ret(Some(BooleanConstant(_))) |
            ByteCode::Ret(Some(ComparisonResult(_))) |
            ByteCode::Ret(None) |
            ByteCode::JumpConditional(_, _) |
            ByteCode::FunctionArguments(_) |
            ByteCode::Call(_) => (), // do nothing
            ByteCode::PseudoArrayInit { id, size_in_bytes} => add_array_location(&mut stack_map, *id, *size_in_bytes),


            ByteCode::Mov(unary_op) |
            ByteCode::Lea(unary_op) |
            ByteCode::Negate(unary_op) => handle_unary_op(unary_op, &mut stack_map),
            ByteCode::Add(binary_op) |
            ByteCode::Sub(binary_op) |
            ByteCode::Mul(binary_op) |
            ByteCode::Div(binary_op) |
            ByteCode::Xor(binary_op) |
            ByteCode::Mod(binary_op) => handle_binary_op(binary_op, &mut stack_map),

            ByteCode::Compare(comparsion_op) => handle_comparison_op(comparsion_op, &mut stack_map),

            ByteCode::Ret(Some(Value::VirtualRegister(ref vrefdata))) => add_location(&mut stack_map, vrefdata),
            _ => unimplemented!("{:?}", *instr)
        }
    }



    // 16 byte align the stack

    stack_map.stack_space_used = stack_map.stack_size;

    stack_map.stack_size = if stack_map.stack_size & 0b1111 == 0 {
        stack_map.stack_size
    } else {
        (stack_map.stack_size | 0b1111) + 1
    };

    stack_map
}

fn handle_unary_op(unary_op: &UnaryOperation, stack_map: &mut StackMap) {

    add_if_register(&unary_op.src, stack_map);
    add_if_register(&unary_op.dest, stack_map);
}

fn handle_binary_op( binary_op: &BinaryOperation, stack_map: &mut StackMap) {
    add_if_register(&binary_op.src1, stack_map);
    add_if_register(&binary_op.src2, stack_map);
    add_if_register(&binary_op.dest, stack_map);
}

fn handle_comparison_op(comparison_op: &ComparisonOperation, stack_map: &mut StackMap) {
    add_if_register(&comparison_op.src1, stack_map);
    add_if_register(&comparison_op.src2, stack_map);
}

fn add_if_register(value: &Value, stack_map: &mut StackMap) {

    match value {
        Value::VirtualRegister(src) => add_location(stack_map, src),
        Value::DynamicStackOffset {id: _,index, offset: _, size: _ } => add_if_register(index, stack_map),
        Value::IndirectAddress {base, index, offset: _, size: _ } => {
            add_if_register(base, stack_map);
            if let Some(x) = index {
                add_if_register(x, stack_map);
            }
        }
        _ => (),
    }
}

fn add_location(map: &mut StackMap, data: &VirtualRegisterData) {
    if !map.reg_to_stack_slot.contains_key(&data.id) {

        // 4 byte align the data
        let slot_size = if data.size >= 4 && data.size & 0b11 == 0 {
            data.size
        } else {
            (data.size | 0b11) + 1
        };


        map.stack_size += slot_size;
        map.reg_to_stack_slot.insert(data.id, StackSlot{ offset: map.stack_size, size: slot_size} );
    }
}

fn add_array_location(map: &mut StackMap, id: u32, size_in_bytes: u32) {
    if !map.array_to_stack_slot.contains_key(&id) {
        let slot_size = if size_in_bytes >= 4 && size_in_bytes & 0b11 == 0 {
            size_in_bytes
        } else {
            (size_in_bytes | 0b11) + 1
        };

        map.array_to_stack_slot.insert(id, StackSlot { offset: map.stack_size, size: slot_size });
        map.stack_size += slot_size;
    } else {
        ice!("Multiple array declarations for array id {}", id);
    }
}

fn print_stack_map(function: &Function, stack_map: &StackMap) {

    if function.has_attribute(FunctionAttribute::External) {
        return;
    }

    // acquire handle to stop interleaved prints
    let stdout = std::io::stdout();
    let _stdout_handle = stdout.lock();

    println!("Function '{}'", function.name);
    println!("    Stack size (unalinged) {}", stack_map.stack_space_used);
    println!("    Stack size (16 byte aligned) {}", stack_map.stack_size);
    println!();

    let mut stack_slot_to_entity = vec![];

    for (id, slot) in stack_map.reg_to_stack_slot.iter() {
        let tuple = (slot.clone(), format!("VR{}", id));
        stack_slot_to_entity.push(tuple);
    }

    for (id, slot) in stack_map.array_to_stack_slot.iter() {
        let tuple = (slot.clone(), format!("Array {}", id));
        stack_slot_to_entity.push(tuple);
    }

    stack_slot_to_entity.sort_by(|(slot, _), (slot2, _)| slot.offset.cmp(&slot2.offset));


    for (slot, entity) in stack_slot_to_entity.iter() {
        println!("    Stack slot {}, size {}, occupied by {}", slot.offset, slot.size, entity);
    }
    println!()
}

// update instructions to use the stack allocated variables (use stack where possible, otherwise
// move from stack to register and then from register to stack.
//
// Also fix instructions to use the two-address code forms where necessary
// FIXME: Lots of unnecessary reallocations here, updating inplace could be smarter
fn update_instructions(function: &Function, stack_map: &mut StackMap) -> Function {
    let final_code = update_instructions_to_stack_form(&function.code, stack_map);

    Function {
        name: function.name.clone(),
        code: final_code,
        parameter_count: function.parameter_count,
        attributes: function.attributes.clone(),
    }
}

fn update_instructions_to_stack_form(code: &Vec<ByteCode>, stack_map: &mut StackMap) -> Vec<ByteCode> {
    let mut updated_instructions = vec![];
    for instr in code.iter() {
        match instr {
            ByteCode::Mov(unary_op) => handle_mov_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Lea(unary_op) => handle_lea_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Negate(unary_op) => handle_negate_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Add(binary_op) => handle_add_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Sub(binary_op) => handle_sub_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Mul(binary_op) => handle_mul_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Div(binary_op) => handle_div_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Xor(binary_op) => handle_xor_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Mod(binary_op) => handle_mod_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Ret(value) => handle_return_value_allocation(value, &mut updated_instructions, stack_map),
            ByteCode::Compare(comparison_op) => handle_comparison(comparison_op, &mut updated_instructions, stack_map),

            ByteCode::Jump(_) |
            ByteCode::JumpConditional(_, _) |
            ByteCode::Label(_) |
            ByteCode::Call(_) => {
              updated_instructions.push(instr.clone());
            },
            ByteCode::FunctionArguments(args) =>
                handle_function_arguments(args, &mut updated_instructions, stack_map),
            ByteCode::PseudoArrayInit { id: _, size_in_bytes: _} => (), // pseudo opcode for stack size adjustment,
            _ => unimplemented!("Not implemented for:\n{:#?}\n", instr),
        }
    }

    updated_instructions
}

fn handle_mov_allocation(unary_op: &UnaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &mut StackMap) {

    match unary_op {
        /*
           A = constant

           emit:

           MOV A, constant

        */
        UnaryOperation{dest: VirtualRegister(ref vregdata), src: IntegerConstant(value)} => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
               dest: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
               src: IntegerConstant(*value),
            }));
        },
        UnaryOperation{dest: VirtualRegister(ref vregdata), src: BooleanConstant(value)} => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: StackOffset{offset: stack_slot.offset, size: stack_slot.size},
                src: BooleanConstant(*value),
            }));
        },
        /*
            A = B

            since A and B are both stack slots, we need to use tmp reg

            emit:

            MOV tmp_reg, B
            MOV A, tmp_reg
        */
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
        UnaryOperation {
            src: ComparisonResult(comparison_type),
            dest: VirtualRegister(vregdata),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: IntegerConstant(0),
                        dest: StackOffset{
                            offset: stack_slot.offset,
                            size: stack_slot.size
                        }
                    }
                )
            );


            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: ComparisonResult(comparison_type.clone()),
                        dest: StackOffset {
                            offset: stack_slot.offset,
                            size: stack_slot.size,
                        }
                    }
                )
            )
        },
        UnaryOperation {
            src: Value::ReturnValue,
            dest: VirtualRegister(vregdata),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(RETURN_VALUE_REGISTER),
                        dest: StackOffset{
                            offset: stack_slot.offset,
                            size: stack_slot.size
                        }
                    }
                )
            );
        },
        UnaryOperation {
            src: FunctionParameter(param_type, pos),
            dest: VirtualRegister(vregdata),
        } => {
            match param_type {
                // integers, and integer-like values like pointers
                Type::Integer | Type::Boolean | Type::Reference(_) => {
                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
                    if *pos < 6 {
                        updated_instructions.push(
                            ByteCode::Mov(
                                UnaryOperation {
                                    src: get_destination_for_integer_and_pointer_argument(*pos),
                                    dest: StackOffset {
                                        offset: stack_slot.offset,
                                        size: stack_slot.size,
                                    }
                                }
                            ));
                    } else {
                        let reg = PhysicalRegister(get_register_for_size(stack_slot.size));

                        // mov from known offset to reg
                        updated_instructions.push(
                            ByteCode::Mov(UnaryOperation {
                                src: StackOffset {
                                    size: stack_slot.size,
                                    offset: (-16i32 - 8*(*pos as i32 -6)) as u32,
                                },
                                dest: reg.clone(),
                            }));

                        updated_instructions.push(
                            ByteCode::Mov(
                                UnaryOperation {
                                    src: reg.clone(),
                                    dest: StackOffset {
                                        size: stack_slot.size,
                                        offset: stack_slot.offset,
                                    }
                                }
                            ));

                        // adjust stack size to account for pushed arguments, so that 16 byte alignment is maintained

                        stack_map.stack_size -= stack_slot.size;
                        if stack_map.stack_size < stack_map.stack_space_used {
                            stack_map.stack_size += 16;
                        }
                    }

                },
                _ => unimplemented!("Not implemented for non-integral function parameters, got type: {:?}", param_type),
            }
        },
        UnaryOperation {
            src: IntegerConstant(val),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } => {

            let array_stack = &stack_map.array_to_stack_slot[id];
            match **index {
                VirtualRegister(ref vregdata) => {
                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
                    let tmp_reg = get_register_for_size(*size);
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: stack_slot.size,
                                    offset: stack_slot.offset,
                                },
                                dest: PhysicalRegister(tmp_reg.clone()),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: IntegerConstant(*val),
                                dest: DynamicStackOffset {
                                    id: *id,
                                    size: *size,
                                    offset: get_indexed_array_offset(*offset, array_stack),
                                    index: Box::new(PhysicalRegister(tmp_reg)),
                                }
                            }
                        ));

                },
                IntegerConstant(index) => {
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: IntegerConstant(*val),
                                dest: StackOffset {
                                    size: *size,
                                    offset: get_constant_array_offset(*offset, *size, index, &array_stack),
                                }
                            }
                        ));
                }
                _ => ice!("Unexpected dynamic index {:?} ", index)
            }

        },
        UnaryOperation {
            src: BooleanConstant(val),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } => {

            let array_stack = &stack_map.array_to_stack_slot[id];
            match **index {
                VirtualRegister(ref vregdata) => {
                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
                    let tmp_reg = get_register_for_size(*size);
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: stack_slot.size,
                                    offset: stack_slot.offset,
                                },
                                dest: PhysicalRegister(tmp_reg.clone()),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: BooleanConstant(*val),
                                dest: DynamicStackOffset {
                                    id: *id,
                                    size: *size,
                                    offset: get_indexed_array_offset(*offset, array_stack),
                                    index: Box::new(PhysicalRegister(tmp_reg)),
                                }
                            }
                        ));

                },
                IntegerConstant(index) => {
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: BooleanConstant(*val),
                                dest: StackOffset {
                                    size: *size,
                                    offset: get_constant_array_offset(*offset, *size, index, &array_stack),
                                }
                            }
                        ));
                }
                _ => ice!("Unexpected dynamic index {:?} ", index)
            }
        },
        UnaryOperation {
            src: VirtualRegister(src_vregdata),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } => {
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let tmp2_reg = get_register_for_size2(*size);
            let array_stack = &stack_map.array_to_stack_slot[id];
            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: StackOffset {
                            size: src_stack_slot.size,
                            offset: src_stack_slot.offset,
                        },
                        dest: PhysicalRegister(tmp2_reg.clone()),
                    }
                ));

            match **index {
                VirtualRegister(ref vregdata) => {
                    let tmp_reg = get_register_for_size(*size);
                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];


                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: stack_slot.size,
                                    offset: stack_slot.offset,
                                },
                                dest: PhysicalRegister(tmp_reg.clone()),
                            }
                        ));
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: PhysicalRegister(tmp2_reg),
                                dest: DynamicStackOffset {
                                    id: *id,
                                    size: *size,
                                    offset: get_indexed_array_offset(*offset, array_stack),
                                    index: Box::new(PhysicalRegister(tmp_reg)),
                                }
                            }
                        ));
                },
                IntegerConstant(val) => {
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: PhysicalRegister(tmp2_reg),
                                dest: StackOffset {
                                    size: *size,
                                    offset: get_constant_array_offset(*offset, src_stack_slot.size, val,  array_stack),
                                }
                            }
                        ));
                },
                _=> ice!("Unexpected dynamic index  {:?} ", index),
            }
        },
        UnaryOperation {
            src: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            },
            dest: VirtualRegister(dest_vregdata),

        } => {

            let value_register = get_register_for_size2(*size);
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let array_stack = &stack_map.array_to_stack_slot[id];
            match **index {
                VirtualRegister(ref vregdata) => {
                    let index_reg = get_register_for_size(*size);

                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: stack_slot.size,
                                    offset: stack_slot.offset,
                                },
                                dest: PhysicalRegister(index_reg.clone()),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: DynamicStackOffset {
                                    id: *id,
                                    size: *size,
                                    offset: get_indexed_array_offset(*offset, array_stack),
                                    index: Box::new(PhysicalRegister(index_reg)),
                                },
                                dest: PhysicalRegister(value_register),
                            }
                        ));

                },
                IntegerConstant(val) => {
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: *size,
                                    offset: get_constant_array_offset(*offset, *size, val, &array_stack),
                                },
                                dest: PhysicalRegister(value_register.clone()),
                            }
                        ));
                }
                _ => ice!("Unexpected dynamic index {:?} ", index)
            }


            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(value_register.clone()),
                        dest: StackOffset {
                            size: dest_stack_slot.size,
                            offset: dest_stack_slot.offset,
                        },
                    }
                ));
        },

        /*
            MOV tmp_reg, ptr_stack_location
            MOV tmp_reg2, indirect_read_using_tmp_reg
            MOV value_location, tmp_reg_2

        */
        UnaryOperation {
            src: IndirectAddress {
                base,
                index,
                offset,
                size,
            },
            dest: VirtualRegister(dest_vregdata),
        } => {

            let base_vregdata = if let VirtualRegister(ref vregdata) = **base {
                vregdata
            } else {
                ice!("Base register is not a virtual register: {}", base);
            };

            let base_stack_slot= &stack_map.reg_to_stack_slot[&base_vregdata.id];
            let base_register = get_register_for_size(base_stack_slot.size);

            let value_register = get_register_for_size2(*size);
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];


            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: StackOffset {
                            size: base_stack_slot.size,
                            offset: base_stack_slot.offset,
                        },
                        dest: PhysicalRegister(base_register.clone()),
                    }
                ));

                let (index, offset) = match index {
                    // bit clunky as box can't be matched against directly
                    Some(boxed) => {
                        match &**boxed {
                            VirtualRegister(vreg_data) => {
                                let stack_slot = &stack_map.reg_to_stack_slot[&vreg_data.id];
                                let index_reg = get_register_for_size3(stack_slot.size);

                                updated_instructions.push(
                                    ByteCode::Mov(
                                        UnaryOperation {
                                            src: StackOffset {
                                                size: stack_slot.size,
                                                offset: stack_slot.offset,
                                            },
                                            dest: PhysicalRegister(index_reg.clone()),
                                        }
                                    ));

                                (Some(Box::new(PhysicalRegister(index_reg.clone()))), *offset)
                            },
                            IntegerConstant(index) => {
                                let index = (-(*index)*(dest_stack_slot.size as i32)) as u32;
                                (None, Some(index))
                            },
                            _ => ice!("Unexpected index-operation {}", *boxed)
                        }
                    },
                    None  => (None, *offset),
                };

                updated_instructions.push(
                    ByteCode::Mov(
                        UnaryOperation {
                            src: IndirectAddress {
                                base: Box::new(Value::PhysicalRegister(base_register)),
                                index: index,
                                offset: offset.clone(),
                                size: *size,

                            } ,
                            dest: PhysicalRegister(value_register),
                        }
                    ));



            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(value_register.clone()),
                        dest: StackOffset {
                            size: dest_stack_slot.size,
                            offset: dest_stack_slot.offset,
                        },
                    }
                ));
        },
        UnaryOperation {
            src: VirtualRegister(src_vregdata),
            dest: IndirectAddress {
                base,
                index,
                offset,
                size,
            },
        } => {

            let base_vregdata = if let VirtualRegister(ref vregdata) = **base {
                vregdata
            } else {
                ice!("Base register is not a virtual register: {}", base);
            };

            let base_stack_slot= &stack_map.reg_to_stack_slot[&base_vregdata.id];
            let base_register = get_register_for_size(base_stack_slot.size);

            let value_register = get_register_for_size2(*size);
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];


            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: StackOffset {
                            size: base_stack_slot.size,
                            offset: base_stack_slot.offset,
                        },
                        dest: PhysicalRegister(base_register.clone()),
                    }
                ));

            let (index, offset) = match index {
                // bit clunky as box can't be matched against directly
                Some(boxed) => {
                    match &**boxed {
                        VirtualRegister(vreg_data) => {
                            let stack_slot = &stack_map.reg_to_stack_slot[&vreg_data.id];
                            let index_reg = get_register_for_size3(stack_slot.size);

                            updated_instructions.push(
                                ByteCode::Mov(
                                    UnaryOperation {
                                        src: StackOffset {
                                            size: stack_slot.size,
                                            offset: stack_slot.offset,
                                        },
                                        dest: PhysicalRegister(index_reg.clone()),
                                    }
                                ));

                            (Some(Box::new(PhysicalRegister(index_reg.clone()))), *offset)
                        },
                        IntegerConstant(index) => {
                            let index = (-(*index)*(src_stack_slot.size as i32)) as u32;
                            (None, Some(index))
                        },
                        _ => ice!("Unexpected index-operation {}", *boxed)
                    }
                },
                None  => (None, *offset),
            };

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: StackOffset {
                            size: src_stack_slot.size,
                            offset: src_stack_slot.offset,
                        },
                        dest: PhysicalRegister(value_register.clone()),
                    }
                ));

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(value_register),
                        dest: IndirectAddress {
                            base: Box::new(Value::PhysicalRegister(base_register)),
                            index: index,
                            offset: offset.clone(),
                            size: *size,
                        } ,
                    }
                ));
        },
        UnaryOperation {
            src: IntegerConstant(val),
            dest: IndirectAddress {
                base,
                index,
                offset,
                size,
            },
        } => {

            let base_vregdata = if let VirtualRegister(ref vregdata) = **base {
                vregdata
            } else {
                ice!("Base register is not a virtual register: {}", base);
            };

            let base_stack_slot= &stack_map.reg_to_stack_slot[&base_vregdata.id];
            let base_register = get_register_for_size(base_stack_slot.size);

            let value_register = get_register_for_size2(*size);

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: StackOffset {
                            size: base_stack_slot.size,
                            offset: base_stack_slot.offset,
                        },
                        dest: PhysicalRegister(base_register.clone()),
                    }
                ));

            let (index, offset) = match index {
                // bit clunky as box can't be matched against directly
                Some(boxed) => {
                    match &**boxed {
                        VirtualRegister(vreg_data) => {
                            let stack_slot = &stack_map.reg_to_stack_slot[&vreg_data.id];
                            let index_reg = get_register_for_size3(stack_slot.size);

                            updated_instructions.push(
                                ByteCode::Mov(
                                    UnaryOperation {
                                        src: StackOffset {
                                            size: stack_slot.size,
                                            offset: stack_slot.offset,
                                        },
                                        dest: PhysicalRegister(index_reg.clone()),
                                    }
                                ));

                            (Some(Box::new(PhysicalRegister(index_reg.clone()))), *offset)
                        },
                        IntegerConstant(index) => {
                            let index = (-(*index)*(*size as i32)) as u32;
                            (None, Some(index))
                        },
                        _ => ice!("Unexpected index-operation {}", *boxed)
                    }
                },
                None  => (None, *offset),
            };

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: IntegerConstant(*val),
                        dest: PhysicalRegister(value_register.clone()),
                    }
                ));

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(value_register),
                        dest: IndirectAddress {
                            base: Box::new(Value::PhysicalRegister(base_register)),
                            index: index,
                            offset: offset.clone(),
                            size: *size,
                        } ,
                    }
                ));
        },
        _ => unimplemented!("Not implemented for {:#?}", unary_op),
    }
}

fn handle_lea_allocation(unary_op: &UnaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &mut StackMap) {
    match unary_op {

        /*
            Emit:
            LEA tmp_reg, address_of_array
            MOV dest_stack_slot, tmp_reg

        */
        UnaryOperation {
            src: ArrayPtr { id },
            dest: VirtualRegister(dest_vregdata)
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.array_to_stack_slot[&id];
            let value_reg = get_register_for_size2(PTR_SIZE);


            updated_instructions.push(
                ByteCode::Lea(
                    UnaryOperation {
                        src: StackOffset {
                            offset: src_stack_slot.offset + src_stack_slot.size - ARRAY_LENGTH_SLOT_SIZE,
                            size: PTR_SIZE, // not really used in this context, we care about the offset only
                        },
                        dest: PhysicalRegister(value_reg.clone()),
                    }
                ));

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(value_reg.clone()),
                        dest: StackOffset {
                            offset: dest_stack_slot.offset,
                            size: dest_stack_slot.size,
                        }
                    }
                ));
        },
        _ => todo!("Not impemented for {:#?}", unary_op),
    }
}

fn handle_negate_allocation(unary_op: &UnaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match unary_op {
        UnaryOperation{
            src: VirtualRegister(src_vregdata),
            dest: VirtualRegister(dst_vregdata),
        } if src_vregdata.id == dst_vregdata.id => {
            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];

            updated_instructions.push(ByteCode::Negate(UnaryOperation {
                src: StackOffset {
                    offset: stack_slot.offset,
                    size: stack_slot.size,
                },
                dest: StackOffset {
                    offset: stack_slot.offset,
                    size: stack_slot.size,
                }
            }));
        },
        UnaryOperation{
            src: VirtualRegister(src_vregdata),
            dest: VirtualRegister(dst_vregdata),
        } if src_vregdata.id != dst_vregdata.id => {
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dst_vregdata.id];

            let reg = get_register_for_size(dst_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                src: StackOffset {
                    offset: src_stack_slot.offset,
                    size: src_stack_slot.size,
                },
                dest: PhysicalRegister(reg),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                src: PhysicalRegister(reg),
                dest: StackOffset {
                    offset: dest_stack_slot.offset,
                    size: dest_stack_slot.size,
                },
            }));

            updated_instructions.push(ByteCode::Negate(
                UnaryOperation {
                    src: StackOffset {
                        offset: dest_stack_slot.offset,
                        size: dest_stack_slot.size,
                    },
                    dest: StackOffset {
                        offset: dest_stack_slot.offset,
                        size: dest_stack_slot.size,
                    }
                }
            ));

        }
        _ => unimplemented!("Not implemented for: {:#?}", unary_op),
    }
}


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

fn handle_mod_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match binary_op {

        /*
            A = constant / constant

            emit:

            MOV EAX, dividend
            MOV TMP_REGISTER, divisor
            SIGN_EXTEND EAX
            IDIV TMP_REGISTER
            MOV A, EDX

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
                    src: PhysicalRegister(X64Register::EDX),
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
            MOV A, EDX


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
                    src: PhysicalRegister(X64Register::EDX),
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
            MOV A, EDX


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
                    src: PhysicalRegister(X64Register::EDX),
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
                    src: PhysicalRegister(X64Register::EDX),
                    dest: StackOffset { offset: dest_stack_slot.offset, size: dest_stack_slot.size },
                })
            );
        },
        _ => unimplemented!("Not implemented for {:#?}", binary_op),
    }
}

fn handle_xor_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match binary_op {

        /*
        a = a XOR constant

        directly encodable, just emit:

        XOR stack_slot, constant

        */
        BinaryOperation {
            src1: VirtualRegister(src_vregdata),
            src2: constant @ IntegerConstant(_),
            dest: VirtualRegister(dest_vregdata),
        } if src_vregdata.id == dest_vregdata.id => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let stack = StackOffset {
                size: dest_stack_slot.size,
                offset: dest_stack_slot.offset,
            };

            updated_instructions.push(ByteCode::Xor(BinaryOperation {
                src1: stack.clone(),
                src2: constant.clone(),
                dest: stack,
            }));
        },
        /*
        a = b XOR constant

        not directly encodable,  emit:

        MOV tmp_reg, b
        MOV a, tmp_reg
        XOR a, constant

        */
        BinaryOperation {
            src1: VirtualRegister(src_vregdata),
            src2: constant @ IntegerConstant(_),
            dest: VirtualRegister(dest_vregdata),
        } if src_vregdata.id != dest_vregdata.id => {

            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let reg = get_register_for_size(src1_stack_slot.size);

            let src_stack = StackOffset {
                size: src1_stack_slot.size,
                offset: src1_stack_slot.offset,
            };

            let dest_stack = StackOffset {
                size: dest_stack_slot.size,
                offset: dest_stack_slot.offset,
            };

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                src: src_stack,
                dest: PhysicalRegister(reg),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(reg),
                dest: dest_stack.clone(),
            }));

            updated_instructions.push(ByteCode::Xor(BinaryOperation {
                src1: dest_stack.clone(),
                src2: constant.clone(),
                dest: dest_stack,
            }));
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
        Some(IntegerConstant(_)) => {
            updated_instructions.push(ByteCode::Ret(value.clone()));
        },
        Some(BooleanConstant(_)) => {
            updated_instructions.push(ByteCode::Ret(value.clone()));
        },
        Some(ComparisonResult(_)) => {
            // FIXME: Replace with xor rax, rax
            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: IntegerConstant(0),
                    dest: PhysicalRegister(X64Register::RAX),
                }
            ));
            updated_instructions.push(ByteCode::Ret(value.clone()));

        },
        None => updated_instructions.push(ByteCode::Ret(None)),
        _ => unimplemented!("Not implemented for {:#?}:", value),
    }
}

fn handle_comparison(comparison_op: &ComparisonOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match comparison_op {

        /*
            a CMP b

            emit:

            MOV tmp_reg, a
            CMP tmp_reg, b

        */
        ComparisonOperation{
            src1: IntegerConstant(val1),
            src2: IntegerConstant(val2)
        } => {

            let reg = get_register_for_size(4);
            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: IntegerConstant(*val1),
                    dest: PhysicalRegister(reg),
                }
            ));

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: PhysicalRegister(reg),
                        src2: IntegerConstant(*val2),
                    }
                )
            );

        },
        /*
            A CMP constant

            emit:
            CMP a, constant

        */
        ComparisonOperation {
            src1: VirtualRegister(src_vregdata),
            src2: IntegerConstant(val),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: StackOffset {
                            offset: stack_slot.offset,
                            size: stack_slot.size,
                        },
                        src2: IntegerConstant(*val),
                    }
                )
            )
        },
        ComparisonOperation {
            src1: VirtualRegister(src_vregdata),
            src2: constant @ BooleanConstant(_),
        } | ComparisonOperation {
            src1: constant @ BooleanConstant(_),
            src2: VirtualRegister(src_vregdata)
        } => {
            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: StackOffset {
                            offset: stack_slot.offset,
                            size: stack_slot.size,
                        },
                        src2: constant.clone()
                    }
                )
            )

        },
        /*
            constant CMP A

            emit:
            MOV tmp_reg, constant
            CMP tmp_reg, A
        */
        ComparisonOperation {
            src1: IntegerConstant(val),
            src2: VirtualRegister(src_vregdata),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let reg = get_register_for_size(stack_slot.size);

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: IntegerConstant(*val),
                    dest: PhysicalRegister(reg),
                }
            ));

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: PhysicalRegister(reg),
                        src2: StackOffset {
                            offset: stack_slot.offset,
                            size: stack_slot.size,
                        }
                    }
                )
            );

        },
        /*
            A CMP B

            emit:
            MOV tmp_reg, A
            CMP tmp_reg, B



        */
        ComparisonOperation {
            src1: VirtualRegister(src1_vregdata),
            src2: VirtualRegister(src2_vregdata),
        } => {

            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
            let reg = get_register_for_size(src1_stack_slot.size);

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: StackOffset {
                        offset: src1_stack_slot.offset,
                        size: src1_stack_slot.size,
                    },
                    dest: PhysicalRegister(reg),
                }
            ));

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: PhysicalRegister(reg),
                        src2: StackOffset {
                            offset: src2_stack_slot.offset,
                            size: src2_stack_slot.size,
                        }
                    }
                )
            );
        },
        _ => unimplemented!("{:#?}", comparison_op),
    }
}

fn handle_function_arguments(args: &Vec<Value>,  updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {

    /*
    stack arguments need to pushed right-to left, so first iterate over args that go into regs,
    then reverse-iterate if there are more than 6 args and push those into stack
    */

    for i in (0..args.len()).rev() {
        let value =  &args[i];

        // register arguments
        if i < 6 {
            match value {
                IntegerConstant(val) => {
                    let dest = get_destination_for_integer_and_pointer_argument(i);

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: IntegerConstant(*val),
                                dest,
                            }
                        )
                    )
                },
                BooleanConstant(val) => {
                    let dest = get_destination_for_integer_and_pointer_argument(i);

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: BooleanConstant(*val),
                                dest,
                            }
                        )
                    )
                },
                VirtualRegister(vregdata) => {
                    let dest = get_destination_for_integer_and_pointer_argument(i);
                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: stack_slot.size,
                                    offset: stack_slot.offset,
                                },
                                dest,
                            }
                        ));
                },
                _ => unimplemented!("Not implemented for:\n{:#?}", value),
            }
        } else {
            // stack arguments

            let value = match value {
                IntegerConstant(_) => value.clone(),
                VirtualRegister(vregdata) => {
                    // cannot push stack offset directly, as due to usage of RBP, it defaults to
                    // 64 bit operand, and stack slots in general are not of this size (32 bit
                    // for integer). As such, need to move value from stack slot to register, and
                    // push it
                    //
                    //  Example attempt to push stack slot directly:
                    //      push   QWORD PTR [rbp-0x8]
                    //
                    // Could emit a push directly of 8 byte variable in stack though

                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
                    let reg = PhysicalRegister(get_register_for_size(stack_slot.size));

                    updated_instructions.push(
                        ByteCode::Mov(UnaryOperation {
                            src: StackOffset {
                                size: stack_slot.size,
                                offset: stack_slot.offset,
                            },
                            dest: reg.clone(),
                        })
                    );

                    reg
                },
                _ => unimplemented!("Not implemented for:\n{:#?}", value),
            };

            updated_instructions.push(ByteCode::Push(value));
        }
    }
}

// TODO - Make this less stupid
fn get_register_for_size(size: u32) -> X64Register {
    match size {
        1 => X64Register::AL,
        4 => X64Register::EAX,
        8 => X64Register::RAX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size2(size: u32) -> X64Register {
    match size {
        1 => X64Register::BL,
        4 => X64Register::EBX,
        8 => X64Register::RBX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size3(size: u32) -> X64Register {
    match size {
        1 => X64Register::CL,
        4 => X64Register::ECX,
        8 => X64Register::RCX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size_for_division(size: u32) -> X64Register {
    match size {
        1 => X64Register::BL,
        4 => X64Register::EBX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_destination_for_integer_and_pointer_argument(position: usize) -> Value {
    match position {
        0 => PhysicalRegister(X64Register::RDI),
        1 => PhysicalRegister(X64Register::RSI),
        2 => PhysicalRegister(X64Register::RDX),
        3 => PhysicalRegister(X64Register::RCX),
        4 => PhysicalRegister(X64Register::R8),
        5 => PhysicalRegister(X64Register::R9),
        _ => ice!("No register for position {}", position),
    }
}

// as stack grows downwards (subtract value from RBP to get correct spot), we either need to negate index register whenever it is used,
// or access arrays in such way that we always negate array size + offset, then add the positive index (array effectively reversed in memory)
//
// take this account when calculating offsets

fn get_indexed_array_offset(offset: u32, array_stack: &StackSlot) -> u32 {

    let constant_offset = -(offset as i32) + array_stack.offset as i32 + array_stack.size as i32;
    constant_offset as u32
}

fn get_constant_array_offset(offset: u32, size: u32, index: i32, array_stack: &StackSlot) -> u32 {

    let constant_offset = -(offset as i32) + array_stack.offset as i32 + array_stack.size as i32 - (size as i32)*index;
    constant_offset as u32
}

#[cfg(test)]
mod tests {

    use super::*;

    const STACK_OFFSET: u32 = 8;
    const TMP_REGISTER: X64Register = X64Register::EAX;
    const DIV_TMP_REGISTER: X64Register = X64Register::EBX;

    fn get_functions(bytecode: Vec<ByteCode>) -> Vec<Function> {
        vec![
            Function {
                name: "foo".to_owned(),
                code:  bytecode,
                parameter_count: 0,
                attributes: vec![],
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(4),
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(9),
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                }
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(8),
                src1: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(24),
                src1: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(24),
                src1: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 4 + STACK_OFFSET,
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
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
                    size: 4,
                },
            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: IntegerConstant(7),
                src1: StackOffset {
                    offset: 4 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Add(BinaryOperation{
                src2: StackOffset {
                    offset: 4 + STACK_OFFSET,
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
                    offset: 8 + STACK_OFFSET,
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
                ByteCode::Ret(Some(IntegerConstant(20))),
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Ret(Some(IntegerConstant(20))),
            allocated_code[0],
        );
    }

    #[test]
    fn should_work_with_void_return() {
        let functions = get_functions(
            vec![
                ByteCode::Ret(None),
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Ret(None),
            allocated_code[0],
        );
    }

    #[test]
    fn should_allocate_reg_return() {
        let functions = get_functions(
            vec![
                ByteCode::Ret(
                    Some(VirtualRegister(
                    VirtualRegisterData {
                        size: 4,
                        id: 0,
                    }
                ))),
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Ret(
                Some(StackOffset{
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                })),
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

        let allocations = allocate(functions, false);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(9),
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                }
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: IntegerConstant(8),
                src1: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());


        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: IntegerConstant(8),
                src1: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);
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
                src2: StackOffset { offset: 0 + STACK_OFFSET, size: 4},
                src1: PhysicalRegister(TMP_REGISTER),
                dest: PhysicalRegister(TMP_REGISTER),

            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: PhysicalRegister(TMP_REGISTER),
                dest: StackOffset { offset: 0 + STACK_OFFSET , size: 4},
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 4 + STACK_OFFSET,
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
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
                    size: 4,
                },
            }),
            allocated_code[1],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: IntegerConstant(7),
                src1: StackOffset {
                    offset: 4 + STACK_OFFSET,
                    size: 4,
                },
                dest: StackOffset {
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Sub(BinaryOperation{
                src2: StackOffset {
                    offset: 4 + STACK_OFFSET,
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
                    offset: 8 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

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
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset{
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset{
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(3, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset{
                    offset: 0 + STACK_OFFSET,
                    size: 4
                },
                dest: PhysicalRegister(TMP_REGISTER),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Mul(BinaryOperation{
                src2: StackOffset {
                    offset: 4 + STACK_OFFSET,
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
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

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
                    offset: 0 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(5, allocated_code.len());


        assert_eq!(
            ByteCode::Mov(UnaryOperation {
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

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
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
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

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(4, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset{
                    offset: 0 + STACK_OFFSET,
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
                    offset: 4 + STACK_OFFSET,
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
                    offset: 8 + STACK_OFFSET,
                    size: 4,
                },
            }),
            allocated_code[3]
        );
    }

    #[test]
    fn should_allocate_regs_for_constant_constant_comparison() {

        let functions = get_functions(
            vec![
                ByteCode::Compare(ComparisonOperation {
                    src1: IntegerConstant(4),
                    src2: IntegerConstant(8),
                })
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(4),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0]
        );

        assert_eq!(
            ByteCode::Compare(ComparisonOperation{
                src1: PhysicalRegister(X64Register::EAX),
                src2: IntegerConstant(8),
            }),
            allocated_code[1]
        );

    }

    #[test]
    fn should_allocate_regs_for_reg_constant_comparison() {

        let functions = get_functions(
            vec![
                ByteCode::Compare(ComparisonOperation {
                    src1: VirtualRegister(
                        VirtualRegisterData{
                            id: 0,
                            size: 4,
                        }),
                    src2: IntegerConstant(8),
                })
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Compare(ComparisonOperation{
                src1: StackOffset{
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
                src2: IntegerConstant(8),
            }),
            allocated_code[0]
        );
    }

    #[test]
    fn should_allocate_regs_for_constant_reg_comparison() {

        let functions = get_functions(
            vec![
                ByteCode::Compare(ComparisonOperation {
                    src1: IntegerConstant(8),
                    src2: VirtualRegister(
                        VirtualRegisterData{
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: IntegerConstant(8),
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Compare(ComparisonOperation{
                src1: PhysicalRegister(X64Register::EAX),
                src2: StackOffset{
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                },
            }),
            allocated_code[1],
        );
    }

    #[test]
    fn should_allocate_regs_for_reg_reg_comparison() {

        let functions = get_functions(
            vec![
                ByteCode::Compare(ComparisonOperation {
                    src1: VirtualRegister(
                        VirtualRegisterData {
                            id: 1,
                            size: 4,
                        }),
                    src2: VirtualRegister(
                        VirtualRegisterData{
                            id: 0,
                            size: 4,
                        }),
                })
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(2, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4
                },
                dest: PhysicalRegister(X64Register::EAX),
            }),
            allocated_code[0],
        );

        assert_eq!(
            ByteCode::Compare(ComparisonOperation{
                src1: PhysicalRegister(X64Register::EAX),
                src2: StackOffset{
                    offset: 4 + STACK_OFFSET,
                    size: 4,
                },
            }),
            allocated_code[1],
        );
    }

    #[test]
    fn should_allocate_regs_for_reg_boolean_constant_move() {
        let functions = get_functions(
            vec![
                ByteCode::Mov(UnaryOperation {
                    src: BooleanConstant(true),
                    dest: VirtualRegister(
                        VirtualRegisterData{
                            id: 0,
                            size: 1,
                        }),
                })
            ]
        );

        let allocations = allocate(functions, false);

        assert_eq!(1, allocations.len());
        let allocated_code = &allocations[0].0.code;
        assert_eq!(1, allocated_code.len());

        assert_eq!(
            ByteCode::Mov(UnaryOperation{
                src: BooleanConstant(true),
                dest: StackOffset {
                    offset: 0 + STACK_OFFSET,
                    size: 4,
                }}),
            allocated_code[0],
        );

    }
}