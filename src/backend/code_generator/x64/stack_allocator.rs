use super::super::super::byte_generator::byte_code::{
    Function,
    ByteCode,
    Value,
    Value::*,
    UnaryOperation,
    VirtualRegisterData,
    BinaryOperation,
    ComparisonOperation
};

use super::x64_register::X64Register;

const PTR_SIZE: u32 =  8;

use rayon::prelude::*;
use std::collections::HashMap;

use crate::common::{
    function_attributes::FunctionAttribute,
    types::Type,
};
use crate::backend::byte_generator::byte_code::Value::DynamicStackOffset;

#[derive(Debug, Clone)]
struct StackSlot {
    offset: u32,
    size: u32,
}

impl From<StackSlot> for Value {
    fn from(stack_slot: StackSlot) -> Self {
        (&stack_slot).into()
    }
}

impl From<&StackSlot> for Value {
    fn from(stack_slot: &StackSlot) -> Self {
        Value::StackOffset {
            offset: stack_slot.offset,
            size: stack_slot.size,
        }
    }
}

impl From<X64Register> for Value {
    fn from(reg: X64Register) -> Self {
        PhysicalRegister(reg)
    }
}

impl From<&X64Register> for Value {
    fn from(reg: &X64Register) -> Self {
        reg.clone().into()
    }
}


#[derive(Debug)]
struct StackMap {
    reg_to_stack_slot: HashMap<u32, StackSlot>,
    object_to_stack_slot: HashMap<u32, StackSlot>, // reg and array can be same
    stack_size: u32, // stack size, size chosen so that stack is aligned to 16 byte boundary (adjusted for arg pushes)
    stack_space_used: u32, // stack size, unaligned.
}

impl StackMap {
    fn new() -> StackMap {
        StackMap {
            reg_to_stack_slot: HashMap::new(),
            object_to_stack_slot: HashMap::new(),
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
            ByteCode::Ret(Some(LongConstant(_))) |
            ByteCode::Ret(Some(IntegerConstant(_))) |
            ByteCode::Ret(Some(ShortConstant(_))) |
            ByteCode::Ret(Some(ByteConstant(_))) |
            ByteCode::Ret(Some(ComparisonResult(_))) |
            ByteCode::Ret(None) |
            ByteCode::JumpConditional(_, _) |
            ByteCode::FunctionArguments(_) |
            ByteCode::Call(_) => (), // do nothing
            ByteCode::PseudoArrayInit { id, size_in_bytes} => add_array_location(&mut stack_map, *id, *size_in_bytes),
            ByteCode::PseudoStructInit { size_in_bytes, id } => add_struct_location(&mut stack_map, *id, *size_in_bytes),


            ByteCode::Mov(unary_op) |
            ByteCode::Movzx(unary_op) |
            ByteCode::Movsx(unary_op) |
            ByteCode::Lea(unary_op) |
            ByteCode::Not(unary_op) |
            ByteCode::Negate(unary_op) => handle_unary_op(unary_op, &mut stack_map),
            ByteCode::Add(binary_op) |
            ByteCode::Sub(binary_op) |
            ByteCode::Mul(binary_op) |
            ByteCode::Div(binary_op) |
            ByteCode::Mod(binary_op) |
            ByteCode::Sar(binary_op) |
            ByteCode::Shr(binary_op) |
            ByteCode::And(binary_op) |
            ByteCode::Or(binary_op) |
            ByteCode::Xor(binary_op) |
            ByteCode::Shl(binary_op) => handle_binary_op(binary_op, &mut stack_map),

            ByteCode::Compare(comparison_op) => handle_comparison_op(comparison_op, &mut stack_map),

            ByteCode::Ret(Some(Value::VirtualRegister(ref vrefdata))) => add_location(&mut stack_map, vrefdata),
            _ => unimplemented!("{:#?}", *instr)
        }
    }

    // 16 byte align the stack

    stack_map.stack_space_used = stack_map.stack_size;
    align_slot_to_data_size(&mut stack_map, 16);



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
        Value::ArrayPtr { offset: Some(value)  , ..} => add_if_register(value, stack_map),
        _ => (),
    }
}

fn add_location(map: &mut StackMap, data: &VirtualRegisterData) {
    if !map.reg_to_stack_slot.contains_key(&data.id) {

        align_slot_to_data_size(map, data.size);
        ice_if!(!data.size.is_power_of_two(), "Virtual register {:?} has size which is not power of two!", data);
        map.stack_size += data.size;
        map.reg_to_stack_slot.insert(data.id, StackSlot{ offset: map.stack_size, size: data.size } );
    }
}

fn align_slot_to_data_size(stack_map: &mut StackMap, size: u32) {
    ice_if!(!size.is_power_of_two(), "Stack must be aligned to power of two values, now aligning to {}", size);
    stack_map.stack_size = if stack_map.stack_size & (size-1) == 0 {
        stack_map.stack_size
    } else {
        (stack_map.stack_size | (size-1)) + 1
    };
}

fn add_array_location(map: &mut StackMap, id: u32, size_in_bytes: u32) {
    if !map.object_to_stack_slot.contains_key(&id) {
        let slot_size = if size_in_bytes >= 4 && size_in_bytes & 0b11 == 0 {
            size_in_bytes
        } else {
            (size_in_bytes | 0b11) + 1
        };

        map.object_to_stack_slot.insert(id, StackSlot { offset: map.stack_size, size: slot_size });
        map.stack_size += slot_size;
    } else {
        ice!("Multiple array declarations for array id {}", id);
    }
}

fn add_struct_location(map: &mut StackMap, id: u32, size_in_bytes: u32) {
    if !map.object_to_stack_slot.contains_key(&id) {
        align_slot_to_data_size(map, 8); // 8 byte align, should mean arbitrary first object is aligned, and struct is already internally aligned

        map.object_to_stack_slot.insert(id, StackSlot { offset: map.stack_size, size: size_in_bytes });
        map.stack_size += size_in_bytes;
    } else {
        ice!("Multiple struct declarations for struct id {}", id);
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

    for (id, slot) in stack_map.object_to_stack_slot.iter() {
        let tuple = (slot.clone(), format!("Object {}", id));
        stack_slot_to_entity.push(tuple);
    }

    stack_slot_to_entity.sort_by(|(slot, _), (slot2, _)| slot.offset.cmp(&slot2.offset));


    for (slot, entity) in stack_slot_to_entity.iter() {
        println!("    Stack slot at position 0x{:x}, size {}, occupied by {}", slot.offset, slot.size, entity);
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
    updated_instructions.reserve(code.len());
    for instr in code.iter() {
        match instr {
            ByteCode::Mov(unary_op) => handle_mov_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Movsx(unary_op) => handle_movsx_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Lea(unary_op) => handle_lea_allocation(unary_op, &mut updated_instructions, stack_map),
            ByteCode::Negate(unary_op) => handle_unary_allocation(
                ByteCode::Negate,
                unary_op,
                &mut updated_instructions,
                stack_map),
            ByteCode::Add(binary_op) =>
                handle_binary_allocation(
                    ByteCode::Add,
                    OpProperties { commutativity: Commutativity::Commutative },
                    binary_op,
                    &mut updated_instructions,
                    stack_map),

            ByteCode::Sub(binary_op) =>
                handle_binary_allocation(
                    ByteCode::Sub,
                    OpProperties {commutativity: Commutativity::AntiCommutative },
                    binary_op,
                    &mut updated_instructions,
                    stack_map),
            ByteCode::Mul(binary_op) => handle_mul_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Div(binary_op) => handle_div_allocation(binary_op, &mut updated_instructions, stack_map),
            ByteCode::Mod(binary_op) => handle_mod_allocation(binary_op, &mut updated_instructions, stack_map),

            ByteCode::Shl(binary_op) =>
                handle_shift_allocation(ByteCode::Shl, binary_op, &mut updated_instructions, stack_map),
            ByteCode::Sar(binary_op) =>
                handle_shift_allocation(ByteCode::Sar, binary_op, &mut updated_instructions, stack_map),
            ByteCode::Shr(binary_op) =>
                handle_shift_allocation(ByteCode::Shr, binary_op, &mut updated_instructions, stack_map),

            ByteCode::And(binary_op) =>
                handle_binary_allocation(
                    ByteCode::And,
                    OpProperties {commutativity: Commutativity::Commutative },
                    binary_op,
                    &mut updated_instructions,
                    stack_map),
            ByteCode::Or(binary_op) =>
                handle_binary_allocation(
                    ByteCode::Or,
                    OpProperties { commutativity: Commutativity::Commutative },
                    binary_op,
                    &mut updated_instructions,
                    stack_map),
            ByteCode::Xor(binary_op) =>
                handle_binary_allocation(
                    ByteCode::Xor,
                    OpProperties { commutativity: Commutativity::Commutative },
                    binary_op,
                    &mut updated_instructions,
                    stack_map),
            ByteCode::Not(unary_op) => handle_unary_allocation(
                ByteCode::Not,
                &unary_op,
                &mut updated_instructions,
                stack_map),
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
            ByteCode::PseudoArrayInit { .. } |
            ByteCode::PseudoStructInit { .. } => (), // pseudo opcode for stack size adjustment,
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

           MOV A, constant IF constant fits in 32 bits

           otherwise emit:
           MOV tmp_reg, constant
           MOV A, tmp_reg

        */
        UnaryOperation{
            dest: VirtualRegister(ref vregdata),
            src: LongConstant(val)
        }  => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            ice_if!(vregdata.size > stack_slot.size, "Attempt to store {} bytes into stack slot with size {}", vregdata.size, stack_slot.size);

            mov_long_constant_to_stack(updated_instructions, val, stack_slot)
        },
        UnaryOperation{
            dest: VirtualRegister(ref vregdata),
            src: val @ IntegerConstant(_)
        } |
        UnaryOperation{
            dest: VirtualRegister(ref vregdata),
            src: val @ ShortConstant(_)
        } |
        UnaryOperation{
            dest: VirtualRegister(ref vregdata),
            src: val @ ByteConstant(_)
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            ice_if!(vregdata.size > stack_slot.size, "Attempt to store {} bytes into stack slot with size {}", vregdata.size, stack_slot.size);
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
               dest: StackOffset{offset: stack_slot.offset, size: vregdata.size},
               src: val.clone(),
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

            ice_if!(src_vregdata.size > src_stack_slot.size, "Attempt to read {} bytes from stack slot with size {}", src_vregdata.size, src_stack_slot.size);
            ice_if!(dest_vregdata.size > dest_stack_slot.size, "Attempt to store {} bytes into stack slot with size {}", dest_vregdata.size, dest_stack_slot.size);

            ice_if!(dest_vregdata.size != src_vregdata.size, "Stack slot sizes do not match: {} vs {}", dest_vregdata.size, src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                dest: PhysicalRegister(get_register_for_size(dest_vregdata.size)),
                src: StackOffset{offset: src_stack_slot.offset, size: src_vregdata.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_vregdata.size},
                src: PhysicalRegister(get_register_for_size(dest_vregdata.size)),
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
                        src: PhysicalRegister(get_return_value_register_for_size(stack_slot.size)),
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
                Type::Long | Type::Integer | Type::Short | Type::Boolean | Type::Byte | Type::Reference(_) => {
                    let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
                    if *pos < 6 {


                        // only limited number of registers have 8 bit aliases. Emit additional move
                        // from source reg to a register with its 8 bit alias, then move this tmp reg using its 32/64 bit alias to stack
                        let src = if vregdata.size == 1 {
                            let byte_alias = get_register_for_size(1);
                            let qword_alias = get_register_for_size(4);

                            updated_instructions.push(
                                ByteCode::Mov(
                                    UnaryOperation {
                                        src: get_destination_for_integer_and_pointer_argument(*pos, 4),
                                        dest: PhysicalRegister(qword_alias),
                                    }
                                ));

                            PhysicalRegister(byte_alias)
                        } else {
                            get_destination_for_integer_and_pointer_argument(*pos, stack_slot.size)
                        };

                        updated_instructions.push(
                            ByteCode::Mov(
                                UnaryOperation {
                                    src,
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
            src: LongConstant(value),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } => {
            let fits_in_32_bit = i64_fits_in_i32(*value);
            let array_stack = &stack_map.object_to_stack_slot[id];
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
                                dest: PhysicalRegister(tmp_reg.get_alias_for_size(stack_slot.size as u8)),
                            }
                        ));

                    let src = if fits_in_32_bit {
                        LongConstant(*value)
                    } else {
                        let tmp2_reg =  get_register_for_size2(8);

                        updated_instructions.push(
                            ByteCode::Mov(
                                UnaryOperation {
                                    src: LongConstant(*value),
                                    dest: PhysicalRegister(tmp2_reg),
                                }
                            ));


                        PhysicalRegister(tmp2_reg)
                    };

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src,
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

                    let src = if fits_in_32_bit {
                        LongConstant(*value)
                    } else {
                        let tmp2_reg =  get_register_for_size2(8);

                        updated_instructions.push(
                            ByteCode::Mov(
                                UnaryOperation {
                                    src: LongConstant(*value),
                                    dest: PhysicalRegister(tmp2_reg),
                                }
                            ));


                        PhysicalRegister(tmp2_reg)
                    };

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src,
                                dest: StackOffset {
                                    size: *size,
                                    offset: get_constant_object_offset(*offset, *size, index, &array_stack),
                                }
                            }
                        ));
                }
                _ => ice!("Unexpected dynamic index {:?} ", index)
            }
        },
        UnaryOperation {
            src: val @ IntegerConstant(_),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } |
        UnaryOperation {
            src: val @ ShortConstant(_),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } |
        UnaryOperation {
            src: val @ ByteConstant(_),
            dest: DynamicStackOffset {
                id,
                size,
                offset,
                index,
            }
        } => {

            let array_stack = &stack_map.object_to_stack_slot[id];
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
                                dest: PhysicalRegister(tmp_reg.get_alias_for_size(stack_slot.size as u8)),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: val.clone(),
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
                                src: val.clone(),
                                dest: StackOffset {
                                    size: *size,
                                    offset: get_constant_object_offset(*offset, *size, index, &array_stack),
                                }
                            }
                        ));
                }
                _ => ice!("Unexpected dynamic index {:?} ", index)
            }

        },

        /*
            arr[foo] = byte_constant,

            Emit:

               IF foo is variable

                    MOV TMP_REG, index_var_stack_storage_location
                    MOV [stack_ptr + offset + TMP_REG*index_var_size], byte_constant

               IF foo is constant:

                    MOV [stack_ptr + constant_offset], byte_constant

        */
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
            let array_stack = &stack_map.object_to_stack_slot[id];
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
                    let index_reg = get_register_for_size(4);
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
                                src: PhysicalRegister(tmp2_reg),
                                dest: DynamicStackOffset {
                                    id: *id,
                                    size: *size,
                                    offset: get_indexed_array_offset(*offset, array_stack),
                                    index: Box::new(PhysicalRegister(index_reg)),
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
                                    offset: get_constant_object_offset(*offset, src_stack_slot.size, val, array_stack),
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
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &value_register,
                object_stack_slot,
                updated_instructions,
                stack_map);


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
                                let index_reg = get_register_for_size3(4);

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
                                index,
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
                            index,
                            offset: offset.clone(),
                            size: *size,
                        } ,
                    }
                ));
        },
        UnaryOperation {
            src: val @ ByteConstant(_),
            dest: IndirectAddress {
                base,
                index,
                offset,
                size,
            }
        } |
        UnaryOperation {
            src: val @ LongConstant (_),
            dest: IndirectAddress {
                base,
                index,
                offset,
                size,
            }
       } |
        UnaryOperation {
            src: val @ IntegerConstant (_),
            dest: IndirectAddress {
                base,
                index,
                offset,
                size,
            }
       } |
       UnaryOperation {
            src: val @ ShortConstant (_),
            dest: IndirectAddress {
                base,
                index,
                offset,
                size,
            }
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
                        src: val.clone(),
                        dest: PhysicalRegister(value_register.clone()),
                    }
                ));

            updated_instructions.push(
                ByteCode::Mov(
                    UnaryOperation {
                        src: PhysicalRegister(value_register),
                        dest: IndirectAddress {
                            base: Box::new(Value::PhysicalRegister(base_register)),
                            index,
                            offset: offset.clone(),
                            size: *size,
                        } ,
                    }
                ));
        },
        _ => unimplemented!("Not implemented for {:#?}", unary_op),
    }
}

fn mov_long_constant_to_stack(updated_instructions: &mut Vec<ByteCode>, val: &i64, stack_slot: &StackSlot) {
    if i64_fits_in_i32(*val) {
        updated_instructions.push(ByteCode::Mov(UnaryOperation {
            dest: StackOffset { offset: stack_slot.offset, size: 8 },
            src: LongConstant(*val),
        }));
    } else {
        let tmp_reg = get_register_for_size(8);
        updated_instructions.push(ByteCode::Mov(UnaryOperation {
            dest: PhysicalRegister(tmp_reg),
            src: LongConstant(*val),
        }));
        updated_instructions.push(ByteCode::Mov(UnaryOperation {
            dest: StackOffset { offset: stack_slot.offset, size: 8 },
            src: PhysicalRegister(tmp_reg),
        }));
    }
}


fn handle_movsx_allocation(unary_op: &UnaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &mut StackMap) {
    match unary_op {
          UnaryOperation{dest: VirtualRegister(ref dest_vregdata), src: VirtualRegister(ref src_vregdata)} => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];


            updated_instructions.push(ByteCode::Movsx(UnaryOperation {
                dest: PhysicalRegister(get_register_for_size(dest_stack_slot.size)),
                src: StackOffset{offset: src_stack_slot.offset, size: src_stack_slot.size},
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation {
                dest: StackOffset{offset: dest_stack_slot.offset, size: dest_vregdata.size},
                src: PhysicalRegister(get_register_for_size(dest_vregdata.size)),
            }));


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
            src: ArrayPtr { id, offset, .. },
            dest: VirtualRegister(dest_vregdata)
        } if offset.is_none() => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.object_to_stack_slot[&id];
            let value_reg = get_register_for_size2(PTR_SIZE);


            updated_instructions.push(
                ByteCode::Lea(
                    UnaryOperation {
                        src: StackOffset {
                            offset: src_stack_slot.offset + src_stack_slot.size,
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
        UnaryOperation {
            src: ArrayPtr { id, offset: Some(value), array_size },
            dest: VirtualRegister(dest_vregdata)
        } => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.object_to_stack_slot[&id];
            let value_reg = get_register_for_size2(PTR_SIZE);

            match **value {
                Value::IntegerConstant(offset) => {
                       updated_instructions.push(
                           ByteCode::Lea(
                               UnaryOperation {
                                   src: StackOffset {
                                       offset: src_stack_slot.offset + src_stack_slot.size - array_size*offset as u32,
                                       size: PTR_SIZE, // not really used in this context, we care about the offset only
                                   },
                                   dest: PhysicalRegister(value_reg.clone()),
                        }
                    ));
                },
                Value::VirtualRegister(ref start_value_vregdata) => {
                    let start_stack_slot = &stack_map.reg_to_stack_slot[&start_value_vregdata.id];
                    updated_instructions.push(
                        ByteCode::Lea(
                            UnaryOperation {
                                src: StackOffset {
                                    offset: src_stack_slot.offset + src_stack_slot.size,
                                    size: PTR_SIZE, // not really used in this context, we care about the offset only
                                },
                                dest: PhysicalRegister(value_reg.clone()),
                            }
                        ));

                    let tmp_reg = get_register_for_size3(start_stack_slot.size);


                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: StackOffset {
                                    size: start_stack_slot.size,
                                    offset: start_stack_slot.offset,
                                },
                                dest: PhysicalRegister(tmp_reg.clone()),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Mul(
                            BinaryOperation {
                                src1: PhysicalRegister(tmp_reg.clone()),
                                src2: IntegerConstant(*array_size as i32),
                                dest: PhysicalRegister(tmp_reg.clone()),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Add(
                            BinaryOperation {
                                src1: PhysicalRegister(value_reg.clone()),
                                src2: PhysicalRegister(tmp_reg.get_alias_for_size(value_reg.size()).clone()),
                                dest: PhysicalRegister(value_reg.clone()),
                            }
                        ));



                },
                _ => ice!("Unexpected value type {:?}", value),
            }

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
        }
        _ => todo!("Not impemented for {:#?}", unary_op),
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
            src1: src1 @ IntegerConstant(_),
            src2: src2 @ IntegerConstant(_),
        } => {
            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg = get_register_for_size(stack_slot.size);
            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: src1.clone(),
                    dest: reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: reg.into(),
                        src1: reg.into(),
                        src2: src2.clone(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: reg.into(),
                    dest: stack_slot.into(),
                })
            );
        },
        /*
            A = B * constant OR A = constant*B
            in case the latter, switch around to former

            directly encodable, as long as destination is a register and constant fits immediate32. Need to add few moves from/to/stack

            for bytes, we use integer multiplication due to missing encoding for r8*imm8; use r32*imm8


            emit:

            MOV tmp_register, B
            IMUL tmp_register, tmp_register, constant
            MOV A, tmp_register

            if byte, replace first MOV with MOVZX, adjust input/output sizes accordingly so that multiplication is 32 bit operation but 8 bit destination is used

            if constant does not fit imm32, emit:

            MOV tmp_register_1, B
            MOV tmp_register_2, constant
            IMUL tmp_register_1, tmp_register_2,
            MOV A, tmp_register




        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
            src2: VirtualRegister(ref src_vregdata),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];


            // no instruction for 8 bit multiply, need to use encoding for different sizse
            let dest_size = if dest_stack_slot.size == 1 { 4 } else { dest_stack_slot.size };
            let src_size = if src_stack_slot.size == 1 { 4 } else { src_stack_slot.size };
            let reg = get_register_for_size(dest_size);

            if src_stack_slot.size == 1 {
                updated_instructions.push(
                    ByteCode::Movzx(UnaryOperation {
                        src: src_stack_slot.into(),
                        dest: reg.get_alias_for_size(src_size as u8).into(),
                    })
                );
            } else {
                updated_instructions.push(
                    ByteCode::Mov(UnaryOperation {
                        src: src_stack_slot.into(),
                        dest: reg.get_alias_for_size(src_size as u8).into(),
                    })
                );
            }



            match immediate {
                LongConstant(constant) if !i64_fits_in_i32(*constant)  => {
                    let reg2 = get_register_for_size2(dest_stack_slot.size);
                    updated_instructions.push(ByteCode::Mov(UnaryOperation{
                        src: immediate.clone(),
                        dest: reg2.into(),
                    }));
                    updated_instructions.push(ByteCode::Mul(BinaryOperation {
                        dest: reg.into(),
                        src1: reg.into(),
                        src2: reg2.into(),
                    }));
                },
                LongConstant(constant) if i64_fits_in_i32(*constant) => {
                    updated_instructions.push(
                        ByteCode::Mul(
                            BinaryOperation {
                                dest: reg.into(),
                                src1: reg.into(),
                                src2: IntegerConstant(*constant as i32),
                            }));
                }
                _ => {
                    updated_instructions.push(
                        ByteCode::Mul(
                            BinaryOperation {
                                dest: reg.into(),
                                src1: reg.into(),
                                src2: immediate.clone(),
                            }));
                }
            }

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: reg.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );

        },

        /*
            A = B * constant OR A = constant*B
            in case the latter, switch around to former

            directly encodable, as long as destination is a register and constant fits immediate32. Need to add few moves from/to/stack

            for bytes, we use integer multiplication due to missing encoding for r8*imm8; use r32*imm8


            emit:

            MOV tmp_register, B
            IMUL tmp_register, tmp_register, constant
            MOV A, tmp_register

            if byte, replace first MOV with MOVZX, adjust input/output sizes accordingly so that multiplication is 32 bit operation but 8 bit destination is used

            if constant does not fit imm32, emit:

            MOV tmp_register_1, B
            MOV tmp_register_2, constant
            IMUL tmp_register_1, tmp_register_2,
            MOV A, tmp_register




        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
            src2: DynamicStackOffset { index, offset, size, id },
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[&id];




            // no instruction for 8 bit multiply, need to use encoding for different sizse
            let dest_size = if dest_stack_slot.size == 1 { 4 } else { dest_stack_slot.size };
            let reg = get_register_for_size(dest_size);

            if dest_stack_slot.size == 1 {
                emit_movzx_dynamic_stack_offset_to_reg(
                    index,
                    *size,
                    *offset,
                    *id,
                    &reg,
                    object_stack_slot,
                    updated_instructions,
                    stack_map
                );
            } else {
                emit_mov_dynamic_stack_offset_to_reg(
                    index,
                    *size,
                    *offset,
                    *id,
                    &reg,
                    object_stack_slot,
                    updated_instructions,
                    stack_map
                );
            }



            match immediate {
                LongConstant(constant) if !i64_fits_in_i32(*constant)  => {
                    let reg2 = get_register_for_size2(dest_stack_slot.size);
                    updated_instructions.push(ByteCode::Mov(UnaryOperation{
                        src: immediate.clone(),
                        dest: reg2.into(),
                    }));
                    updated_instructions.push(ByteCode::Mul(BinaryOperation {
                        dest: reg.into(),
                        src1: reg.into(),
                        src2: reg2.into(),
                    }));
                },
                LongConstant(constant) if i64_fits_in_i32(*constant) => {
                    updated_instructions.push(
                        ByteCode::Mul(
                            BinaryOperation {
                                dest: reg.into(),
                                src1: reg.into(),
                                src2: IntegerConstant(*constant as i32),
                            }));
                }
                _ => {
                    updated_instructions.push(
                        ByteCode::Mul(
                            BinaryOperation {
                                dest: reg.into(),
                                src1: reg.into(),
                                src2: immediate.clone(),
                            }));
                }
            }

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: reg.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
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
        } if dest_vregdata.size >= 4 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
            let reg = get_register_for_size(dest_stack_slot.size);
            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: src1_stack_slot.into(),
                    dest: reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: reg.into(),
                        src1: reg.into(),
                        src2: src2_stack_slot.into(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
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
            src1: DynamicStackOffset{ index, offset, size, id },
            src2: VirtualRegister(ref src_vregdata),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: DynamicStackOffset{ index, offset, size, id },
        } if dest_vregdata.size >= 4 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size(dest_stack_slot.size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map,
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: reg.into(),
                        src1: reg.into(),
                        src2: src_stack_slot.into(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset{ index, offset, size, id },
            src2: DynamicStackOffset{ index: index2, offset: offset2, size: size2, id: id2 },
        } if dest_vregdata.size >= 4 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let object_stack_slot2 = &stack_map.object_to_stack_slot[id2];
            let reg = get_register_for_size(dest_stack_slot.size);
            let reg2 = get_register_for_size2(dest_stack_slot.size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map,
            );

            emit_mov_dynamic_stack_offset_to_reg(
                index2,
                *size2,
                *offset2,
                *id2,
                &reg2,
                object_stack_slot2,
                updated_instructions,
                stack_map,
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: reg.into(),
                        src1: reg.into(),
                        src2: reg2.into(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata),
        } if dest_vregdata.size <= 2 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];

            let dword_reg1 = get_register_for_size(4);
            let dest_reg = get_register_for_size(dest_vregdata.size);

            let dword_reg2 = get_register_for_size2(4);


            updated_instructions.push(
                ByteCode::Movzx(UnaryOperation{
                    src: src1_stack_slot.into(),
                    dest: dword_reg1.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Movzx(UnaryOperation{
                    src: src2_stack_slot.into(),
                    dest: dword_reg2.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation{
                        dest: dword_reg1.into(),
                        src1: dword_reg1.into(),
                        src2: dword_reg2.into(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: dest_reg.into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset{ index, offset, size, id },
            src2: VirtualRegister(ref src_vregdata),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: DynamicStackOffset{ index, offset, size, id },
        } if dest_vregdata.size <= 2 => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let dword_reg1 = get_register_for_size(4);
            let dword_reg2 = get_register_for_size2(4);
            let dest_reg = get_register_for_size(dest_vregdata.size);

            emit_movzx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &dword_reg1,
                object_stack_slot,
                updated_instructions,
                stack_map,
            );

            updated_instructions.push(
                ByteCode::Movzx(UnaryOperation {
                    src: src_stack_slot.into(),
                    dest: dword_reg2.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation {
                        dest: dword_reg1.into(),
                        src1: dword_reg1.into(),
                        src2: dword_reg2.into(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: dest_reg.into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset{ index, offset, size, id },
            src2: DynamicStackOffset{ index: index2, offset: offset2, size: size2, id: id2  },
        } if dest_vregdata.size <= 2 => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let object_stack_slot2 = &stack_map.object_to_stack_slot[id2];

            let dword_reg1 = get_register_for_size(4);
            let dword_reg2 = get_register_for_size2(4);
            let dest_reg = get_register_for_size(dest_vregdata.size);

            emit_movzx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &dword_reg1,
                object_stack_slot,
                updated_instructions,
                stack_map,
            );

            emit_movzx_dynamic_stack_offset_to_reg(
                index2,
                *size2,
                *offset2,
                *id2,
                &dword_reg2,
                object_stack_slot2,
                updated_instructions,
                stack_map,
            );



            updated_instructions.push(
                ByteCode::Mul(
                    BinaryOperation {
                        dest: dword_reg1.into(),
                        src1: dword_reg1.into(),
                        src2: dword_reg2.into(),
                    })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: dest_reg.into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },

        _ => ice!("Not implemented for {:#?}", binary_op),

    }
}

fn handle_div_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    handle_div_mod_common(binary_op, updated_instructions, stack_map, X64Register::EAX)
}

fn handle_mod_allocation(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    handle_div_mod_common(binary_op, updated_instructions, stack_map, X64Register::EDX);
}

fn handle_div_mod_common(binary_op: &BinaryOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap, result_register: X64Register) {
    match binary_op {

        /*
            A = constant / constant, long

            primarily exists to trigger division-by-zero, as constant operations are expected to be folded earlier

            emit:

            MOV AX, dividend
            MOV TMP_REGISTER, divisor
            SIGN_EXTEND AX
            IDIV TMP_REGISTER
            MOV A, EAX OR EDX

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1:
                dividend @ LongConstant(_) |
                dividend @ IntegerConstant(_) |
                dividend @ ShortConstant(_) |
                dividend @ ByteConstant(_),
            src2:
                divisor @ LongConstant(_) |
                divisor @ ShortConstant(_) |
                divisor @ IntegerConstant(_) |
                divisor @ ByteConstant(_),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let reg = get_register_for_size_for_division(stack_slot.size);


            let (dividend_reg, dividend) = if dest_vregdata.size > 1 {
                (X64Register::EAX.get_alias_for_size(dest_vregdata.size as u8), dividend.clone())
            } else {
                let dividend = match dividend {
                    ByteConstant(value) => ShortConstant(*value as i16),
                    _ => ice!("Unexpected dividend {}", dividend),
                };
                (X64Register::AX, dividend)
            };

            let dividend_reg: Value = dividend_reg.into();

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: dividend.clone(),
                    dest: dividend_reg.clone(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: divisor.clone(),
                    dest: reg.into()
                })
            );

            if dest_vregdata.size > 1 {
                updated_instructions.push(
                    ByteCode::SignExtend(UnaryOperation {
                        src: X64Register::EAX.get_alias_for_size(dest_vregdata.size as u8).into(),
                        dest: X64Register::EDX.get_alias_for_size(dest_vregdata.size as u8).into(),
                    })
                );
            }

            updated_instructions.push(
                ByteCode::Div(BinaryOperation{
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_vregdata.size as u8).into(),
                    dest: stack_slot.into(),
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
            MOV A, EAX OR EDX


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let reg = get_register_for_size_for_division(dest_stack_slot.size);

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: dividend_stack_slot.into(),
                    dest: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: immediate.clone(),
                    dest: PhysicalRegister(reg)
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: X64Register::RDX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src2: reg.into(),
                })
            );


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },

        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: ByteConstant(divisor),
        } => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let reg = get_register_for_size_for_division(dest_stack_slot.size);



            updated_instructions.push(
                ByteCode::Movsx(UnaryOperation {
                    src: dividend_stack_slot.into(),
                    dest: X64Register::EAX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*divisor as i32), // reuse 32 bit division code
                    dest: reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation{
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation{
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: reg.get_alias_for_size(4).into(),
                })
            );


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );

        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size_for_division(dest_stack_slot.size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: immediate.clone(),
                    dest: PhysicalRegister(reg)
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: X64Register::RDX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src2: reg.into(),
                })
            );


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2: ByteConstant(divisor),
        } => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size_for_division(dest_stack_slot.size);

            emit_movsx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &X64Register::EAX.into(),
                object_stack_slot,
                updated_instructions,
                stack_map);


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*divisor as i32), // reuse 32 bit division code
                    dest: reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation{
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation{
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: reg.get_alias_for_size(4).into(),
                })
            );


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );

        },

        /*
            A = constant / B

            emit:

            MOV EAX, constant
            SIGN_EXTEND EAX
            IDIV B
            MOV A, EAX OR EDX


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_),
            src2: VirtualRegister(ref divisor_vregdata),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&divisor_vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: immediate.clone(),
                    dest: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: X64Register::RDX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src2: divisor_stack_slot.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: ByteConstant(dividend),
            src2: VirtualRegister(ref divisor_vregdata),
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&divisor_vregdata.id];


            let src_reg = get_register_for_size2(1);

            ice_if!(src_reg == X64Register::AL, "Register collision");


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*dividend as i32),
                    dest: X64Register::EAX.into(),
                })
            );


            updated_instructions.push(
                ByteCode::Movsx(UnaryOperation {
                    src: divisor_stack_slot.into(),
                    dest: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_),
            src2: DynamicStackOffset { index, offset, size, id },
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let divisor_reg = get_register_for_size2(dest_vregdata.size);

            ice_if!(divisor_reg.get_alias_for_size(4) == X64Register::EAX
                || divisor_reg.get_alias_for_size(4) == X64Register::RDX,
                "Bad divisor reg {:?}", divisor_reg);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &divisor_reg.get_alias_for_size(dest_stack_slot.size as u8).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: immediate.clone(),
                    dest: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::RAX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: X64Register::RDX.get_alias_for_size(dest_stack_slot.size as u8).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::RAX.into(), // Not really used, instruction hardcodes
                    src2: divisor_reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: ByteConstant(dividend),
            src2: DynamicStackOffset { index, offset, size, id },
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];


            let src_reg = get_register_for_size2(1);

            ice_if!(src_reg == X64Register::AL, "Register collision");


            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: IntegerConstant(*dividend as i32),
                    dest: X64Register::EAX.into(),
                })
            );

            emit_movsx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &src_reg.get_alias_for_size(4).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        /*
            A = B / C (or A = A / B)

            emit:

            MOV EAX, B
            SIGN_EXTEND EAX
            IDIV C
            MOV A, EAX OR EDX
        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: VirtualRegister(ref divisor_vregdata),
        } if dest_vregdata.size >= 2 => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&divisor_vregdata.id];

            let operand_sizes = dest_stack_slot.size as u8;

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: dividend_stack_slot.into(),
                    dest: X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                    dest: X64Register::EDX.get_alias_for_size(operand_sizes).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src2: divisor_stack_slot.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: VirtualRegister(ref divisor_vregdata),
        } if dest_vregdata.size == 1 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&divisor_vregdata.id];

            let src_reg = get_register_for_size2(1);
            ice_if!(src_reg == X64Register::AL, "Register collision");

            updated_instructions.push(
                ByteCode::Movsx(UnaryOperation {
                    src: dividend_stack_slot.into(),
                    dest: X64Register::EAX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Movsx(UnaryOperation{
                    src: divisor_stack_slot.into(),
                    dest: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: DynamicStackOffset { index, offset, size, id },
        } if dest_vregdata.size >= 2 => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];

            let divisor_reg = get_register_for_size2(dest_stack_slot.size);
            let operand_sizes = dest_stack_slot.size as u8;

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: dividend_stack_slot.into(),
                    dest: X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                })
            );

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &divisor_reg.get_alias_for_size(operand_sizes).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);


            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                    dest: X64Register::EDX.get_alias_for_size(operand_sizes).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src2: divisor_reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref dividend_vregdata),
            src2: DynamicStackOffset { index, offset, size, id },
        } if dest_vregdata.size == 1 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let dividend_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let src_reg = get_register_for_size2(1);
            ice_if!(src_reg == X64Register::AL, "Register collision");

            updated_instructions.push(
                ByteCode::Movsx(UnaryOperation {
                    src: dividend_stack_slot.into(),
                    dest: X64Register::EAX.into(),
                })
            );

            emit_movsx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &src_reg.get_alias_for_size(4).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2: VirtualRegister(ref dividend_vregdata),
        } if dest_vregdata.size >= 2 => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];

            let divisor_reg = get_register_for_size2(dest_stack_slot.size);
            let operand_sizes = dest_stack_slot.size as u8;

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: divisor_stack_slot.into(),
                    dest: divisor_reg.get_alias_for_size(operand_sizes).into(),
                })
            );

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);


            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                    dest: X64Register::EDX.get_alias_for_size(operand_sizes).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src2: divisor_reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2: VirtualRegister(ref dividend_vregdata),
        } if dest_vregdata.size == 1 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let divisor_stack_slot = &stack_map.reg_to_stack_slot[&dividend_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let src_reg = get_register_for_size2(1);
            ice_if!(src_reg == X64Register::AL, "Register collision");

            updated_instructions.push(
                ByteCode::Movsx(UnaryOperation {
                    src: divisor_stack_slot.into(),
                    dest: src_reg.get_alias_for_size(4).into(),
                })
            );

            emit_movsx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &X64Register::EAX.get_alias_for_size(4).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2: DynamicStackOffset { index: index2, offset: offset2, size: size2, id: id2 },
        } if dest_vregdata.size >= 2 => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot1 = &stack_map.object_to_stack_slot[id];
            let object_stack_slot2 = &stack_map.object_to_stack_slot[id2];

            let divisor_reg = get_register_for_size2(dest_stack_slot.size);
            let operand_sizes = dest_stack_slot.size as u8;

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                object_stack_slot1,
                updated_instructions,
                stack_map);


            emit_mov_dynamic_stack_offset_to_reg(
                index2,
                *size2,
                *offset2,
                *id2,
                &divisor_reg.get_alias_for_size(operand_sizes).into(),
                object_stack_slot2,
                updated_instructions,
                stack_map);


            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.get_alias_for_size(operand_sizes).into(),
                    dest: X64Register::EDX.get_alias_for_size(operand_sizes).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.get_alias_for_size(operand_sizes).into(), // Not really used, instruction hardcodes
                    src2: divisor_reg.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(dest_stack_slot.size as u8).into(),
                    dest: dest_stack_slot.into(),
                })
            );
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: DynamicStackOffset { index, offset, size, id },
            src2: DynamicStackOffset { index: index2, offset: offset2, size: size2, id: id2 },
        } if dest_vregdata.size == 1 => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let object_stack_slot2 = &stack_map.object_to_stack_slot[id2];
            let src_reg = get_register_for_size2(1);
            ice_if!(src_reg == X64Register::AL, "Register collision");

            emit_movsx_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &X64Register::EAX.get_alias_for_size(4).into(),
                object_stack_slot,
                updated_instructions,
                stack_map);

            emit_movsx_dynamic_stack_offset_to_reg(
                index2,
                *size2,
                *offset2,
                *id2,
                &src_reg.get_alias_for_size(4).into(),
                object_stack_slot2,
                updated_instructions,
                stack_map);

            updated_instructions.push(
                ByteCode::SignExtend(UnaryOperation {
                    src: X64Register::EAX.into(),
                    dest: X64Register::EDX.into(),
                })
            );

            updated_instructions.push(
                ByteCode::Div(BinaryOperation {
                    dest: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src1: X64Register::EAX.into(), // Not really used, instruction hardcodes
                    src2: src_reg.get_alias_for_size(4).into(),
                })
            );

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation {
                    src: result_register.get_alias_for_size(1).into(),
                    dest: dest_stack_slot.into(),
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
        Some(ByteConstant(_)) |
        Some(ShortConstant(_)) |
        Some(IntegerConstant(_)) |
        Some(LongConstant(_)) => {
            updated_instructions.push(ByteCode::Ret(value.clone()));
        },
        None => updated_instructions.push(ByteCode::Ret(None)),
        _ => unimplemented!("Not implemented for {:#?}:", value),
    }
}

fn handle_comparison(comparison_op: &ComparisonOperation, updated_instructions: &mut Vec<ByteCode>, stack_map: &StackMap) {
    match comparison_op {

        /*
            A CMP constant

            emit:
            CMP a, constant

        */
        ComparisonOperation {
            src1: VirtualRegister(src_vregdata),
            src2: LongConstant(val),
        } => {
            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            if i64_fits_in_i32(*val) {
                updated_instructions.push(
                    ByteCode::Compare(
                        ComparisonOperation {
                            src1: stack_slot.into(),
                            src2: IntegerConstant(*val as i32),
                        }
                    )
                )
            } else {
                let reg = get_register_for_size(8);
                updated_instructions.push(ByteCode::Mov(
                    UnaryOperation {
                        src: LongConstant(*val),
                        dest: reg.into(),
                    }
                ));
                updated_instructions.push(
                    ByteCode::Compare(
                        ComparisonOperation {
                            src1: stack_slot.into(),
                            src2: reg.into(),
                        }
                    )
                )
            }
        },
        ComparisonOperation {
            src1: VirtualRegister(src_vregdata),
            src2:
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: stack_slot.into(),
                        src2: immediate.clone(),
                    }
                )
            )
        },
        ComparisonOperation {
            src1: DynamicStackOffset { index, offset, size, id },
            src2: LongConstant(val),
        } => {

            let reg = get_register_for_size(*size);
            let object_stack_slot = &stack_map.object_to_stack_slot[id];

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            if i64_fits_in_i32(*val) {
                updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: reg.into(),
                        src2: IntegerConstant(*val as i32),
                    }
                ));
            } else {
                let reg2 = get_register_for_size2(8);
                updated_instructions.push(ByteCode::Mov(
                    UnaryOperation {
                        src: LongConstant(*val),
                        dest: reg2.into(),
                    }
                ));
                updated_instructions.push(
                    ByteCode::Compare(
                        ComparisonOperation {
                            src1: reg.into(),
                            src2: reg2.into(),
                        }
                    ));
            }
        },
        ComparisonOperation {
            src1: DynamicStackOffset { index, offset, size, id },
            src2:
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
        } => {

            let reg = get_register_for_size(*size);
            let object_stack_slot = &stack_map.object_to_stack_slot[id];

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: reg.into(),
                        src2: immediate.clone(),
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
            src1:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @  ShortConstant(_) |
                immediate @ ByteConstant(_),
            src2: VirtualRegister(src_vregdata),
        } => {

            let stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let reg = get_register_for_size(stack_slot.size);

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: immediate.clone(),
                    dest: reg.into(),
                }
            ));

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: reg.into(),
                        src2: stack_slot.into(),
                    }
                )
            );

        },
        ComparisonOperation {
            src1:
                immediate @ LongConstant(_) |
                immediate @ IntegerConstant(_) |
                immediate @ ShortConstant(_) |
                immediate @ ByteConstant(_),
            src2: DynamicStackOffset { index, offset, size, id },
        } => {

            let reg = get_register_for_size(*size);
            let imm_reg = get_register_for_size2(*size);
            let object_stack_slot = &stack_map.object_to_stack_slot[id];

                updated_instructions.push(ByteCode::Mov(
                    UnaryOperation {
                        src: immediate.clone(),
                        dest: imm_reg.into(),
                    }));

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation{
                        src1: imm_reg.into(),
                        src2: reg.into(),
                    }
                )
            )
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
                    src: src1_stack_slot.into(),
                    dest: reg.into(),
                }
            ));

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation {
                        src1: reg.into(),
                        src2: src2_stack_slot.into(),
                    }
                )
            );
        },
        ComparisonOperation {
            src1: DynamicStackOffset{ index, offset, size, id },
            src2: VirtualRegister(src2_vregdata),
        } => {

            let  object_stack_slot = &stack_map.object_to_stack_slot[id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
            let reg = get_register_for_size(*size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );


            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation {
                        src1: reg.into(),
                        src2: src2_stack_slot.into(),
                    }
                )
            );
        },
        ComparisonOperation {
            src1: VirtualRegister(src2_vregdata),
            src2: DynamicStackOffset{ index, offset, size, id },
        } => {

            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
            let reg = get_register_for_size(*size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation {
                        src1: src1_stack_slot.into(),
                        src2: reg.into(),
                    }
                )
            );
        },
        ComparisonOperation {
            src1: DynamicStackOffset{ index, offset, size, id },
            src2: DynamicStackOffset{ index: index2, offset: offset2, size: size2, id: id2 },
        } => {

            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let object_stack_slot2 = &stack_map.object_to_stack_slot[id2];
            let reg = get_register_for_size(*size);
            let reg2 = get_register_for_size2(*size2);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            emit_mov_dynamic_stack_offset_to_reg(
                index2,
                *size2,
                *offset2,
                *id2,
                &reg2,
                object_stack_slot2,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(
                ByteCode::Compare(
                    ComparisonOperation {
                        src1: reg.into(),
                        src2: reg2.into(),
                    }
                )
            );
        },
        _ => ice!("{:#?}", comparison_op),
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
                LongConstant(val) => {
                    let dest = get_destination_for_integer_and_pointer_argument(i, 8);

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: LongConstant(*val),
                                dest,
                            }
                        )
                    )
                },
                IntegerConstant(val) => {
                    let dest = get_destination_for_integer_and_pointer_argument(i, 4);

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: IntegerConstant(*val),
                                dest,
                            }
                        )
                    )
                },
                ShortConstant(val) => {
                    let dest = get_destination_for_integer_and_pointer_argument(i, 2);

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: ShortConstant(*val),
                                dest,
                            }
                        )
                    )
                },
                ByteConstant(val) => {
                    // most registers don't have 8 bit aliases (looking at you, RDI...)
                    // move to TMP register instead, then move TMP to proper reg

                    // register aliases
                    let byte_reg = get_register_for_size(1);
                    let dword_reg = get_register_for_size(4);

                    let dest = get_destination_for_integer_and_pointer_argument(i, 4);

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: ByteConstant(*val),
                                dest: PhysicalRegister(byte_reg),
                            }
                        ));

                    updated_instructions.push(
                        ByteCode::Mov(
                            UnaryOperation {
                                src: PhysicalRegister(dword_reg),
                                dest,
                            }
                        ));
                },
                VirtualRegister(vregdata) => {


                    match vregdata.size {
                        1 => {
                            // most registers don't have 8 bit aliases (looking at you, RDI...)
                            // move to TMP register instead, then move TMP to proper reg


                            // register aliases
                            let byte_reg = get_register_for_size(1);
                            let dword_reg = get_register_for_size(4);

                            let dest = get_destination_for_integer_and_pointer_argument(i, 4);
                            let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];

                            updated_instructions.push(
                                ByteCode::Mov(
                                    UnaryOperation {
                                        src: StackOffset {
                                            size: stack_slot.size,
                                            offset: stack_slot.offset,
                                        },
                                        dest: PhysicalRegister(byte_reg),
                                    }
                                ));

                            updated_instructions.push(
                                ByteCode::Mov(
                                    UnaryOperation {
                                        src: PhysicalRegister(dword_reg),
                                        dest,
                                    }
                                ));
                        },
                        2 | 4 | 8 => {
                            let dest = get_destination_for_integer_and_pointer_argument(i, vregdata.size);
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
                        }
                        _ => ice!("Bad virtual register size {}", vregdata.size),
                    }

                },
                DynamicStackOffset { size, index, offset, id } => {

                    let object_stack = &stack_map.object_to_stack_slot[id];
                    match **index {
                        IntegerConstant(value) => {
                            if *size >= 2 {
                                let dest = get_destination_for_integer_and_pointer_argument(i, *size);
                                updated_instructions.push(
                                    ByteCode::Mov(
                                        UnaryOperation {
                                            src: StackOffset {
                                                size: *size,
                                                offset: get_constant_object_offset(*offset, *size, value, &object_stack),
                                            },
                                            dest,
                                        }
                                    ));
                            } else {
                                let dest = get_destination_for_integer_and_pointer_argument(i, 4);
                                // handle byte args through temp reg due to how encoding works
                                let tmp_reg = get_register_for_size(1);


                                updated_instructions.push(
                                    ByteCode::Mov(UnaryOperation {
                                        src: StackOffset {
                                                size: *size,
                                                offset: get_constant_object_offset(*offset, *size, value, &object_stack),
                                        },
                                        dest: PhysicalRegister(tmp_reg.clone()),
                                    })
                                );


                                updated_instructions.push(
                                    ByteCode::Movsx(UnaryOperation {
                                        src: PhysicalRegister(tmp_reg),
                                        dest,
                                    }));

                            }

                        },
                        _ => todo!(),
                    }
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
                DynamicStackOffset { size, index, offset, id } => {
                    let object_stack = &stack_map.object_to_stack_slot[id];

                    match **index {
                        IntegerConstant(value) => {
                            let dest = get_register_for_size(*size);
                            updated_instructions.push(
                                 ByteCode::Mov(
                                    UnaryOperation {
                                        src: StackOffset {
                                            size: *size,
                                            offset: get_constant_object_offset(*offset, *size, value, &object_stack),
                                        },
                                        dest: PhysicalRegister(dest.clone()),
                                    }
                                ));

                            PhysicalRegister(dest)
                        }
                        _ => todo!(),
                    }
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
        2 => X64Register::AX,
        4 => X64Register::EAX,
        8 => X64Register::RAX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size2(size: u32) -> X64Register {
    match size {
        1 => X64Register::BL,
        2 => X64Register::BX,
        4 => X64Register::EBX,
        8 => X64Register::RBX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size3(size: u32) -> X64Register {
    match size {
        1 => X64Register::CL,
        2 => X64Register::CX,
        4 => X64Register::ECX,
        8 => X64Register::RCX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_register_for_size_for_division(size: u32) -> X64Register {
    match size {
        1 => X64Register::BL,
        2 => X64Register::BX,
        4 => X64Register::EBX,
        8 => X64Register::RBX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_return_value_register_for_size(size: u32) -> X64Register {
    match size {
        1 => X64Register::AL,
        2 => X64Register::AX,
        4 => X64Register::EAX,
        8 => X64Register::RAX,
        _ => ice!("Invalid register size {}", size),
    }
}

fn get_destination_for_integer_and_pointer_argument(position: usize, size: u32) -> Value {

    match size {
        2 => {
            match position {
                0 => PhysicalRegister(X64Register::DI),
                1 => PhysicalRegister(X64Register::SI),
                2 => PhysicalRegister(X64Register::DX),
                3 => PhysicalRegister(X64Register::CX),
                4 => PhysicalRegister(X64Register::R8w),
                5 => PhysicalRegister(X64Register::R9w),
                _ => ice!("No register for position {}", position),
            }
        },
        4 => {
            match position {
                0 => PhysicalRegister(X64Register::EDI),
                1 => PhysicalRegister(X64Register::ESI),
                2 => PhysicalRegister(X64Register::EDX),
                3 => PhysicalRegister(X64Register::ECX),
                4 => PhysicalRegister(X64Register::R8d),
                5 => PhysicalRegister(X64Register::R9d),
                _ => ice!("No register for position {}", position),
            }
        },
        8 => {
            match position {
                0 => PhysicalRegister(X64Register::RDI),
                1 => PhysicalRegister(X64Register::RSI),
                2 => PhysicalRegister(X64Register::RDX),
                3 => PhysicalRegister(X64Register::RCX),
                4 => PhysicalRegister(X64Register::R8),
                5 => PhysicalRegister(X64Register::R9),
                _ => ice!("No register for position {}", position),
            }
        },
        _ => ice!("Bad register size {} ", size),
    }

}

// as stack grows downwards (subtract value from RBP to get correct spot), we either need to negate index register whenever it is used,
// or access arrays/objects in such way that we always negate array size + offset, then add the positive index (array effectively reversed in memory)
//
// take this account when calculating offsets

fn get_indexed_array_offset(offset: u32, object_stack_slot: &StackSlot) -> u32 {

    let constant_offset = -(offset as i32) + object_stack_slot.offset as i32 + object_stack_slot.size as i32;
    constant_offset as u32
}

fn get_constant_object_offset(offset: u32, size: u32, index: i32, object_stack_slot: &StackSlot) -> u32 {

    let constant_offset = -(offset as i32) + object_stack_slot.offset as i32 + object_stack_slot.size as i32 - (size as i32)*index;
    constant_offset as u32
}


fn offset_for_reg(map: &StackMap, id: u32) -> Value{
    let reg = &map.reg_to_stack_slot[&id];
    reg.into()
}


fn i64_fits_in_i32(val: i64) -> bool {
    val >= i32::MIN as i64 && val <= i32::MAX as i64
}


fn emit_mov_dynamic_stack_offset_to_reg(
    index: &Box<Value>,
    size: u32,
    offset: u32,
    id: u32,
    register: &X64Register,
    object_stack_slot: &StackSlot,
    updated_instructions: &mut  Vec<ByteCode>,
    stack_map: &StackMap) {

    emit_mov_with_opcode_dynamic_stack_offset_to_reg(
        ByteCode::Mov,
        index,
        size,
        offset,
        id,
        register,
        object_stack_slot,
        updated_instructions,
        stack_map
    );

}

fn emit_movzx_dynamic_stack_offset_to_reg(
    index: &Box<Value>,
    size: u32,
    offset: u32,
    id: u32,
    register: &X64Register,
    object_stack_slot: &StackSlot,
    updated_instructions: &mut  Vec<ByteCode>,
    stack_map: &StackMap) {

    emit_mov_with_opcode_dynamic_stack_offset_to_reg(
        ByteCode::Movzx,
        index,
        size,
        offset,
        id,
        register,
        object_stack_slot,
        updated_instructions,
        stack_map
    );
}


fn emit_movsx_dynamic_stack_offset_to_reg(
    index: &Box<Value>,
    size: u32,
    offset: u32,
    id: u32,
    register: &X64Register,
    object_stack_slot: &StackSlot,
    updated_instructions: &mut  Vec<ByteCode>,
    stack_map: &StackMap) {

    emit_mov_with_opcode_dynamic_stack_offset_to_reg(
        ByteCode::Movsx,
        index,
        size,
        offset,
        id,
        register,
        object_stack_slot,
        updated_instructions,
        stack_map
    );
}

fn emit_mov_with_opcode_dynamic_stack_offset_to_reg<T>(
    bytecode: T,
    index: &Box<Value>,
    size: u32,
    offset: u32,
    id: u32,
    register: &X64Register,
    object_stack_slot: &StackSlot,
    updated_instructions: &mut  Vec<ByteCode>,
    stack_map: &StackMap) where T: Fn(UnaryOperation) -> ByteCode  {

      match **index {
            VirtualRegister(ref vregdata) => {
                let index_reg = get_register_for_size(4); // index should be integer
                let stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];

                updated_instructions.push(
                    ByteCode::Mov(
                        UnaryOperation {
                            src: stack_slot.into(),
                            dest: index_reg.into(),
                        }
                    ));

                updated_instructions.push(
                    bytecode(
                        UnaryOperation {
                            src: DynamicStackOffset {
                                id,
                                size,
                                offset: get_indexed_array_offset(offset, object_stack_slot),
                                index: Box::new(index_reg.into()),
                            },
                            dest: register.into(),
                        }
                    ));

            },
            IntegerConstant(val) => {
                updated_instructions.push(
                    bytecode(
                        UnaryOperation {
                            src: StackOffset {
                                size,
                                offset: get_constant_object_offset(offset, size, val, object_stack_slot),
                            },
                            dest: register.into(),
                        }
                    ));
            }
            _ => ice!("Unexpected dynamic index {:?} ", index)
        }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Commutativity {
    Commutative,
    AntiCommutative,
}

struct OpProperties {
    commutativity: Commutativity,
}

fn handle_binary_allocation<T>(
    enum_type: T,
    operation_properties: OpProperties,
    binary_op: &BinaryOperation,
    updated_instructions: &mut Vec<ByteCode>,
    stack_map: &StackMap)  where T: Fn(BinaryOperation) -> ByteCode {

    match binary_op {

        /*
            A = constant OP constant1
            not directly encodable

            emit:

            MOV A, constant
            OP A, constant1

        */
        /*

            A = A OP long_constant

            no support for imm64, so:

            * emit as integer-constant, if fits imm32
            * emit extra move to r64 otherwise
        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: src1 @ LongConstant(_),
            src2: src2 @ LongConstant(_),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: src1 @ IntegerConstant(_),
            src2: src2 @ IntegerConstant(_),
        } |
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: src1 @ ShortConstant(_),
            src2: src2 @ ShortConstant(_),
        } |
        BinaryOperation {
            dest: VirtualRegister(ref dest_vregdata),
            src1: src1 @ ByteConstant(_),
            src2: src2 @ ByteConstant(_),
        }
        => {
            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            updated_instructions.push(
                ByteCode::Mov(UnaryOperation{
                    src: src1.clone(),
                    dest: stack_slot.into(),
                })
            );

            updated_instructions.push(
                enum_type(
                    BinaryOperation{
                        dest: stack_slot.into(),
                        src1: stack_slot.into(),
                        src2: src2.clone(),
                    }));
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: LongConstant(src2_val)} if dest_vregdata.id == src_vregdata.id => {

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            if i64_fits_in_i32(*src2_val)  {
                updated_instructions.push(enum_type(BinaryOperation{
                    dest: stack_slot.into(),
                    src1: stack_slot.into(),
                    src2: IntegerConstant(*src2_val as i32),
                }));
            } else {
                let tmp = get_register_for_size(8);

                updated_instructions.push(ByteCode::Mov(UnaryOperation{
                    dest: tmp.into(),
                    src: LongConstant(*src2_val),
                }));

                updated_instructions.push(enum_type(BinaryOperation{
                    dest: stack_slot.into(),
                    src1: stack_slot.into(),
                    src2: tmp.into(),
                }));
            }
        },
        /*
            A = integer_constant OP somereg/somestack

            swap constants around and call this function recursively, if commutative
        */
        BinaryOperation{
            dest,
            src1:
                src1 @ LongConstant(_) |
                src1 @ IntegerConstant(_) |
                src1 @ ShortConstant(_) |
                src1 @ ByteConstant(_),
            src2,
        } |
        BinaryOperation {
            dest,
            src1: src1 @ VirtualRegister(..),
            src2: src2 @ DynamicStackOffset { .. },
        } if operation_properties.commutativity == Commutativity::Commutative => {

            handle_binary_allocation(
                enum_type,
                operation_properties,
                &BinaryOperation {
                    dest: dest.clone(),
                    src1: src2.clone(),
                    src2: src1.clone(),
                },
                updated_instructions,
                stack_map);
        }
        /*
            A = A OP constant
            encodable as is, just emit the instruction

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2:
                constant @ IntegerConstant(_) |
                constant @ ShortConstant(_) |
                constant @ ByteConstant(_)
        } if dest_vregdata.id == src_vregdata.id => {

            let stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            updated_instructions.push(enum_type(BinaryOperation{
                dest: stack_slot.into(),
                src1: stack_slot.into(),
                src2: constant.clone(),
            }));
        },
        /*
            A = B OP integer_constant

            cannot encode directly, so emit:

            if constant fits in 32 bit immediate:

            mov tmp_reg, stack_slot_b
            mov stack_slot_a, tmp_reg
            OP stack_slot_a, constant

            else:

            mov tmp_reg, stack_slot_b
            mov stack_slot_a, tmp_reg
            mov tmp_reg, constant
            OP stack_slot_a, reg
        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: LongConstant(src2_val)} if dest_vregdata.id != src_vregdata.id => {

            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg= get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: reg.into(),
                src: src_stack_slot.into(),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: dest_stack_slot.into(),
                src: reg.into(),
            }));

            if i64_fits_in_i32(*src2_val) {
                updated_instructions.push(enum_type(BinaryOperation{
                    dest: dest_stack_slot.into(),
                    src1: dest_stack_slot.into(),
                    src2: IntegerConstant(*src2_val as i32),
                }));
            } else {
                let tmp = get_register_for_size(8);
                updated_instructions.push(ByteCode::Mov(UnaryOperation{
                    dest: tmp.into(),
                    src: LongConstant(*src2_val),
                }));

                updated_instructions.push(enum_type(BinaryOperation{
                    dest: dest_stack_slot.into(),
                    src1: dest_stack_slot.into(),
                    src2: tmp.into(),
                }));

            }
        },
        /*
            A = B OP constant
            cannot encode directly, so emit:

            mov tmp_reg, stack_slot_b
            mov stack_slot_a, tmp_reg
            OP stack_slot, constant

        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2:
                constant @ IntegerConstant(_) |
                constant @ ShortConstant(_) |
                constant @ ByteConstant(_)
        } if dest_vregdata.id != src_vregdata.id => {

            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg= get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: reg.into(),
                src: src_stack_slot.into(),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: dest_stack_slot.into(),
                src: reg.into(),
            }));

            updated_instructions.push(enum_type(BinaryOperation{
                dest: dest_stack_slot.into(),
                src1: dest_stack_slot.into(),
                src2: constant.clone(),
            }));
        },
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src_vregdata),
            src2: ShortConstant(src2_val)} if dest_vregdata.id != src_vregdata.id => {

            let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

            let reg= get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: reg.into(),
                src: src_stack_slot.into(),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: dest_stack_slot.into(),
                src: reg.into(),
            }));

            updated_instructions.push(enum_type(BinaryOperation{
                dest: dest_stack_slot.into(),
                src1: dest_stack_slot.into(),
                src2: ShortConstant(*src2_val),
            }));
        }
        /*
            A = dynamic_stack_location OP long_constant

            As reg will be converted to stack location, needs intermediate instructions. Emit:

            if constant fits in r32:

            MOV tmp_reg, dyn_stack_loc,
            OP tmp_reg, constant
            MOV A, tmp_reg

            Otherwise:
            MOV tmp_reg, dyn_stack_loc,
            MOV imm_reg, constant
            OP tmp_reg, imm_reg
            MOV A, tmp_reg

        */
        BinaryOperation {
            src1: DynamicStackOffset {
                index,
                offset,
                size,
                id
            },
            src2: LongConstant(immediate),
            dest: VirtualRegister(vregdata)
        }  => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size(vregdata.size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            if i64_fits_in_i32(*immediate) {
                updated_instructions.push(enum_type(
                    BinaryOperation {
                        src1: reg.into(),
                        src2: IntegerConstant(*immediate as i32),
                        dest: reg.into(),
                    }
                ));
            } else {
                let tmp = get_register_for_size2(8);
                updated_instructions.push(ByteCode::Mov(UnaryOperation{
                    dest: tmp.into(),
                    src: LongConstant(*immediate),
                }));

                updated_instructions.push(enum_type(BinaryOperation{
                    dest: reg.into(),
                    src1: reg.into(),
                    src2: tmp.into(),
                }));

            }


            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                }
            ));

        },
        /*
            A = dynamic_stack_location OP constant

            As reg will be converted to stack location, needs intermediate instructions. Emit:

            MOV tmp_reg, dyn_stack_loc,
            OP tmp_reg, constant
            MOV A, tmp_reg

        */
        BinaryOperation {
            src1: DynamicStackOffset {
                index,
                offset,
                size,
                id
            },
            src2:
                constant @ IntegerConstant(_) |
                constant @ ShortConstant(_) |
                constant @ ByteConstant(_),
            dest: VirtualRegister(vregdata)
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size(vregdata.size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(enum_type(
                BinaryOperation {
                    src1: reg.into(),
                    src2: constant.clone(),
                    dest: reg.into(),
                }
            ));

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                }
            ));

        },

        /*
            A = constant OP dynamic_stack_location

            As reg will be converted to stack location, needs intermediate instructions. Emit:

            MOV tmp_reg, dyn_stack_loc,
            MOV A, constant
            OP A, tmp_reg

        */
        BinaryOperation {
            src1:
                constant @ LongConstant(_) |
                constant @ IntegerConstant(_) |
                constant @ ShortConstant(_) |
                constant @ ByteConstant(_),
            src2: DynamicStackOffset {
                index,
                offset,
                size,
                id
            },
            dest: VirtualRegister(vregdata)
        } if operation_properties.commutativity == Commutativity::AntiCommutative => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size(vregdata.size);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: constant.clone(),
                    dest: dest_stack_slot.into(),
                }
            ));

            updated_instructions.push(enum_type(
                BinaryOperation {
                    src1: dest_stack_slot.into(),
                    src2: reg.into(),
                    dest: dest_stack_slot.into()
                }
            ));
        },
        /*
            A = dynamic_stack_location OP B

            As reg will be converted to stack location, needs intermediate instructions. Emit:

            MOV tmp_reg, dyn_stack_loc,
            OP tmp_reg, src2
            MOV A, tmp_reg
        */
        BinaryOperation {
            src1: DynamicStackOffset {
                index,
                offset,
                size,
                id
            },
            src2: VirtualRegister(src2_vregdata),
            dest: VirtualRegister(dest_vregdata)
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size(dest_vregdata.size);
            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(enum_type(
                BinaryOperation {
                    src1: reg.into(),
                    src2: src2_stack_slot.into(),
                    dest: reg.into(),
                }
            ));

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                }
            ));
        },
        /*
            A = dynamic_stack_location OP dynamic stack location

            MOV tmp_reg, dyn_stack_loc1,
            MOV tmp_reg2, dyn_stack_Loc2      use mov instead of OP tmp_reg, dyn_stack_loc to simplify code gen; and we'd need to emit extra mov for constant integer index anyway
            OP tmp_reg1, temp_reg_2,
            MOV A, tmp_reg
        */
        BinaryOperation {
            src1: DynamicStackOffset {
                index,
                offset,
                size,
                id
            },
            src2: DynamicStackOffset { index: index2, offset: offset2, size: size2, id: id2 },
            dest: VirtualRegister(dest_vregdata)
        } => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let object_stack_slot1 = &stack_map.object_to_stack_slot[id];
            let object_stack_slot2 = &stack_map.object_to_stack_slot[id2];
            let reg = get_register_for_size(dest_vregdata.size);
            let reg2 = get_register_for_size2(dest_vregdata.size);
            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot1,
                updated_instructions,
                stack_map
            );

            emit_mov_dynamic_stack_offset_to_reg(
                index2,
                *size2,
                *offset2,
                *id2,
                &reg2,
                object_stack_slot2,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(enum_type(
                BinaryOperation {
                    src1: reg.into(),
                    src2: reg2.into(),
                    dest: reg.into(),
                }
            ));

            updated_instructions.push(ByteCode::Mov(
                UnaryOperation {
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                }
            ));
        },
        /*
            A = B OP dynamic_stack_location if anticommutative

            As reg will be converted to stack location, needs intermediate instructions. Emit:

            MOV tmp_reg, B    omit if A == B
            MOV A, tmp_reg    omit if A == B
            MOV tmp_reg, dynamic_stack_location,
            OP A, tmp_reg,

        */
        BinaryOperation {
            src1: VirtualRegister(src1_vregdata),
            src2: DynamicStackOffset {
                index,
                offset,
                size,
                id
            },
            dest: VirtualRegister(dest_vregdata)
        } if operation_properties.commutativity == Commutativity::AntiCommutative => {
            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src1_stack_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let reg = get_register_for_size(dest_vregdata.size);

            if dest_vregdata.id != src1_vregdata.id {
                updated_instructions.push( ByteCode::Mov( UnaryOperation {
                    src: src1_stack_slot.into(),
                    dest: reg.into(),
                }));

                updated_instructions.push( ByteCode::Mov( UnaryOperation {
                    src: reg.into(),
                    dest: dest_stack_slot.into(),
                }));
            }

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(enum_type(
                BinaryOperation {
                    src1: dest_stack_slot.into(),
                    src2: reg.into(),
                    dest: dest_stack_slot.into(),
                }
            ));
        },
    /*
        A = constant OP B if anticommutative
        not directly encodable

        Emit:

        MOV tmp_reg, immediate
        OP tmp_reg, B
        MOV A, tmp_reg

        Note: A is not clobbered during calculation, so safe for case when loc(A) == loc(B)
     */
        BinaryOperation {
            dest: VirtualRegister(ref dest_vregdata),
            src1:
                constant @ LongConstant(_) |
                constant @ IntegerConstant(_) |
                constant @ ShortConstant(_) |
                constant @ ByteConstant(_),
            src2: VirtualRegister(ref src_vregdata) } if operation_properties.commutativity == Commutativity::AntiCommutative => {
        let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
        let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

        let reg = get_register_for_size(src_vregdata.size);

        updated_instructions.push(ByteCode::Mov(UnaryOperation{
            src: constant.clone(),
            dest: reg.into(),
        }));

        updated_instructions.push(enum_type(BinaryOperation{
            dest: reg.into(),
            src1: reg.into(),
            src2: src_stack_slot.into(),
        }));

        updated_instructions.push(ByteCode::Mov(UnaryOperation{
            dest: dest_stack_slot.into(),
            src: reg.into(),
        })

        );
    },
        /*
            A = A OP B
            not directly encodable, as A and B are both memory operands, need to use tmp reg

            emit:

            MOV tmp_reg, b
            OP A, tmp_reg


        */
        BinaryOperation{
            dest: VirtualRegister(ref dest_vregdata),
            src1: VirtualRegister(ref src1_vregdata),
            src2: VirtualRegister(ref src2_vregdata)} if dest_vregdata.id == src1_vregdata.id => {

            let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
            let src2_stack_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];

            let reg = get_register_for_size(src2_stack_slot.size);
            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: reg.into(),
                src: src2_stack_slot.into(),
            }));

            updated_instructions.push(enum_type(BinaryOperation{
                dest: dest_stack_slot.into(),
                src1: dest_stack_slot.into(),
                src2: reg.into(),
            }));
        }

        /*
            A = B OP C
            not diretly encodable, three address form + max one memory operand per instruction

            emit:

            MOV tmp_reg, B
            OP tmp_reg, C
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
                dest: reg.into(),
                src: src1_stack_slot.into(),
            }));

            updated_instructions.push(enum_type(BinaryOperation{
                dest: reg.into(),
                src1: reg.into(),
                src2: src2_stack_slot.into(),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                dest: dest_stack_slot.into(),
                src: reg.into(),
            }));
        }
        _ => ice!("Not implemented for {:#?}", enum_type(binary_op.clone())),
    }
}

fn handle_shift_allocation<T>(
    enum_type: T,
    binary_op: &BinaryOperation,
    updated_instructions: &mut Vec<ByteCode>,
    stack_map: &StackMap) where T: Fn(BinaryOperation) -> ByteCode {

        match binary_op {

        /*
            y = x OP constant

            emit:

            mov reg, x
            OP reg, count
            mov y, reg
        */
            BinaryOperation {
                dest: VirtualRegister(dest_vregdata),
                src1: VirtualRegister(src_vregdata),
                src2:
                    constant @ LongConstant(_) |
                    constant @ IntegerConstant(_) |
                    constant @ ShortConstant(_) |
                    constant @ ByteConstant(_),
            } if dest_vregdata.id != src_vregdata.id => {

                let src1_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
                let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
                let reg = get_register_for_size(dest_vregdata.size);


                updated_instructions.push(
                    ByteCode::Mov(UnaryOperation{
                        src: src1_stack_slot.into(),
                        dest: reg.get_alias_for_size(src1_stack_slot.size as u8).into(),
                    }));

                let count = match constant {
                    LongConstant(value) => *value as i32,
                    IntegerConstant(value) => *value,
                    ShortConstant(value) => *value as i32,
                    ByteConstant(value) => *value as i32,
                    _ => ice!("Unexpected value {:?}", constant),
                };

                updated_instructions.push(
                    enum_type(BinaryOperation{
                        src1: reg.into(),
                        src2: IntegerConstant(count),
                        dest: reg.into(),
                    }));

                updated_instructions.push(
                    ByteCode::Mov(UnaryOperation {
                        src: reg.into(),
                        dest: dest_stack_slot.into(),
                }));
            },

            /*
                x = x << constant

                emit:

                shl [x], count
            */
            BinaryOperation {
                dest: VirtualRegister(dest_vregdata),
                src1: VirtualRegister(src_vregdata),
                src2:
                    constant @ LongConstant(_) |
                    constant @ IntegerConstant(_) |
                    constant @ ShortConstant(_) |
                    constant @ ByteConstant(_),
            } if dest_vregdata.id == src_vregdata.id => {

                let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];

                let count = match constant {
                    LongConstant(value) => *value as i32,
                    IntegerConstant(value) => *value,
                    ShortConstant(value) => *value as i32,
                    ByteConstant(value) => *value as i32,
                    _ => ice!("Unexpected value {:?}", constant),
                };

                updated_instructions.push(
                    enum_type(BinaryOperation{
                        src1: dest_stack_slot.into(),
                        src2: IntegerConstant(count),
                        dest: dest_stack_slot.into(),
                    }));
            },
            /*
                x = constant OP x
                OR
                x = constant OP y

                emit

                mov cl, x OR y
                mov x, constant

                OP [x], cl

            */

            BinaryOperation {
                dest: VirtualRegister(dest_vregdata),
                src1:
                    constant @ LongConstant(_) |
                    constant @ IntegerConstant(_) |
                    constant @ ShortConstant(_) |
                    constant @ ByteConstant(_),
                src2: VirtualRegister(src_vregdata),
            } => {

                let src_stack_slot = &stack_map.reg_to_stack_slot[&src_vregdata.id];
                let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
                let reg = X64Register::CL;

                updated_instructions.push(
                    ByteCode::Mov(UnaryOperation{
                        src: src_stack_slot.into(),
                        dest: reg.get_alias_for_size(src_stack_slot.size as u8).into(),
                    }));

                let stack_offset: Value = dest_stack_slot.into();

                 match constant {
                     LongConstant(count) => mov_long_constant_to_stack(updated_instructions, count, dest_stack_slot),
                     _ => {
                         let count = match constant {
                             IntegerConstant(value) => *value,
                             ShortConstant(value) => *value as i32,
                             ByteConstant(value) => *value as i32,
                             _ => ice!("Unexpected value {:?}", constant),
                         };
                         updated_instructions.push(
                             ByteCode::Mov(UnaryOperation{
                                 src: IntegerConstant(count),
                                 dest: stack_offset.clone(),
                             }));

                     }
                };

                updated_instructions.push(
                    enum_type(BinaryOperation{
                        src1: stack_offset.clone(),
                        src2: reg.get_alias_for_size(1).into(),
                        dest: stack_offset,
                    }));
            },
            BinaryOperation {
                dest: VirtualRegister(dest_vregdata),
                src1: VirtualRegister(src1_vregdata) ,
                src2: VirtualRegister(src2_vregdata),
            } => {

                let count_slot = &stack_map.reg_to_stack_slot[&src2_vregdata.id];
                let value_slot = &stack_map.reg_to_stack_slot[&src1_vregdata.id];
                let dest_stack_slot = &stack_map.reg_to_stack_slot[&dest_vregdata.id];
                let cl_reg = X64Register::CL;

                updated_instructions.push(
                    ByteCode::Mov(UnaryOperation{
                        src: count_slot.into(),
                        dest: cl_reg.get_alias_for_size(count_slot.size as u8).into(),
                    }));


                let dest_slot: Value = dest_stack_slot.into();

                if dest_stack_slot.offset != value_slot.offset {

                    let reg = get_register_for_size(value_slot.size);
                    ice_if!(reg.get_alias_for_size(1) == cl_reg, "Register collision");

                    updated_instructions.push(
                        ByteCode::Mov(UnaryOperation{
                            src: value_slot.into(),
                            dest: reg.into(),
                        }));

                    updated_instructions.push(
                        ByteCode::Mov(UnaryOperation{
                            src: reg.into(),
                            dest: dest_slot.clone(),
                        }));
                }

                updated_instructions.push(
                    enum_type(BinaryOperation{
                        src1: dest_slot.clone(),
                        src2: X64Register::CL.into(),
                        dest: dest_slot,
                    }));
            }
            _ => ice!("Not implemented for {:#?}", enum_type(binary_op.clone())),
    }
}

fn handle_unary_allocation<T>(
    enum_type: T,
    unary_op: &UnaryOperation,
    updated_instructions:
    &mut Vec<ByteCode>,
    stack_map: &StackMap) where T: Fn(UnaryOperation) -> ByteCode {
    match unary_op {

        /*
            a = OP b

            Emit:
            MOV tmp_reg, b
            OP tmp_reg
            MOV a, tmp_reg

        */
        UnaryOperation {
            src: VirtualRegister(src_vregdata),
            dest: VirtualRegister(dest_vregdata)
        } if src_vregdata.id != dest_vregdata.id => {

            let src_offset = offset_for_reg(stack_map, src_vregdata.id);
            let dest_offset = offset_for_reg(stack_map, dest_vregdata.id);
            let reg = get_register_for_size(src_vregdata.size);

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                src: src_offset,
                dest: reg.into(),
            }));
            updated_instructions.push(enum_type(UnaryOperation{
                src: reg.into(),
                dest: reg.into(),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                src: reg.into(),
                dest: dest_offset,
            }));

        }
        /*
            a = OP a

            Emit:
            OP a

        */
        UnaryOperation {
            src: VirtualRegister(src_vregdata),
            dest: VirtualRegister(dest_vregdata)
        } if src_vregdata.id == dest_vregdata.id => {

            let dest_offset = offset_for_reg(stack_map, dest_vregdata.id);

            updated_instructions.push(enum_type(UnaryOperation{
                src: dest_offset.clone(),
                dest: dest_offset
            }));
        }
        UnaryOperation {
            src: DynamicStackOffset{ index, offset, size, id } ,
            dest: VirtualRegister(dest_vregdata)
        } => {

            let reg = get_register_for_size(*size);
            let object_stack_slot = &stack_map.object_to_stack_slot[id];
            let dest_offset = offset_for_reg(stack_map, dest_vregdata.id);

            emit_mov_dynamic_stack_offset_to_reg(
                index,
                *size,
                *offset,
                *id,
                &reg,
                object_stack_slot,
                updated_instructions,
                stack_map
            );

            updated_instructions.push(enum_type(UnaryOperation{
                src: reg.into(),
                dest: reg.into(),
            }));

            updated_instructions.push(ByteCode::Mov(UnaryOperation{
                src: reg.into(),
                dest: dest_offset,
            }));

        },
        _ => ice!("Not implemented for {:#?}", enum_type(unary_op.clone())),
    }
}