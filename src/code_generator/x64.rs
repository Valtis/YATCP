
use byte_generator::ByteCode; 
use byte_generator::UnaryOperation;
use byte_generator::BinaryOperation;
use byte_generator::ComparisonOperation;
use byte_generator::Source;
use byte_generator::ComparisonType;

use byte_generator;

use code_generator;

use byteorder::{ByteOrder, LittleEndian };

use std::collections::HashMap;
use std::collections::HashSet;
use std::mem;

const MOV_IMMEDIATE_32_BIT_TO_REG_MEM: u8 = 0xC7;
const MOV_REG_REG_32_BIT : u8 = 0x89;

const ADD_WITH_8_BIT_CONSTANT : u8 = 0x83;
const ADD_WITH_32_BIT_CONSTANT : u8 = 0x81;
const ADD_REG_FROM_REG_MEM : u8 = 0x01;

const SUB_WITH_8_BIT_CONSTANT : u8 = 0x83;
const SUB_WITH_32_BIT_CONSTANT : u8 = 0x81;
const SUB_REG_FROM_REG_MEM : u8 = 0x29;

const SIGNED_MUL_WITH_8_BIT_CONSTANT : u8 = 0x6B;
const SIGNED_MUL_WITH_32_BIT_CONSTANT : u8 = 0x69;
const SIGNED_MUL_WITH_64_BIT_REGISTERS : u16 = 0x0FAF; 

const CMP_RAX_WITH_CONSTANT : u8 = 0x3D;
const CMP_REG_WITH_CONSTANT : u8 = 0x81;
const CMP_REG_WITH_REGMEM : u8 = 0x3B;

const JUMP_32BIT_NEAR_RELATIVE : u8 = 0xE9;

const JUMP_IF_LESS : u8 = 0x7C;
const JUMP_IF_LESS_OR_EQ : u8 = 0x7E;
const JUMP_IF_EQ : u8 = 0x74;
const JUMP_IF_GREATER_OR_EQ : u8= 0x7D;
const JUMP_IF_GREATER : u8 = 0x7F;

const SET_BYTE_IF_LESS : u16 = 0x0F9C;
const SET_BYTE_IF_LESS_OR_EQ : u16 = 0x0F9E;
const SET_BYTE_IF_EQ : u16 = 0x0F94;
const SET_BYTE_IF_GREATER_OR_EQ : u16 = 0x0F9D;
const SET_BYTE_IF_GREATER : u16 = 0x0F9F;




const SIGNED_DIV_64_BIT : u8 = 0xF7;

const SIGN_EXTENSION : u8 = 0x99;

const NOP : u8 = 0x90;

const NEAR_RETURN : u8 = 0xC3;

const PUSH : u8 = 0x50;
const POP : u8 = 0x58;


const ACCUMULATOR : u32 = 99999;


// wrxb bits for rex prefix
const REX_64_BIT_OPERAND: u8 = 0x08; 
const REX_EXT_RM_BIT: u8 = 0x01; 
const REX_EXT_REG_BIT: u8 = 0x04; 

// MOD bits for addressing
const MOD_REGISTER_DIRECT_ADDRESSING : u8 = 0xC0;



// flags for instruction emit
const FLAG_64_BIT_OPERANDS : u32 = 0x01;


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Register {
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
}

enum Mode {
    Register, // treat r/m field as a second register
}

// the reg bits in MODR/M section may either contain opcode extension
// or a operand register. Encode this in enum so these two different
// values won't be confused 
enum ModReg {
    Register(u32),
    OpCode(u8),
}

enum SizedOpCode {
    OpCode8(u8),
    OpCode16(u16),
}

// used to store jumps that need the target patched afterwards
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

impl Register {
    fn encoding(&self) -> u8 {
        match *self {
            Register::RAX | Register::R8 => 0x00,
            Register::RCX | Register::R9 => 0x01,
            Register::RDX | Register::R10 => 0x02,
            Register::RBX | Register::R11 => 0x03,
            Register::RSP | Register::R12 => 0x04,
            Register::RBP | Register::R13 => 0x05,
            Register::RSI | Register::R14 => 0x06,
            Register::RDI | Register::R15 => 0x07,
        }
    } 

    fn is_extended_reg(&self) -> bool {
       match *self {
            Register::R8 |
            Register::R9 |
            Register::R10 |
            Register::R11 |
            Register::R12 |
            Register::R13 |
            Register::R14 |
            Register::R15 => true,
            _ => false
        }  
    }
}

pub struct X64CodeGen {
    bytecode_functions: Vec<byte_generator::Function>,
    asm_code: Vec<u8>,
    label_pos: HashMap<u32, usize>,
    jumps_requiring_updates: Vec<JumpPatch>,
    functions: Vec<code_generator::Function>,
    reg_map: HashMap<u32, Register>,
    used_registers: HashSet<Register>,
}

impl X64CodeGen {

    pub fn new(bytecode_functions: Vec<byte_generator::Function>) -> X64CodeGen {
        let mut map = HashMap::new();
        // RSP, RBP not used for general register allocations
        map.insert(0, Register::RCX);
        map.insert(1, Register::RDX);
        map.insert(2, Register::RBX);
        map.insert(3, Register::RSI);
        map.insert(4, Register::RDI);
        map.insert(5, Register::R8); 
        map.insert(6, Register::R9); 
        map.insert(7, Register::R10);
        map.insert(8, Register::R11);
        map.insert(9, Register::R12); 
        map.insert(10, Register::R13); 
        map.insert(11, Register::R14); 
        map.insert(12, Register::R15); 
        map.insert(ACCUMULATOR, Register::RAX); 

        X64CodeGen {
            bytecode_functions: bytecode_functions,
            asm_code: vec![],
            label_pos: HashMap::new(),
            jumps_requiring_updates: vec![],
            functions: vec![],
            reg_map: map,
            used_registers: HashSet::new(),
        }
    }

    pub fn generate_code(&mut self) {


        self.transform_code(); 
        // horrible hack. initial transformation may leave some 
        // operations in a state that requires secondary transformation
        // TODO: Transformation is primarily a hack around the fact that
        // register allocation does not exists yet. Replace and fix
        self.transform_code(); 


        // TODO: Handle borrow checker errors more elegantly
        let functions = self.bytecode_functions.clone();
        for f in functions.iter() {
            self.allocate_registers();
            self.get_used_registers(&f.code);

            let mut func = code_generator::Function {
                name: f.name.clone(),
                start: self.asm_code.len(),
                length: 0,
            };

            self.emit_function_prologue();
            println!("{}\n", f.name);

            for b in f.code.iter() {
                println!(    "{:?}", b);
                match *b {
                    ByteCode::Nop => self.emit_nop(), 
                    ByteCode::Mov(ref operands) => self.emit_mov(operands),
                    ByteCode::Add(ref operands) => self.emit_add(operands),
                    ByteCode::Sub(ref operands) => self.emit_sub(operands),
                    ByteCode::Mul(ref operands) => self.emit_mul(operands),
                    ByteCode::Div(ref operands) => self.emit_div(operands),
                    ByteCode::Ret => self.emit_ret(),
                    ByteCode::SignExtend(ref operands) => self.emit_sign_extension(operands),
                    ByteCode::Compare(ref operands) => self.emit_comparison(operands),
                    ByteCode::Label(id) => self.handle_label(id),
                    ByteCode::Jump(id) => self.emit_unconditional_jump(id),
                    ByteCode::JumpConditional(id, ref jmp_type) => 
                        self.emit_conditional_jump(id, jmp_type),
                 }
            }
            self.update_jumps(); 
            func.length = self.asm_code.len() - func.start;
            self.functions.push(func);           
        }
    }

    fn get_used_registers(&mut self, code: &Vec<ByteCode>) {
        for b in code.iter() {
            println!(    "{:?}", b);
            match *b {
                ByteCode::Mov(ref operands) | 
                ByteCode::SignExtend(ref operands) => {
                    if let Source::Register(reg) = operands.src {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }

                    if let Source::Register(reg) = operands.dest {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }
                },
                ByteCode::Add(ref operands) |
                ByteCode::Sub(ref operands) |
                ByteCode::Mul(ref operands) |
                ByteCode::Div(ref operands)  => {
                    if let Source::Register(reg) = operands.src1 {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }

                    if let Source::Register(reg) = operands.src2 {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }

                    if let Source::Register(reg) = operands.dest {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }
                },            
                ByteCode::Compare(ref operands) => {
                    if let Source::Register(reg) = operands.src1 {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }

                    if let Source::Register(reg) = operands.src2 {
                        self.used_registers.insert(self.reg_map[&reg].clone());
                    }
                },
                _ => {},
             }
        }
    }

    // transforms the internal bytecode to a more suitable form than
    // can be then consumed by the code gen.
    fn transform_code(&mut self) {

        for f in self.bytecode_functions.iter_mut() {
            let mut free_register = 0;

            for b in f.code.iter() {
                match *b {
                    ByteCode::Add(ref op) | ByteCode::Sub(ref op) | ByteCode::Mul(ref op) | ByteCode::Div(ref op) => {
                        if let Source::Register(val) = op.dest {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 

                        if let Source::Register(val) = op.src1 {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 

                        if let Source::Register(val) = op.src2 {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 
                    },
                    ByteCode::Mov(ref op) | ByteCode::SignExtend(ref op) => {
                        if let Source::Register(val) = op.dest {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 

                        if let Source::Register(val) = op.src {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 
                    },  
                    ByteCode::Compare(ref op) => {
                        if let Source::Register(val) = op.src1 {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 

                        if let Source::Register(val) = op.src2 {
                            if free_register <= val && val != ACCUMULATOR {
                                free_register = val + 1;
                            }
                        } 
                    },
                    ByteCode::Nop | 
                    ByteCode::Ret | 
                    ByteCode::Label(_) | 
                    ByteCode::Jump(_) |
                    ByteCode::JumpConditional(_, _) => {},
                }
            }

            // Replace contants in addition operations with registers
            let mut new_code = vec![];

            for b in f.code.iter() {
                match *b {

                    // change addition of two constants
                    // into mov + addition.
                    //
                    // Also transform add const, reg into add reg, const
                    // as this simplifies code gen a bit. 
                    //
                    // Also handle the case where the dest and src1 are different registers, 
                    // as x86 sub instruction implicitly uses src1 as dest as well
                    ByteCode::Add(ref operands) => {

                         let mut new_operands = operands.clone();

                         match operands.src1 {
                            Source::IntegerConstant(_) => {
                                match operands.src2 {
                                    // two integer constant addition - generate a mov in between
                                    // so that we get reg, const addition
                                    Source::IntegerConstant(_) => {
                                        new_code.push(ByteCode::Mov(UnaryOperation {
                                        src: operands.src1.clone(),
                                        dest: operands.dest.clone(),
                                    }));      

                                    new_operands.src1 = operands.dest.clone(); 
                                    },
                                    // swap operands
                                    Source::Register(_) => {
                                        mem::swap(&mut new_operands.src1, &mut new_operands.src2);
                                    },
                                    _ => {},
                                }

                              
                            }
                            Source::Register(reg) => {
                                if let Source::Register(reg2) = operands.dest {
                                    if reg != reg2 {
                                        new_code.push(ByteCode::Mov(UnaryOperation {
                                            src: operands.src1.clone(),
                                            dest: operands.dest.clone(),
                                        }));  

                                        new_operands.src1 = new_operands.dest.clone();
                                    }
                                }
                            },
                            _ => unimplemented!(),
                        }

                        new_code.push(ByteCode::Add(new_operands));

                    },

                    // change subtraction of two constants
                    // into mov + subtraction.
                    // This covers both cases where the bytecode is sub constant, constant
                    // or sub constant, register
                    // as the x86 sub operation does not allow subtracting two constants
                    // from each other, or subtracting register from constant
                    //
                    // Also handle the case where the dest and src1 are different registers, 
                    // as x86 sub instruction implicitly uses src1 as dest as well

                    ByteCode::Sub(ref operands) => {

                         let mut new_operands = operands.clone();

                         match operands.src1 {
                            Source::IntegerConstant(_) => {

                                new_code.push(ByteCode::Mov(UnaryOperation {
                                    src: operands.src1.clone(),
                                    dest: operands.dest.clone(),
                                }));      

                                new_operands.src1 = operands.dest.clone(); 
                            },
                            Source::Register(reg) => {
                                if let Source::Register(reg2) = operands.dest {
                                    if reg != reg2 {

                                        new_code.push(ByteCode::Mov(UnaryOperation {
                                            src: operands.src1.clone(),
                                            dest: operands.dest.clone(),
                                        }));      
                                        new_operands.src1 = operands.dest.clone(); 
                                    }
                                }
                            }
                            _ => {} ,
                        }

                        new_code.push(ByteCode::Sub(new_operands));
                    },
                    // change multiplications of two constants
                    // into mov + multiplication with register & constant
                    // so this can actually be encoded using
                    // 
                    // change multiplication in form of r1 = r2 * r3 into
                    // mov r2 -> r1
                    // r1 = r1 * r3
                    ByteCode::Mul(ref operands) => {
                        let mut new_operands = operands.clone();

                        match (&operands.dest, &operands.src1, &operands.src2) {
                            (_, &Source::IntegerConstant(_), &Source::IntegerConstant(_)) => {
                                new_code.push(ByteCode::Mov(UnaryOperation {
                                    src: operands.src1.clone(),
                                    dest: Source::Register(free_register),
                                }));      

                                new_operands.src1 = Source::Register(free_register); 
                                free_register += 1;  
                            },
                            (&Source::Register(r1), 
                                &Source::Register(r2), 
                                &Source::Register(r3)) =>  {
                                // swap source registers
                                if r1 != r2 && r1 == r3 {
                                    let tmp = new_operands.src1;
                                    new_operands.src1 = new_operands.src2;
                                    new_operands.src2 = tmp;
                                } else if r1 != r2 && r1 != r3 {
                                    new_code.push(ByteCode::Mov(UnaryOperation {
                                        src: operands.src1.clone(),
                                        dest: operands.dest.clone(),
                                    }));      

                                    new_operands.src1 = new_operands.dest.clone();
                                } 

                            }
                            _ => {},
                        };



                        new_code.push(ByteCode::Mul(new_operands));
                    },
                    // div implicitly uses rax/rdx as both destination and divdend
                    // make sure those registers are backed up and restored, as well as
                    // populated with correct values
                    ByteCode::Div(ref operands) => {
                        let mut new_operands = operands.clone();

                        let rdx_backup = Source::Register(free_register); 
                        free_register += 1;

                        new_code.push(ByteCode::Mov(UnaryOperation {
                            src: Source::Register(1), // 1 -> RDX
                            dest: rdx_backup.clone(),
                        }));    

                        // move src1 into rax and zero r
                        new_code.push(ByteCode::Mov(UnaryOperation {
                            src: operands.src1.clone(), 
                            dest: Source::Register(ACCUMULATOR),
                        }));  

                        // sign extend rax into rdx
                        new_code.push(ByteCode::SignExtend(UnaryOperation {
                                src: Source::Register(ACCUMULATOR), 
                                dest: Source::Register(1),
                        }));  


                        // if src2 is immediate value, store it in a register first
                        if let Source::IntegerConstant(_) = operands.src2 {
                            let divisor = Source::Register(free_register); 
                            free_register += 1;

                            new_code.push(ByteCode::Mov(UnaryOperation {
                                src: operands.src2.clone(), // 1 -> RDX
                                dest: divisor.clone(),
                            }));  

                            new_operands.src2 = divisor;

                        }
                        
                        new_code.push(ByteCode::Div(new_operands));

                        // restore old value of rdx
                        new_code.push(ByteCode::Mov(UnaryOperation {
                            src: rdx_backup, // 1 -> RDX
                            dest: Source::Register(1),
                        })); 
                        // move the value to the expected destination
                        new_code.push(ByteCode::Mov(UnaryOperation {
                                src: Source::Register(ACCUMULATOR), // 1 -> RDX
                                dest: operands.dest.clone(),
                        }));  

                    },

                    ByteCode::Compare(ref operands) => {
                    // convert compare(intconst, intconst into move rax - cmp rax, instconst)
                        match (&operands.src1, &operands.src2) {
                            (&Source::IntegerConstant(_), &Source::IntegerConstant(_)) => {
                                // move first argument into RAX
                                new_code.push(ByteCode::Mov(UnaryOperation {
                                    src: operands.src1.clone(),
                                    dest: Source::Register(ACCUMULATOR),
                                }));

                                // and use that in the comparison
                                let mut new_operands = operands.clone();
                                new_operands.src1 = Source::Register(ACCUMULATOR);

                                new_code.push(ByteCode::Compare(new_operands));
                                //panic!("Foo: {:?}", new_code[new_code.len() - 1]);
                            },
                            _ => { new_code.push(b.clone()) }, 
                        }
                    },
                    _ => new_code.push(b.clone())
                }                       
            }

            f.code = new_code;
        }

    }


    fn allocate_registers(&mut self) {
        // TODO - implement. 
    }

    fn emit_nop(&mut self) {
        self.asm_code.push(NOP);
    }

    fn emit_sign_extension(&mut self, operands: &UnaryOperation) {
        if let Source::Register(src_reg) = operands.src {
            if let Source::Register(dest_reg) = operands.dest {
                if src_reg == ACCUMULATOR || dest_reg == 1 /* evil hardcoded number... */ {
                    self.asm_code.push(0x48);
                    self.asm_code.push(SIGN_EXTENSION);
                    return;
                }
            } 
        } 

        unimplemented!();
    }

    fn emit_mov(&mut self, operand: &UnaryOperation) {
        match operand.src {
            Source::IntegerConstant(val) => {
                let bytes_in_u32 = 4;
                self.emit_instruction(
                    SizedOpCode::from(MOV_IMMEDIATE_32_BIT_TO_REG_MEM),
                    Some((
                        Mode::Register,
                        ModReg::OpCode(0),
                        &operand.dest)),
                    Some((val, bytes_in_u32)),
                    FLAG_64_BIT_OPERANDS);
            },
            Source::Register(reg) => {

                self.emit_instruction(
                    SizedOpCode::from(MOV_REG_REG_32_BIT),
                    Some((
                        Mode::Register,
                        ModReg::Register(reg),
                        &operand.dest)),
                    None,
            FLAG_64_BIT_OPERANDS);
            },
            Source::ComparisonResult(ref cmp_type) => {
                self.emit_set_byte(cmp_type, operand);
            },
            _ => unimplemented!(),
        }
    }

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

    fn emit_add(&mut self, operand: &BinaryOperation) {
        let dest = if let Source::Register(reg) = operand.dest {
            reg
        } else {
            panic!("Internal compiler error: Non-register destination for addition: {:?}", operand);
        };

        match operand.src1 {
            Source::IntegerConstant(_) => {
                ice!("Immediate operand as first argument to add: {:?}", operand);    
            },
            _ => {},
        }

        match operand.src2 {
            Source::IntegerConstant(val) => {
                self.emit_const_integer_add(dest, val);
                return;
            },
            _ => {},
        }

        // reg - reg sub or memory addresses
        let mut wrxb_bits = 0u8;
        let mut src_reg_bits = 0u8;
        let mut dest_reg_bits = 0u8;

        if operand.dest != operand.src1 {
            panic!("Internal compiler error: src1 and dest arguments must be identical for sub instruction, was: {:?}",
                operand);
        }

        match operand.dest {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    wrxb_bits |= REX_EXT_RM_BIT;
                }
                dest_reg_bits |= self.reg_map[&reg].encoding();
            },
            _ => unimplemented!(),
        }  

        match operand.src2 {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    wrxb_bits |= REX_EXT_REG_BIT;
                }
                src_reg_bits |= self.reg_map[&reg].encoding() << 3;
            },
            _ => unimplemented!(), // operand from memory address needs to be handled
        }

        let prefix = self.create_rex_prefix(REX_64_BIT_OPERAND | wrxb_bits);
        println!("WRXB byte: 0{:b}", prefix);
        self.asm_code.push(prefix);     
        self.asm_code.push(ADD_REG_FROM_REG_MEM);
        self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);
        println!("ModRM byte: {:b}", MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);
    }

    // emits code for reg <- reg/mem address * 32 bit integer constant
    fn emit_const_integer_add(&mut self, destination_reg: u32, constant: i32) {
        let (const_bytes, opcode) = if constant <= 127 && constant >= -128 {
            (1, ADD_WITH_8_BIT_CONSTANT)
        } else {
            (4, ADD_WITH_32_BIT_CONSTANT)
        };

        self.emit_instruction(
            SizedOpCode::from(opcode), 
            Some((Mode::Register,
            ModReg::OpCode(0), 
            &Source::Register(destination_reg))),
            Some((constant, const_bytes)),
            FLAG_64_BIT_OPERANDS);
    }
    
    fn emit_sub(&mut self, operand: &BinaryOperation) {
        let dest = if let Source::Register(reg) = operand.dest {
            reg
        } else {
            panic!("Internal compiler error: Non-register destination for subtraction: {:?}", operand);
        };

        match operand.src1 {
            Source::IntegerConstant(_) => { 
                panic!("Internal compiler error: Immediate operand as first argument to sub: {:?}", operand);    
            },
            _ => {},
        }

        match operand.src2 {
            Source::IntegerConstant(val) => {
            // not tested so let's not invoke it yet
            self.emit_const_integer_sub(dest, val); return; },
            _ => {},
        }

        // reg - reg sub or memory addresses
        let mut wrxb_bits = 0u8;
        let mut src_reg_bits = 0u8;
        let mut dest_reg_bits = 0u8;

        if operand.dest != operand.src1 {
            panic!("Internal compiler error: src1 and dest arguments must be identical for sub instruction, was: {:?}",
                operand);
        }

        match operand.dest {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    wrxb_bits |= REX_EXT_RM_BIT;
                }
                dest_reg_bits |= self.reg_map[&reg].encoding();
            },
            _ => unimplemented!(),
        }  

        match operand.src2 {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    wrxb_bits |= REX_EXT_REG_BIT;
                }
                src_reg_bits |= self.reg_map[&reg].encoding() << 3;
            },
            _ => unimplemented!(), // operand from memory address needs to be handled
        }

        let prefix = self.create_rex_prefix(REX_64_BIT_OPERAND | wrxb_bits);
        println!("WRXB byte: 0{:b}", prefix);
        self.asm_code.push(prefix);     
        self.asm_code.push(SUB_REG_FROM_REG_MEM);
        self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);
        println!("ModRM byte: {:b}", MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);
    }

    // emits code for reg <- reg/mem address * 32 bit integer constant
    fn emit_const_integer_sub(&mut self, destination_reg: u32, constant: i32) {
        let (const_bytes, opcode) = if constant <= 127 && constant >= -128 {
            (1, SUB_WITH_8_BIT_CONSTANT)
        } else {
            (4, SUB_WITH_32_BIT_CONSTANT)
        };
        self.emit_instruction(
            SizedOpCode::from(opcode),
            Some((Mode::Register,
            ModReg::OpCode(5), 
            &Source::Register(destination_reg))),
            Some((constant, const_bytes)),
            FLAG_64_BIT_OPERANDS);
    }

    fn emit_mul(&mut self, operand: &BinaryOperation) {

        // sanity check - should always have register destination
        let dest_reg_id = match operand.dest {
            Source::Register(dest) => dest,
            _ => ice!("Non-register destination for multiplication: {:?}", operand),
        };

        match operand.src1 {
            Source::IntegerConstant(val) => { 
                self.emit_const_integer_mult(
                    dest_reg_id, 
                    &operand.src2, 
                    val); 
                return;
             },
            _ => {},
        }

        match operand.src2 {
            Source::IntegerConstant(val) => { 
                self.emit_const_integer_mult(
                    dest_reg_id, 
                    &operand.src1, 
                    val); 
                return; },
            _ => {},
        }


        match (&operand.dest, &operand.src1, &operand.src2) {
            (
                &Source::Register(dest), 
                &Source::Register(src1),
                &Source::Register(src2)
            ) => {
                if src1 != dest {
                    ice!(
                        "Invalid mul opcode - src1 and dest do not match: {:?}", 
                        operand);
                }
                self.emit_instruction(
                    SizedOpCode::from(SIGNED_MUL_WITH_64_BIT_REGISTERS),
                    Some(
                        (Mode::Register,
                        ModReg::Register(dest), 
                        &Source::Register(src2))),
                    None,
                    FLAG_64_BIT_OPERANDS);
                return;
            } ,
            _ => {},
        }

        unimplemented!();

    }

    // emits code for reg <- reg/mem address * 32 bit integer constant
    fn emit_const_integer_mult(&mut self, destination_reg: u32 ,  src_val: &Source, constant: i32) {
        let (const_bytes, opcode) = if constant <= 127 && constant >= -128 {
            (1, SIGNED_MUL_WITH_8_BIT_CONSTANT)
        } else {
            (4, SIGNED_MUL_WITH_32_BIT_CONSTANT)
        };

        self.emit_instruction(
            SizedOpCode::from(opcode), 
            Some((Mode::Register, 
            ModReg::Register(destination_reg), 
            src_val)),
            Some((constant, const_bytes)),
            FLAG_64_BIT_OPERANDS); 
    }

    fn emit_comparison(&mut self, operands: &ComparisonOperation)  {
        match (&operands.src1, &operands.src2) {
            (&Source::Register(ACCUMULATOR), &Source::IntegerConstant(i)) => {
                self.emit_instruction(
                    SizedOpCode::from(CMP_RAX_WITH_CONSTANT), 
                    None,
                    Some((i, 4)),
                    FLAG_64_BIT_OPERANDS); 
            },
            (&Source::Register(reg), &Source::IntegerConstant(i)) => {
                self.emit_instruction(
                    SizedOpCode::from(CMP_REG_WITH_CONSTANT), 
                    Some((
                        Mode::Register,
                        ModReg::OpCode(7),
                        &Source::Register(reg),
                    )),
                    Some((i, 4)),
                    FLAG_64_BIT_OPERANDS); 
            },
            (&Source::Register(reg1), &Source::Register(reg2)) => {
                self.emit_instruction(
                    SizedOpCode::from(CMP_REG_WITH_REGMEM), 
                    Some((
                        Mode::Register,
                        ModReg::Register(reg1),
                        &Source::Register(reg2),
                    )),
                    None,
                    FLAG_64_BIT_OPERANDS); 
            }
            _ => unimplemented!(),
        }
    } 

    fn emit_unconditional_jump(&mut self, id: u32) {

        self.jumps_requiring_updates.push(JumpPatch::Jump(id, self.asm_code.len()));
        self.asm_code.push(JUMP_32BIT_NEAR_RELATIVE);
        // place for the 
        for _ in 0..4 {
            self.asm_code.push(0x00);
        }
    
    }

    fn emit_conditional_jump(&mut self, id: u32, jmp_type: &ComparisonType) {
        let jmp_code = match *jmp_type {
            ComparisonType::Less => JUMP_IF_LESS,
            ComparisonType::LessOrEq => JUMP_IF_LESS_OR_EQ,
            ComparisonType::Equals => JUMP_IF_EQ,
            ComparisonType::GreaterOrEq => JUMP_IF_GREATER_OR_EQ,
            ComparisonType::Greater => JUMP_IF_GREATER,     
        };

        self.jumps_requiring_updates.push(JumpPatch::ConditionalShortJump(id, self.asm_code.len()));
        self.asm_code.push(jmp_code);         
        self.asm_code.push(0x00); // target placeholder

/*        if self.label_pos.contains_key(&id) {
            let target = self.label_pos[&id];
            let op_size = 2i32;
            let offset : i32 = target as i32 - self.asm_code.len() as i32 - op_size;
            if offset < -128 || offset > 127 {
                panic!("Not implemented for jumps larger than a byte");
            }

            self.asm_code.push(jmp_code);
            self.asm_code.push((offset as i8) as u8)
        } else {
            unimplemented!();
        }*/
    }

    fn emit_instruction(
        &mut self,         
        opcode: SizedOpCode,
        modrm: Option<(Mode, ModReg, &Source)>,
        constant: Option<(i32, usize)>,
        flags: u32) {
        
        let mut wrxb_bits = 0u8;
        let regrm_field : Option<u8> = if let Some((mode, mod_reg, mod_rm)) = modrm {
            let mut field = 0u8;  
            field |= match mode {
                Mode::Register => MOD_REGISTER_DIRECT_ADDRESSING, 
            };

            match mod_reg {
                ModReg::Register(reg_id) => {
                    if self.reg_map[&reg_id].is_extended_reg() {
                        wrxb_bits |= REX_EXT_REG_BIT;
                    }
                    field |= self.reg_map[&reg_id].encoding() << 3;
                }
                ModReg::OpCode(opcode) => {
                    field |= opcode << 3;
                }
                
            }
            match *mod_rm {
                Source::Register(reg_id) => {
                    if self.reg_map[&reg_id].is_extended_reg() {
                        wrxb_bits |= REX_EXT_RM_BIT;
                    }
                    field |= self.reg_map[&reg_id].encoding();
                },
                Source::ReturnRegister => {
                    if self.reg_map[&ACCUMULATOR].is_extended_reg() {
                        wrxb_bits |= REX_EXT_RM_BIT;
                    }
                    field |= self.reg_map[&ACCUMULATOR].encoding(); 
                }
                ref x => panic!("unimplemented: {:?}", x), // operand from memory address needs to be handled
            }
            Some(field)
        } else {
            None
        };

        if flags & FLAG_64_BIT_OPERANDS != 0 { 
            wrxb_bits |= REX_64_BIT_OPERAND;  
        }
            let prefix = self.create_rex_prefix(wrxb_bits);
            self.asm_code.push(prefix);   

        match opcode {
            SizedOpCode::OpCode8(val) => self.asm_code.push(val),
            SizedOpCode::OpCode16(val) => {
                let mut buffer = [0; 2];
                LittleEndian::write_u16(&mut buffer, val);
                self.asm_code.push(buffer[1]);
                self.asm_code.push(buffer[0]);              

            }
        }
        

        if let Some(field) = regrm_field {
            self.asm_code.push(field);
        }

        if let Some((constant_val, constant_size)) = constant {
            let mut buffer = [0; 4];
            LittleEndian::write_i32(&mut buffer, constant_val);

            for i in 0..constant_size {
                self.asm_code.push(buffer[i]);
            }
        }
    }

    fn emit_div(&mut self, operand: &BinaryOperation) {

        let mut wrxb_bits = REX_64_BIT_OPERAND;
        let mut src_reg_bits = 0;

        if let Source::Register(reg) = operand.src2 {
            src_reg_bits |= self.reg_map[&reg].encoding();
            if self.reg_map[&reg].is_extended_reg() {
                wrxb_bits |= REX_EXT_RM_BIT;
            }
        } else {
            unimplemented!();
        }

        let prefix = self.create_rex_prefix(wrxb_bits);
        self.asm_code.push(prefix);
        self.asm_code.push(SIGNED_DIV_64_BIT);
        self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | (111 << 3) | src_reg_bits);

    }

    fn emit_ret(&mut self) {
        self.emit_function_epilogue();
        self.asm_code.push(NEAR_RETURN);
    }

    fn emit_function_prologue(&mut self) {
        if self.used_registers.contains(&Register::RBP) {
            self.push(Register::RBP);
        }

        if self.used_registers.contains(&Register::RBX) {
            self.push(Register::RBX);
        }

        if self.used_registers.contains(&Register::R12) {
            self.push(Register::R12);
        }

        if self.used_registers.contains(&Register::R13) {
            self.push(Register::R13);
        }

        if self.used_registers.contains(&Register::R14) {
            self.push(Register::R14);
        }

        if self.used_registers.contains(&Register::R15) {
            self.push(Register::R15);
        }
    }

    fn emit_function_epilogue(&mut self) {
        if self.used_registers.contains(&Register::R15) {
            self.pop(Register::R15);
        }

        if self.used_registers.contains(&Register::R14) {
            self.pop(Register::R14);
        }

        if self.used_registers.contains(&Register::R13) {
            self.pop(Register::R13);
        }

        if self.used_registers.contains(&Register::R12) {
            self.pop(Register::R12);
        }

        if self.used_registers.contains(&Register::RBX) {
            self.pop(Register::RBX);
        }

        if self.used_registers.contains(&Register::RBP) {
            self.pop(Register::RBP);
        }
    }

    fn push(&mut self, register: Register) {
        if register.is_extended_reg() {
            self.asm_code.push(0x41);
        }
        self.asm_code.push(PUSH + register.encoding());
    }

    fn pop(&mut self, register: Register) {
        if register.is_extended_reg() {
            self.asm_code.push(0x41);
        }
        self.asm_code.push(POP + register.encoding());
    }

    // REX prefix is used to encode things related to 64 bit instruction set
    // REX prefix always starts with bit pattern 0100 (0x4)
    // remaining bits are called WRXB, so the whole pattern is 0100WRXB
    // the meaning of these bits is the following (from http://wiki.osdev.org/X86-64_Instruction_Encoding):
    // W   1 bit   When 1, a 64-bit operand size is used. Otherwise, when 0, the default operand size is used (which is 32-bit for most but not all instructions, see this table).
    //
    // R   1 bit   This 1-bit value is an extension to the MODRM.reg field. See Registers.
    // X   1 bit   This 1-bit value is an extension to the SIB.index field. See 64-bit addressing.
    // B   1 bit   This 1-bit value is an extension to the MODRM.rm field or the SIB.base field. See 64-bit addressing.  
    fn create_rex_prefix(&self, wrxb: u8) -> u8 {
        0x40 | wrxb
    }

    fn handle_label(&mut self, id: u32) {
        self.label_pos.insert(id, self.asm_code.len());
    }

    fn update_jumps(&mut self) {
        for jump in self.jumps_requiring_updates.iter() {
            match *jump {
                JumpPatch::Jump(label_id, jump_opcode_location) => {
                    if self.label_pos.contains_key(&label_id) {
                        let jump_address = self.label_pos[&label_id];
                        // Note: Breaks if the offset is larger than i32::maxval
                        // (although jump of that size is hopefully unlikely)
                        let unconditional_jump_size = 5; // 1 for opcode, 4 for location
                        let offset : i32 = jump_address as i32 - jump_opcode_location as i32 - unconditional_jump_size;
                        
                        let mut buffer = [0; 4];
                        LittleEndian::write_i32(&mut buffer, offset);
                        
                        for i in 0..4 {
                            // +1 so that the one-byte opcode is skipped 
                            self.asm_code[jump_opcode_location + 1 + i] = buffer[i];
                        }


                    } else {
                        ice!("No jump target for jump stored");
                    }
                },
                JumpPatch::ConditionalShortJump(label_id, jump_opcode_location) => {
                    if self.label_pos.contains_key(&label_id) {
                        let target = self.label_pos[&label_id];                      
                        let op_size = 2i32; // size of the whole operation (opcode + operand)
                        let offset : i32 = target as i32 - jump_opcode_location as i32 - op_size;
                        if offset < -128 || offset > 127 {
                            panic!("Not implemented for jumps larger than a byte");
                        }
                        // +1 so that the one-byte opcocde is skipped
                        self.asm_code[jump_opcode_location + 1] = (offset as i8) as u8;
                    } else {
                        ice!("No jump target for conditional short jump stored");
                    }
                }
            } 
        }
    }
    
    pub fn get_code(&self) -> Vec<u8> {
        self.asm_code.clone()
    }

    pub fn get_functions(&self) -> Vec<code_generator::Function> {
        self.functions.clone()
    }

}
