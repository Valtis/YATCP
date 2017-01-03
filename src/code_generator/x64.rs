
use byte_generator::ByteCode; 
use byte_generator::UnaryOperation;
use byte_generator::BinaryOperation;
use byte_generator::Source;


use byteorder::{ByteOrder, LittleEndian };

use std::collections::HashMap;

const MOV_IMMEDIATE_32_BIT_TO_REG_MEM: u8 = 0xC7;
const MOV_REG_REG_32_BIT : u8 = 0x89;


const SUB_WITH_8_BIT_CONSTANT : u8 = 0x83;
const SUB_WITH_32_BIT_CONSTANT : u8 = 0x83;
const SUB_REG_FROM_REG_MEM : u8 = 0x29;

const SIGNED_MUL_WITH_8_BIT_CONSTANT : u8 = 0x6B;
const SIGNED_MUL_WITH_32_BIT_CONSTANT : u8 = 0x69;

const SIGNED_DIV_64_BIT : u8 = 0xF7;

const SIGN_EXTENSION : u8 = 0x99;

const NOP : u8 = 0x90;

const NEAR_RETURN : u8 = 0xC3;

const PUSH : u8 = 0x50;
const POP : u8 = 0x58;


const ACCUMULATOR : u32 = 999;


// wrxb bits for rex prefix
const REX_64_BIT_OPERAND: u8 = 0x08; 
const REX_EXT_RM_BIT: u8 = 0x01; 
const REX_EXT_REG_BIT: u8 = 0x04; 

// MOD bits for addressing
const MOD_REGISTER_DIRECT_ADDRESSING : u8 = 0xC0;

#[derive(Clone, Copy)]
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
    bytecode: Vec<ByteCode>,
    asm_code: Vec<u8>,
    reg_map: HashMap<u32, Register>,
}

impl X64CodeGen {

    pub fn new(bytecode: Vec<ByteCode>) -> X64CodeGen {
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
            bytecode: bytecode,
            asm_code: vec![],
            reg_map: map,
        }
    }

    pub fn generate_code(&mut self) {
        self.transform_code(); 
        self.allocate_registers();
        self.emit_function_prologue();

        // TODO: Handle borrow checker errors more elegantly
        let bcode = self.bytecode.clone();
        for b in bcode.iter() {
            println!("{:?}", b);
            match *b {
                ByteCode::Nop => self.emit_nop(), 
                ByteCode::Mov(ref operands) => self.emit_mov(operands),
                ByteCode::Sub(ref operands) => self.emit_sub(operands),
                ByteCode::Mul(ref operands) => self.emit_mul(operands),
                ByteCode::Div(ref operands) => self.emit_div(operands),
                ByteCode::Ret => self.emit_ret(),
                ByteCode::SignExtend(ref operands) => self.emit_sign_extension(operands),
                _ => unimplemented!(),
            }
        }
    }

    // transforms the internal bytecode to a more suitable form than
    // can be then consumed by the code gen.
    fn transform_code(&mut self) {
        // TODO: Ensure that opcodes in form of dst = src1 * src2 are transformed
        // as x64 implicitly uses dst register as src1 as well (dst = dst * src2)

        // This is used as the first free register when generating new temporaries
        // for 

        let mut free_register = 0;

        for b in self.bytecode.iter() {
            match *b {
                ByteCode::Add(ref op) | ByteCode::Sub(ref op) | ByteCode::Mul(ref op) | ByteCode::Div(ref op) => {
                    if let Source::Register(val) = op.dest {
                        if free_register <= val {
                            free_register = val + 1;
                        }
                    } 

                    if let Source::Register(val) = op.src1 {
                        if free_register <= val {
                            free_register = val + 1;
                        }
                    } 

                    if let Source::Register(val) = op.src2 {
                        if free_register <= val {
                            free_register = val + 1;
                        }
                    } 
                },
                ByteCode::Mov(ref op) | ByteCode::SignExtend(ref op) => {
                    if let Source::Register(val) = op.dest {
                        if free_register <= val {
                            free_register = val + 1;
                        }
                    } 

                    if let Source::Register(val) = op.src {
                        if free_register <= val {
                            free_register = val + 1;
                        }
                    } 
                },  
                ByteCode::Nop | ByteCode::Ret => {},
            }
        }

        // Replace contants in addition operations with registers
        let mut new_code = vec![];

        for b in self.bytecode.iter() {
            match *b {

                // change subtraction of two constants
                // into mov + subtraction when first operand is constant.
                // This covers both cases where the bytecode is sub constant, constant
                // or sub constant, register
                // as the x86 sub operation does not allow subtracting two constants
                // from each otther, or subtracting register from constant
                //
                // Also handle the case where the dest and src1 are different registers, 
                // as x86 sub instruction implicitly uses src1 as dest as well

                ByteCode::Sub(ref operands) => {

                     let mut new_operands = operands.clone();

                     match operands.src1 {
                        Source::IntegerConstant(val) => {

                            new_code.push(ByteCode::Mov(UnaryOperation {
                                src: operands.src1.clone(),
                                dest: operands.dest.clone(),
                            }));      

                            new_operands.src1 = operands.dest.clone(); 
                            free_register += 1;  
                            
                        },
                        Source::Register(reg) => {
                            if let Source::Register(reg2) = operands.src1 {
                                if reg != reg2 {
                                    // TODO: Handle the case where sub src1 and dest registers
                                    // are different and generate a move to fix this (like above)
                                    unimplemented!();
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
                ByteCode::Mul(ref operands) => {
                    let mut new_operands = operands.clone();

                    match operands.src1 {
                        Source::IntegerConstant(val) => {
                            // only transform if the second arg is an integer as well
                            if let Source::IntegerConstant(_) = operands.src2 {                                  
                                new_code.push(ByteCode::Mov(UnaryOperation {
                                    src: operands.src1.clone(),
                                    dest: Source::Register(free_register),
                                }));      

                                new_operands.src1 = Source::Register(free_register); 
                                free_register += 1;  
                            }                             

                        },
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
                    if let Source::IntegerConstant(val) = operands.src2 {
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
                _ => new_code.push(b.clone())
            };                       
        }

        self.bytecode = new_code;
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
        let mut WRXB_bits = 0u8;
        let mut register = 0u8;
        match operand.dest {
            Source::Register(reg) => {
                // register encoding is is in form of 0x0y, where y is number between 0 - f.
                // first bit toggles the registers r8 - r15 and must be encoded in the
                // REX prefix. Rest of the bits form an octal that select the register.
                if self.reg_map[&reg].is_extended_reg() {
                    WRXB_bits = REX_EXT_RM_BIT;
                }
                register = self.reg_map[&reg].encoding();
            }
            // SYSTEMV api returns value in rax
            Source::ReturnRegister => {
                if self.reg_map[&ACCUMULATOR].is_extended_reg() {
                    WRXB_bits = REX_EXT_RM_BIT;
                }
                register = self.reg_map[&ACCUMULATOR].encoding();
            },
            _ => unimplemented!(), // mov to memory address needs to be implemented
        };
    
        match operand.src {
            Source::IntegerConstant(val) => {

                // rex prefix, which contains bits that extend the 32 bit instruction set into 64 bit
                let prefix = self.create_rex_prefix(REX_64_BIT_OPERAND | WRXB_bits);
                self.asm_code.push(prefix);
                // opcode
                self.asm_code.push(MOV_IMMEDIATE_32_BIT_TO_REG_MEM);
                // ModR/M byte. First two bits encode addressing mode, 
                // next three bits encode either op code extension (used by some instructions)
                // or a 3 bit register reference, which is source or destination register on
                // some instructions. REX.R bit extends this to 4 bits, so that r8 - r15 registers
                // can be used 
                self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | register);

                let mut buffer = [0; 4];
                LittleEndian::write_i32(&mut buffer, val);
                for b in buffer.iter() {
                    self.asm_code.push(*b);
                }
            },
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    WRXB_bits = WRXB_bits | REX_EXT_REG_BIT;
                }

                register = register | (self.reg_map[&reg].encoding() << 3);

                // rex prefix, which contains bits that extend the 32 bit instruction set into 64 bit
                let prefix = self.create_rex_prefix(REX_64_BIT_OPERAND | WRXB_bits);
                self.asm_code.push(prefix);
                self.asm_code.push(MOV_REG_REG_32_BIT);
                self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | register);

            },
            _ => { self.asm_code.push(0x00); self.asm_code.push(0x00); self.asm_code.push(0x00); self.asm_code.push(0x00); },
        }
    }
    
    fn emit_sub(&mut self, operand: &BinaryOperation) {
        let dest = if let Source::Register(reg) = operand.dest {
            reg
        } else {
            panic!("Internal compiler error: Non-register destination for subtraction: {:?}", operand);
        };

        match operand.src1 {
            Source::IntegerConstant(val) => { 
                panic!("Internal compiler error: Immediate operand as first argument to sub: {:?}", operand);    
            },
            _ => {},
        }

        match operand.src2 {
            Source::IntegerConstant(val) => {

            // not tested so let's not invoke it yet
            unimplemented!();
            self.emit_const_integer_sub(dest, val, &operand.src1); return; },
            _ => {},
        }

        // reg - reg sub or memory addresses
        let mut WRXB_bits = 0u8;
        let mut src_reg_bits = 0u8;
        let mut dest_reg_bits = 0u8;

        if operand.dest != operand.src1 {
            panic!("Internal compiler error: src1 and dest arguments must be identical for sub instruction, was: {:?}",
                operand);
        }

        match operand.dest {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    WRXB_bits |= REX_EXT_RM_BIT;
                }
                dest_reg_bits = self.reg_map[&reg].encoding();
            },
            _ => unimplemented!(),
        }  

        match operand.src2 {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    WRXB_bits |= REX_EXT_REG_BIT;
                }
                src_reg_bits = self.reg_map[&reg].encoding() << 3;
            },
            _ => unimplemented!(), // operand from memory address needs to be handled
        }

        let prefix = self.create_rex_prefix(REX_64_BIT_OPERAND | WRXB_bits);
        println!("WRXB byte: 0{:b}", prefix);
        self.asm_code.push(prefix);     
        self.asm_code.push(SUB_REG_FROM_REG_MEM);
        self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);
        println!("ModRM byte: {:b}", MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);
    }

    // emits code for reg <- reg/mem address * 32 bit integer constant
    fn emit_const_integer_sub(&mut self, destination_reg: u32, constant: i32, src_val: &Source) {
        let (const_bytes, opcode) = if constant <= 127 && constant >= -128 {
            (1, SIGNED_MUL_WITH_8_BIT_CONSTANT)
        } else {
            (4, SIGNED_MUL_WITH_32_BIT_CONSTANT)
        };
        self.emit_const_arith_op(opcode, destination_reg, constant, const_bytes, src_val);
    }

    // Arithmetic operations currently generate a move to/from accumulator register
    // if this src1/dest aren't accumulator already. 
    fn emit_mul(&mut self, operand: &BinaryOperation) {

        let dest = if let Source::Register(reg) = operand.dest {
            reg
        } else {
            panic!("Internal compiler error: Non-register destination for multiplication: {:?}", operand);
        };

        match operand.src1 {
            Source::IntegerConstant(val) => { 
                self.emit_const_integer_mult(dest, val, &operand.src2); return },
            _ => {},
        }

        match operand.src2 {
            Source::IntegerConstant(val) => { self.emit_const_integer_mult(dest, val, &operand.src1); return; },
            _ => {},
        }

        unimplemented!();
    }

    // emits code for reg <- reg/mem address * 32 bit integer constant
    fn emit_const_integer_mult(&mut self, destination_reg: u32, constant: i32, src_val: &Source) {
        let (const_bytes, opcode) = if constant <= 127 && constant >= -128 {
            (1, SIGNED_MUL_WITH_8_BIT_CONSTANT)
        } else {
            (4, SIGNED_MUL_WITH_32_BIT_CONSTANT)
        };
        self.emit_const_arith_op(opcode, destination_reg, constant, const_bytes, src_val);
    }

    fn emit_const_arith_op(
        &mut self, 
        opcode: u8,
        destination_reg: u32, 
        constant: i32, 
        const_bytes: usize, 
        src_val: &Source) {
        
        let mut WRXB_bits = 0u8;
        let mut src_reg_bits = 0u8;
        let mut dest_reg_bits = 0u8;

        if self.reg_map[&destination_reg].is_extended_reg() {
            WRXB_bits |= REX_EXT_REG_BIT;
        }
        dest_reg_bits = self.reg_map[&destination_reg].encoding() << 3;

        match *src_val {
            Source::Register(reg) => {
                if self.reg_map[&reg].is_extended_reg() {
                    WRXB_bits |= REX_EXT_RM_BIT;
                }
                src_reg_bits = self.reg_map[&reg].encoding();
            },
            _ => unimplemented!(), // operand from memory address needs to be handled
        }

        let prefix = self.create_rex_prefix(REX_64_BIT_OPERAND | WRXB_bits);
        self.asm_code.push(prefix);
     
        self.asm_code.push(opcode);

        self.asm_code.push(MOD_REGISTER_DIRECT_ADDRESSING | src_reg_bits | dest_reg_bits);

        let mut buffer = [0; 4];
        LittleEndian::write_i32(&mut buffer, constant);

        for i in 0..const_bytes {
            self.asm_code.push(buffer[i]);
        }
    }

    fn emit_div(&mut self, operand: &BinaryOperation) {

        let mut wrxb_bits = REX_64_BIT_OPERAND;
        let mut src_reg_bits = 0;

        if let Source::Register(reg) = operand.src2 {
            src_reg_bits = self.reg_map[&reg].encoding();
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
        // currently just save all the registers that are expected
        // to be callee-saved, whether they are used or not
        // save RBP, RBX, and R12–R15,
        self.push(Register::RBP);
        self.push(Register::RBX);
        self.push(Register::R12);
        self.push(Register::R13);
        self.push(Register::R14);
        self.push(Register::R15);
    }

    fn emit_function_epilogue(&mut self) {
        self.pop(Register::R15);
        self.pop(Register::R14);
        self.pop(Register::R13);
        self.pop(Register::R12);
        self.pop(Register::RBX);
        self.pop(Register::RBP);
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

    pub fn get_code(self) -> Vec<u8> {
        self.asm_code
    }

}
