#![allow(dead_code)]

use std::collections::HashMap;


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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Opcode {
    Add,
    Sub,
    And,
    Or,
    Xor,
    Cmp,
    Shl,
    Sar,
    Shr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum Mode {
    FromRegToReg, // reg = reg OP reg
    FromRegToMemory, // R/M = R/M OP reg
    FromMemoryToReg, // reg  = reg OP R/M
    ImmediateWithMemory, // mem = mem OP immediate
    ImmediateWithRegister, // mem = mem OP immediate
}

pub struct OpcodeRegistry {
    opcodes: HashMap<Opcode, HashMap<Mode, HashMap<u32, Vec<SizedOpCode> >>>,
    opcode_extensions: HashMap<Opcode, u8>,
}

impl OpcodeRegistry {


    pub fn get_opcode(&self, opcode: Opcode, mode: Mode, size: u32) -> SizedOpCode {
        let class = self.opcodes.get(&opcode).unwrap_or_else(|| ice!("Unknown opcode {:?}", opcode));
        let operands = class.get(&mode).unwrap_or_else(|| ice!("Unknown mode {:?} for opcode {:?}", mode, opcode));
        operands.get(&size).unwrap_or_else(|| ice!("No opcode of size {:?} for opcode {:?} with mode {:?}", size, opcode, mode))[0]
    }

    pub fn get_opcode_with_alternatives(&self, opcode: Opcode, mode: Mode, size: u32) -> &Vec<SizedOpCode> {
        let class = self.opcodes.get(&opcode).unwrap_or_else(|| ice!("Unknown opcode {:?}", opcode));
        let operands = class.get(&mode).unwrap_or_else(|| ice!("Unknown mode {:?} for opcode {:?}", mode, opcode));
        operands.get(&size).unwrap_or_else(|| ice!("No opcode of size {:?} for opcode {:?} with mode {:?}", size, opcode, mode))
    }

    pub fn get_opcode_extension(&self, opcode: Opcode) -> u8 {
        *self.opcode_extensions.get(&opcode).unwrap_or_else(|| ice!("Unknown opcode extension {:?}", opcode))
    }

    pub fn new() -> OpcodeRegistry {
        let mut reg = OpcodeRegistry {
            opcodes: HashMap::new(),
            opcode_extensions: HashMap::new(),
        };

        reg.populate_add_opcodes();
        reg.populate_sub_opcodes();
        reg.populate_and_opcodes();
        reg.populate_or_opcodes();
        reg.populate_xor_opcodes();
        reg.populate_cmp_opcodes();
        reg.populate_shl_opcodes();
        reg.populate_sar_opcodes();
        reg.populate_shr_opcodes();

        reg
    }


    fn populate_add_opcodes(&mut self) {

        self.opcode_extensions.insert(Opcode::Add, ADD_OPCODE_EXT);

        self.opcodes.insert(Opcode::Add, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Add).unwrap();


        modes.insert(Mode::FromRegToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToReg).unwrap();
        op_sizes.insert(1, vec![ADD_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![ADD_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![ADD_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![ADD_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![ADD_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![ADD_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![ADD_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![ADD_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromMemoryToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromMemoryToReg).unwrap();
        op_sizes.insert(1, vec![ADD_RM_TO_REG_8_BIT.into()]);
        op_sizes.insert(2, vec![ADD_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(4, vec![ADD_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(8, vec![ADD_RM_TO_REG_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![ADD_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![ADD_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
    }

    fn populate_sub_opcodes(&mut self) {

        self.opcode_extensions.insert(Opcode::Sub, SUB_OPCODE_EXT);

        self.opcodes.insert(Opcode::Sub, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Sub).unwrap();


        modes.insert(Mode::FromRegToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToReg).unwrap();
        op_sizes.insert(1, vec![SUB_REG_FROM_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SUB_REG_FROM_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SUB_REG_FROM_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![SUB_REG_FROM_RM_32_BIT.into()]);


        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![SUB_REG_FROM_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SUB_REG_FROM_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SUB_REG_FROM_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![SUB_REG_FROM_RM_32_BIT.into()]);


        modes.insert(Mode::FromMemoryToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromMemoryToReg).unwrap();
        op_sizes.insert(1, vec![SUB_RM_FROM_REG_8_BIT.into()]);
        op_sizes.insert(2, vec![SUB_RM_FROM_REG_32_BIT.into()]);
        op_sizes.insert(4, vec![SUB_RM_FROM_REG_32_BIT.into()]);
        op_sizes.insert(8, vec![SUB_RM_FROM_REG_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![SUB_IMMEDIATE_8_BIT_FROM_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT.into(), SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT.into(), SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT.into(), SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![SUB_IMMEDIATE_8_BIT_FROM_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT.into(), SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT.into(), SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT.into(), SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT.into()]);
    }


    fn populate_and_opcodes(&mut self) {

        self.opcode_extensions.insert(Opcode::And, AND_OPCODE_EXT);

        self.opcodes.insert(Opcode::And, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::And).unwrap();


        modes.insert(Mode::FromRegToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToReg).unwrap();
        op_sizes.insert(1, vec![AND_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![AND_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![AND_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![AND_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![AND_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![AND_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![AND_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![AND_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromMemoryToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromMemoryToReg).unwrap();
        op_sizes.insert(1, vec![AND_RM_TO_REG_8_BIT.into()]);
        op_sizes.insert(2, vec![AND_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(4, vec![AND_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(8, vec![AND_RM_TO_REG_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![AND_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![AND_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), AND_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![AND_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), AND_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![AND_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), AND_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![AND_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![AND_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), AND_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![AND_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), AND_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![AND_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), AND_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
    }

    fn populate_or_opcodes(&mut self) {

        self.opcode_extensions.insert(Opcode::Or, OR_OPCODE_EXT);

        self.opcodes.insert(Opcode::Or, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Or).unwrap();


        modes.insert(Mode::FromRegToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToReg).unwrap();
        op_sizes.insert(1, vec![OR_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![OR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![OR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![OR_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![OR_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![OR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![OR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![OR_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromMemoryToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromMemoryToReg).unwrap();
        op_sizes.insert(1, vec![OR_RM_TO_REG_8_BIT.into()]);
        op_sizes.insert(2, vec![OR_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(4, vec![OR_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(8, vec![OR_RM_TO_REG_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![OR_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![OR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), OR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![OR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), OR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![OR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), OR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![OR_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![OR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), OR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![OR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), OR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![OR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), OR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
    }

    fn populate_xor_opcodes(&mut self) {

        self.opcode_extensions.insert(Opcode::Xor, XOR_OPCODE_EXT);

        self.opcodes.insert(Opcode::Xor, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Xor).unwrap();


        modes.insert(Mode::FromRegToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToReg).unwrap();
        op_sizes.insert(1, vec![XOR_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![XOR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![XOR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![XOR_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![XOR_REG_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![XOR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![XOR_REG_TO_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![XOR_REG_TO_RM_32_BIT.into()]);


        modes.insert(Mode::FromMemoryToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromMemoryToReg).unwrap();
        op_sizes.insert(1, vec![XOR_RM_TO_REG_8_BIT.into()]);
        op_sizes.insert(2, vec![XOR_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(4, vec![XOR_RM_TO_REG_32_BIT.into()]);
        op_sizes.insert(8, vec![XOR_RM_TO_REG_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![XOR_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![XOR_IMMEDIATE_8_BIT_TO_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT.into(), XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT.into()]);
    }

    fn populate_cmp_opcodes(&mut self) {

        self.opcode_extensions.insert(Opcode::Cmp, CMP_OPCODE_EXT);

        self.opcodes.insert(Opcode::Cmp, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Cmp).unwrap();


        modes.insert(Mode::FromRegToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToReg).unwrap();
        op_sizes.insert(1, vec![CMP_RM_8_BIT_WITH_REG.into()]);
        op_sizes.insert(2, vec![CMP_RM_32_BIT_WITH_REG.into()]);
        op_sizes.insert(4, vec![CMP_RM_32_BIT_WITH_REG.into()]);
        op_sizes.insert(8, vec![CMP_RM_32_BIT_WITH_REG.into()]);

        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![CMP_RM_8_BIT_WITH_REG.into()]);
        op_sizes.insert(2, vec![CMP_RM_32_BIT_WITH_REG.into()]);
        op_sizes.insert(4, vec![CMP_RM_32_BIT_WITH_REG.into()]);
        op_sizes.insert(8, vec![CMP_RM_32_BIT_WITH_REG.into()]);

        modes.insert(Mode::FromMemoryToReg, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromMemoryToReg).unwrap();
        op_sizes.insert(1, vec![CMP_REG_WITH_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![CMP_REG_WITH_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![CMP_REG_WITH_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![CMP_REG_WITH_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![CMP_IMMEDIATE_8_BIT_WITH_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT.into(), CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT.into(), CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT.into(), CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![CMP_IMMEDIATE_8_BIT_WITH_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT.into(), CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT.into(), CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT.into()]);
        // 8 byte immediate not supported, will be 4 byte
        op_sizes.insert(8, vec![CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT.into(), CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT.into()]);
    }

    fn populate_shl_opcodes(&mut self) {
        self.opcode_extensions.insert(Opcode::Shl, SHL_OPCODE_EXT);

        self.opcodes.insert(Opcode::Shl, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Shl).unwrap();

        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![SHL_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SHL_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SHL_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![SHL_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![SHL_RM_8_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_8_BIT_ONCE.into()]);
        op_sizes.insert(2, vec![SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(4, vec![SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(8, vec![SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_32_BIT_ONCE.into()]);

        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![SHL_RM_8_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_8_BIT_ONCE.into()]);
        op_sizes.insert(2, vec![SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(4, vec![SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(8, vec![SHL_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHL_RM_32_BIT_ONCE.into()]);
    }


    fn populate_sar_opcodes(&mut self) {
        self.opcode_extensions.insert(Opcode::Sar, SAR_OPCODE_EXT);

        self.opcodes.insert(Opcode::Sar, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Sar).unwrap();

        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![SAR_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SAR_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SAR_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![SAR_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![SAR_RM_8_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_8_BIT_ONCE.into()]);
        op_sizes.insert(2, vec![SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(4, vec![SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(8, vec![SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_32_BIT_ONCE.into()]);

        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![SAR_RM_8_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_8_BIT_ONCE.into()]);
        op_sizes.insert(2, vec![SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(4, vec![SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(8, vec![SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SAR_RM_32_BIT_ONCE.into()]);
    }

    fn populate_shr_opcodes(&mut self) {
        self.opcode_extensions.insert(Opcode::Shr, SHR_OPCODE_EXT);

        self.opcodes.insert(Opcode::Shr, HashMap::new());
        let modes = self.opcodes.get_mut(&Opcode::Shr).unwrap();

        modes.insert(Mode::FromRegToMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::FromRegToMemory).unwrap();
        op_sizes.insert(1, vec![SHR_RM_8_BIT.into()]);
        op_sizes.insert(2, vec![SHR_RM_32_BIT.into()]);
        op_sizes.insert(4, vec![SHR_RM_32_BIT.into()]);
        op_sizes.insert(8, vec![SHR_RM_32_BIT.into()]);


        modes.insert(Mode::ImmediateWithMemory, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithMemory).unwrap();
        op_sizes.insert(1, vec![SHR_RM_8_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_8_BIT_ONCE.into()]);
        op_sizes.insert(2, vec![SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(4, vec![SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(8, vec![SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_32_BIT_ONCE.into()]);

        modes.insert(Mode::ImmediateWithRegister, HashMap::new());
        let op_sizes= modes.get_mut(&Mode::ImmediateWithRegister).unwrap();
        op_sizes.insert(1, vec![SHR_RM_8_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_8_BIT_ONCE.into()]);
        op_sizes.insert(2, vec![SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(4, vec![SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_32_BIT_ONCE.into()]);
        op_sizes.insert(8, vec![SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE.into(), SHR_RM_32_BIT_ONCE.into()]);
    }
}

pub const OPERAND_SIZE_OVERRIDE: u8 = 0x66;

pub const MOV_IMMEDIATE_32_BIT_TO_REG_BASE: u8 = 0xB8; // register encoding will be binary OR'ed into opcode
pub const MOV_IMMEDIATE_32_BIT_TO_RM: u8 = 0xC7;
pub const MOV_REG_TO_RM_32_BIT: u8 = 0x89;
pub const MOV_RM_TO_REG_32_BIT: u8 = 0x8B;

pub const MOV_IMMEDIATE_8_BIT_TO_REG_BASE: u8 = 0xB0; // register encoding will be binary OR'ed into opcode
pub const MOV_IMMEDIATE_8_BIT_TO_RM: u8 = 0xC6;
pub const MOV_REG_TO_RM_8_BIT: u8 = 0x88;
pub const MOV_RM_TO_REG_8_BIT: u8 = 0x8A;

pub const MOV_SIGN_EXTEND_RM_8_BIT_TO_REG_32_BIT: u16 = 0x0FBE;
pub const MOV_SIGN_EXTEND_RM_16_BIT_TO_REG_32_BIT: u16 = 0x0FBF;
pub const MOV_SIGN_EXTEND_RM_32_BIT_TO_REG_32_BIT: u8 = 0x63; // with REX, turns to 32bit -> 64bit

pub const MOV_ZERO_EXTEND_RM_8_BIT_TO_REG_32_BIT: u16 = 0x0FB6;
pub const MOV_ZERO_EXTEND_RM_16_BIT_TO_REG_32_BIT: u16 = 0x0FB7;

pub const LEA_ADDR_TO_REG_64_BIT: u8 = 0x8D;

pub const ADD_IMMEDIATE_32_BIT_TO_RM_32_BIT: u8 = 0x81;
pub const ADD_REG_TO_RM_32_BIT: u8 = 0x01;
pub const ADD_RM_TO_REG_32_BIT: u8 = 0x03;

pub const ADD_IMMEDIATE_8_BIT_TO_RM_32_BIT: u8 = 0x83;
pub const ADD_REG_TO_RM_8_BIT: u8 = 0x00;
pub const ADD_RM_TO_REG_8_BIT: u8 = 0x02;
pub const ADD_IMMEDIATE_8_BIT_TO_RM_8_BIT: u8 = 0x80;

pub const ADD_OPCODE_EXT: u8 = 0;


pub const SUB_IMMEDIATE_32_BIT_FROM_RM_32_BIT: u8 = 0x81;
pub const SUB_IMMEDIATE_8_BIT_FROM_RM_32_BIT: u8 = 0x83;
pub const SUB_RM_FROM_REG_32_BIT: u8 = 0x2B;
pub const SUB_REG_FROM_RM_32_BIT: u8 = 0x29;

pub const SUB_IMMEDIATE_8_BIT_FROM_RM_8_BIT: u8 = 0x80;
pub const SUB_RM_FROM_REG_8_BIT: u8 = 0x2A;
pub const SUB_REG_FROM_RM_8_BIT: u8 = 0x28;

pub const SUB_OPCODE_EXT: u8 = 0x05;

pub const SIGNED_MUL_RM_32_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0x6B;
pub const SIGNED_MUL_RM_32_BIT_WITH_32_BIT_IMMEDIATE: u8 = 0x69;
pub const SIGNED_MUL_RM_WITH_REG_32_BIT: u16 = 0x0FAF;

pub const SIGNED_DIV_RM_8_BIT: u8 = 0xF6;
pub const SIGNED_DIV_RM_32_BIT: u8 = 0xF7;
pub const DIV_OPCODE_EXT: u8 = 0x07;

pub const AND_RM_TO_REG_8_BIT: u8 = 0x22;
pub const AND_RM_TO_REG_32_BIT: u8 = 0x23;
pub const AND_REG_TO_RM_32_BIT: u8 = 0x21;
pub const AND_REG_TO_RM_8_BIT: u8 = 0x20;
pub const AND_IMMEDIATE_8_BIT_TO_RM_8_BIT: u8 = 0x80;
pub const AND_IMMEDIATE_8_BIT_TO_RM_32_BIT: u8 = 0x83;
pub const AND_IMMEDIATE_32_BIT_TO_RM_32_BIT: u8 = 0x81;

pub const AND_OPCODE_EXT: u8 = 0x04;

pub const OR_RM_TO_REG_8_BIT: u8 = 0x0A;
pub const OR_RM_TO_REG_32_BIT: u8 = 0x0B;
pub const OR_REG_TO_RM_8_BIT: u8 = 0x08;
pub const OR_REG_TO_RM_32_BIT: u8 = 0x09;
pub const OR_IMMEDIATE_8_BIT_TO_RM_8_BIT: u8 = 0x80;
pub const OR_IMMEDIATE_8_BIT_TO_RM_32_BIT: u8 = 0x83;
pub const OR_IMMEDIATE_32_BIT_TO_RM_32_BIT: u8 = 0x81;

pub const OR_OPCODE_EXT: u8 = 0x01;


pub const XOR_RM_TO_REG_8_BIT: u8 = 0x32;
pub const XOR_RM_TO_REG_32_BIT: u8 = 0x33;
pub const XOR_REG_TO_RM_8_BIT: u8 = 0x30;
pub const XOR_REG_TO_RM_32_BIT: u8 = 0x31;
pub const XOR_IMMEDIATE_8_BIT_TO_RM_8_BIT: u8 = 0x80;
pub const XOR_IMMEDIATE_8_BIT_TO_RM_32_BIT: u8 = 0x83;
pub const XOR_IMMEDIATE_32_BIT_TO_RM_32_BIT: u8 = 0x81;
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

pub const SHL_OPCODE_EXT: u8 = 0x04;

pub const SAR_RM_32_BIT_ONCE: u8 = 0xD1;
pub const SAR_RM_32_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC1;
pub const SAR_RM_32_BIT: u8 = 0xD3;

pub const SAR_RM_8_BIT_ONCE: u8 = 0xD0;
pub const SAR_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC0;
pub const SAR_RM_8_BIT: u8 = 0xD2;

pub const SAR_OPCODE_EXT: u8 = 0x07;

pub const SHR_RM_32_BIT_ONCE: u8 = 0xD1;
pub const SHR_RM_32_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC1;
pub const SHR_RM_32_BIT: u8 = 0xD3;

pub const SHR_RM_8_BIT_ONCE: u8 = 0xD0;
pub const SHR_RM_8_BIT_WITH_8_BIT_IMMEDIATE: u8 = 0xC0;
pub const SHR_RM_8_BIT: u8 = 0xD2;

pub const SHR_OPCODE_EXT: u8 = 0x05;


pub const NEGATE_RM_8_BIT: u8 = 0xF6;
pub const NEGATE_RM_32_BIT: u8 = 0xF7;

pub const NEGATE_OPCODE_EXT: u8 = 0x03;

pub const CMP_IMMEDIATE_8_BIT_WITH_RM_32_BIT: u8 = 0x83;
pub const CMP_IMMEDIATE_32_BIT_WITH_RM_32_BIT: u8 = 0x81;
pub const CMP_RM_32_BIT_WITH_REG: u8 = 0x39;
pub const CMP_REG_WITH_RM_32_BIT: u8 = 0x3B;

pub const CMP_IMMEDIATE_8_BIT_WITH_RM_8_BIT: u8 = 0x80;
pub const CMP_RM_8_BIT_WITH_REG: u8 = 0x38;
pub const CMP_REG_WITH_RM_8_BIT: u8 = 0x3A;

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



