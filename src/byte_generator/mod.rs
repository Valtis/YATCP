use tac_generator::Statement;
use tac_generator::Operator;
use tac_generator::Operand;
use tac_generator;

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub src: Source,
    pub dest: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub src1: Source,
    pub src2: Source,
    pub dest: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Source {
    Register(u32),
    IntegerConstant(i32),
    ReturnRegister, // special register, signifies the register that the calling convention uses to return values
}

#[derive(Debug, Clone)]
pub enum ByteCode {
    Nop,
    Add(BinaryOperation),
    Sub(BinaryOperation),
    Mul(BinaryOperation),
    Div(BinaryOperation),
    Mov(UnaryOperation),
    SignExtend(UnaryOperation),
    Ret,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub code: Vec<ByteCode> 
}

pub struct ByteGenerator {
    pub bytecode_functions: Vec<Function>,
    tac_functions: Vec<tac_generator::Function>,
    next_register: u32,
    variable_id_to_register: HashMap<u32, u32>,
}

impl ByteGenerator {
    pub fn new(functions: Vec<tac_generator::Function>) -> ByteGenerator {
        ByteGenerator {
            bytecode_functions: vec![],
            tac_functions: functions,
            next_register: 0,
            variable_id_to_register: HashMap::new(),
        }
    }

    pub fn generate_bytecode(&mut self) {
        // clone to fix borrow issue. TODO: Figure out how to avoid an unnecessary copy.
        let functions = self.tac_functions.clone();
        for f in functions {
            self.bytecode_functions.push(Function {
                name: f.name,
                code: vec![],
            });
            self.next_register = 0;
            for s in f.statements {
                match s {
                   Statement::Assignment(Some(Operator::Plus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Plus, op1, op2, dest),
                   Statement::Assignment(Some(Operator::Minus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Minus, op1, op2, dest),
                   Statement::Assignment(Some(Operator::Multiply), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Multiply, op1, op2, dest),
                   Statement::Assignment(Some(Operator::Divide), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Divide, op1, op2, dest),
                   Statement::Assignment(None, Some(ref dest), None, Some(ref op)) => self.emit_move(op, dest),
                   Statement::Return(val) => self.emit_return(val),
                   _ => panic!("Not implemented: {:?}", s),
                }
            }
        }
    }

    fn emit_return(&mut self, retval: Option<Operand>) {
        if let Some(op) = retval {
            let data = UnaryOperation { src: self.get_source(&op), dest: Source::ReturnRegister };
            self.current_function().code.push(ByteCode::Mov(data));
        }

        self.current_function().code.push(ByteCode::Ret); 
    }

    fn emit_move(&mut self, op: &Operand, dest: &Operand) {    
        let data = self.form_unary_operation(op, dest);
        self.current_function().code.push(ByteCode::Mov(data));
    }

    fn emit_binary_op(&mut self, operator: Operator, op1: &Operand, op2: &Operand, dest: &Operand) {
        let data = self.form_binary_operation(op1, op2, dest);
        let code = self.form_binary_code(operator, data);
        self.current_function().code.push(code);
    }

    fn form_unary_operation(&mut self, op: &Operand, dest: &Operand) -> UnaryOperation {
        let src = self.get_source(op);
        let dest = self.get_source(dest);
        UnaryOperation { src: src, dest: dest }
    }

    fn form_binary_operation(&mut self, op1: &Operand, op2: &Operand, dest: &Operand) -> BinaryOperation {
        let src1 = self.get_source(op1);
        let src2 = self.get_source(op2);
        let op_dest = self.get_source(dest);

        BinaryOperation { src1: src1, src2: src2, dest: op_dest }
    }

    fn get_source(&mut self, op: &Operand) -> Source {
        match op {
            &Operand::Variable(ref info) => self.get_register_for(info.id),
            &Operand::Integer(i32) => Source::IntegerConstant(i32),
            _ => unimplemented!(),
        }
    }

    fn get_register_for(&mut self, variable: u32) -> Source {
        if self.variable_id_to_register.contains_key(&variable) {
            let reg  = self.variable_id_to_register[&variable];
            Source::Register(reg)
        } else {
            let reg = self.next_register;
            self.next_register += 1;
            self.variable_id_to_register.insert(variable, reg);
            Source::Register(reg)
        }
    }

    fn form_binary_code(&mut self, operator: Operator, data: BinaryOperation) -> ByteCode {
        match operator {
            Operator::Plus => ByteCode::Add(data),
            Operator::Minus => ByteCode::Sub(data),
            Operator::Multiply => ByteCode::Mul(data),
            Operator::Divide => ByteCode::Div(data),
        }
    }

    fn current_function(&mut self) -> &mut Function {
        self.bytecode_functions.last_mut().unwrap_or_else(|| panic!("Internal compiler error: Empty function array"))
    }
}