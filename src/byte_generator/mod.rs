use tac_generator::Statement;
use tac_generator::Operator;
use tac_generator::Operand;

use std::collections::HashMap;

#[derive(Debug)]
pub struct BinaryOperation {
    src1: Source,
    src2: Source,
    dest: Source,
}

#[derive(Debug)]
pub enum Source {
    Register(u32),
    IntegerConstant(i32),
}

#[derive(Debug)]
pub enum ByteCode {
    Nop,
    Add(BinaryOperation),
    Sub(BinaryOperation),
    Mul(BinaryOperation),
    Div(BinaryOperation),
}

pub struct ByteGenerator {
    pub code: Vec<ByteCode>,
    pub statements: Vec<Statement>,
    next_register: u32,
    variable_id_to_register: HashMap<u32, u32>,
}

impl ByteGenerator {
    pub fn new(statements: Vec<Statement>) -> ByteGenerator {
        ByteGenerator {
            code: vec![],
            statements: statements,
            next_register: 0,
            variable_id_to_register: HashMap::new(),
        }
    }


    pub fn generate_bytecode(&mut self) {
        // clone to fix borrow issue. TODO: Figure out how to avoid an unnecessary copy.
        let statements = self.statements.clone(); 
        for s in statements {
            match s {

               Statement::Assignment(Some(Operator::Plus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Plus, op1, op2, dest),
               Statement::Assignment(Some(Operator::Minus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Minus, op1, op2, dest),
               Statement::Assignment(Some(Operator::Multiply), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Multiply, op1, op2, dest),
               Statement::Assignment(Some(Operator::Divide), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Divide, op1, op2, dest),
               _ => println!("Something else"),
            }
        }
    }

    fn emit_binary_op(&mut self, operator: Operator, op1: &Operand, op2: &Operand, dest: &Operand) {
        let data = self.form_binary_operation(op1, op2, dest);
        let code = self.form_binary_code(operator, data);
        self.code.push(code);
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
            Operator::Minus => ByteCode::Sub(data),
            Operator::Multiply => ByteCode::Mul(data),
            _=> unimplemented!(),
        }
    }
}