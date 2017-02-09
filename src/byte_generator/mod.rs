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
pub struct ComparisonOperation {
    pub src1: Source,
    pub src2: Source,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonType {
    Less,
    LessOrEq,
    Equals,
    GreaterOrEq,    
    Greater,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Source {
    Register(u32),
    IntegerConstant(i32),
    ComparisonResult(ComparisonType),
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
    Compare(ComparisonOperation),
    Label(u32),
    Jump(u32),
    JumpConditional(u32, ComparisonType),
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
    variable_to_comparison_result: HashMap<u32, ComparisonType>
}

impl ByteGenerator {
    pub fn new(functions: Vec<tac_generator::Function>) -> ByteGenerator {
        ByteGenerator {
            bytecode_functions: vec![],
            tac_functions: functions,
            next_register: 0,
            variable_id_to_register: HashMap::new(),
            variable_to_comparison_result: HashMap::new(),
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

            let cmp = vec![Operator::Less, Operator::LessOrEq, Operator::Equals, Operator::GreaterOrEq, Operator::Greater];

            for s in f.statements {
                match s {
                    Statement::Assignment(Some(Operator::Plus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Plus, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Minus), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Minus, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Multiply), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Multiply, op1, op2, dest),
                    Statement::Assignment(Some(Operator::Divide), Some(ref dest), Some(ref op1), Some(ref op2)) => self.emit_binary_op(Operator::Divide, op1, op2, dest),
                    Statement::Assignment(None, Some(ref dest), None, Some(ref op)) => self.emit_move(op, dest),
                    Statement::Assignment(
                        Some(ref x), 
                        Some(ref dest),
                        Some(ref op1),
                        Some(ref op2)) if cmp.contains(x) => 
                        self.emit_comparison(x, op1, op2, dest),

                   Statement::Return(val) => self.emit_return(val),
                   Statement::Label(id) => self.emit_label(id),
                   Statement::Jump(id) => self.emit_jump(id),
                   Statement::JumpIfTrue(ref operand, id) => self.emit_conditional_jump(operand, id),
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

    fn emit_comparison(&mut self,
        operator: &Operator, 
        op1: &Operand, 
        op2: &Operand,
        dest: &Operand) {

        let comparison_type = match *operator {
            Operator::Less => ComparisonType::Less,
            Operator::LessOrEq => ComparisonType::LessOrEq,
            Operator::Equals => ComparisonType::Equals,
            Operator::GreaterOrEq => ComparisonType::GreaterOrEq,
            Operator::Greater => ComparisonType::Greater,
            _ => ice!("Invalid operator '{}' for comparison", operator),
        };

        let src1 = self.get_source(op1);
        let src2 = self.get_source(op2);

        if let Operand::Variable(_, id) = *dest {
            // mark the variable as comparison result
            // this is needed, so that code gen knows to generate appropriate
            // jmp command or read from cpu status register
            self.variable_to_comparison_result.
                insert(id, comparison_type.clone());
        } else {
            ice!("Non-variable destination for comparison results");
        }

        self.current_function().code.push(
            ByteCode::Compare(
                ComparisonOperation{ 
                    src1: src1,
                    src2: src2,
                }));
    }

    fn emit_label(&mut self, id: u32) {
        self.current_function().code.push(ByteCode::Label(id));
    }

    fn emit_jump(&mut self, id: u32) {        
        self.current_function().code.push(ByteCode::Jump(id));
    }

    fn emit_conditional_jump(&mut self, op: &Operand, id: u32) {
        let src =  self.get_source(op);
        if let Source::ComparisonResult(cmp_type) = src {
            self.current_function().
            code.
            push(
                ByteCode::JumpConditional(id, cmp_type));
        } else {
            ice!("Invalid operand type in conditional jump");
        }
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
            &Operand::Variable(_, id) => {
                if self.variable_to_comparison_result.contains_key(&id) {
                    Source::ComparisonResult(
                        self.variable_to_comparison_result[&id].clone())
                }
                else {
                    self.get_register_for(id)
                }
            },
            &Operand::Integer(i32) => Source::IntegerConstant(i32),
            x => panic!("Not implemented yet for {}", x),
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
            _ => ice!("Invalid operator '{}'", operator),
        }
    }

    fn current_function(&mut self) -> &mut Function {
        self.bytecode_functions.last_mut().unwrap_or_else(|| panic!("Internal compiler error: Empty function array"))
    }
}