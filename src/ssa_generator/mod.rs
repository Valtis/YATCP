use ast::AstNode;
use ast::AstType;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::collections::HashMap;

pub enum SSAStatement {
    Register(RegisterInfo),
    Conditional(ConditionInfo),
}

pub struct RegisterInfo {    
    reg_id: u32,
    left_operand: Option<SSAOperand>,
    operation: Option<SSAOperation>,
    right_operand: Option<SSAOperand>,
}

pub struct ConditionInfo {
    left_operand: SSAOperand,
    right_operand: SSAOperand,
}

enum SSAOperand {
    Integer(i32),
    Register(u32),
}

enum SSAOperation {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl Display for SSAStatement {
    fn fmt(&self, formatter: &mut Formatter) -> Result {

        match *self {
            SSAStatement::Register(ref i) => {
                write!(formatter, "r{} = {} {} {} ", i.reg_id, 
                    match i.left_operand {
                        Some(ref o) => format!("{}", o),
                        None => "".to_string(),
                    },
                    match i.operation {
                        Some(ref o) => format!("{}", o),
                        None => "".to_string(),
                    },
                    match i.right_operand {
                        Some(ref o) => format!("{}", o),
                        None => "".to_string(),
                    },
                )
            }
            SSAStatement::Conditional(ref i) => 
                write!(formatter, "check_equality {} {}", i.left_operand, i.right_operand),    
        }
        
    }
}



impl Display for SSAOperand {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            SSAOperand::Integer(val) => format!("{}", val),
            SSAOperand::Register(val) => format!("r{}", val),
        })
    }
}


impl Display for SSAOperation {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            SSAOperation::Plus => "+",
            SSAOperation::Minus => "-",
            SSAOperation::Multiply => "*",
            SSAOperation::Divide => "/",
        })
    }
}

fn new_register_statement(reg_id: u32, 
                          left_operand: Option<SSAOperand>,
                          operation: Option<SSAOperation>,
                          right_operand: Option<SSAOperand>) -> SSAStatement {

    let info = RegisterInfo {
        reg_id: reg_id,
        left_operand: left_operand,
        operation: operation,
        right_operand: right_operand,
    };
    SSAStatement::Register(info)
}

fn new_conditional_statement(left_operand: SSAOperand, 
                             right_operand: SSAOperand) -> SSAStatement {

    let info = ConditionInfo {
        left_operand: left_operand,
        right_operand: right_operand,
    };

    SSAStatement::Conditional(info)
}


pub struct SSAGenerator {
    pub statements: Vec<SSAStatement>, 
    next_reg_id: u32,
    variable_ids_to_registers: HashMap<u32, u32>,
}


impl SSAGenerator {
    pub fn new() -> SSAGenerator {
        SSAGenerator {
            statements: vec![],
            next_reg_id: 0,
            variable_ids_to_registers: HashMap::new(),
        }
    }    

    pub fn generate_ssa(&mut self, node: &AstNode) {
       self.do_generate(node);
    }

    fn do_generate(&mut self, node: &AstNode) {
         match node.node_type {
            AstType::Block => self.handle_block_node(node),
            AstType::Function(_) => self.handle_function_node(node),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration_node(node),
            AstType::VariableAssignment(_) => self.handle_variable_assignment_node(node),
            AstType::Return => self.handle_return_statement_node(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) =>
                self.handle_arithmetic_node(node),
            AstType::Integer(val) => self.handle_integer_node(val),
            AstType::Identifier(_) => { /* Do nothing */ },
            AstType::If => self.handle_if_node(node),
            AstType::Equals => self.handle_equals_node(node),
            _ => panic!("Not implemented: {}", node),
        }
    }

    fn handle_block_node(&mut self, node: &AstNode) {       
        for ref child in node.get_children() {
            self.do_generate(child);
        } 
    }

    fn handle_function_node(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.do_generate(child);
        } 
    }

    fn handle_variable_declaration_node(&mut self, node: &AstNode) {
        let children = node.get_children();
        assert!(children.len() == 1);
        self.do_generate(&children[0]);
        let reg_id = self.get_last_reg_id();
        match node.node_type {
            AstType::VariableDeclaration(ref i) => 
                { self.variable_ids_to_registers.insert(i.id, reg_id); },
            _ => panic!(
                    "Internal compiler error: Invalid ast node type for variable declaration: {}",
                    node),
        }
    }

    fn handle_variable_assignment_node(&mut self, node: &AstNode) {
       let children = node.get_children();

        assert!(children.len() == 1);

        self.do_generate(&children[0]);
        
        let reg_id = self.get_last_reg_id();
        match node.node_type {
            AstType::VariableAssignment(ref i) => 
                { self.variable_ids_to_registers.insert(i.id, reg_id); },
            _ => panic!(
                    "Internal compiler error: Invalid ast node type for variable assignment: {}",
                    node),
        }
    }

    fn handle_return_statement_node(&mut self, node: &AstNode) {
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {

        let operation = match node.node_type {
            AstType::Plus(_) => SSAOperation::Plus,
            AstType::Minus(_) => SSAOperation::Minus,
            AstType::Multiply(_) => SSAOperation::Multiply,
            AstType::Divide(_) => SSAOperation::Divide,
            _ => panic!(
                    "Internal compiler error: Inappropriate node type for aritmetic node: {}", 
                     node),
        };
        let children = node.get_children();
        let left_operand = if children.len() > 0 {
            Some(self.get_operand(&children[0]))
        } else {
            None
        };

        let right_operand = if children.len() > 1 {
            Some(self.get_operand(&children[1]))
        } else {
            None
        };

        
        let id = self.get_next_id();
        self.statements.push(new_register_statement(id, left_operand, Some(operation), right_operand));
    }

    fn handle_integer_node(&mut self, val: i32) {
        let id = self.get_next_id();
        self.statements.push(new_register_statement(id, Some(SSAOperand::Integer(val)), None, None));
    }

    fn handle_if_node(&mut self, node: &AstNode) {
        let children = node.get_children();
        
        // condition
        self.do_generate(&children[0]);
    }

    fn handle_equals_node(&mut self, node: &AstNode) {
        let children = node.get_children();
        let first = self.get_operand(&children[0]);
        let second = self.get_operand(&children[1]);
        self.statements.push(new_conditional_statement(first, second))
    }

    fn get_operand(&mut self, node: &AstNode) -> SSAOperand {
        match node.node_type {
            AstType::Integer(val) => SSAOperand::Integer(val),

            // generate statement, then get the virtual register of this statement
            AstType::Plus(_) | AstType::Minus(_) |
            AstType::Multiply(_) | AstType::Divide(_) => { 
                self.do_generate(node);
                let reg_id = self.get_last_reg_id();
                
                SSAOperand::Register(reg_id)
            },
            AstType::Identifier(ref i) => {
                let reg_id = self.variable_ids_to_registers[&i.id];
                SSAOperand::Register(reg_id)
            },
            _ => panic!("Not implemented: {}", node),
        }       
    }

    fn get_last_reg_id(&self) -> u32 {
        let size = self.statements.len();
        match self.statements[size - 1] {
            SSAStatement::Register(ref i) => i.reg_id,
            _ => panic!("Internal compiler error: Invalid SSAStatement {}"),
        }
    }
    fn get_next_id(&mut self) -> u32 {
        let id = self.next_reg_id;
        self.next_reg_id += 1;
        id
    }
}