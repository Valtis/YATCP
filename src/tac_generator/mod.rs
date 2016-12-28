use ast::AstNode;
use ast::AstType;
use semcheck::Type;

use symbol_table::SymbolTable;
use symbol_table::SymbolType;
use symbol_table::VariableInfo;

#[derive(Clone, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Return,
}

#[derive(Clone, Debug)]
pub enum Operand {
    Variable(VariableInfo),
    Integer(i32),
    Float(f32),
    Double(f64),
    Boolean(bool),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(Option<Operator>, Option<Operand>, Option<Operand>, Option<Operand>),
}


pub struct TACGenerator {
    statements: Vec<Statement>,
    operands: Vec<Operand>, 
    id_counter: u32,
    symbol_table: SymbolTable,
}

impl TACGenerator {
    pub fn new(id_counter: u32) -> TACGenerator {
        TACGenerator {
            statements: vec![],
            operands: vec![],
            id_counter: id_counter,
            symbol_table: SymbolTable::new(),            
        }
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }


    pub fn generate_tac(&mut self, node: &AstNode) {
        match node.node_type {
            AstType::Block(_) => self.handle_block(node),
            AstType::Function(_) => self.handle_function(node),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration(node),
            AstType::VariableAssignment(_) => self.handle_variable_assignment(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) => 
                self.handle_arithmetic_node(node),
            AstType::Integer(_) => self.handle_constant(node),
            AstType::Return(_) => self.handle_return(node),
            _ => panic!("Not implemented: {}", node),
        }
    }

    fn handle_block(&mut self, node: &AstNode) {

        if let AstType::Block(ref block) = node.node_type {
            if let Some(ref entry) = *block {
               self.symbol_table.push(entry.clone());
            }
            else {
                panic!("Internal compiler error: No symbol table information attached to AST block node");
            }
        } else {
            panic!("Internal compiler error: Expected block node but was {:?}", node.node_type)
        }

        for ref child in node.get_children() {
            self.generate_tac(child);
        }

        self.symbol_table.pop();
    }

    fn handle_function(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.generate_tac(child);
        }
    }

    fn handle_variable_declaration(&mut self, node: &AstNode) {
        self.declaration_assignment_common(node)
    }

    fn handle_variable_assignment(&mut self, node: &AstNode) {
        self.declaration_assignment_common(node)
    }

    fn declaration_assignment_common(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.generate_tac(child);
        }   

        let name = match node.node_type {
            AstType::VariableDeclaration(ref info) => &info.name,
            AstType::VariableAssignment(ref info) => &info.name,
            _ => panic!("Invalid type")
        };

        let symbol = self.symbol_table.find_symbol(&name).unwrap_or_else(
                || panic!("Internal compiler error: No symbol '{}' found during TAC construction", name));

        let variable_info = match symbol.symbol_type {
            SymbolType::Variable(variable_info) => variable_info,
            _ => panic!("Internal compiler error: Expected variable but was {:?}", symbol.symbol_type),
        };

        self.statements.push(Statement::Assignment(None, Some(Operand::Variable(variable_info)), None, self.operands.pop()));
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {
        let children = node.get_children();
        assert!(children.len() == 2);

        let left = self.get_operand(&children[0]);
        let right = self.get_operand(&children[1]);
        
        let operator = match node.node_type {
            AstType::Plus(_) => Operator::Plus,
            AstType::Minus(_) => Operator::Minus,
            AstType::Multiply(_) => Operator::Multiply,
            AstType::Divide(_) => Operator::Divide,
            _ => panic!("Internal compiler error: Invalid node {}", node),
        };

        let id = self.get_next_id();

        let variable = Operand::Variable(VariableInfo::new(self.get_type(&left), id));

        self.statements.push(Statement::Assignment(
            Some(operator), Some(variable.clone()), Some(left), Some(right),
        ));

        self.operands.push(variable) 
    }


    fn handle_constant(&mut self, node : &AstNode) {
        let operand = self.get_operand(node);
        self.operands.push(operand);
    }

    fn handle_return(&mut self, node: &AstNode) {
        assert!(node.children.len() == 1);
        let operand = self.get_operand(node);
    }


    fn get_type(&self, operand: &Operand) -> Type {
        match *operand {
            Operand::Variable(ref info) => info.variable_type,
            Operand::Integer(_) => Type::Integer,
            Operand::Float(_) => Type::Float,
            Operand::Double(_) => Type::Double,
            Operand::Boolean(_) => Type::Boolean,
        }
    }

    fn get_operand(&mut self, node: &AstNode) -> Operand {
       /* match node.node_type {
            AstType::Integer(value) => Operand::Integer(value),

            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) => { self.generate_tac(node);  }
            _ => { println!("{:?}", node); unimplemented!() }
        }*/

        self.generate_tac(node);
        return self.operands.pop().unwrap_or_else(|| panic!("Internal compiler error: Operand stack empty"));
    }
    fn get_next_id(&mut self) -> u32 {
        let ret = self.id_counter;
        self.id_counter += 1;
        ret
    }
}