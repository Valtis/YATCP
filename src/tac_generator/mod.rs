use ast::AstNode;
use ast::AstType;

pub struct TACGenerator {
    pub statements: Vec<u8>,
    temporary_counter: u32,
}

pub enum TACStatement {
    AssignmentStatement(AssignmentInfo),
}

pub struct AssignmentInfo {
    foo: u8,
}


impl TACGenerator {
    pub fn new() -> TACGenerator {
        TACGenerator {
            statements: vec![],
            temporary_counter: 0,
        }
    }

    pub fn generate_tac(&mut self, node: &AstNode) {
        match node.node_type {
            AstType::Block => self.handle_block(node),
            AstType::Function(_) => self.handle_function(node),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration(node),
            AstType::VariableAssignment(_) => self.handle_variable_assignment(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) => 
                self.handle_arithmetic_node(node),
            _ => panic!("Not implemented: {}", node),
        }
    }

    fn handle_block(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.generate_tac(child);
        }
    }

    fn handle_function(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.generate_tac(child);
        }
    }

    fn handle_variable_declaration(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.generate_tac(child);
        }   
        match node.node_type {
            AstType::VariableDeclaration(ref info) => println!("replace^ {}.{}", info.name, info.id),
            _ => panic!("Invalid type")
        }
    }

    fn handle_variable_assignment(&mut self, node: &AstNode) {
        for ref child in node.get_children() {
            self.generate_tac(child);
        }   
        match node.node_type {
            AstType::VariableAssignment(ref info) => println!("replace^ {}.{}", info.name, info.id),
            _ => panic!("Invalid type")
        }
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {
        let children = node.get_children();
        assert!(children.len() == 2);

        let left = self.get_operand(&children[0]);
        let right = self.get_operand(&children[1]);
        
        let operand = match node.node_type {
            AstType::Plus(_) => "+",
            AstType::Minus(_) => "-",
            AstType::Multiply(_) => "*",
            AstType::Divide(_) => "/",
            _ => panic!("Internal compiler error: Invalid node {}", node),
        };

        let id = self.get_next_id();
        println!("t{} = {} {} {}", id, left, operand, right);
    }


    fn get_operand(&mut self, node: &AstNode) -> String {
        match node.node_type {
            AstType::Integer(value) => value.to_string(), 
            _ => {
                self.generate_tac(node);
                format!("t{}", self.temporary_counter - 1)
            }
        }
    }

    fn get_next_id(&mut self) -> u32 {
        let ret = self.temporary_counter;
        self.temporary_counter += 1;
        ret
    }
}