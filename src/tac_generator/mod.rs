use ast::AstNode;
use ast::AstType;
use ast::IdentifierInfo;
use ast::FunctionInfo;

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
    Return(Option<Operand>)
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub statements: Vec<Statement>
}

impl Function {
    fn new(name: String) -> Function {
        Function {
            name: name,
            statements: vec![],
        }
    }
}


pub struct TACGenerator {
    functions: Vec<Function>,
    function_stack: Vec<Function>,
    operands: Vec<Operand>, 
    id_counter: u32,
    symbol_table: SymbolTable,
}

impl TACGenerator {
    pub fn new(id_counter: u32) -> TACGenerator {
        TACGenerator {
            functions: vec![],
            function_stack: vec![],
            operands: vec![],
            id_counter: id_counter,
            symbol_table: SymbolTable::new(),            
        }
    }

    pub fn functions(&self) -> &Vec<Function> {
        &self.functions
    }


    pub fn generate_tac(&mut self, node: &AstNode) {
        match node.node_type {
            AstType::Block(_) => self.handle_block(node),
            AstType::Function(ref info) => self.handle_function(node, info),
            AstType::VariableDeclaration(_) => self.handle_variable_declaration(node),
            AstType::VariableAssignment(_) => self.handle_variable_assignment(node),
            AstType::Plus(_) | AstType::Minus(_) | AstType::Multiply(_) | AstType::Divide(_) => 
                self.handle_arithmetic_node(node),
            AstType::Integer(_) => self.handle_constant(node),
            AstType::Identifier(ref info) => self.handle_identifier(node, info),
            AstType::Return => self.handle_return(node),

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

    fn handle_function(&mut self, node: &AstNode, info: &FunctionInfo) {
        let function = Function::new(info.name.clone());
        self.function_stack.push(function);

        for ref child in node.get_children() {
            self.generate_tac(child);
        }
        self.functions.push(
            self.function_stack.pop().unwrap_or_else(
                || panic!("Internal compiler error: Function stack empty when generating function 3AC")));        
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

        let variable_info = self.get_variable_info(name);

        let operand = self.operands.pop();
        self.current_function().statements.push(Statement::Assignment(None, Some(Operand::Variable(variable_info)), None, operand));
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {
        let children = node.get_children();
        assert!(children.len() == 2);

        let left = self.get_operand(&children[0]);
        let right = self.get_operand(&children[1]);
        println!("    Left: {:?}     Right: {:?}", left, right);
        
        let operator = match node.node_type {
            AstType::Plus(_) => Operator::Plus,
            AstType::Minus(_) => Operator::Minus,
            AstType::Multiply(_) => Operator::Multiply,
            AstType::Divide(_) => Operator::Divide,
            _ => panic!("Internal compiler error: Invalid node {}", node),
        };

        let id = self.get_next_id();

        let variable = Operand::Variable(VariableInfo::new(self.get_type(&left), id));

        self.current_function().statements.push(Statement::Assignment(
            Some(operator), Some(variable.clone()), Some(left), Some(right),
        ));

        self.operands.push(variable) 
    }

    fn handle_identifier(&mut self, node: &AstNode, info: &IdentifierInfo) {
        let name = &info.name;

        let variable_info = self.get_variable_info(name);

        self.operands.push(Operand::Variable(variable_info));
    }

    fn handle_constant(&mut self, node : &AstNode) {
        let operand = match node.node_type {
            AstType::Integer(val) => Operand::Integer(val),
            _ => panic!("Internal compiler error: Unexpected type {:?} when constant was expected during TAC generation", node.node_type),
        };

        self.operands.push(operand);
    }

    fn handle_return(&mut self, node: &AstNode) {
        assert!(node.get_children().len() <= 1);
        
        let operand = if node.get_children().len() == 1 {
            Some(self.get_operand(&node.get_children()[0]))
        } else {
            None
        };

        self.current_function().statements.push(Statement::Return(operand));
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
        self.generate_tac(node);
        self.operands.pop().unwrap_or_else(|| panic!("Internal compiler error: Operand stack empty"))
    }

    fn get_next_id(&mut self) -> u32 {
        let ret = self.id_counter;
        self.id_counter += 1;
        ret
    }

    fn get_variable_info(&self, name: &String) -> VariableInfo {
        let symbol = self.symbol_table.find_symbol(name).unwrap_or_else(
                || panic!("Internal compiler error: No symbol '{}' found during TAC construction", name));

        match symbol.symbol_type {
            SymbolType::Variable(variable_info) => variable_info,
            _ => panic!("Internal compiler error: Expected variable but was {:?}", symbol.symbol_type),
        }
    }

    fn current_function(&mut self) -> &mut Function {
        self.function_stack.last_mut().unwrap_or_else(|| panic!("Internal compiler error: Function stack empty"))
    } 

}