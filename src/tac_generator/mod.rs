use ast::AstNode;
use ast::FunctionInfo;
use ast::DeclarationInfo;
use ast::NodeInfo;

use semcheck::Type;

use symbol_table::TableEntry;
use symbol_table::SymbolTable;
use symbol_table::Symbol;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

#[derive(Clone, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Less,
}

const tmp_name : &'static str = "tmp";

impl Display for Operator {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operator::Plus => "+".to_string(),
            Operator::Minus => "-".to_string(),
            Operator::Multiply => "*".to_string(),
            Operator::Divide => "/".to_string(),
            Operator::Less => "<".to_string(),
        })
    }
}

#[derive(Clone, Debug)]
pub enum Operand {
    Variable(DeclarationInfo, u32),
    Integer(i32),
    Float(f32),
    Double(f64),
    Boolean(bool),
}

impl Display for Operand {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operand::Variable(ref info, id) => format!("{}_{}", info.name, id),
            Operand::Integer(v) => format!("{}i", v),
            Operand::Float(v) => format!("{}f", v),
            Operand::Double(v) => format!("{}d", v),
            Operand::Boolean(v) => v.to_string(),
        })
    }
}


#[derive(Clone, Debug)]
pub enum Statement {
    Assignment(Option<Operator>, Option<Operand>, Option<Operand>, Option<Operand>),
    Label(u32),
    Jump(u32),
    JumpIfTrue(Operand, u32),
    Return(Option<Operand>)
}

fn opt_to_str<T: Display>(op: &Option<T>) -> String {
    match *op {
        Some(ref val) => format!("{}", val),
        None => "None".to_string(),
    }
}

impl Display for Statement {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Statement::Assignment(ref op, ref v1, ref v2, ref v3) => 
                format!("({}, {}, {}, {})", 
                    opt_to_str(op), 
                    opt_to_str(v1), 
                    opt_to_str(v2), 
                    opt_to_str(v3)),       
            Statement::Return(ref v1) => 
                format!("(return {})", opt_to_str(v1)), 
            Statement::Label(id) => format!("Label {}", id),
            Statement::Jump(id) => format!("Jump {}", id),
            Statement::JumpIfTrue(ref op, id) => 
                format!("Jump {} if {}", id, op),  
        })
    }
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
    label_counter: u32,
    symbol_table: SymbolTable,
}

impl TACGenerator {
    pub fn new(start_id: u32) -> TACGenerator {
        TACGenerator {
            functions: vec![],
            function_stack: vec![],
            operands: vec![],
            id_counter: start_id,
            label_counter: 0,
            symbol_table: SymbolTable::new(),            
        }
    }

    pub fn functions(&self) -> &Vec<Function> {
        &self.functions
    }


    pub fn generate_tac(&mut self, node: &AstNode) {
        match *node {
            AstNode::Block(ref children, ref sym_tab, ref node_info) => 
                self.handle_block(children, sym_tab, node_info),
            AstNode::Function(ref child, ref info) => 
                self.handle_function(child, info),
            AstNode::VariableDeclaration(ref child, ref info) => 
                self.handle_variable_declaration(child, info),
            AstNode::VariableAssignment(ref child, ref name, _) => 
                self.handle_variable_assignment(child, name),
            AstNode::Plus(_, _, _) | 
            AstNode::Minus(_, _, _) |
            AstNode::Multiply(_, _, _) | 
            AstNode::Divide(_, _, _) => 
                self.handle_arithmetic_node(node),
            AstNode::Integer(_, _) => self.handle_constant(node),
            AstNode::Identifier(ref name, _) => 
                self.handle_identifier(name),
            AstNode::Return(ref child, _) => self.handle_return(child),
            AstNode::While(ref expr, ref block, _) => 
                self.handle_while(expr, block),
            AstNode::If(ref expr, ref if_blk, ref opt_else_blk, _) =>
                self.handle_if(expr, if_blk, opt_else_blk),
            AstNode::Less(ref left, ref right, _) =>
                self.handle_less(left, right),
            ref x => panic!("Three-address code generation not implemented for '{}'", x),
        }
    }

    fn handle_block(
        &mut self,
        children: &Vec<AstNode>,
        table_entry: &Option<TableEntry>,
        node_info: &NodeInfo) {

        if let Some(ref entry) = *table_entry {
           self.symbol_table.push(entry.clone());
        }
        else {
            ice!("No symbol table information attached to AST block node");
        }
    
        for ref child in children {
            self.generate_tac(child);
        }

        self.symbol_table.pop();
    }

    fn handle_function(&mut self, child: &AstNode, info: &FunctionInfo) {

        let function = Function::new(info.name.clone());
        self.function_stack.push(function);

        self.generate_tac(child);
    
        self.functions.push(
            self.function_stack.pop().unwrap_or_else(
                || ice!("Function stack empty when generating function 3AC")));
    }

    fn handle_variable_declaration(
        &mut self, 
        child: &AstNode,
        info: &DeclarationInfo) {
        self.declaration_assignment_common(child, &info.name);
    }

    fn handle_variable_assignment(
        &mut self, 
        child: &AstNode,
        name: &String) {
        self.declaration_assignment_common(child, name);
    }

    fn declaration_assignment_common(
        &mut self, 
        child: &AstNode,
        name: &String) {

        self.generate_tac(child);
        
        let (variable_info, id) = self.get_variable_info_and_id(name);

        let operand = self.operands.pop();

        /* 
        complex expressions (eg. a = 3*12 + 124 ...etc) create temporaries
        when the operations are split into three-address code form. The way
        the temporaries are generated currently will cause a unnecessary
        temporary to be created, for example the expression

        a = 4 + 8 + 12

        would generate the following operations:

        tmp_1 = 4 + 8
        tmp_2 = tmp_1 + 12
        a = tmp_2
        
        that is, the last two operations could be combined. So let's do that         
        */

        // peek the last operation in statements
        let len = self.current_function().statements.len();
        let mut gen_new = true;
        if len > 0 {
            match self.current_function().statements[len - 1] {
                Statement::Assignment(
                    Some(_), // don't care 
                    Some(Operand::Variable(ref mut tmp_info, ref mut tmp_id)), 
                    Some(_), // don't care
                    Some(_), // don't care
                    ) => {                    

                    *tmp_info = variable_info.clone();
                    *tmp_id = id; 
                    // do not generate new assignment
                    gen_new = false;
                },
                _ => {}
            }
        } 

        if gen_new {
            self.generate_assignment(
                variable_info,
                 id, 
                 operand)
        }
    }

    fn generate_assignment(
        &mut self, 
        var_info: DeclarationInfo, 
        id: u32,
        operand: Option<Operand>) {
        self.current_function().statements.push(
            Statement::Assignment(
                None, 
                Some(
                    Operand::Variable(
                        var_info, id)), 
                None, 
                operand));
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {
      
       let (operator, left_child, right_child) = match *node {
            AstNode::Plus(ref left, ref right, _) => 
                (Operator::Plus, left, right),
            AstNode::Minus(ref left, ref right, _) => 
                (Operator::Minus, left, right),
            AstNode::Multiply(ref left, ref right, _) => 
                (Operator::Multiply, left, right),
            AstNode::Divide(ref left, ref right, _) => 
                (Operator::Divide, left, right),
            _ => ice!("Invalid node '{}' passed when arithmetic node expected", node),
        };

        let left_op = self.get_operand(left_child);
        let right_op = self.get_operand(right_child);


        if self.get_type(&left_op) != self.get_type(&right_op) {
            ice!(
                "Left and right operand have differing types: '{}' vs '{}'",
                self.get_type(&left_op),
                self.get_type(&right_op));
        }

        let id = self.get_next_id();

        let temp = Operand::Variable(
            DeclarationInfo::new_alt(
                tmp_name.to_string(), 
                self.get_type(&left_op),
                0, 0, 0),
            id);

        self.current_function().statements.push(Statement::Assignment(
            Some(operator), Some(temp.clone()), Some(left_op), Some(right_op),
        ));

        self.operands.push(temp) 
    }

    fn handle_identifier(&mut self, name: &String) {

        let (variable_info, id) = self.get_variable_info_and_id(name);
        self.operands.push(Operand::Variable(variable_info, id));
    }

    fn handle_constant(&mut self, node : &AstNode) {
        let operand = match *node {
            AstNode::Integer(val, _) => Operand::Integer(val),
            _ => ice!("Unexpected node '{:?}' encountered when constant was expected during TAC generation", node),
        };

        self.operands.push(operand);
    }

    fn handle_return(&mut self, child: &Option<Box<AstNode>>) {        
        let operand = if let Some(ref node) = *child {
            Some(self.get_operand(node))
        } else {
            None
        };

        self.current_function().statements.push(Statement::Return(operand));
    }

    fn handle_while(&mut self, expr: &AstNode, block: &AstNode) {
        let comparison_label_id = self.get_label_id();
        let block_label_id = self.get_label_id();

        self.current_function().statements.push(
            Statement::Jump(comparison_label_id)); 
        self.current_function().statements.push(
            Statement::Label(block_label_id)); 
        self.generate_tac(block);        
        self.current_function().statements.push(
            Statement::Label(comparison_label_id)); 
        let operand = self.get_operand(expr);
        self.current_function().statements.push(
            Statement::JumpIfTrue(operand, block_label_id)); 
    }


    fn handle_if(
        &mut self, 
        expr: &AstNode, 
        if_blk: &AstNode, 
        opt_else_blk: &Option<Box<AstNode>>) {

        let comparison_label_id = self.get_label_id();
        let if_blk_label = self.get_label_id();
        let else_blk_label = self.get_label_id();
        let out_label = self.get_label_id();


        // jump to condition evaluation
        self.current_function().statements.push(
            Statement::Jump(comparison_label_id)); 
        // true branch
        self.current_function().statements.push(            
            Statement::Label(if_blk_label)); 
        self.generate_tac(if_blk); 
        self.current_function().statements.push(
            Statement::Jump(out_label)); 

        // false branch, if present
        if let Some(ref else_blk) = *opt_else_blk {
            self.current_function().statements.push(            
            Statement::Label(else_blk_label)); 
            self.generate_tac(else_blk);                   
            self.current_function().statements.push(
                Statement::Jump(out_label));         
        }

        // perform comparison
        self.current_function().statements.push(
            Statement::Label(comparison_label_id)); 
        let operand = self.get_operand(expr);

        // jump to true branch, if comparison is true
        self.current_function().statements.push(
            Statement::JumpIfTrue(operand, if_blk_label)); 
        // either fall through, or jump to else branch
        if let Some(_) = *opt_else_blk {
             self.current_function().statements.push(
                Statement::Jump(else_blk_label));         
        }
        self.current_function().statements.push(
            Statement::Label(out_label));



    }

    fn handle_less(&mut self, left: &AstNode, right: &AstNode) {

        let operator = Operator::Less;
        let id = self.get_next_id();
        
        let left_op = self.get_operand(left);
        let right_op = self.get_operand(right);

        let temp = Operand::Variable(
            DeclarationInfo::new_alt(
                tmp_name.to_string(), 
                self.get_type(&left_op),
                0, 0, 0),
            id);

        self.current_function().statements.push(Statement::Assignment(
            Some(operator), Some(temp.clone()), Some(left_op), Some(right_op),
        ));

        self.operands.push(temp);
    }

    fn get_type(&self, operand: &Operand) -> Type {
        match *operand {
            Operand::Variable(ref info, _) => info.variable_type,
            Operand::Integer(_) => Type::Integer,
            Operand::Float(_) => Type::Float,
            Operand::Double(_) => Type::Double,
            Operand::Boolean(_) => Type::Boolean,
        }
    }

    fn get_operand(&mut self, node: &AstNode) -> Operand {
        self.generate_tac(node);
        self.operands.pop().unwrap_or_else(|| ice!("Operand stack empty"))
    }

    fn get_next_id(&mut self) -> u32 {
        let ret = self.id_counter;
        self.id_counter += 1;
        ret
    }

    fn get_label_id(&mut self) -> u32 {
        let ret = self.label_counter;
        self.label_counter += 1;
        ret
    }

    fn get_variable_info_and_id(&self, name: &String) -> (DeclarationInfo, u32) {
        let symbol = self.symbol_table.find_symbol(name).unwrap_or_else(
                || ice!("No symbol '{}' found during TAC construction", name));

        match symbol {
            Symbol::Variable(variable_info, id) => (variable_info, id),
            _ => ice!("Invalid symbol found during TAC construction - expected variable but got '{:?}' instead", symbol),
        }
    }

    fn current_function(&mut self) -> &mut Function {
        self.function_stack.last_mut().unwrap_or_else(|| panic!("Internal compiler error: Function stack empty"))
    } 

} 