mod peephole_optimizations;
use peephole_optimizations::optimize;

use crate::ast::{AstNode, AstInteger, FunctionInfo, DeclarationInfo, NodeInfo};
use crate::semcheck::Type;
use crate::function_attributes::FunctionAttribute;

use crate::symbol_table::{TableEntry, SymbolTable, Symbol};

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Less,
    LessOrEq,
    Equals,
    NotEquals,
    GreaterOrEq,
    Greater
}

pub const TMP_NAME : &'static str = "%tmp";

impl Display for Operator {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operator::Plus => "+".to_string(),
            Operator::Minus => "-".to_string(),
            Operator::Multiply => "*".to_string(),
            Operator::Divide => "/".to_string(),
            Operator::Less => "<".to_string(),
            Operator::LessOrEq => "<=".to_string(),
            Operator::Equals => "==".to_string(),
            Operator::NotEquals => "!=".to_string(),
            Operator::GreaterOrEq => ">=".to_string(),
            Operator::Greater => ">".to_string(),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Variable(DeclarationInfo, u32),
    SSAVariable(DeclarationInfo, u32, u32),
    Integer(i32),
    Float(f32),
    Double(f64),
    Boolean(bool),
    // special operand to represent initialized, but unknown, value
    // used for things like function parameters
    Initialized(Type),
}

impl Display for Operand {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operand::Variable(ref info, id) => format!("{}_{}", info.name, id),
            Operand::SSAVariable(ref info, id, ssa_id) =>
                format!("{}_{}:{}", info.name, id, ssa_id),
            Operand::Integer(v) => format!("{}i", v),
            Operand::Float(v) => format!("{}f", v),
            Operand::Double(v) => format!("{}d", v),
            Operand::Boolean(v) => v.to_string(),
            Operand::Initialized(ref t) => format!(
                "<initialized {} value>", t),
        })
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Assignment(Option<Operator>, Option<Operand>, Option<Operand>, Option<Operand>),
    Call(Rc<String>, Vec<Operand>, Option<Operand>),
    Label(u32),
    Jump(u32),
    JumpIfTrue(Operand, u32),
    Return(Option<Operand>),
    Empty,
    PhiFunction(Operand, Vec<Operand>)
}

fn opt_to_str<T: Display>(op: &Option<T>) -> String {
    match *op {
        Some(ref val) => format!("{} ", val),
        None => "".to_owned(),
    }
}

impl Display for Statement {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Statement::Assignment(ref op, ref v1, ref v2, ref v3) =>
                format!("{}= {}{}{}",
                    opt_to_str(v1),
                    opt_to_str(v2),
                    opt_to_str(op),
                    opt_to_str(v3)),
            Statement::Call(ref name, ref operands, ref dest) => {
                let op_str = operands.iter()
                .fold(
                    String::new(),
                    |acc, v| format!("{}, {}", acc, v))
                .chars()
                .skip(2)
                .collect::<String>();

                let dest_str = if let Some(ref op) = *dest {
                    format!("{} =", op)
                } else {
                    String::new()
                };

                format!("{} {}({})",
                    dest_str,
                    name,
                    op_str)
            },
            Statement::Return(ref v1) =>
                format!("return {}", opt_to_str(v1)),
            Statement::Label(id) => format!("Label {}", id),
            Statement::Jump(id) => format!("Jump {}", id),
            Statement::JumpIfTrue(ref op, id) =>
                format!("Jump {} if {}", id, op),
            Statement::PhiFunction(ref dest, ref operands) => {
                let mut op_str = operands.
                    iter().
                    fold(
                        String::new(),
                        |acc, ref t| format!("'{}', {}", t, acc));

                op_str.pop(); op_str.pop();
                format!("{} = phi<{}>", dest, op_str)
            },
            Statement::Empty => "<Empty statement>".to_owned()
        })
    }
}


#[derive(Clone, Debug)]
pub struct Function {
    pub statements: Vec<Statement>,
    pub function_info: FunctionInfo,
    pub attributes: Vec<FunctionAttribute>,
}

impl Function {
    fn new(function_info: FunctionInfo) -> Function {
        Function {
            statements: vec![],
            function_info,
            attributes: vec![],
        }
    }

    pub fn print(&self) {
        let mut counter = 0;
        println!("Function '{}'\n", self.function_info.name);
        for s in &self.statements {
            println!("    {}: {}", counter, s);
            counter += 1;
        }
        println!();
    }


    pub fn has_attribute(&self, attribute: FunctionAttribute) -> bool {
        self.attributes.contains(&attribute)
    }
}




pub struct TACGenerator {
    functions: Vec<Function>,
    function_stack: Vec<Function>,
    operands: Vec<Operand>,
    id_counter: u32,
    label_counter: u32,
    symbol_table: SymbolTable,
    tmp_name: Rc<String>
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
            tmp_name: Rc::new(TMP_NAME.to_string()),
        }
    }

    pub fn generate_tac_functions(mut self, node: &AstNode) -> Vec<Function> {
        self.generate_tac(node);

        for function in self.functions.iter_mut() {
            if function.statements.is_empty() {
               function.statements.push(Statement::Empty); // Makes CFG generation easier, if we don't have completely empty function bodies
            }
        }

        optimize(&mut self.functions);
        self.functions
    }

    fn generate_tac(&mut self, node: &AstNode) {
        match *node {
            AstNode::Block(ref children, ref sym_tab, ref node_info) =>
                self.handle_block(children, sym_tab, node_info),
            AstNode::Function(ref child, ref info) =>
                self.handle_function(child, info),
           AstNode::ExternFunction(ref function_info) =>
                self.handle_extern_function(function_info),
            AstNode::FunctionCall(ref args, ref name, ref node_info) =>
                self.handle_function_call(name, args, node_info),
            AstNode::VariableDeclaration(ref child, ref info) =>
                self.handle_variable_declaration(child, info),
            AstNode::VariableAssignment(ref child, ref name, _) =>
                self.handle_variable_assignment(child, name),
            AstNode::Plus(_, _, _) |
            AstNode::Minus(_, _, _) |
            AstNode::Multiply(_, _, _) |
            AstNode::Divide(_, _, _) =>
                self.handle_arithmetic_node(node),
            AstNode::Integer(_, _) |
            AstNode::Boolean(_, _) => self.handle_constant(node),
            AstNode::Identifier(ref name, _) =>
                self.handle_identifier(name),
            AstNode::Return(ref child, _) => self.handle_return(child),
            AstNode::While(ref expr, ref block, _) =>
                self.handle_while(expr, block),
            AstNode::If(ref expr, ref if_blk, ref opt_else_blk, _) =>
                self.handle_if(expr, if_blk, opt_else_blk),
            AstNode::Less(_, _, _) |
            AstNode::LessOrEq(_, _, _) |
            AstNode::Equals(_, _, _) |
            AstNode::NotEquals(_, _, _) |
            AstNode::GreaterOrEq(_, _, _) |
            AstNode::Greater(_, _, _) =>
                self.handle_comparison(node),
            ref x => panic!("Three-address code generation not implemented for '{}'", x),
        }
    }

    fn handle_block(
        &mut self,
        children: &Vec<AstNode>,
        table_entry: &Option<TableEntry>,
        _node_info: &NodeInfo) {

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

        let function = Function::new(info.clone());
        self.function_stack.push(function);

        // add mock initializations for function parameters, as later stages
        // always assume that variables are first assigned into before usage

        let level = if let AstNode::Block(_, Some(ref entry), _) = *child {
            entry
        } else {
            ice!("Failed to find symbol table entry for function {}",
                info.name);
        };

        for param in info.parameters.iter() {
            let id = if let Some(Symbol::Variable(_, var_id)) =
                level.find_symbol(&param.name) {
                var_id
            } else {
                ice!(
                 "Failed to find symbol '{}' when handling function parameters",
                 param.name);
            };

            self.current_function().statements.push(
                Statement::Assignment(
                    None,
                    Some(Operand::Variable(
                        param.clone(),
                        id)),
                    None,
                    Some(Operand::Initialized(param.variable_type))));
        }


        self.generate_tac(child);

        self.functions.push(
            self.function_stack.pop().unwrap_or_else(
                || ice!("Function stack empty when generating function 3AC")));
    }

    fn handle_extern_function(&mut self, info: &FunctionInfo) {
        let mut function = Function::new(info.clone());
        function.attributes.push(FunctionAttribute::External);
        self.functions.push(function);
    }

    fn handle_function_call(
        &mut self,
        name: &Rc<String>,
        args: &Vec<AstNode>,
        _info: &NodeInfo) {

        let mut function_operands = vec![];
        for arg in args.iter() {
            function_operands.push(self.get_operand(arg));
        }

        let dest = if let Some(sym) = self.symbol_table.find_symbol(name) {
            if let Symbol::Function(ref fi) = sym {
                if fi.return_type == Type::Void {
                    None
                } else {
                    let tmp = self.get_temporary(fi.return_type);
                    self.operands.push(tmp.clone());
                    Some(tmp)
                }

            } else {
                ice!(
                    "Invalid symbol {:?} in symbol table when function expected", sym)
            }
        } else {
            ice!("Failed to find function '{}' from symbol table", name);
        };

        self.current_function().statements.push(
            Statement::Call(
                name.clone(),
                function_operands,
                dest));
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

        self.generate_assignment(
            variable_info,
            id,
            operand)
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

        let tmp_type = self.get_type(&left_op);
        let temp = self.get_temporary(tmp_type);

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
        let operand = match node {
            AstNode::Integer(ast_integer, _) => {
                match ast_integer {
                    AstInteger::Int(val) => Operand::Integer(*val),
                    _ => ice!("Invalid integer type in three-address code generation: {}", ast_integer),
                }
            }
            AstNode::Boolean(val, _) => Operand::Boolean(*val),
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
        condition: &AstNode,
        if_blk: &AstNode,
        opt_else_blk: &Option<Box<AstNode>>) {


        let reversed_condition = self.reverse_condition(condition);


        /*
            * REVERSED COMPARISON
            JUMP TO ELSE_LABEL IF COMPARISON TRUE
                TRUE BLOCK
                OPTIONAL JUMP TO OUT_LABEL IF ELSE BLOCK PRESENT
            ELSE_LABEL
                OPTIONAL ELSE BLOCK
            OPTIONAL OUT_LABEL
        */
        let skip_true_block = self.get_label_id();
        let out_label = self.get_label_id();

        let operand = self.get_operand(&reversed_condition);
        self.current_function().statements.push(
            Statement::JumpIfTrue(operand, skip_true_block));
        self.generate_tac(if_blk);

        if let Some(_) = opt_else_blk {
            self.current_function().statements.push(
                Statement::Jump(out_label));
        }

        self.current_function().statements.push(
            Statement::Label(skip_true_block));

        if let Some(else_block) = opt_else_blk {
            self.generate_tac(else_block);
            self.current_function().statements.push(
                Statement::Label(out_label));
        }
    }

    fn reverse_condition(&mut self, node: &AstNode) -> AstNode {
        match node {

            AstNode::Less(left, right, span) => {
                AstNode::GreaterOrEq(left.clone(), right.clone(), span.clone())
            },
            AstNode::LessOrEq(left, right, span) => {
                AstNode::Greater(left.clone(), right.clone(), span.clone())
            },
            AstNode::Equals(left, right, span) => {
                AstNode::NotEquals(left.clone(), right.clone(), span.clone())
            },
            AstNode::NotEquals(left, right, span) => {
                AstNode::Equals(left.clone(), right.clone(), span.clone())
            },
            AstNode::Greater(left, right, span) => {
                AstNode::LessOrEq(left.clone(), right.clone(), span.clone())
            }
            AstNode::GreaterOrEq(left, right, span) => {
                AstNode::Less(left.clone(), right.clone(), span.clone())
            },
            AstNode::Boolean(value, span) => {
                AstNode::Boolean(!*value, span.clone())
            },
            AstNode::Identifier(_, info) => {
                AstNode::Equals(
                    Box::new(node.clone()),
                    Box::new(AstNode::Boolean(false, info.clone())),
                    info.clone())
            },
            AstNode::FunctionCall(_, _, ref info) => {
                AstNode::Equals(
                    Box::new(node.clone()),
                    Box::new(AstNode::Boolean(false, info.clone())),
                    info.clone())
            },
            _ => ice!("Unexpected node when comparison node expected: {:#?}", node),
        }
    }

    fn handle_comparison(&mut self, node: &AstNode) {
        let (operator, left, right) = match *node {
            AstNode::Less(ref left, ref right, _) =>
                (Operator::Less, left, right),
            AstNode::LessOrEq(ref left, ref right, _) =>
                (Operator::LessOrEq, left, right),
            AstNode::Equals(ref left, ref right, _) =>
                (Operator::Equals, left, right),
            AstNode::NotEquals(ref left, ref right, _) =>
                (Operator::NotEquals, left, right),
            AstNode::GreaterOrEq(ref left, ref right, _) =>
                (Operator::GreaterOrEq, left, right),
            AstNode::Greater(ref left, ref right, _) =>
                (Operator::Greater, left, right),
            _ => ice!("Invalid AstNode '{}' passed for comparison handling", node)
        };

        let left_op = self.get_operand(left);
        let right_op = self.get_operand(right);

        let tmp_type = self.get_type(&left_op);
        let temp = self.get_temporary(tmp_type);

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
            Operand::Initialized(ref t) => t.clone(),
            Operand::SSAVariable(_, _, _) =>
                ice!("Unexpected SSA variable during TAC generation"),
        }
    }

    fn get_operand(&mut self, node: &AstNode) -> Operand {
        self.generate_tac(node);
        self.operands.pop().unwrap_or_else(|| ice!("Operand stack empty"))
    }

    fn get_temporary(&mut self, var_type: Type) -> Operand {
        let id = self.get_next_id();

        Operand::Variable(
            DeclarationInfo::new_alt(
                self.tmp_name.clone(),
                var_type,
                0, 0, 0),
            id)
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
        self.function_stack.last_mut().unwrap_or_else(|| ice!("Internal compiler error: Function stack empty"))
    }
}


#[cfg(test)]
mod tests {

    use super::*;

    use crate::ast::{AstInteger, ArithmeticInfo};

    #[test]
    fn program_with_variable_declarations_produces_correct_tac() {

        /*
        fn a() : int  {
            let a : int = 4;
            let b : int = 9 * 4;
            let c : int = 6;
        }*/
        let func_info = FunctionInfo::new_alt(Rc::new("a".to_string()), Type::Integer, 0, 0, 0);
        let mut block_symtab_entry = TableEntry::new();

        block_symtab_entry.add_symbol(Symbol::Function(func_info.clone()));

        let decl_info_a = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);

        let decl_info_b = DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::Integer,
            0, 0, 0);

        let decl_info_c =  DeclarationInfo::new_alt(
            Rc::new("c".to_string()),
            Type::Integer,
            0, 0, 0);

        let mut func_symtab_entry = TableEntry::new();
        func_symtab_entry.add_symbol(Symbol::Variable(decl_info_a.clone(), 0));
        func_symtab_entry.add_symbol(Symbol::Variable(decl_info_b.clone(), 1));
        func_symtab_entry.add_symbol(Symbol::Variable(decl_info_c.clone(), 2));


        let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(4),
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    decl_info_a.clone(),
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Multiply(
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(9),
                                            NodeInfo::new(0, 0, 0)
                                        )
                                        ),
                                        Box::new(AstNode::Integer(
                                            AstInteger::from(4),
                                            NodeInfo::new(0, 0, 0)
                                        )
                                        ),
                                        ArithmeticInfo::new_alt(0, 0, 0),
                                    )),
                                    decl_info_b.clone(),
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(6),
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    decl_info_c.clone(),
                                ),
                            ],
                            Some(func_symtab_entry),
                            NodeInfo::new(0, 0, 0)
                        )),
                    func_info,
                )
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

        let mut generator = TACGenerator::new(3);
        let functions = generator.generate_tac_functions(&node);

        assert_eq!(1, functions.len());
        assert_eq!(3, functions[0].statements.len());

        assert_eq!(
            Statement::Assignment(
                None,
                Some(Operand::Variable(decl_info_a.clone(), 0)),
                None,
                Some(Operand::Integer(4))),
            functions[0].statements[0]);

        assert_eq!(
            Statement::Assignment(
                Some(Operator::Multiply),
                Some(Operand::Variable(decl_info_b.clone(), 1)),
                Some(Operand::Integer(9)),
                Some(Operand::Integer(4))),
            functions[0].statements[1]);


        assert_eq!(
            Statement::Assignment(
                None,
                Some(Operand::Variable(decl_info_c.clone(), 2)),
                None,
                Some(Operand::Integer(6))),
            functions[0].statements[2]);
    }


    #[test]
    fn function_call_generates_correct_tac() {
        /*
        -- foo omitted, not actually required --

        fn a() : int  {
            let a : int = 4;
            let b : int = foo(a, 9);
        }*/

        let mut block_symtab_entry = TableEntry::new();
        let foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()), Type::Integer, 0, 0, 0);

        let func_info = FunctionInfo::new_alt(Rc::new("a".to_string()), Type::Integer, 0, 0, 0);

        block_symtab_entry.add_symbol(Symbol::Function(func_info.clone()));
        block_symtab_entry.add_symbol(Symbol::Function(foo_info.clone()));

        let decl_info_a = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);

        let decl_info_b = DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::Integer,
            0, 0, 0);

        let mut func_symtab_entry = TableEntry::new();
        func_symtab_entry.add_symbol(Symbol::Variable(decl_info_a.clone(), 0));
        func_symtab_entry.add_symbol(Symbol::Variable(decl_info_b.clone(), 1));


        let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::from(4),
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    decl_info_a.clone(),
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Identifier(
                                                Rc::new("a".to_string()),
                                                NodeInfo::new(0, 0, 0)),
                                            AstNode::Integer(
                                                AstInteger::from(9),
                                                NodeInfo::new(0, 0, 0))
                                        ],
                                        Rc::new("foo".to_string()),
                                        NodeInfo::new(0, 0, 0),
                                    )),
                                    decl_info_b.clone(),
                                ),
                            ],
                            Some(func_symtab_entry),
                            NodeInfo::new(0, 0, 0)
                        )),
                    func_info,
                )
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

        let mut generator = TACGenerator::new(2);
        let functions = generator.generate_tac_functions(&node);

        assert_eq!(1, functions.len());
        assert_eq!(3, functions[0].statements.len());

        assert_eq!(
            Statement::Assignment(
                None,
                Some(Operand::Variable(decl_info_a.clone(), 0)),
                None,
                Some(Operand::Integer(4))),
            functions[0].statements[0]);

        assert_eq!(
            Statement::Call(
                Rc::new("foo".to_string()),
                vec![
                    Operand::Variable(decl_info_a, 0),
                    Operand::Integer(9)
                ],
                Some(Operand::Variable(
                    DeclarationInfo::new_alt(
                        Rc::new("%tmp".to_string()),
                        Type::Integer,
                        0, 0, 0),
                    2))),
            functions[0].statements[1]);


        assert_eq!(
            Statement::Assignment(
                None,
                Some(Operand::Variable(decl_info_b.clone(), 1)),
                None,
                Some(
                    Operand::Variable(
                        DeclarationInfo::new_alt(
                            Rc::new("%tmp".to_string()),
                            Type::Integer,
                            0, 0, 0),
                        2))),
            functions[0].statements[2]);
    }


    #[test]
    fn function_with_parameters_generates_correct_tac() {


        /*
        fn foo(a: int) : int {

        }
        */

        let mut block_symtab_entry = TableEntry::new();
        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()), Type::Integer, 0, 0, 0);

        let decl_info_a = DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            0, 0, 0);

        foo_info.parameters.push(decl_info_a.clone());

        block_symtab_entry.add_symbol(Symbol::Function(foo_info.clone()));

        let mut func_symtab_entry = TableEntry::new();
        func_symtab_entry.add_symbol(Symbol::Variable(decl_info_a.clone(), 0));


        let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            Some(func_symtab_entry),
                            NodeInfo::new(0, 0, 0)
                        )),
                    foo_info,
                )
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

        let mut generator = TACGenerator::new(1);
        let functions = generator.generate_tac_functions(&node);

        assert_eq!(1, functions.len());
        assert_eq!(1, functions[0].statements.len());

        assert_eq!(
            Statement::Assignment(
                None,
                Some(Operand::Variable(decl_info_a.clone(), 0)),
                None,
                Some(Operand::Initialized(Type::Integer))),
            functions[0].statements[0]);
    }

    #[test]
    fn function_call_to_void_function_does_not_emit_destination() {



        /*
        fn foo() : int {
            bar();
        }

        fn bar() : void() {
        }
        */

        let mut block_symtab_entry = TableEntry::new();
        let mut foo_info = FunctionInfo::new_alt(
            Rc::new("foo".to_string()), Type::Integer, 0, 0, 0);

        let mut bar_info = FunctionInfo::new_alt(
            Rc::new("bar".to_string()), Type::Void, 0, 0, 0);

        block_symtab_entry.add_symbol(Symbol::Function(foo_info.clone()));
        block_symtab_entry.add_symbol(Symbol::Function(bar_info.clone()));


        let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::FunctionCall(
                                    vec![],
                                    Rc::new("bar".to_owned()),
                                    NodeInfo::new(0, 0, 0),
                                )
                            ],
                            Some(TableEntry::new()),
                            NodeInfo::new(0, 0, 0)
                        )),
                    foo_info,
                ),
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            Some(TableEntry::new()),
                            NodeInfo::new(0, 0, 0)
                        )),
                    bar_info,
                ),
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

        let mut generator = TACGenerator::new(1);
        let functions = generator.generate_tac_functions(&node);

        assert_eq!(2, functions.len());

        let (foo_func, bar_func) = if *functions[0].function_info.name == "foo" {
            (&functions[0], &functions[1])
        } else {
            (&functions[1], &functions[0])
        };

        assert_eq!(1, foo_func.statements.len());
        assert_eq!(1, bar_func.statements.len());

        assert_eq!(
            Statement::Call(Rc::new("bar".to_owned()), vec![], None),
            foo_func.statements[0],
        );

        assert_eq!(Statement::Empty, bar_func.statements[0]);
    }
}