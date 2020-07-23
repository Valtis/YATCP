mod peephole_optimizations;
use peephole_optimizations::optimize;

use crate::ast::{AstNode, AstInteger, FunctionInfo, DeclarationInfo, ExtraDeclarationInfo, Span as Span, ArithmeticInfo};
use crate::semcheck::{Type, ARRAY_LENGTH_PROPERTY};
use crate::function_attributes::FunctionAttribute;

use crate::symbol_table::{TableEntry, SymbolTable, Symbol};

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::rc::Rc;

pub const ARRAY_LENGTH_SLOT_SIZE: u32 = 4;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Less,
    LessOrEq,
    Equals,
    NotEquals,
    GreaterOrEq,
    Greater,
    Xor,
}

pub const TMP_NAME : &'static str = ".tmp";

impl Display for Operator {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Operator::Plus => "+".to_string(),
            Operator::Minus => "-".to_string(),
            Operator::Multiply => "*".to_string(),
            Operator::Divide => "/".to_string(),
            Operator::Modulo=> "%".to_string(),
            Operator::Less => "<".to_string(),
            Operator::LessOrEq => "<=".to_string(),
            Operator::Equals => "==".to_string(),
            Operator::NotEquals => "!=".to_string(),
            Operator::GreaterOrEq => ">=".to_string(),
            Operator::Greater => ">".to_string(),
            Operator::Xor => "^".to_string(),
        })
    }
}


// TODO replace tuples with structs for better readability
#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Variable(DeclarationInfo, u32),
    AddressOf{ variable_info: DeclarationInfo, id: u32, },
    ArrayIndex{
        id: u32,
        index_operand: Box<Operand>,
        variable_info: DeclarationInfo,
    },
    ArrayLength {
        id: u32,
        variable_info: DeclarationInfo,
    },
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
            Operand::ArrayIndex { id, ref index_operand, ref variable_info} => {
                format!("{}_{}[{}]", variable_info.name, id, index_operand)
            },
            Operand::ArrayLength { id, ref variable_info } => format!("{}_{}.length", variable_info.name, id),
            Operand::Variable(ref info, id) => format!("{}_{}", info.name, id),
            Operand::AddressOf { ref variable_info, ref id } => format!("&{}_{}", variable_info.name, id),
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
    Array{ id: u32, length: i32, size_in_bytes: u32 },
    Call(Rc<String>, Vec<Operand>, Option<Operand>),
    Label(u32),
    Jump(u32),
    JumpIfTrue(Operand, u32),
    JumpIfFalse(Operand, u32),
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
            Statement::Array{ id: _, length, size_in_bytes} => format!("Init<Array[{}], {} bytes>", length, size_in_bytes),
            Statement::Call(ref name, ref operands, ref dest) => {
                let op_str = operands.iter()
                .fold(
                    String::new(),
                    |acc, v| format!("{}, {}", acc, v))
                .chars()
                .skip(2)
                .collect::<String>();

                let dest_str = if let Some(ref op) = *dest {
                    format!("{} = ", op)
                } else {
                    String::new()
                };

                format!("{}{}({})",
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
            Statement::JumpIfFalse(ref op, id) =>
                format!("Jump {} if not {}", id, op),
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
        match node {
            AstNode::Block{ref statements, ref block_symbol_table_entry, ref span} =>
                self.handle_block(statements, block_symbol_table_entry, span),
            AstNode::Function{ref block, ref function_info } =>
                self.handle_function(block, function_info),
           AstNode::ExternFunction{ref function_info} =>
                self.handle_extern_function(function_info),
            AstNode::FunctionCall{ ref arguments, ref function_name, ref span} =>
                self.handle_function_call(function_name, arguments, span),
            AstNode::VariableDeclaration{ref initialization_expression, ref declaration_info} =>
                self.handle_variable_declaration(initialization_expression, declaration_info),
            AstNode::VariableAssignment{ref expression, ref name, ..} =>
                self.handle_variable_assignment(expression, name),
            AstNode::ArrayAccess { index_expression, indexable_expression} => {
                self.handle_array_access(indexable_expression, index_expression);
            },
            AstNode::ArrayAssignment { assignment_expression, index_expression, variable_name, span: _, } =>
                self.handle_array_assignment(variable_name, index_expression, assignment_expression),
            AstNode::MemberAccess {
                object,
                member,
                span: _,
            } => self.handle_member_access(object, member),
            AstNode::Plus { .. } |
            AstNode::Minus { .. } |
            AstNode::Multiply { .. } |
            AstNode::Divide { .. } |
            AstNode::Modulo { .. } =>
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
            AstNode::Negate{ expression, arithmetic_info } => self.handle_negate(expression, arithmetic_info),
            AstNode::BooleanAnd{ left_expression, right_expression, span} => {
                self.handle_boolean_and(left_expression, right_expression, span);
            },
            AstNode::BooleanOr{ left_expression, right_expression, span} => {
                self.handle_boolean_or(left_expression, right_expression, span);
            },
            AstNode::BooleanNot{ expression, span} => {
                self.handle_not(expression, span);
            }
            x => panic!("Three-address code generation not implemented for '{}'", x),
        }
    }

    fn handle_block(
        &mut self,
        children: &Vec<AstNode>,
        table_entry: &Option<TableEntry>,
        _span: &Span) {

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

        let level = if let AstNode::Block{ block_symbol_table_entry: Some(ref entry), ..} = *child {
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
                    Some(Operand::Initialized(param.variable_type.clone()))));
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
        _info: &Span) {


        let (dest, params) = if let Some(sym) = self.symbol_table.find_symbol(name) {
            if let Symbol::Function(fi) = sym {
                if fi.return_type == Type::Void {
                    (None, fi.parameters)
                } else {
                    let tmp = self.get_temporary(fi.return_type.clone());
                    self.operands.push(tmp.clone());
                    (Some(tmp), fi.parameters)
                }
            } else {
                ice!(
                    "Invalid symbol {:?} in symbol table when function expected", sym)
            }
        } else {
            ice!("Failed to find function '{}' from symbol table", name);
        };

        let mut function_operands = vec![];
        for (i, arg) in args.iter().enumerate() {

            let mut operand = self.get_operand(arg);

            // handle auto referenced array args, generate mov address -> tmp reg and use reg as argument
            if let Operand::Variable(info, id ) = operand.clone() {

                if info.variable_type.is_array() && !info.variable_type.is_reference() {
                    if let Type::Reference(ref param) = params[i].variable_type {
                        if param.is_array() {
                            // arg is array, param is ref to array - generate address move code
                            operand = self.get_temporary(params[i].variable_type.clone());
                            self.current_function().statements.push(
                                Statement::Assignment(
                                    None,
                                    Some(operand.clone()),
                                    None,
                                    Some(Operand::AddressOf { variable_info: info, id  })
                            ));
                        }
                    }

                }
            }

            function_operands.push(operand);
        }




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

        if info.variable_type.is_array() {
            self.handle_array_declaration(child, info);
        } else {
            self.declaration_assignment_common(child, &info.name);
        }
    }

    fn handle_array_declaration(
        &mut self,
        child: &AstNode,
        info: &DeclarationInfo
    ) {
        self.generate_tac(child);
        let (variable_info, id) = self.get_variable_info_and_id(&info.name);
        let operand = self.operands.pop().unwrap_or_else(|| ice!("No initialization value provided for array"));

        let length = if let Some(ExtraDeclarationInfo::ArrayDimension(ref dims)) = variable_info.extra_info {
            let mut length = 1 as u64;
            for dim in dims.iter() {
                match dim {
                    AstInteger::Int(val) => length *= *val as u64,
                    _ => ice!("Invalid array dimension {:?}", dim),
                }
            }

            ice_if!(length > std::i32::MAX as u64,"Array length exceeds maximum size");
            length as i32
        } else {
            ice!("Invalid extra declaration field {:?} for an array", variable_info.extra_info);
        };

        self.store_array_length(id,
                                length,
                                (length as u32)*variable_info.variable_type.get_array_basic_type().size_in_bytes() + ARRAY_LENGTH_SLOT_SIZE);
        self.emit_array_initialization(id, variable_info, length, operand);
    }

    fn store_array_length(&mut self,
                          id: u32,
                          length: i32,
                          size_in_bytes: u32) {
        self.current_function().statements.push(Statement::Array{ id, length, size_in_bytes });
    }

    fn emit_array_initialization(
        &mut self,
        id: u32,
        variable_info: DeclarationInfo,
        size: i32,
        operand: Operand) {
        // FIXME: Right now there is no runtime, so initialing the array is done like this. Replacing this with a call to memset would likely make more sense

        /*
            i = 0;
            START:
            i <= ARRAY_SIZE
            JUMP_IF_FALSE END
            array[i] = OPERAND
            i = i + 1
            JUMP START
            END:


        */


        let start_label_id= self.get_label_id();
        let end_label_id = self.get_label_id();
        let index_var = self.get_temporary(Type::Integer); // optimization opportunity - overlap this with array length field, would save 4 bytes of stack space
        let cmp_result = self.get_temporary(Type::Boolean);


        self.current_function().statements.push(
            Statement::Assignment(None, Some(index_var.clone()), None, Some(Operand::Integer(0))));

        self.current_function().statements.push(
            Statement::Label(start_label_id));
        self.current_function().statements.push(
            Statement::Assignment(
                Some(Operator::Less),
                Some(cmp_result.clone()),
                Some(index_var.clone()),
                Some(Operand::Integer(size))
        ));

        self.current_function().statements.push(
            Statement::JumpIfFalse(cmp_result, end_label_id));

        self.current_function().statements.push(
            Statement::Assignment(
                None,
                Some(Operand::ArrayIndex {
                    id,
                    variable_info: variable_info.clone(),
                    index_operand: Box::new(index_var.clone()),
                }),
                None,
                Some(operand)));

        self.current_function().statements.push(
            Statement::Assignment(
                Some(Operator::Plus),
                Some(index_var.clone()),
                Some(index_var.clone()),
                Some(Operand::Integer(1))));

        self.current_function().statements.push(
            Statement::Jump(start_label_id));
        self.current_function().statements.push(
            Statement::Label(end_label_id));

        // init array length
        // Dirtyish hack: To ensure that the indexing operatino correctly indexes with 4 byte length,
        // we pretend that the array is an integer array. Otherwise the 'arr[-1] = length' operation
        // would fail with arrays with non-integer base type (e.g. boolean arrays)
        let mut init_length_into = variable_info.clone();
        init_length_into.variable_type = Type::IntegerArray; //

        self.current_function().statements.push(
            Statement::Assignment(
                None,
                Some(Operand::ArrayIndex {
                    id,
                    variable_info: init_length_into,
                    index_operand: Box::new(Operand::Integer(-1)),
                }),
                None,
                Some(Operand::Integer(size))));

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

    fn handle_array_access(&mut self,
        indexable_expression: &AstNode,
        index_expression: &AstNode,
    ) {

        let index = self.get_operand(index_expression);

        let (array_type, name) = match indexable_expression {
            AstNode::Identifier(ref name, _) => {
                if let Some(Symbol::Variable(ref info, _)) = self.symbol_table.find_symbol(name) {
                    if info.variable_type.is_array() {
                        (info.variable_type.get_array_basic_type(), name.clone())
                    } else {
                        ice!("Non-array type '{}' variable '{}'", info.variable_type, name)
                    }
                } else {
                    ice!("Invalid symbol '{:?}'", self.symbol_table.find_symbol(name));
                }
            },
            _ => ice!("Invalid AST node '{:?}'", indexable_expression),
        };

        let dst = self.get_temporary(array_type);

        let (var_info, id) = self.get_variable_info_and_id(&name);

        self.current_function().statements.push(
            Statement::Assignment(
                None,
                Some(dst.clone()),
                None,
                Some(Operand::ArrayIndex {
                    index_operand: Box::new(index),
                    variable_info: var_info,
                    id
                })
            )
        );

        self.operands.push(dst);
    }

    fn handle_array_assignment(
        &mut self,
        variable_name: &str,
        index_expression: &AstNode,
        assignment_expression: &AstNode) {

        let index_operand = self.get_operand(index_expression);
        let assignment_operand = self.get_operand(assignment_expression);

        let (var_info, id) = self.get_variable_info_and_id(variable_name);

        self.current_function().statements.push(
            Statement::Assignment(
                None,
                Some(Operand::ArrayIndex {
                    index_operand: Box::new(index_operand),
                    variable_info: var_info,
                    id,
                }),
                None,
                Some(assignment_operand),
            )
        );
    }

    fn handle_member_access(
        &mut self,
        object: &AstNode,
        member: &AstNode) {

        if let AstNode::Identifier(name, _) = object {
            let (var_info, id) = self.get_variable_info_and_id(name);
            ice_if!(!var_info.variable_type.is_array(), "Access not implemented for non-arrays");

            if let AstNode::Identifier(name, _) = member {

                ice_if!(**name != ARRAY_LENGTH_PROPERTY, "Not implemented for non-length properties: {:?}", member);

                let tmp = self.get_temporary(var_info.variable_type.get_array_basic_type());

                self.current_function().statements.push(
                    Statement::Assignment(
                        None,
                        Some(tmp.clone()),
                        None,
                        Some(Operand::ArrayLength{ id, variable_info: var_info }),
                    ));

                self.operands.push(tmp);
            } else {
                ice!("Member access TAC generation not impelmented for member: {:?}", member)
            }




        } else {
            todo!("Member access TAC generation not implemented for: {:?}", object)
        }
    }

    fn handle_arithmetic_node(&mut self, node: &AstNode) {

       let (operator, left_child, right_child) = match *node {
            AstNode::Plus{ref left_expression, ref right_expression, ..} =>
                (Operator::Plus, left_expression, right_expression),
            AstNode::Minus{ref left_expression, ref right_expression, ..} =>
                (Operator::Minus, left_expression, right_expression),
            AstNode::Multiply{ref left_expression, ref right_expression, ..} =>
                (Operator::Multiply, left_expression, right_expression),
            AstNode::Divide{ref left_expression, ref right_expression, ..} =>
                (Operator::Divide, left_expression, right_expression),
           AstNode::Modulo{ref left_expression, ref right_expression, ..} =>
               (Operator::Modulo, left_expression, right_expression),
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

        /*
            COMPARE
            JUMP TO ELSE_LABEL IF COMPARISON FALSE
                TRUE BLOCK
                OPTIONAL JUMP TO OUT_LABEL IF ELSE BLOCK PRESENT
            ELSE_LABEL
                OPTIONAL ELSE BLOCK
            OPTIONAL OUT_LABEL
        */
        let skip_true_block = self.get_label_id();
        let out_label = self.get_label_id();

        let operand = self.get_operand(&condition);
        self.current_function().statements.push(
            Statement::JumpIfFalse(operand, skip_true_block));

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

        let temp = self.get_temporary(Type::Boolean);

        self.current_function().statements.push(Statement::Assignment(
            Some(operator), Some(temp.clone()), Some(left_op), Some(right_op),
        ));

        self.operands.push(temp);
    }

    fn handle_negate(&mut self, child_node: &AstNode, arith_info: &ArithmeticInfo) {
        let op = self.get_operand(child_node);

        let temp = self.get_temporary(arith_info.node_type.clone());
        self.current_function().statements.push(Statement::Assignment(
            Some(Operator::Minus),
            Some(temp.clone()),
            None,
            Some(op)
        ));

        self.operands.push(temp);
    }

    fn handle_boolean_and(&mut self, left: &AstNode, right: &AstNode, _span: &Span) {
       /*

            GET FIRST EXPRESSION RESULT
            JUMP TO out_lbl IF FALSE
            GET SECOND EXPRESSION RESULT
            out_lbl:
       */

        let out_label = self.get_label_id();
        let tmp = self.get_temporary(Type::Boolean);


        let operand = self.get_operand(left);
        self.current_function().statements.push(
            Statement::Assignment(None, Some(tmp.clone()), None, Some(operand.clone())));
        self.current_function().statements.push(
            Statement::JumpIfFalse(operand, out_label));
        let operand = self.get_operand(right);
        self.current_function().statements.push(
            Statement::Assignment(None, Some(tmp.clone()), None, Some(operand)));
        self.operands.push(tmp);
        self.current_function().statements.push(
            Statement::Label(out_label));
    }

    fn handle_boolean_or(&mut self, left: &AstNode, right: &AstNode, _span: &Span) {
        /*
            GET FIRST EXPRESSION RESULT
            JUMP TO out_lbl IF TRUE
            GET SECOND EXPRESSION RESULT
            out_lbl:
        */

        let out_label = self.get_label_id();
        let tmp = self.get_temporary(Type::Boolean);

        let operand = self.get_operand(left);
        self.current_function().statements.push(
            Statement::Assignment(None, Some(tmp.clone()), None, Some(operand.clone())));
        self.current_function().statements.push(
            Statement::JumpIfTrue(operand, out_label));
        let operand = self.get_operand(right);
        self.current_function().statements.push(
            Statement::Assignment(None, Some(tmp.clone()), None, Some(operand)));
        self.operands.push(tmp);
        self.current_function().statements.push(
            Statement::Label(out_label));
    }

    fn handle_not(&mut self, node: &AstNode, _span: &Span) {
        let operand = self.get_operand(node);
        let tmp = self.get_temporary(Type::Boolean);
        self.current_function().statements.push(
            Statement::Assignment(
                Some(Operator::Xor),
                Some(tmp.clone()),
                Some(operand),
                Some(Operand::Integer(1))));

        self.operands.push(tmp);
    }

    fn get_type(&self, operand: &Operand) -> Type {
        match *operand {
            Operand::Variable(ref info, _) => info.variable_type.clone(),
            Operand::AddressOf { ref variable_info, id: _ } => {
              Type::Reference(Box::new(variable_info.variable_type.clone()))
            },
            Operand::Integer(_) => Type::Integer,
            Operand::Float(_) => Type::Float,
            Operand::Double(_) => Type::Double,
            Operand::Boolean(_) => Type::Boolean,
            Operand::Initialized(ref t) => t.clone(),
            Operand::ArrayIndex {index_operand: _, id: _, variable_info: ref var_info } => {
                var_info.variable_type.get_array_basic_type()
            },
            Operand::ArrayLength { id: _, variable_info: _, } => Type::Integer,
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

    fn get_variable_info_and_id(&self, name: &str) -> (DeclarationInfo, u32) {
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