use crate::token::{Token, TokenSubType};
use crate::semcheck::Type;
use crate::symbol_table;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::fmt::Debug;
use std::iter;

use std::rc::Rc;

fn get_text_from_identifier(identifier: &Token) -> Rc<String> {
    match identifier.token_subtype {
        TokenSubType::Identifier(ref text) => text.clone(),
        _ => ice!("Expected identifier but was '{}' instead", identifier),
    }
}

fn get_type_from_type_token(variable_type: &Token) -> Type {
  match variable_type.token_subtype {
    TokenSubType::IntegerType => Type::Integer,
    TokenSubType::StringType => Type::String,
    TokenSubType::FloatType => Type::Float,
    TokenSubType::DoubleType => Type::Double,
    TokenSubType::BooleanType => Type::Boolean,
    TokenSubType::VoidType => Type::Void,
    _ => ice!("Expected type but was '{}' instead", variable_type),
  }
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstInteger {
    Int(i32),
    IntMaxPlusOne,
    Invalid(u64),
}

impl From<u64> for AstInteger {
    fn from(val: u64) -> AstInteger {
        if val <= i32::max_value() as u64 {
            AstInteger::Int(val as i32)
        } else if val == i32::max_value() as u64 + 1 {
            AstInteger::IntMaxPlusOne
        } else {
            AstInteger::Invalid(val)
        }
    }
}

impl From<i32> for AstInteger {
    fn from(val: i32) -> AstInteger {
        AstInteger::Int(val)
    }
}



impl Display for AstInteger {
     fn fmt(&self, formatter: &mut Formatter) -> Result {
         write!(formatter, "{}",
                match self {
                    AstInteger::Invalid(val) => format!("(Integer overflow {}", val),
                    AstInteger::Int(val) => format!("{}", val),
                    AstInteger::IntMaxPlusOne => "Int max plus one (2147483648".to_owned(),
            })
    }
}

// NOTE: When adding nodes, rememver to update the eq method to include the new node.
// TODO update rest of the nodes from tuples to structs for better readability
#[derive(Clone)]
pub enum AstNode {
    Block(Vec<AstNode>, Option<symbol_table::TableEntry>, NodeInfo),
    Function(Box<AstNode>, FunctionInfo),
    ExternFunction(FunctionInfo),
    FunctionCall(Vec<AstNode>, Rc<String>, NodeInfo),
    VariableDeclaration(Box<AstNode>, DeclarationInfo),
    VariableAssignment(Box<AstNode>, Rc<String>, NodeInfo),
    ArrayAssignment{index_expression: Box<AstNode>, assignment_expression: Box<AstNode>, variable_name: Rc<String>, span: NodeInfo },

    MemberAccess { object: Box<AstNode>, member: Box<AstNode>, span: NodeInfo },
    Plus(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Minus(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Multiply(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Divide(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Modulo(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Negate(Box<AstNode>, ArithmeticInfo),

    BooleanAnd(Box<AstNode>, Box<AstNode>, NodeInfo),
    BooleanOr(Box<AstNode>, Box<AstNode>, NodeInfo),

    Return(Option<Box<AstNode>>, ArithmeticInfo),

    While(Box<AstNode>, Box<AstNode>, NodeInfo),
    If(Box<AstNode>, Box<AstNode>, Option<Box<AstNode>>, NodeInfo),

    Less(Box<AstNode>, Box<AstNode>, NodeInfo),
    LessOrEq(Box<AstNode>, Box<AstNode>, NodeInfo),
    Equals(Box<AstNode>, Box<AstNode>, NodeInfo),
    NotEquals(Box<AstNode>, Box<AstNode>, NodeInfo),
    GreaterOrEq(Box<AstNode>, Box<AstNode>, NodeInfo),
    Greater(Box<AstNode>, Box<AstNode>, NodeInfo),
    Not(Box<AstNode>, NodeInfo),

    // Signed integer, but we may need to store INT_MAX +1 while negation is still unresolved
    Integer(AstInteger, NodeInfo),
    Float(f32, NodeInfo),
    Double(f64, NodeInfo),
    Text(Rc<String>, NodeInfo),
    Boolean(bool, NodeInfo),
    Identifier(Rc<String>, NodeInfo),
    ArrayAccess{index_expression: Box<AstNode>, indexable_expression: Box<AstNode>},

    ErrorNode,
}

impl Display for AstNode {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
      write!(formatter, "{}", match self {
            AstNode::Block(_, _, _) => "Block".to_string(),
            AstNode::Function(_, ref i) => {

                let param_str = i.parameters.iter().fold(
                    String::new(),
                    |acc, ref t| format!("{}, {} : {}", acc, t.name, t.variable_type))
                .chars()
                .skip(2).collect::<String>();

                format!("Function {}({}) -> {}",
                    i.name,
                    param_str,
                    i.return_type)
            },
            AstNode::ExternFunction(ref i) => {
                let param_str = i.parameters.iter().fold(
                    String::new(),
                    |acc, ref t| format!("{}, {} : {}", acc, t.name, t.variable_type))
                .chars()
                .skip(2).collect::<String>();

                format!("Extern Function {}({}) -> {}",
                    i.name,
                    param_str,
                    i.return_type)
            },
            AstNode::FunctionCall(_, ref name, _) =>
                format!("Call function {}", name),
            AstNode::VariableDeclaration(_, ref i) => {
                match i.variable_type {
                    _ if i.variable_type.is_array() => {
                        let mut dim_str = "".to_owned();

                        if let Some(ExtraDeclarationInfo::ArrayDimension(ref dims)) = i.extra_info {
                            for dim in dims.iter() {
                                match dim {
                                    AstInteger::Int(val) => dim_str = format!("{}[{}]", dim_str, val),
                                    AstInteger::IntMaxPlusOne => dim_str = format!("{}[{}]", dim_str, (std::i32::MAX as u64) + 1),
                                    AstInteger::Invalid(val) => dim_str = format!("{}[{}<invalid>]", dim_str, val),
                                }
                            }
                        } else {
                            ice!("Non-array dimension information on array declaration");
                        }

                        format!("Variable declaration '{}' : {}{}", i.name, i.variable_type, dim_str)
                    },
                    _ => format!("Variable declaration '{}' : {}", i.name, i.variable_type),
                }
            }
            AstNode::VariableAssignment(_, ref name, _ ) =>
                format!("Variable assignment '{}'", name),
            AstNode::ArrayAssignment {
                index_expression: _,
                assignment_expression: _,
                variable_name: name,
                span: _
            } => format!("Array assignment '{}'", name),
            AstNode::MemberAccess { object: _, member: _, span: _} => format!("Member access"),
            AstNode::Integer(val, _) => format!("Integer: {}", val),
            AstNode::Float(val, _) => format!("Float: {}", val),
            AstNode::Double(val, _) => format!("Double: {}", val),
            AstNode::Text(ref text, _) => format!("Text: {}", text),
            AstNode::Identifier(ref name, _) => format!("Identifier: {}", name),
            AstNode::ArrayAccess{index_expression: _, indexable_expression: _}=> format!("Array access"),
            AstNode::Boolean(ref value, _) => format!("Boolean: {}", value),
            AstNode::Plus(_, _, _) => "Plus".to_string(),
            AstNode::Minus(_, _, _) => "Minus".to_string(),
            AstNode::Multiply(_, _, _) => "Multiply".to_string(),
            AstNode::Divide(_, _, _) => "Divide".to_string(),
            AstNode::Modulo(_, _, _) => "Modulo".to_string(),
            AstNode::BooleanAnd(_, _, _) => "And".to_string(),
            AstNode::BooleanOr(_, _, _) => "Or".to_string(),
            AstNode::Negate(_, _) => "Negate".to_string(),
            AstNode::Return(_, _) => "Return".to_string(),
            AstNode::While(_, _, _) => "While".to_string(),
            AstNode::If(_, _, _, _) => "If".to_string(),
            AstNode::Less(_, _, _) => "Less".to_string(),
            AstNode::LessOrEq(_, _, _) => "LessOrEq".to_string(),
            AstNode::Equals(_, _, _) => "Equals".to_string(),
            AstNode::NotEquals(_, _, _) => "NotEquals".to_string(),
            AstNode::GreaterOrEq(_, _, _) => "GreaterOrEq".to_string(),
            AstNode::Greater(_, _, _) => "Greater".to_string(),
            AstNode::Not(_, _) => "Not".to_string(),
            AstNode::ErrorNode => "<syntax error>".to_string(),
      })
  }
}

// primarily implemented for tests so that assert_eq! pretty prints the tree
// when test fails
impl Debug for AstNode {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "\n{}", self.print_impl(0))
    }
}

impl AstNode {
    pub fn print(&self) {
        println!("{}", self.print_impl(0));
    }

    fn print_impl(&self, intendation: usize) -> String {
        let int_str = iter::repeat(" ").take(intendation).collect::<String>();
        let mut string = format!("{}{}\n", int_str, self);

        let next_int = intendation + 2;
        match *self {
            AstNode::Block(ref children, _, _) => {
                for c in children {
                    string = format!("{}{}", string, c.print_impl(next_int));
                }
            },
            AstNode::Function(ref child, _) => string = format!("{}{}", string, child.print_impl(next_int)),
            AstNode::ExternFunction(_) => { /* do nothing, no children */},
            AstNode::FunctionCall(ref args, _, _) => {
                for arg in args {
                    string = format!("{}{}", string, arg.print_impl(next_int));
                }
            },
            AstNode::VariableDeclaration(ref child, _) =>
                string = format!("{}{}", string, child.print_impl(next_int)),
            AstNode::VariableAssignment(ref child, _, _) =>
                string = format!("{}{}", string, child.print_impl(next_int)),
            AstNode::ArrayAssignment {
                index_expression: ref index,
                assignment_expression: ref assign,
                variable_name: _,
                span: _,
            } => {
                string = format!("{}{}", string, index.print_impl(next_int));
                string = format!("{}{}", string, assign.print_impl(next_int));
            },
            AstNode::MemberAccess {
                object: ref member_access,
                ref member,
                span: _,
            } => {
                string = format!("{}{}", string, member_access.print_impl(next_int));
                string = format!("{}{}", string, member.print_impl(next_int));
            }
            AstNode::Integer(_, _) |
            AstNode::Float(_, _) |
            AstNode::Double(_, _) => {},
            AstNode::Text(_, _) => {},
            AstNode::Identifier(_, _) => {},
            AstNode::ArrayAccess{ref index_expression,  ref indexable_expression} => {
                string = format!("{}{}", string, indexable_expression.print_impl(next_int));
                string = format!("{}{}", string, index_expression.print_impl(next_int));
            },
            AstNode::Boolean(_, _) => {},
            AstNode::BooleanAnd(ref left, ref right, _) |
            AstNode::BooleanOr(ref left, ref right, _) |
            AstNode::Plus(ref left, ref right, _) |
            AstNode::Minus(ref left, ref right, _) |
            AstNode::Multiply(ref left, ref right, _ ) |
            AstNode::Divide(ref left, ref right, _) |
            AstNode::Modulo(ref left, ref right, _) => {
                string = format!("{}{}", string, left.print_impl(next_int));
                string = format!("{}{}", string, right.print_impl(next_int));
            },
            AstNode::Negate(ref child, _) => {
                string = format!("{}{}", string, child.print_impl(next_int));
            },
            AstNode::Return(ref opt_child, _) => {
                if let Some(ref child) = *opt_child {
                    string = format!("{}{}", string, child.print_impl(next_int))
                }
            },
            AstNode::While(ref expr, ref child, _) => {
                string = format!("{}{}", string, expr.print_impl(next_int));
                string = format!("{}{}", string, child.print_impl(next_int));
            },
            AstNode::If(ref expr, ref child, ref opt_else_blk, _) => {
                string = format!("{}{}", string, expr.print_impl(next_int));
                string = format!("{}{}", string, child.print_impl(next_int));
                if let Some(ref else_blk) = *opt_else_blk {
                    string = format!("{}{}", string, else_blk.print_impl(next_int));
                }
            },
            AstNode::Less(ref left, ref right, _) |
            AstNode::LessOrEq(ref left, ref right, _) |
            AstNode::Equals(ref left, ref right, _) |
            AstNode::NotEquals(ref left, ref right, _) |
            AstNode::GreaterOrEq(ref left, ref right, _) |
            AstNode::Greater(ref left, ref right, _) => {
                string = format!("{}{}", string, left.print_impl(next_int));
                string = format!("{}{}", string, right.print_impl(next_int));
            }
            AstNode::Not(ref child, _) => {
                string = format!("{}{}", string, child.print_impl(next_int));
            },
            AstNode::ErrorNode => {}
        }

        string
    }

    pub fn span(&self) -> NodeInfo {
        let empty = NodeInfo::new(0, 0, 0);
        let x = match self {
            AstNode::Block(_, _, span) => *span,
            AstNode::Function(_, info) => info.node_info,
            AstNode::ExternFunction(info) => info.node_info,
            AstNode::FunctionCall(_, _, info) => *info,
            AstNode::VariableDeclaration(_, info) => info.node_info,
            AstNode::VariableAssignment(_, _, span) => *span,
            AstNode::ArrayAssignment{
                index_expression: _,
                assignment_expression: _,
                variable_name: _,
                span,
            } => *span,
            AstNode::MemberAccess {
                object: _,
                member: _,
                span,
            } => *span,
            AstNode::BooleanAnd(_, _, span) |
            AstNode::BooleanOr(_, _, span) => *span,
            AstNode::Plus(_, _, info) |
            AstNode::Minus(_, _, info) |
            AstNode::Multiply(_, _, info) |
            AstNode::Divide(_, _, info) |
            AstNode::Modulo(_, _, info)=> info.node_info,
            AstNode::Negate(_,  info) => info.node_info,
            AstNode::Return(_, info) => info.node_info,
            AstNode::While(_, _, span) => *span,
            AstNode::If(_, _, _, span) => *span,
            AstNode::Less(_, _, span) |
            AstNode::LessOrEq(_, _, span) |
            AstNode::Equals(_, _, span) |
            AstNode::NotEquals(_, _, span) |
            AstNode::GreaterOrEq(_, _, span) |
            AstNode::Greater(_, _, span) => *span,
            AstNode::Integer(_, span) => *span,
            AstNode::Float(_, span) => *span,
            AstNode::Double(_, span) => *span,
            AstNode::Text(_, span) => *span,
            AstNode::Identifier(_, span) => *span,
            AstNode::ArrayAccess { index_expression: _, indexable_expression} => indexable_expression.span(),
            AstNode::Boolean(_, span) => *span,
            AstNode::Not(_, span) => *span,
            AstNode::ErrorNode => empty,
        };
        x
    }
}

impl PartialEq for AstNode {
    fn eq(&self, other: &AstNode) -> bool {
        match (self, other) {
            (AstNode::Block(s_chld, _s_entry, s_info),
             AstNode::Block(o_chld, _o_entry, o_info)) => {
                // disregard symbol table contents for now
                *s_chld == *o_chld && *s_info == *o_info
            }
            (AstNode::Function(s_blk, s_fi),
             AstNode::Function(o_blk, o_fi)) => {
                s_blk == o_blk && s_fi == o_fi
            },
            (AstNode::ExternFunction(s_fi),
             AstNode::ExternFunction(o_fi)) => {
                s_fi == o_fi
            },
            (AstNode::FunctionCall(s_args, s_name, s_info),
             AstNode::FunctionCall(o_args, o_name, o_info)) => {
                s_args == o_args && s_name == o_name && s_info == o_info
            }
            (AstNode::VariableDeclaration(s_chld, s_vi),
             AstNode::VariableDeclaration(o_chld, o_vi)) => {
                s_chld == o_chld && s_vi == o_vi
            },
            (AstNode::VariableAssignment(s_chld, s_name, s_ni),
             AstNode::VariableAssignment(o_chld, o_name, o_ni)) => {
                s_chld == o_chld && s_name == o_name && s_ni == o_ni
            },
            (AstNode::ArrayAssignment { index_expression: s_indx, assignment_expression: s_asgn, variable_name: s_name, span: s_span },
             AstNode::ArrayAssignment { index_expression: o_indx, assignment_expression: o_asgn, variable_name: o_name, span: o_span })
                => {
                s_indx == o_indx && s_asgn == o_asgn && s_name == o_name && s_span == o_span
            },
            (AstNode::MemberAccess { object: s_expr, member: s_member, span: s_span},
             AstNode::MemberAccess { object: o_expr, member: o_member, span: o_span}) => {
                s_expr == o_expr && s_member == o_member && s_span == o_span
            },
            (AstNode::Integer(s_num, s_ni),
             AstNode::Integer(o_num, o_ni)) => {
                *s_num == *o_num && s_ni == o_ni
            },
            (AstNode::Float(s_num, s_ni),
             AstNode::Float(o_num, o_ni)) => {
                s_num == o_num && s_ni == o_ni
            },
            (AstNode::Double(s_num, s_ni),
             AstNode::Double(o_num, o_ni)) => {
                s_num == o_num && s_ni == o_ni
            },
            (AstNode::Text(s_text, s_ni),
             AstNode::Text(o_text, o_ni)) => {
                s_text == o_text && s_ni == o_ni
            },
            (AstNode::Boolean(s_bool, s_ni),
             AstNode::Boolean(o_bool, o_ni)) => {
                s_bool == o_bool && s_ni == o_ni
            },
            (AstNode::Identifier(s_name, s_ni),
             AstNode::Identifier(o_name, o_ni)) => {
                s_name == o_name && s_ni == o_ni
            }
            (AstNode::Plus(s_lchld, s_rchld, s_ai),
             AstNode::Plus(o_lchld, o_rchld, o_ai)) |
            (AstNode::Minus(s_lchld, s_rchld, s_ai),
             AstNode::Minus(o_lchld, o_rchld, o_ai)) |
            (AstNode::Multiply(s_lchld, s_rchld, s_ai),
             AstNode::Multiply(o_lchld, o_rchld, o_ai)) |
            (AstNode::Divide(s_lchld, s_rchld, s_ai),
             AstNode::Divide(o_lchld, o_rchld, o_ai))
              => {
                s_lchld == o_lchld && s_rchld == o_rchld && s_ai == o_ai
            },
            (AstNode::ArrayAccess { indexable_expression: s_indexable_expr, index_expression: s_idx },
             AstNode::ArrayAccess { indexable_expression: o_indexable_expr, index_expression: o_idx },
            ) => {
                s_indexable_expr == o_indexable_expr && s_idx == o_idx
            }
            (AstNode::Negate(s_child, s_ai),
             AstNode::Negate(o_child, o_ai)) => {
                s_child == o_child && s_ai == o_ai
            },
            (AstNode::Return(s_child, s_ai),
             AstNode::Return(o_child, o_ai)) => {
                s_child == o_child && s_ai == o_ai
            },
            (AstNode::While(s_lchld, s_rchld, s_ni),
             AstNode::While(o_lchld, o_rchld, o_ni)) => {
                s_lchld == o_lchld && s_rchld == o_rchld && s_ni == o_ni
            },
            (AstNode::If(
                s_lchld, s_rchld, s_opt_else_blk, s_ni),
             AstNode::If(
                o_lchld, o_rchld, o_opt_else_blk, o_ni)) => {
                s_lchld == o_lchld && s_rchld == o_rchld &&
                s_opt_else_blk == o_opt_else_blk && s_ni == o_ni
            }
            (AstNode::Less(s_lchld, s_rchld, s_ni),
             AstNode::Less(o_lchld, o_rchld, o_ni)) |
            (AstNode::LessOrEq(s_lchld,  s_rchld, s_ni),
             AstNode::LessOrEq(o_lchld, o_rchld, o_ni)) |
            (AstNode::Equals(s_lchld, s_rchld, s_ni),
             AstNode::Equals(o_lchld, o_rchld, o_ni)) |
            (AstNode::NotEquals(s_lchld, s_rchld, s_ni),
             AstNode::NotEquals(o_lchld, o_rchld, o_ni)) |
            (AstNode::GreaterOrEq(s_lchld, s_rchld, s_ni),
             AstNode::GreaterOrEq(o_lchld, o_rchld, o_ni)) |
            (AstNode::Greater(s_lchld, s_rchld, s_ni),
             AstNode::Greater(o_lchld, o_rchld, o_ni)) => {
                s_lchld == o_lchld && s_rchld == o_rchld && s_ni == o_ni
            },
            (AstNode::BooleanAnd(s_lchild, s_rchild, s_span),
            AstNode::BooleanAnd(o_lchild, o_rchild, o_span)) |
            (AstNode::BooleanOr(s_lchild, s_rchild, s_span),
             AstNode::BooleanOr(o_lchild, o_rchild, o_span))=> {
                s_lchild == o_lchild && s_rchild == o_rchild && s_span == o_span
            },
            (AstNode::Not(s_child, s_span),
            AstNode::Not(o_child, o_span)) => {
                s_child == o_child && s_span == o_span
            },
            (AstNode::ErrorNode, &AstNode::ErrorNode) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct NodeInfo {
    pub line: i32,
    pub column: i32,
    pub length: i32,
}

impl NodeInfo {
    pub fn new(line: i32, column: i32, length: i32) -> NodeInfo {
        NodeInfo {
            line,
            column,
            length,
        }
    }
}

impl From<Token> for NodeInfo {
    fn from(val: Token) -> NodeInfo {
        NodeInfo{
            line: val.line,
            column: val.column,
            length: val.length,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: Rc<String>,
    pub parameters: Vec<DeclarationInfo>,
    pub return_type: Type,
    pub node_info: NodeInfo,
}

impl FunctionInfo {
    pub fn new(
        identifier: &Token,
        return_type: &Token) -> FunctionInfo {

        FunctionInfo::new_alt(
            get_text_from_identifier(identifier),
            get_type_from_type_token(return_type),
            identifier.line,
            identifier.column,
            identifier.length)
    }

    pub fn new_alt(
        name: Rc<String>,
        return_type: Type,
        line: i32,
        column: i32,
        length: i32,
        ) -> FunctionInfo {
        FunctionInfo {
            name: name,
            parameters: vec![],
            return_type: return_type,
            node_info: NodeInfo::new(
                line, column, length),
        }

    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationInfo {
    pub name: Rc<String>,
    pub variable_type: Type,
    pub node_info: NodeInfo,
    pub extra_info: Option<ExtraDeclarationInfo>,
}

impl DeclarationInfo {
    pub fn new(identifier: &Token, variable_type: &Token) -> DeclarationInfo {
        DeclarationInfo::new_alt(
            get_text_from_identifier(identifier),
            get_type_from_type_token(variable_type),
            identifier.line,
            identifier.column,
            identifier.length)
    }

    pub fn new_alt(
        name: Rc<String>,
        var_type: Type,
        line: i32,
        column: i32,
        length: i32) -> DeclarationInfo {

        DeclarationInfo {
            name: name,
            variable_type: var_type,
            node_info: NodeInfo::new(line, column, length),
            extra_info: None,
        }
    }

    pub fn new_with_extra_info(
        identifier: &Token,
        variable_type: &Token,
        extra_info: Option<ExtraDeclarationInfo>) -> DeclarationInfo {
        DeclarationInfo {
            name: get_text_from_identifier(identifier),
            variable_type: get_type_from_type_token(variable_type),
            node_info: NodeInfo::new(identifier.line, identifier.column, identifier.length),
            extra_info,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExtraDeclarationInfo {
    ArrayDimension(Vec<AstInteger>), // left-to-right, in declaration order
}



#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticInfo {
    pub node_type: Type,
    pub node_info: NodeInfo
}

impl ArithmeticInfo {
    pub fn new(token: &Token) -> ArithmeticInfo {
        ArithmeticInfo::new_alt(token.line, token.column, token.length)
    }

    pub fn new_alt(
        line: i32,
        column: i32,
        length: i32) -> ArithmeticInfo {

        ArithmeticInfo {
            node_type: Type::Uninitialized,
            node_info: NodeInfo::new(line, column, length)
        }
    }
}
