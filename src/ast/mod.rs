use token::Token;
use token::TokenSubType;
use semcheck::Type;
use symbol_table;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::fmt::Debug;
use std::iter;

fn get_text_from_identifier(identifier: &Token) -> String {
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

#[derive(Clone)]
pub enum AstNode {
    Block(Vec<AstNode>, Option<symbol_table::TableEntry>, NodeInfo),
    Function(Box<AstNode>, FunctionInfo),
    VariableDeclaration(Box<AstNode>, DeclarationInfo),
    VariableAssignment(Box<AstNode>, String, NodeInfo),
    Plus(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Minus(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Multiply(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Divide(Box<AstNode>, Box<AstNode>, ArithmeticInfo),
    Negate(Box<AstNode>, ArithmeticInfo),
    Return(Option<Box<AstNode>>, ArithmeticInfo),
    Integer(i32, NodeInfo),
    Float(f32, NodeInfo),
    Double(f64, NodeInfo),
    Text(String, NodeInfo),
    Boolean(bool, NodeInfo),
    Identifier(String, NodeInfo),
    ErrorNode,
}

impl Display for AstNode {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
      write!(formatter, "{}", match *self {
          AstNode::Block(_, _, _) => "Block".to_string(),
          AstNode::Function(_, ref i) => 
                format!("Function '{}' -> {}", i.name, i.return_type),
          AstNode::VariableDeclaration(_, ref i) => 
                format!("Variable declaration '{}' : {}", i.name, i.variable_type),
          AstNode::VariableAssignment(_, ref name, _ ) => 
                format!("Variable assignment '{}'", name),
          AstNode::Integer(val, _) => format!("Integer: {}", val),
          AstNode::Float(val, _) => format!("Float: {}", val),
          AstNode::Double(val, _) => format!("Double: {}", val),
          AstNode::Text(ref text, _) => format!("Text: {}", text),
          AstNode::Identifier(ref name, _) => format!("Identifier: {}", name),
          AstNode::Boolean(ref value, _) => format!("Boolean: {}", value),
          AstNode::Plus(_, _, _) => "Plus".to_string(),
          AstNode::Minus(_, _, _) => "Minus".to_string(),
          AstNode::Multiply(_, _, _) => "Multiply".to_string(),
          AstNode::Divide(_, _, _) => "Divide".to_string(),
          AstNode::Negate(_, _) => "Negate".to_string(),
          AstNode::Return(_, _) => "Return".to_string(),
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
            AstNode::VariableDeclaration(ref child, _) =>
                string = format!("{}{}", string, child.print_impl(next_int)),
            AstNode::VariableAssignment(ref child, _, _) => 
                string = format!("{}{}", string, child.print_impl(next_int)),
            AstNode::Integer(_, _) |
            AstNode::Float(_, _) |
            AstNode::Double(_, _) => {},
            AstNode::Text(_, _) => {},
            AstNode::Identifier(_, _) => {},
            AstNode::Boolean(_, _) => {},
            AstNode::Plus(ref left, ref right, _) |
            AstNode::Minus(ref left, ref right, _) |
            AstNode::Multiply(ref left, ref right, _ ) | 
            AstNode::Divide(ref left, ref right, _) => {
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
            }
            AstNode::ErrorNode => {}
        }

        string
    }

    pub fn line(&self) -> i32 {
        match *self {
            AstNode::Block(_, _, ref info) => info.line,
            AstNode::Function(_, ref info) => info.node_info.line,
            AstNode::VariableDeclaration(_, ref info) => info.node_info.line,
            AstNode::VariableAssignment(_, _, ref info) => info.line,
            AstNode::Plus(_, _, ref info) |
            AstNode::Minus(_, _, ref info) |
            AstNode::Multiply(_, _, ref info) |
            AstNode::Divide(_, _, ref info) => info.node_info.line,            
            AstNode::Negate(_, ref info) => info.node_info.line,
            AstNode::Return(_, ref info) => info.node_info.line,
            AstNode::Integer(_, ref info) => info.line,
            AstNode::Float(_, ref info) => info.line,
            AstNode::Double(_, ref info) => info.line,
            AstNode::Text(_, ref info) => info.line, 
            AstNode::Identifier(_, ref info) => info.line,
            AstNode::Boolean(_, ref info) => info.line, 
            AstNode::ErrorNode => 0,
        }
    }

    pub fn column(&self) -> i32 {
        match *self {
            AstNode::Block(_, _, ref info) => info.column,
            AstNode::Function(_, ref info) => info.node_info.column,
            AstNode::VariableDeclaration(_, ref info) => info.node_info.column,
            AstNode::VariableAssignment(_, _, ref info) => info.column,
            AstNode::Plus(_, _, ref info) |
            AstNode::Minus(_, _, ref info) |
            AstNode::Multiply(_, _, ref info) |
            AstNode::Divide(_, _, ref info) => info.node_info.column,
            AstNode::Negate(_, ref info) => info.node_info.column,
            AstNode::Return(_, ref info) => info.node_info.column,
            AstNode::Integer(_, ref info) => info.column,
            AstNode::Float(_, ref info) => info.column,
            AstNode::Double(_, ref info) => info.column,
            AstNode::Text(_, ref info) => info.column,
            AstNode::Identifier(_, ref info) => info.column,
            AstNode::Boolean(_, ref info) => info.column, 
            AstNode::ErrorNode => 0,
        }
    }

    pub fn length(&self) -> i32 {
        match *self {
            AstNode::Block(_, _, ref info) => info.length,
            AstNode::Function(_, ref info) => info.node_info.length,
            AstNode::VariableDeclaration(_, ref info) => info.node_info.length,
            AstNode::VariableAssignment(_, _, ref info) => info.length,
            AstNode::Plus(_, _, ref info) |
            AstNode::Minus(_, _, ref info) |
            AstNode::Multiply(_, _, ref info) |
            AstNode::Divide(_, _, ref info) => info.node_info.length,
            AstNode::Negate(_, ref info) => info.node_info.length,
            AstNode::Return(_, ref info) => info.node_info.length,
            AstNode::Integer(_, ref info) => info.length,
            AstNode::Float(_, ref info) => info.length,
            AstNode::Double(_, ref info) => info.length,
            AstNode::Text(_, ref info) => info.length,
            AstNode::Identifier(_, ref info) => info.length,
            AstNode::Boolean(_, ref info) => info.column, 
            AstNode::ErrorNode => 0,
        }
    }
}

impl PartialEq for AstNode {   
    fn eq(&self, other: &AstNode) -> bool {
        match (self, other) {
            (&AstNode::Block(ref s_chld, ref s_entry, ref s_info), 
                &AstNode::Block(ref o_chld, ref o_entry, ref o_info)) => {
                // disregard symbol table contents for now
                *s_chld == *o_chld && *s_info == *o_info 
            }
            (&AstNode::Function(ref s_blk, ref s_fi), 
             &AstNode::Function(ref o_blk, ref o_fi)) => {
                s_blk == o_blk && s_fi == o_fi
            }
            (&AstNode::VariableDeclaration(ref s_chld, ref s_vi), 
             &AstNode::VariableDeclaration(ref o_chld, ref o_vi)) => {
                s_chld == o_chld && s_vi == o_vi
            },
            (&AstNode::VariableAssignment(ref s_chld, ref s_name, ref s_ni), 
             &AstNode::VariableAssignment(ref o_chld, ref o_name, ref o_ni)) => {
                s_chld == o_chld && s_name == o_name && s_ni == o_ni
            },
            (&AstNode::Integer(s_num, ref s_ni),
             &AstNode::Integer(o_num, ref o_ni)) => {
                s_num == o_num && s_ni == o_ni 
            },
            (&AstNode::Float(s_num, ref s_ni),
             &AstNode::Float(o_num, ref o_ni)) => {
                s_num == o_num && s_ni == o_ni
            },
            (&AstNode::Double(s_num, ref s_ni),
             &AstNode::Double(o_num, ref o_ni)) => {
                s_num == o_num && s_ni == o_ni
            },
            (&AstNode::Text(ref s_text, ref s_ni),
             &AstNode::Text(ref o_text, ref o_ni)) => {
                s_text == o_text && s_ni == o_ni
            },
            (&AstNode::Boolean(ref s_bool, ref s_ni),
             &AstNode::Boolean(ref o_bool, ref o_ni)) => {
                s_bool == o_bool && s_ni == o_ni
            },
            (&AstNode::Identifier(ref s_name, ref s_ni),
             &AstNode::Identifier(ref o_name, ref o_ni)) => {
                s_name == o_name && s_ni == o_ni
            }
            (&AstNode::Plus(ref s_lchld, ref s_rchld, ref s_ai),
             &AstNode::Plus(ref o_lchld, ref o_rchld, ref o_ai)) |
            (&AstNode::Minus(ref s_lchld, ref s_rchld, ref s_ai),
             &AstNode::Minus(ref o_lchld, ref o_rchld, ref o_ai)) |
            (&AstNode::Multiply(ref s_lchld, ref s_rchld, ref s_ai),
             &AstNode::Multiply(ref o_lchld, ref o_rchld, ref o_ai)) |
            (&AstNode::Divide(ref s_lchld, ref s_rchld, ref s_ai),
             &AstNode::Divide(ref o_lchld, ref o_rchld, ref o_ai))
              => {
                s_lchld == o_lchld && s_rchld == o_rchld && s_ai == o_ai
            },
            (&AstNode::Negate(ref s_child, ref s_ai),
             &AstNode::Negate(ref o_child, ref o_ai)) => {
                s_child == o_child && s_ai == o_ai
            },
            (&AstNode::Return(ref s_child, ref s_ai),
             &AstNode::Return(ref o_child, ref o_ai)) => {
                s_child == o_child && s_ai == o_ai
            },
            (&AstNode::ErrorNode, &AstNode::ErrorNode) => true,
            _ => false,
        }        
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct NodeInfo {
    pub line: i32,
    pub column: i32,
    pub length: i32,
}

impl NodeInfo {
    pub fn new(line: i32, column: i32, length: i32) -> NodeInfo {
        NodeInfo {
            line: line,
            column: column,
            length: length,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: String,
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
        name: String,
        return_type: Type,
        line: i32,
        column: i32,
        length: i32,
        ) -> FunctionInfo {
        FunctionInfo {
            name: name.to_string(),
            return_type: return_type,
            node_info: NodeInfo::new(
                line, column, length),
        }

    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationInfo {
    pub name: String,
    pub variable_type: Type,
    pub node_info: NodeInfo,
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
        name: String, 
        var_type: Type, 
        line: i32, 
        column: i32,
        length: i32) -> DeclarationInfo {
        
        DeclarationInfo {
            name: name,
            variable_type: var_type,
            node_info: NodeInfo::new(line, column, length)
        }
    }
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
