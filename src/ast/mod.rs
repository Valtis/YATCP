use token::SyntaxToken;
use token::TokenSubType;
use semcheck::Type;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;


fn get_text_from_identifier(identifier: &SyntaxToken) -> String {
    match identifier.token_subtype {
        TokenSubType::Identifier(ref text) => text.clone(),
        _ => panic!("Internal compiler error: Expected identifier but was {}", identifier),
    }
}

fn get_type_from_type_token(variable_type: &SyntaxToken) -> Type {
  match variable_type.token_subtype {
    TokenSubType::IntegerType => Type::Integer,
    TokenSubType::StringType => Type::String,
    TokenSubType::FloatType => Type::Float,
    TokenSubType::DoubleType => Type::Double,
    TokenSubType::BooleanType => Type::Boolean,
    TokenSubType::VoidType => Type::Void,    
    _ => panic!("Internal compiler error: expected type but was {}", variable_type),
  }
}

#[derive(Clone, Debug)]
pub enum AstType {
    Block,
    Function(FunctionInfo),
    VariableDeclaration(DeclarationInfo),
    Plus(ArithmeticInfo),
    Minus(ArithmeticInfo),
    Multiply(ArithmeticInfo),
    Divide(ArithmeticInfo),
    Identifier(String),
    Integer(i32),
    Double(f64),
}

pub struct AstNode {
  children: Vec<AstNode>,
  pub node_type: AstType,
}

impl AstNode {
    pub fn new(children: Vec<AstNode>, t: AstType) -> AstNode {
      AstNode {
        children: children,
        node_type: t,
      }
    }

    pub fn set_children(&mut self, nodes: Vec<AstNode>) {
      self.children = nodes;
    }

    pub fn get_children(&self) -> &Vec<AstNode> {
      &self.children
    }

    pub fn get_mutable_children(&mut self) -> &mut Vec<AstNode> {
      &mut self.children
    }
}

impl Display for AstNode {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
      write!(formatter, "{}", match self.node_type {
          AstType::Block => "Block".to_string(),
          AstType::Function(ref i) => format!("Function {}", i.name),
          AstType::VariableDeclaration(ref i) => format!("variable declaration {}", i.name),
          AstType::Plus(_) => "plus".to_string(),
          AstType::Minus(_) => "minus".to_string(),
          AstType::Multiply(_) => "multiply".to_string(),
          AstType::Divide(_) => "divide".to_string(),
          AstType::Identifier(ref id) => format!("identifier {}", id),
          AstType::Integer(i) => format!("integer {}", i),
          AstType::Double(i) => format!("double {}", i),
      })
  }
}

#[derive(Clone, Debug)]
pub struct FunctionInfo {
    name: String,
}

#[derive(Clone, Debug)]
pub struct DeclarationInfo {
    pub name: String,
    pub variable_type: Type, 
    pub line: i32,
    pub column: i32,
}

#[derive(Clone, Debug)]
pub struct ArithmeticInfo {
    pub node_type: Type,
    pub line: i32,
    pub column: i32,
}

impl FunctionInfo {
    pub fn new(identifier: SyntaxToken) -> FunctionInfo {
        FunctionInfo {
            name: get_text_from_identifier(&identifier),
        }
    }
}

impl DeclarationInfo {
    pub fn new(identifier: SyntaxToken, variable_type: SyntaxToken) -> DeclarationInfo {
        DeclarationInfo {
            name: get_text_from_identifier(&identifier),
            variable_type: get_type_from_type_token(&variable_type), 
            line: identifier.line,
            column: identifier.column,
        }
    }
}

impl ArithmeticInfo {
    pub fn new(token: SyntaxToken) -> ArithmeticInfo {
        ArithmeticInfo {
            node_type: Type::Uninitialized,
            line: token.line,
            column: token.column,
        }
    }

    pub fn update_type(&mut self, t: Type) {
      self.node_type = t;
    }
}