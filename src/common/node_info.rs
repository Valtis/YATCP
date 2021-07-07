use crate::common::{types::Type, variable_attributes::VariableAttribute};

use std::collections::HashSet;
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub line: i32,
    pub column: i32,
    pub length: i32,
}

impl Span {
    pub fn new(line: i32, column: i32, length: i32) -> Span {
        Span {
            line,
            column,
            length,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionInfo {
    pub name: Rc<String>,
    pub parameters: Vec<DeclarationInfo>,
    pub return_type: Type,
    pub span: Span,
}

impl FunctionInfo {
    pub fn new(
        name: Rc<String>,
        span: Span,
        return_type:  Type) -> FunctionInfo {

        FunctionInfo {
            name,
            return_type,
            span,
            parameters: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInfo {
    pub name: Rc<String>,
    pub fields: Vec<DeclarationInfo>,
    pub span: Span,
}

impl StructInfo {
    pub fn new(
        name: Rc<String>,
        span: Span) -> StructInfo {

        StructInfo {
            name,
            span,
            fields: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeclarationInfo {
    pub name: Rc<String>,
    pub variable_type: Type,
    pub span: Span,
    pub attributes: HashSet<VariableAttribute>,
}

impl DeclarationInfo {
    pub fn new(name: Rc<String>, span: Span, variable_type: Type) -> DeclarationInfo {
        DeclarationInfo {
            name,
            variable_type,
            span,
            attributes: HashSet::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArithmeticInfo {
    pub node_type: Type,
    pub span: Span
}

impl ArithmeticInfo {
    pub fn new(span: Span) -> ArithmeticInfo {
        ArithmeticInfo {
            node_type: Type::Uninitialized,
            span,
        }
    }
}
