use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Byte,
    Boolean,
    IntegralNumber,
    Integer,
    Double,
    Float,
    String,
    Void,
    ByteArray, // TODO: Consider replacing with Array(Box<Type>)
    IntegerArray,
    BooleanArray,
    Uninitialized,
    InitializerList(Box<Type>),
    Reference(Box<Type>),
    Invalid, // type error occurred
}

impl Type {
    pub fn size_in_bytes(&self) -> u32 {
        match *self {
            Type::IntegralNumber => ice!("Generic integral number has no size"),
            Type::Boolean => 1,
            Type::Byte => 1,
            Type::Integer => 4,
            Type::Double => 8,
            Type::Float => 4,
            Type::String => unimplemented!(),
            Type::Void => ice!("Requesting size of a void type"),
            Type::ByteArray => unimplemented!(), // TODO define semantics
            Type::BooleanArray=> unimplemented!(), // TODO define semantics
            Type::IntegerArray => unimplemented!(), // TODO define semantics
            Type::InitializerList(_) => ice!("Requesting size of an initializer list"),
            Type::Reference(_) => 8,
            Type::Uninitialized => ice!("Requesting size of an uninitialized type"),
            Type::Invalid => ice!("Requesting size of an invalid type"),
        }
    }

    pub fn is_array(&self) -> bool {
        match *self {
            Type::IntegralNumber => false,
            Type::Byte => false,
            Type::Boolean => false,
            Type::Integer => false,
            Type::Double => false,
            Type::Float => false,
            Type::String => false,
            Type::Void => false,
            Type::IntegerArray => true,
            Type::ByteArray => true,
            Type::BooleanArray => true,
            Type::Uninitialized => false,
            Type::Reference(ref x) => x.is_array(),
            Type::InitializerList(_) => false,
            Type::Invalid => false,
        }
    }

    pub fn is_reference(&self) -> bool {
        match *self {
            Type::Reference(_) => true,
            _ => false,
        }
    }

    pub fn is_integral(&self) -> bool {
        match *self {
            Type::Integer => true,
            Type::Byte => true,
            _ => false,
        }
    }

    pub fn get_array_basic_type(&self) -> Type {
        match *self {
            Type::BooleanArray => Type::Boolean,
            Type::ByteArray => Type::Byte,
            Type::IntegerArray => Type::Integer,
            Type::Invalid => Type::Invalid,
            Type::Reference(ref x) if x.is_array() => x.get_array_basic_type(),
            _ => ice!("{} is not an array type but requested basic type anyway", self),
        }
    }

    pub fn get_array_type_from_basic_type(&self) -> Type {
        match *self {
            Type::Integer => Type::IntegerArray,
            Type::Boolean => Type::BooleanArray,
            Type::Byte => Type::ByteArray,
            Type::Invalid => Type::Invalid,
            _ => ice!("{} is not valid basic type for arrays, but requested array type anyway", self),
        }
    }
}

impl Display for Type {
  fn fmt(&self, formatter: &mut Formatter) -> Result {
        Display::fmt( &match *self {
            // For printing purposes, we treat integral numbers as integers
            Type::IntegralNumber => "Integer".to_owned(),
            Type::Byte=> "Byte".to_owned(),
            Type::Boolean => "Boolean".to_owned(),
            Type::Integer => "Integer".to_owned(),
            Type::Double => "Double".to_owned(),
            Type::Float => "Float".to_owned(),
            Type::String=> "String".to_owned(),
            Type::Void => "Void".to_owned(),
            Type::ByteArray=> "Byte array".to_owned(),
            Type::BooleanArray=> "Boolean array".to_owned(),
            Type::IntegerArray => "Integer array".to_owned(),
            Type::Reference(ref x) => format!("Reference to {}", x),
            Type::InitializerList(_) => "Initializer list".to_owned(),
            Type::Uninitialized => "Uninitialized".to_owned(),
            Type::Invalid => "Invalid".to_owned(),
      }, formatter)
  }
}
