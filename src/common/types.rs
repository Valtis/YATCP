use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Byte,
    Boolean,
    IntegralNumber,
    Long,
    Integer,
    Double,
    Float,
    String,
    Void,
    Uninitialized,
    Array(Box<Type>, Vec<i32>),
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
            Type::Long => 8,
            Type::Double => 8,
            Type::Float => 4,
            Type::String => unimplemented!(),
            Type::Void => ice!("Requesting size of a void type"),
            Type::Array(_, _) => unimplemented!(), // TODO define semantics
            Type::InitializerList(_) => ice!("Requesting size of an initializer list"),
            Type::Reference(_) => 8,
            Type::Uninitialized => ice!("Requesting size of an uninitialized type"),
            Type::Invalid => ice!("Requesting size of an invalid type"),
        }
    }

    pub const fn is_array(&self) -> bool {
        match *self {
            Type::Array(_, _) => true,
            Type::Reference(ref x) => x.is_array(),
            _ => false,
        }
    }

    pub const fn is_invalid(&self) -> bool {
        match *self {
            Type::Invalid => true,
            Type::Array(ref x, _) => x.is_invalid(),
            Type::Reference(ref x) => x.is_invalid(),
            Type::InitializerList(ref x) => x.is_invalid(),
            _ => false,
        }
    }

    pub const fn is_reference(&self) -> bool {
        match *self {
            Type::Reference(_) => true,
            _ => false,
        }
    }

    pub const fn is_integral(&self) -> bool {
        match *self {
            Type::Long => true,
            Type::Integer => true,
            Type::Byte => true,
            Type::IntegralNumber => true,
            _ => false,
        }
    }

    // common enough in type checks that this gets its own utility function
    pub const fn get_integral_types() -> [Type; 4] {
        [
            Type::Long,
            Type::Integer,
            Type::Byte,
            Type::IntegralNumber ]
    }

    pub const fn get_numeric_types() -> [Type; 6] {
        [
            Type::Long,
            Type::IntegralNumber,
            Type::Integer,
            Type::Byte,
            Type::Float,
            Type::Double
        ]
    }

    pub fn get_array_basic_type(&self) -> Type {
        match *self {
            Type::Array(ref inner, _) => *inner.clone(),
            Type::Invalid => Type::Invalid,
            Type::Reference(ref x) if x.is_array() => x.get_array_basic_type(),
            _ => ice!("{} is not an array type but requested basic type anyway", self),
        }
    }

    pub fn get_array_dimensions(&self) -> Vec<i32> {
        match *self {
            Type::Array(_, ref dims) => dims.clone(),
            _ => ice!("Requested length of non-array or referenced array '{}'", self),
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
            Type::Long => "Long".to_owned(),
            Type::Double => "Double".to_owned(),
            Type::Float => "Float".to_owned(),
            Type::String=> "String".to_owned(),
            Type::Void => "Void".to_owned(),
            Type::Array(ref inner, _) => format!("{} array", inner),
            Type::Reference(ref x) => format!("Reference to {}", x),
            Type::InitializerList(_) => "Initializer list".to_owned(),
            Type::Uninitialized => "Uninitialized".to_owned(),
            Type::Invalid => "Invalid".to_owned(),
      }, formatter)
  }
}
