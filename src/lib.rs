#[macro_use]
mod ice;

pub mod frontend;
pub mod middleend;
pub mod backend;

pub mod error_reporter;
pub mod string_table;


mod symbol_table;
mod cfg;
mod ssa_generator;
mod optimizer;
mod byte_generator;
mod code_generator;
mod obj_generator;
mod common;

