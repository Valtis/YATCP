#[macro_use]
mod ice;

pub mod frontend;
pub mod middleend;
pub mod backend;

pub mod lexer;
pub mod parser;
pub mod ast;
pub mod semcheck;
pub mod error_reporter;
pub mod string_table;


mod symbol_table;
mod tac_generator;
mod cfg;
mod ssa_generator;
mod optimizer;
mod byte_generator;
mod code_generator;
mod obj_generator;
mod common;

