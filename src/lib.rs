#[macro_use]
mod ice;


pub mod string_table;
pub mod token;
pub mod parser;
pub mod lexer;
pub mod ast;
pub mod semcheck;
pub mod symbol_table;
pub mod tac_generator;
pub mod cfg;
pub mod ssa_generator;
pub mod optimizer;
pub mod byte_generator;
pub mod code_generator;
pub mod obj_generator;
pub mod error_reporter;


pub mod frontend;
pub mod middleend;
pub mod backend;