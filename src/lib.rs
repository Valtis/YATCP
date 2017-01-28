extern crate ansi_term;
extern crate byteorder;

#[macro_use]
mod ice;

pub mod token;
pub mod parser;
pub mod lexer;
pub mod ast;
pub mod semcheck;
pub mod symbol_table;
pub mod tac_generator;
pub mod cfg;
pub mod ssa_generator;
pub mod byte_generator;
pub mod code_generator;
pub mod obj_generator;
pub mod error_reporter;


