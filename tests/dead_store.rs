extern crate compiler;

use compiler::ast::DeclarationInfo;
use compiler::semcheck::Type;

use compiler::cfg::CFG;
use compiler::cfg::Adj;
use compiler::cfg::basic_block::BasicBlock;

use compiler::optimizer::dead_store::remove_dead_stores;

use compiler::tac_generator::Operand;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Function;

use std::rc::Rc;

// FIXME: Needs more test cases
