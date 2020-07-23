pub mod const_prop;
pub mod dead_code;
pub mod dead_store;
pub mod merge_block;
pub mod conditional_jump_conversion;

use crate::cfg::CFG;
use self::const_prop::propagate_and_fold_constants;
use self::dead_code::remove_dead_code;
use crate::tac_generator::tac_code::Function;

use std::collections::HashMap;
use std::rc::Rc;

pub fn optimize(
    functions: &mut Vec<Function>,
    function_cfgs: &mut HashMap<Rc<String>, CFG>) {

    // TODO: Saner handling
    // Running later optimization passes may uncover additional
    // optimization changes for earlier passes. Let's do the easy & stupid thing
    // and just run them in a loop for a while and hope that the result is good enough
    for _ in 0..5 {
        propagate_and_fold_constants(functions);
        remove_dead_code(functions, function_cfgs);
    }
}