mod const_prop;
mod dead_code;
use cfg::CFG;
use self::const_prop::propagate_and_fold_constants;
use self::dead_code::remove_dead_code;
use tac_generator::Function;

use std::collections::HashMap;

pub fn optimize(
    functions: &mut Vec<Function>, 
    function_cfgs: &mut HashMap<String, CFG>) {
    propagate_and_fold_constants(functions);
    remove_dead_code(functions, function_cfgs);
    

    propagate_and_fold_constants(functions);
    remove_dead_code(functions, function_cfgs);
}