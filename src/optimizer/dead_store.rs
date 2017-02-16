use cfg::CFG;

use tac_generator::Function;
use tac_generator::Statement;
use tac_generator::Operand;

use std::collections::HashMap;
use std::collections::HashSet;

pub fn remove_dead_stores(
    function: &mut Function,
    cfg: &mut CFG) {
    let mut changes = true;
    while changes {
        changes = false;
        let mut reads = HashSet::new();
        let mut writes = HashMap::new();

        for (i, s) in function.statements.iter().enumerate() {

            // writes to variables and where they occur
            match *s {
                Statement::Assignment(
                    _,
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id)),
                    _,
                    _) => { writes.insert((*var_id, *ssa_id), i); },
                Statement::PhiFunction(
                     Operand::SSAVariable(_, ref var_id, ref ssa_id),
                    _) => { writes.insert((*var_id, *ssa_id), i); },
                Statement::Call(
                    _,
                    _,
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id))) => {
                        writes.insert((*var_id, *ssa_id), i);
                },
                _ => {},
            }

            // reads
            match *s {
                 Statement::Assignment(
                    _,
                    _,
                    Some(ref val),
                    Some(ref val2)) => {

                    match *val {
                        Operand::SSAVariable(_, ref var_id, ref ssa_id) => {
                            reads.insert((*var_id, *ssa_id));
                        },
                        _ => {},
                    }

                    match *val2 {
                        Operand::SSAVariable(_, ref var_id, ref ssa_id) => {
                            reads.insert((*var_id, *ssa_id));
                        },
                        _ => {},
                    }
                },
                Statement::JumpIfTrue(
                    Operand::SSAVariable(_, ref var_id, ref ssa_id),
                    _) => {

                    reads.insert((*var_id, *ssa_id));
                },
                Statement::Return(
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id))) => {

                    reads.insert((*var_id, *ssa_id));
                },
                Statement::PhiFunction(
                    _,
                    ref operands) => {

                    for o in operands.iter() {
                        match *o {
                            Operand::SSAVariable(_, ref var_id, ref ssa_id) => {
                                reads.insert((*var_id, *ssa_id));
                            },
                            _ => ice!("Invalid operand present in phi-function: {}", o),
                        }
                    }
                },
                Statement::Call(_, ref args, _) => {
                    for arg in args {
                        if let Operand::SSAVariable(_, ref var_id, ref ssa_id) = *arg {
                            reads.insert((*var_id, *ssa_id));
                        }
                    }
                }
                _ => {},
            }
        }

        for v in reads.iter() {
            writes.remove(v);
        }

        // function calls should not be removed, as they may have side effects
        // instead, remove the dead store from the operation
        remove_dead_writes_in_function_calls(function, &mut writes);


        let mut remove_list : Vec<usize> = writes.values().cloned().collect();
        remove_list.sort();

        if remove_list.len() != 0 {
            changes = true;
            cfg.remove_statements(function, remove_list);
        }


        println!("\nAfter removal:\n");
        for (i, s) in function.statements.iter().enumerate() {
            println!("{}:  {}", i, s);
        }
    }
}


fn remove_dead_writes_in_function_calls(
    function: &mut Function,
    writes: &mut HashMap<(u32, u32), usize>) {
    for line in writes.values() {
        if let Statement::Call(_, _, ref mut dest) = function.statements[*line] {
            *dest = None;
        }
    }

    *writes = writes
    .iter()
    .filter(
        |&(_, line)|
        if let Statement::Call(_, _, _) = function.statements[*line] {
            false
        } else {
            true
        })
    .map(|(k, v)| (*k, *v))
    .collect::<HashMap<(u32, u32), usize>>();

}