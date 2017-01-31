use tac_generator::Function;
use tac_generator::Statement;
use tac_generator::Operand;
use tac_generator::TMP_NAME;

use std::collections::HashMap; 
// removes some inefficiencies in the generated three-address code
pub fn optimize(functions: &mut Vec<Function>) {

    for function in functions.iter_mut() {

        println!("Before peephole optimizations for {}\n", function.name);
        for statement in function.statements.iter() {
            println!("{}", statement);
        }
        println!("\n");

        remove_unconditional_jumps_into_unconditional_jumps(function);
        remove_unnecessary_temporaries(function);

        println!("\nAfter peephole opt:\n");
        for s in function.statements.iter() {
            println!("{}", s);    
        }

    }
}


// tac generator may generate unconditiona jumps to unconditional jumps.
// the unconditional jump can obviously skip the jumps inbetween
fn remove_unconditional_jumps_into_unconditional_jumps(function: &mut Function) {

    let mut label_id_pos = HashMap::new();

    for (i, statement) in function.statements.iter().enumerate() {
            match *statement {
                Statement::Label(id) => {
                    label_id_pos.insert(id, i);
                },
                _ => {},
            }
        }

    // borrow checker workaround
    let cloned = function.statements.clone();
    for statement in function.statements.iter_mut() {
        match *statement {
            Statement::Jump(ref mut id) => {
                let mut changes = true;
                while changes {
                    changes = false;
                    let idx = label_id_pos[&*id] + 1;
                    if idx < cloned.len() {
                        match cloned[idx] {
                            Statement::Jump(other_id) => {
                                println!("Updating {} to {}", id, other_id);
                                *id = other_id;
                                println!("Now: {}", id);
                                changes = true;
                            },
                            _ => {}, 
                        }   
                    }
                }
            },
            _ => {},
        }        
    } 
}

fn remove_unnecessary_temporaries(function: &mut Function) {

    /* 
    complex expressions (eg. a = 3*12 + 124 ...etc) create temporaries
    when the operations are split into three-address code form. The way
    the temporaries are generated currently will cause a unnecessary
    temporary to be created, for example the expression

    a = 4 + 8 + 12

    would generate the following operations:

    tmp_1 = 4 + 8
    tmp_2 = tmp_1 + 12
    a = tmp_2
    
    that is, the last two operations could be combined. So let's do that         
    */
    let mut remove_list = vec![];

    for i  in 0..function.statements.len() {
        match function.statements[i].clone() {
            Statement::Assignment(
                None,
                Some(Operand::Variable(ref var_info, ref var_id)), 
                None,
                Some(Operand::Variable(ref tmp_info, ref tmp_id))) => {

                if !(tmp_info.name == TMP_NAME && var_info.name != TMP_NAME) {
                    continue;
                }

                if i == 0 {
                    continue;
                }

                // check the previous instruction
                match function.statements[i-1] {   
                    Statement::Assignment(
                        _, 
                        Some(Operand::Variable(ref mut prev_info, ref mut prev_id)),
                        _,
                        _) => {
                            // if this is the same temporary than in the previous,
                            // update the var info & prev id and add the 
                            // current instruction to the deletion list
                            if prev_info.name == TMP_NAME && prev_id == tmp_id {
                                *prev_info = var_info.clone();
                                *prev_id = *var_id;
                                remove_list.push(i);
                            }
                        },
                    _ => {}
                }
           },
           _ => {},
        }
    }

    // delete the lines that are no longer needed 
    for i in 0..remove_list.len() {
        function.statements.remove(remove_list[i]);
        // make sure the remaining values are still in sync after removal
        for j in i..remove_list.len() {
            remove_list[j] -= 1;
        }
    }
}