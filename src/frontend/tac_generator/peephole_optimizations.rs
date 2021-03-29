use crate::common::tac_code::{Function, Statement, Operand, Operator };

use std::collections::HashMap;
use crate::common::variable_attributes::VariableAttribute;

// removes some inefficiencies in the generated three-address code
pub fn optimize(functions: &mut Vec<Function>) {

    let mut changes = true;
    while changes {
        changes = false;
        for function in functions.iter_mut() {
            changes = changes || replace_boolean_not_on_constant_value(function);
            // FIXME: Update below methods to return change status as well
            replace_conditional_jumps_with_literal_condition_with_unconditional_ones(function);
            remove_unconditional_jumps_into_unconditional_jumps(function);
            remove_unnecessary_temporaries(function);
            merge_successive_labels(function);
        }
    }
}

fn replace_boolean_not_on_constant_value(function: &mut Function) -> bool{
    let mut changes = false;
    for statement in function.statements.iter_mut() {

        match statement {
            // !true or !false
            Statement::Assignment {
                operator: Some(Operator::BitwiseXor),
                destination: Some(ref var @ Operand::Variable(_, _)),
                left_operand: Some(Operand::Boolean(bool_var)),
                right_operand: Some(Operand::Integer(1)) } => {
                *statement = Statement::Assignment {
                    operator: None,
                    destination: Some(var.clone()),
                    left_operand: None,
                    right_operand: Some(Operand::Boolean(!*bool_var))
                };
                changes = true;
            }

            _ => (),
        }
    }

    changes
}

fn replace_conditional_jumps_with_literal_condition_with_unconditional_ones(function: &mut Function) {
    for statement in function.statements.iter_mut() {
        match statement {
            Statement::JumpIfTrue(Operand::Boolean(true), label) => {
                *statement = Statement::Jump(*label);
            },
            Statement::JumpIfTrue(Operand::Boolean(false), _) => {
                *statement = Statement::Empty;
            }
            Statement::JumpIfFalse(Operand::Boolean(true), _) => {
                *statement = Statement::Empty;
            },
            Statement::JumpIfFalse(Operand::Boolean(false), label) => {
                *statement = Statement::Jump(*label);
            },

            _ => (),
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
                                *id = other_id;
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
            Statement::Assignment{
                operator: None,
                destination: Some(Operand::Variable(ref var_info, ref var_id)),
                left_operand: None,
                right_operand: Some(Operand::Variable(ref tmp_info, ref tmp_id))
            } => {

                if !(tmp_info.attributes.contains(&VariableAttribute::Synthetic) && !var_info.attributes.contains(&VariableAttribute::Synthetic)) {
                    continue;
                }

                if i == 0 {
                    continue;
                }

                // check the previous instruction
                match function.statements[i-1] {
                    Statement::Assignment{
                        destination: Some(Operand::Variable(ref mut prev_info, ref mut prev_id)), ..} => {
                            // if this is the same temporary than in the previous,
                            // update the var info & prev id and add the
                            // current instruction to the deletion list
                            if prev_info.attributes.contains(&VariableAttribute::Synthetic) && prev_id == tmp_id {
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


// constructs like while foo { if bar { } } create empty blocks with label,
// that are followed with non-empty block with label. In this case, we
// can merge the two labels
fn merge_successive_labels(function: &mut Function) {
    let mut i = 1;
    loop {
        if i >= function.statements.len() {
            break;
        }
        match function.statements[i] {
            Statement::Label(cur_label) => {
                if let Statement::Label(prev_label) = function.statements[i-1] {
                    i -= 1;
                    merge_labels(function, i, cur_label, prev_label);
                }
            },
            _ => {},
        }

        i += 1;
    }
}

fn merge_labels(
    function: &mut Function,
    pos: usize,
    cur_label: u32,
    prev_label: u32) {
    function.statements.remove(pos);

    for s in function.statements.iter_mut() {
        match *s {
            Statement::Jump(ref mut label_id) |
            Statement::JumpIfTrue(_, ref mut label_id) |
            Statement::JumpIfFalse(_, ref mut label_id)
            if *label_id == prev_label => {
                *label_id = cur_label;
            }
            _ => {},
        }
    }
}

