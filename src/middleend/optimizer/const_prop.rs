use crate::common::tac_code::{Function, Operand, Operator, Statement};

use std::collections::HashMap;

pub fn propagate_and_fold_constants(
    functions: &mut Vec<Function>) {

    for f in functions.iter_mut() {
        optimize_function(f);
    }
}

fn optimize_function(f: &mut Function) {
    let mut changes = true;

    let mut known_constants = HashMap::new();
    while changes {
        changes = false;
        for s in f.statements.iter_mut() {
            changes = changes || do_constant_folding(s);
            changes = changes || do_constant_propagation(s, &mut known_constants);
        }
    }
}

fn do_constant_folding(
    s: &mut Statement) -> bool {

    match s.clone() {
        // integer assignment
        Statement::Assignment{
            operator: Some(operator),
            destination: Some(var @ Operand::SSAVariable(_, _, _)),
            left_operand: Some(Operand::Integer(val)) ,
            right_operand: Some(Operand::Integer(val2))} => {

            let new_val = match operator {
                Operator::Plus => Operand::Integer(val + val2),
                Operator::Minus => Operand::Integer(val - val2),
                Operator::Multiply => Operand::Integer(val * val2),
                Operator::Divide => Operand::Integer(val / val2),
                Operator::Modulo => Operand::Integer(val % val2),
                Operator::Less => Operand::Boolean(val < val2),
                Operator::LessOrEq => Operand::Boolean(val <= val2),
                Operator::Equals => Operand::Boolean(val == val2),
                Operator::NotEquals => Operand::Boolean(val != val2),
                Operator::GreaterOrEq => Operand::Boolean(val >= val2),
                Operator::Greater => Operand::Boolean(val > val2),
                Operator::LogicalShiftLeft=> Operand::Integer(val << val2),
                Operator::LogicalShiftRight => Operand::Integer(((val as u32) >> val2) as i32),
                Operator::ArithmeticShiftRight => Operand::Integer(val >> val2),
                Operator::BitwiseAnd=> Operand::Integer(val & val2),
                Operator::BitwiseXor => Operand::Integer(val ^ val2),
                Operator::BitwiseOr => Operand::Integer(val | val2),
                Operator::BitwiseNot => ice!("Bitwise not is not binary operation"),
                Operator::Cast{..} => todo!(),
            };
            *s = Statement::Assignment{
                operator: None,
                destination: Some(var),
                left_operand: None,
                right_operand: Some(new_val)
            };

            return true;
        }
        _ => {},
    }

    return false;
}

fn do_constant_propagation(
    s: &mut Statement,
    known_constants: &mut HashMap<(u32, u32), Operand>) -> bool {

    match s.clone() {
        //
        Statement::Assignment{
            operator: None,
            destination: Some(Operand::SSAVariable(_, var_id, ssa_id)),
            left_operand: None,
            right_operand: Some(ref val)} => {

            if is_constant(val) &&
                !known_constants.contains_key(&(var_id, ssa_id)) {
                known_constants.insert((var_id, ssa_id), val.clone());
                return true;
            }
            return false;
        },
        Statement::Assignment{
            left_operand: Some(ref val1),
            right_operand: Some(ref val2),
            ..} => {
            let mut changes = false;

            match *val1 {
                Operand::SSAVariable(_, var_id, ssa_id) => {
                    if known_constants.contains_key(&(var_id, ssa_id)) {

                        if let Statement::Assignment{left_operand: ref mut op, .. } = *s {
                            *op = Some(known_constants[&(var_id, ssa_id)].clone());
                            changes = true;
                        }
                    }
                }
                _ => {},
            }

            match *val2 {
                Operand::SSAVariable(_, var_id, ssa_id) => {
                    if known_constants.contains_key(&(var_id, ssa_id)) {
                        if let Statement::Assignment{ right_operand: ref mut op, .. } = *s {
                            *op = Some(known_constants[&(var_id, ssa_id)].clone());
                            changes = true;
                        }
                    }
                }
                _ => {},
            }

            return changes;
        },
        Statement::JumpIfTrue(ref val, _) |
        Statement::JumpIfFalse(ref val, _) => {
            match *val {
                Operand::SSAVariable(_, var_id, ssa_id) => {
                    if known_constants.contains_key(&(var_id, ssa_id)) {
                        if let Statement::JumpIfTrue(ref mut op, _) = *s {
                            *op = known_constants[&(var_id, ssa_id)].clone();
                            return true;
                        }

                        if let Statement::JumpIfFalse(ref mut op, _) = *s {
                            *op = known_constants[&(var_id, ssa_id)].clone();
                            return true;
                        }
                    }
                }
                _ => {},
            }
        },
        Statement::Return(Some(ref val)) => {
            match *val {
                Operand::SSAVariable(_, var_id, ssa_id) => {
                    if known_constants.contains_key(&(var_id, ssa_id)) {
                        if let Statement::Return(ref mut op) = *s {
                            *op = Some(known_constants[&(var_id, ssa_id)].clone());
                            return true;
                        }
                    }
                }
                _ => {},
            }
        },
        _ => {},
    }

    return false;
}

fn is_constant(var: &Operand) -> bool {
    match *var {
        Operand::Variable(_, _) => false,
        Operand::SSAVariable(_, _, _) => false,
        Operand::Integer(_) => true,
        Operand::Byte(_) => true,
        Operand::Float(_) => true,
        Operand::Double(_) => true,
        Operand::Boolean(_) => true,
        Operand::Initialized(_) => false,
        Operand::ArrayIndex { index_operand: _, id: _, variable_info: _ } => false,
        Operand::ArraySlice { .. } => false,
        Operand::AddressOf { variable_info: _, id: _} => false,
    }
}