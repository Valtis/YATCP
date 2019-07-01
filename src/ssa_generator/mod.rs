use crate::ast::DeclarationInfo;
use crate::cfg::{CFG, Adj};
use crate::cfg::basic_block::BasicBlock;
use crate::tac_generator::{Function, Statement, Operand};

use std::collections::HashMap;
use std::rc::Rc;

pub fn convert_to_ssa(
    functions: &mut Vec<Function>,
    function_cfgs: &mut HashMap<Rc<String>, CFG>) {
    for f in functions.iter_mut() {

        let cfg = &mut function_cfgs.get_mut(&f.name).unwrap();
        let (definitions, decl_info) = get_variable_definitions(f, &cfg.basic_blocks);
        place_phi_functions(f, cfg, &definitions, &decl_info);

        println!("After phi function insertion");
        for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
            println!("<BB {}>", bb_id + 1);
            for i in bb.start..bb.end {
                println!("    {}", f.statements[i]);
            }
        }

        rename_variables(cfg, &decl_info, f);


        println!("\nAfter renaming\n");
        for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
            println!("<BB {}>", bb_id + 1);
            for i in bb.start..bb.end {
                println!("    {}", f.statements[i]);
            }
        }

    }
}

fn place_phi_functions(
    function: &mut Function,
    cfg: &mut CFG,
    definitions: &HashMap<u32, Vec<usize>>,
    decl_info: &HashMap<u32, DeclarationInfo>) {

    for variable in definitions.keys() {
        // only assigned into in a single block - phi node is not needed
        if definitions[variable].len() == 1 {
            continue;
        }

        let mut blocks_with_phi = vec![];
        let mut work_list = vec![];
        for bb_id in definitions[variable].iter() {
            work_list.push(*bb_id);
        }

        while !work_list.is_empty() {
            let bb = work_list.pop().unwrap();

            for dom_bb in cfg.dominance_frontier[bb].iter() {
                if !blocks_with_phi.contains(dom_bb) {
                    println!("Inserting into bb {}", dom_bb);
                    insert_phi_function_into(
                        function,
                        *variable,
                        *dom_bb,
                        &mut cfg.basic_blocks,
                        decl_info);

                    blocks_with_phi.push(*dom_bb);
                    if !definitions[variable].contains(dom_bb) {
                        work_list.push(*dom_bb);
                    }
                }
            }
        }
    }
}

fn insert_phi_function_into(
    function: &mut Function,
    variable: u32,
    block: usize,
    basic_blocks: &mut Vec<BasicBlock>,
    decl_info: &HashMap<u32, DeclarationInfo>) {

    let mut insertion = basic_blocks[block].start;

    match function.statements[insertion] {
        Statement::Label(_) => insertion += 1, // insert after label
        _ => {},
    }

    let end = basic_blocks[block].end;
    function.statements.insert(
        insertion,
        Statement::PhiFunction(
            Operand::Variable(
                decl_info[&variable].clone(),
                variable,
            ),
            vec![]));


    basic_blocks[block].end += 1;
    for bb in basic_blocks.iter_mut() {
        if bb.start >= end {
            bb.start += 1;
            bb.end += 1;
        }
    }

}

// for each variable, get the list of basic blocks where this variable is defined
// (assigned into)
fn get_variable_definitions(
    function: &Function,
    basic_blocks: &Vec<BasicBlock>) -> (HashMap<u32, Vec<usize>>, HashMap<u32, DeclarationInfo>) {

    let mut definitions = HashMap::new();
    let mut decl_info = HashMap::new();

    for (bb_id, bb) in basic_blocks.iter().enumerate() {
        for statement in bb.start..bb.end {
            match function.statements[statement] {
                Statement::Call(_, _, Some(Operand::Variable(ref info, id))) |
                Statement::Assignment(_, Some(Operand::Variable(ref info, id)), _, _) => {
                    if !decl_info.contains_key(&id) {
                        decl_info.insert(id, info.clone());
                    }
                    if !definitions.contains_key(&id) {
                        definitions.insert(id, vec![]);
                    }

                    if !definitions[&id].contains(&bb_id) {
                        definitions.get_mut(&id).unwrap().push(bb_id);
                    }

                },
                _ => {},
            }
        }
    }

    (definitions, decl_info)
}

fn rename_variables(
    cfg: &CFG,
    decl_info: &HashMap<u32, DeclarationInfo>,
    function: &mut Function) {

    let mut ssa_ids = HashMap::new();
    let mut variable_definition_stack = HashMap::new();

    search(
        0,
        cfg,
        function,
        &mut ssa_ids,
        &mut variable_definition_stack,
        decl_info);
}

fn search(
    block: usize,
    cfg: &CFG,
    function: &mut Function,
    ssa_ids: &mut HashMap<u32, u32>,
    variable_definition_stack: &mut HashMap<u32, Vec<u32>>,
    decl_info: &HashMap<u32, DeclarationInfo>) {

    println!("\n\nHandling block: {}", block);

    for i in cfg.basic_blocks[block].start..cfg.basic_blocks[block].end {
        match function.statements[i] {
            Statement::Assignment(
                _,
                ref mut dest,
                ref mut src1,
                ref mut src2) => {

                update_rhs_operand(src1, variable_definition_stack);
                update_rhs_operand(src2, variable_definition_stack);
                update_lhs_operand(dest, variable_definition_stack, ssa_ids);
            },
            Statement::Call(_, ref mut args, ref mut dest) => {
                for arg in args.iter_mut() {
                    let mut op = Some(arg.clone());
                    update_rhs_operand(&mut op, variable_definition_stack);
                    *arg = op.unwrap();
                }
                update_lhs_operand(dest, variable_definition_stack, ssa_ids);
            },
            Statement::JumpIfTrue(ref mut src, _) => {
                let mut op = Some(src.clone());
                update_rhs_operand(&mut op, variable_definition_stack);
                *src = op.unwrap();
            },
            Statement::Return(ref mut src) => {
                update_rhs_operand(src, variable_definition_stack);
            },
            Statement::PhiFunction(ref mut dest, _) => {
                let mut op = Some(dest.clone());
                update_lhs_operand(&mut op, variable_definition_stack, ssa_ids);
                *dest = op.unwrap();
            },
            _ => {},
      }
    }

    // add variables to the phi-functions in child blocks
    for child in cfg.adjacency_list[block].iter() {
        if let Adj::Block(block) = *child {
            'outer: for i in cfg.basic_blocks[block].start..cfg.basic_blocks[block].end {
                match function.statements[i] {
                    Statement::PhiFunction(Operand::Variable(_, var_id), ref mut operands) |
                    Statement::PhiFunction(Operand::SSAVariable(_, var_id, _), ref mut operands) => {

                        let stack = &variable_definition_stack[&var_id];
                        let ssa_id = stack[stack.len()-1];

                        operands.push(Operand::SSAVariable(
                            decl_info[&var_id].clone(),
                            var_id,
                            ssa_id));
                    },
                    _ => {},
                }
            }
        }
    }

    for child in immediately_dominated_nodes(cfg, block) {
        search(
            child,
            cfg,
            function,
            ssa_ids,
            variable_definition_stack,
            decl_info);
    }

    for i in cfg.basic_blocks[block].start..cfg.basic_blocks[block].end {
        match function.statements[i] {
            Statement::Assignment(
                _,
                ref dest,
                _,
                _) => {
                if let Some(Operand::SSAVariable(_, id, _)) = *dest {
                    variable_definition_stack.get_mut(&id).unwrap().pop();
                }
            },
            Statement::PhiFunction(ref mut dest, _) => {
                if let Operand::SSAVariable(_, id, _) = *dest {
                    variable_definition_stack.get_mut(&id).unwrap().pop();
                } else {
                    ice!("Invalid operand type during cleanup: '{}'", dest);
                }
            },
            _ => {},
      }
    }

}

fn update_rhs_operand(
    src: &mut Option<Operand>,
    variable_definition_stack: &HashMap<u32, Vec<u32>>) {
    if let Some(Operand::Variable(ref info, ref id)) = src.clone() {
        let stack = &variable_definition_stack[id];
        let new_id = stack[stack.len()-1];
        *src = Some(Operand::SSAVariable(
            info.clone(),
            *id,
            new_id));
    }
}

fn update_lhs_operand(
    dest: &mut Option<Operand>,
    variable_definition_stack: &mut HashMap<u32, Vec<u32>>,
    ssa_ids: &mut HashMap<u32, u32>
    ) {

    if let Some(Operand::Variable(ref info, ref id)) = dest.clone() {

        if !variable_definition_stack.contains_key(id) {
            variable_definition_stack.insert(*id, vec![]);
        }

        let stack = &mut variable_definition_stack.get_mut(id).unwrap();

        if !ssa_ids.contains_key(id) {
            ssa_ids.insert(*id, 0);
        }

        let new_id = ssa_ids[id];

        *dest = Some(Operand::SSAVariable(
            info.clone(),
            *id,
            new_id));
        println!("Pushing into {}", id);
        stack.push(new_id);
        *ssa_ids.get_mut(id).unwrap() += 1;
    }
}


fn immediately_dominated_nodes(cfg: &CFG, block: usize) -> Vec<usize> {
    let mut successor_nodes = vec![];
    for (i, dominator) in cfg.immediate_dominators.iter().enumerate() {
        println!("{}: {}", i, dominator);
        if block == 0 && i == 0 {
            continue;
        }
        if *dominator == block {
            successor_nodes.push(i);
        }

    }
    successor_nodes
}



pub fn destroy_ssa(
    functions: &mut Vec<Function>,
    function_cfgs: &mut HashMap<Rc<String>, CFG>) {

    for f in functions.iter_mut() {
        let mut remove_list = vec![];
        let mut i = 0;
        loop {
            match f.statements[i] {
                Statement::Assignment(_,
                    ref mut op1,
                    ref mut op2,
                    ref mut op3) => {
                    change_to_regular_variable_opt(op1);
                    change_to_regular_variable_opt(op2);
                    change_to_regular_variable_opt(op3);
                }
                Statement::JumpIfTrue(ref mut operand, _) => {
                    change_to_regular_variable(operand);
                },
                Statement::Return(ref mut operand) => {
                    change_to_regular_variable_opt(operand);
                },
                Statement::PhiFunction(_, _) => {
                    remove_list.push(i);
                }
                Statement::Call(_, ref mut args, ref mut dest) => {
                    change_to_regular_variable_opt(dest);
                    for arg in args.iter_mut() {
                        change_to_regular_variable(arg);
                    }
                }
                _ => {},
            }

            i += 1;
            if i >= f.statements.len() {
                break;
            }
        }

        function_cfgs.get_mut(&f.name).unwrap().remove_statements(f, remove_list);
    }
}

fn change_to_regular_variable(operand: &mut Operand) {
    if let Operand::SSAVariable(info, var_id, _) = operand.clone() {
        *operand = Operand::Variable(info, var_id);
    }
}

fn change_to_regular_variable_opt(operand: &mut Option<Operand>) {
    if let Some(Operand::SSAVariable(info, var_id, _)) = operand.clone() {
        *operand = Some(Operand::Variable(info, var_id));
    }
}