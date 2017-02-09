use cfg::CFG;
use cfg::Adj;

use super::merge_block::merge_linear_blocks;
use super::conditional_jump_conversion::convert_jumps;
use tac_generator::Function;
use tac_generator::Operand;
use tac_generator::Operator;
use tac_generator::Statement;

use std::collections::HashMap;
use std::collections::HashSet;



pub fn remove_dead_code(
    functions: &mut Vec<Function>, 
    function_cfgs: &mut HashMap<String, CFG>) {

    for f in functions.iter_mut() {
        let cfg = &mut function_cfgs.get_mut(&f.name).unwrap();

        println!("\n\nBefore dead code elimination pass trivial conditional jump removal\n\n");
        print_cfg(f, cfg);  
        convert_jumps(f, cfg);
        println!("\n\nAfter trivial conditional jump removal\n\n");
        print_cfg(f, cfg);  
        remove_dead_blocks(f, cfg);
        println!("\n\nAfter dead block removal\n\n");;
        print_cfg(f, cfg);  

        remove_trivial_phi_functions(f, cfg);
        remove_dead_stores(f, cfg);     
        println!("\n\nAfter dead store elimination\n\n");;
        print_cfg(f, cfg);  
        merge_linear_blocks(f, cfg);
        println!("\n\nAfter merging linear blocks\n\n");;
        print_cfg(f, cfg);  

        remove_dead_jumps(f, cfg);
        
        println!("\n\nAfter removing dead jumps\n\n");;
        print_cfg(f, cfg); 

        remove_trivial_jumps(f, cfg);
        

        // right now the optimizations do not update these values
        // clear them instead, so that the old values aren't accidentally used
        // later on. 
        cfg.immediate_dominators.clear();
        cfg.dominance_frontier.clear();
    }
}

fn remove_dead_blocks(
    function: &mut Function,
    cfg: &mut CFG) {

    let mut unconnected_blocks = blocks_not_connected_to_entry(cfg);

    for i in 0..unconnected_blocks.len() {
        let id = unconnected_blocks[i];

        // grab any writes this basic block contains
        let mut writes = HashSet::new();
        for i in cfg.basic_blocks[id].start..cfg.basic_blocks[id].end {
           match function.statements[i] {
                Statement::Assignment(
                    _, 
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id)), 
                    _, 
                    _) => { writes.insert((*var_id, *ssa_id)); },
                Statement::PhiFunction(
                     Operand::SSAVariable(_, ref var_id, ref ssa_id),
                    _) => { writes.insert((*var_id, *ssa_id)); },
                _ => {},
            }         
        }

        // and erase the above writes from any phi functions
        
        for s in function.statements.iter_mut() {
            match *s {
                Statement::PhiFunction(
                     _,
                     ref mut operands) => {
                    operands.retain(|v| {
                        if let Operand::SSAVariable(_, ref var_id, ref ssa_id) = *v {
                            !writes.contains(&(*var_id, *ssa_id))
                        } else {
                            ice!("Invalid operand in phi-function: {}", v);
                        }
                    });
                },
                _ =>  {},
            }
        }


        cfg.remove_block(function, id);           

        // decrement block number if block above was removed
        for j in i..unconnected_blocks.len() {
            if unconnected_blocks[j] > id {
                unconnected_blocks[j] -= 1;
            }
        }
    }
}

fn blocks_not_connected_to_entry(
    cfg: &CFG) -> Vec<usize> {

    let mut all_blocks = HashSet::new();

    for bb in 0..cfg.basic_blocks.len() {
        all_blocks.insert(bb);
    }

    let mut visited = HashSet::new();
    depth_first_search(0, &mut visited, cfg);

    
    fn depth_first_search(node: usize, 
        visited: &mut HashSet<usize>,
        cfg: &CFG) {
    
        visited.insert(node);
        for child in cfg.adjacency_list[node].iter() {
            if let Adj::Block(id) = *child {
                if !visited.contains(&id) {
                    depth_first_search(id, visited, cfg);
                }   
            }
        }
    }

    all_blocks.difference(&visited).cloned().collect()
}

// remove phi-functions that only have one operand
fn remove_trivial_phi_functions(
    function: &mut Function,
    cfg: &mut CFG) {
    
    let mut renames : HashMap<(u32, u32), (u32, u32)> = HashMap::new();

    let mut remove_list = vec![];
    for (i, s) in function.statements.iter().enumerate() {
        match *s {
            Statement::PhiFunction(
                Operand::SSAVariable(_, dst_var_id, dst_ssa_id), 
                ref operands) => {
                if operands.len() == 1 {
                    if let Operand::SSAVariable(_, src_var_id, src_ssa_id) = operands[0] {
                        remove_list.push(i);
                        
                        // ensure trivial phi-functions that refer to other trivial
                        // phi functions are handled correctly
                        if renames.contains_key(&(src_var_id, src_ssa_id)) {
                            let prev = renames[&(src_var_id, src_ssa_id)].clone();
                            renames.insert((dst_var_id, dst_ssa_id), prev);
                        } else {
                            renames.insert((dst_var_id, dst_ssa_id), (src_var_id, src_ssa_id));
                        }


                    } else {
                        ice!("Invalid operand for phi-function: {:?}", operands);
                    }

                }
            },
            _ => {},
        }
    }   

    cfg.remove_statements(function, remove_list);

    for s in function.statements.iter_mut() {
        match *s {
            Statement::Assignment(
                _, 
                _,
                Some(ref mut val), 
                Some(ref mut val2)) => {

                match *val {
                    Operand::SSAVariable(_, ref mut var_id, ref mut ssa_id) => {
                        if renames.contains_key(&(*var_id, *ssa_id)) {
                            let (new_var_id, new_ssa_id) = renames[&(*var_id, *ssa_id)];
                            *var_id = new_var_id;
                            *ssa_id = new_ssa_id;
                        }
                    },
                    _ => {},
                }
    
                match *val2 {
                    Operand::SSAVariable(_, ref mut var_id, ref mut ssa_id) => {
                        if renames.contains_key(&(*var_id, *ssa_id)) {
                            let (new_var_id, new_ssa_id) = renames[&(*var_id, *ssa_id)];
                            *var_id = new_var_id;
                            *ssa_id = new_ssa_id;
                        }
                    },
                    _ => {},
                }
            },
            Statement::JumpIfTrue(
                Operand::SSAVariable(_, ref mut var_id, ref mut ssa_id),
                _) => {
                    if renames.contains_key(&(*var_id, *ssa_id)) {
                        let (new_var_id, new_ssa_id) = renames[&(*var_id, *ssa_id)];
                        *var_id = new_var_id;
                        *ssa_id = new_ssa_id;
                    }   
            },
            Statement::Return(
                Some(Operand::SSAVariable(_, ref mut var_id, ref mut ssa_id))) => {
                if renames.contains_key(&(*var_id, *ssa_id)) {
                    let (new_var_id, new_ssa_id) = renames[&(*var_id, *ssa_id)];
                    *var_id = new_var_id;
                    *ssa_id = new_ssa_id;
                }
            },
            Statement::PhiFunction(
                _,
                ref mut operands) => { 
      
                for o in operands.iter_mut() {
                    match *o {
                        Operand::SSAVariable(_, ref mut var_id, ref mut ssa_id) => {
                            if renames.contains_key(&(*var_id, *ssa_id)) {
                                let (new_var_id, new_ssa_id) = renames[&(*var_id, *ssa_id)];
                                *var_id = new_var_id;
                                *ssa_id = new_ssa_id;
                            }
                        },            
                        _ => ice!("Invalid operand present in phi-function: {}", o),
                    }
                }
            },
            _ => {},
    
        }
    }
}


fn remove_dead_stores(
    function: &mut Function,
    cfg: &mut CFG) {
    let mut changes = true;
    while changes {
        changes = false;
        let mut reads = HashSet::new();
        let mut writes = HashMap::new();

        for (i, s) in function.statements.iter().enumerate() {

            // writes
            match *s {
                Statement::Assignment(
                    _, 
                    Some(Operand::SSAVariable(_, ref var_id, ref ssa_id)), 
                    _, 
                    _) => { writes.insert((*var_id, *ssa_id), i); },
                Statement::PhiFunction(
                     Operand::SSAVariable(_, ref var_id, ref ssa_id),
                    _) => { writes.insert((*var_id, *ssa_id), i); },
                _ => {},
            }

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
                _ => {},
            }
        }

        for v in reads.iter() {
            writes.remove(v);
        }


        for (i, s) in function.statements.iter().enumerate() {
            println!("{}:  {}", i, s);
        }

        let mut remove_list : Vec<usize> = writes.values().cloned().collect();
        remove_list.sort();
        if remove_list.len() != 0 {
            changes = true;
            cfg.remove_statements(function, remove_list);
        }
    }
}

fn remove_dead_jumps(
    function: &mut Function,
    cfg: &mut CFG) {

    let mut labels = HashSet::new();
    for s in function.statements.iter() {
        match *s {
            Statement::Label(ref label_id) => { labels.insert(*label_id); },
            _ => {},
        }
    }

    let mut remove_list = vec![];
    for (i, s) in function.statements.iter().enumerate() {
        match *s {
            Statement::Jump(ref label_id) => { 
                if !labels.contains(label_id) {
                    remove_list.push(i);
                }
            },
            _ => {},
        }
    }

    cfg.remove_statements(function, remove_list);
}

// remove unconditional jumps that jump to a label that immediately follows the
// jump 
fn remove_trivial_jumps(
    function: &mut Function,
    cfg: &mut CFG) {

    let mut i = 1;
    loop {
        if i >= function.statements.len() {
            break;
        }

        match function.statements[i] { 
            Statement::Label(label_id) => {
                if let Statement::Jump(jump_id) = function.statements[i-1] {
                    if jump_id == label_id {
                        i -= 1;
                        cfg.remove_statements(function, vec![i]);
                    }
                }
            },
            _ => {},
        }

        i += 1;
    }

}

// Temporary debug code, can be removed
fn print_cfg(f: &Function, cfg: &CFG) {

        let mut counter = 1;
        println!("Function {}", f.name);
        for bb in cfg.basic_blocks.iter() {
            println!("<BB {}>", counter);
            for i in bb.start..bb.end {
               println!("    {}", f.statements[i])
            }
            counter += 1;
        }

        println!("adjacency:\n");

        for i in 0..cfg.basic_blocks.len()  {
            let mut adj_str = cfg.adjacency_list[i].
                iter().
                fold(String::new(), |acc, ref val| format!("{}, {}", val, acc));

            adj_str.pop(); adj_str.pop(); // remove last comma + space
            if adj_str.is_empty() {
                adj_str = "<None>".to_string();
            }
            println!("{}: {}", i+1, adj_str);

        }
        println!("\n");
}