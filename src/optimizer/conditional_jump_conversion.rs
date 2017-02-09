use tac_generator::Function;
use tac_generator::Operand;
use tac_generator::Statement;

use cfg::Adj;
use cfg::CFG;
use cfg::dom_front::calculate_immediate_dominator_opt;

use std::collections::HashMap;

// convert conditional jumps with constant operand into unconditional jumps
pub fn convert_jumps(
    function: &mut Function, 
    cfg: &mut CFG) {
    let mut remove_list = vec![];
    
    let mut label_to_block = HashMap::new();
    let mut removed_edges = vec![];
    removed_edges.resize(cfg.basic_blocks.len(), vec![]);

    for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
        match function.statements[bb.start] {
            Statement::Label(ref label_id) => {label_to_block.insert(*label_id, bb_id); },
            _ => {},
        }
    }

    for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
        match function.statements[bb.end-1] {
            Statement::JumpIfTrue(Operand::Boolean(val), label_id) => {
                if val {
                    function.statements[bb.end-1] = Statement::Jump(label_id);
                    // remove the next block from adjacency_list, as this is
                    // no longer connected to this block
                    removed_edges[bb_id].push(bb_id + 1);
                    cfg.adjacency_list[bb_id].retain(|v| *v != Adj::Block(bb_id + 1));
                } else {
                    remove_list.push(bb.end-1);
                    let target = label_to_block[&label_id];
                    removed_edges[bb_id].push(target);
                    // remove the target block, as this is no longer reachable from this block
                    cfg.adjacency_list[bb_id].retain(|v| *v != Adj::Block(target));
                }

            },
            _ => {},
        }  
    }

    cfg.remove_statements(function, remove_list);
    update_phi_functions(function, cfg, removed_edges);
}

fn update_phi_functions(
    function: &mut Function,
    cfg: &mut CFG,
    removed_edges: Vec<Vec<usize>>) {

    let reaching_defs = calculate_definitions_reaching_end_of_block(
        function, 
        cfg);


    for (bb_id, bb) in cfg.basic_blocks.iter().enumerate() {
        let parents = cfg.get_parent_blocks(bb_id);
        for s in bb.start..bb.end {
            match function.statements[s] {
                Statement::PhiFunction(
                    Operand::SSAVariable(_, id, _), 
                    ref mut operands) => {
                    remove_non_reaching_defs_from_phi(
                        operands, 
                        &reaching_defs,
                        bb_id,
                        id,
                        &parents);
                },
                _ => {},
    
            }
        }
    }
}

// return vector of vectors, where outer vector is indexed by 
// basic block id, and inner vector contains all the variable id/ssa variable id
// pairs which definition reaches the end of the block
fn calculate_definitions_reaching_end_of_block(
    function: &Function,
    cfg: &CFG) -> Vec<HashMap<u32, u32>> {

    let immediate_dominators = calculate_immediate_dominator_opt(cfg);

    let mut reaching_defs = vec![];
    reaching_defs.resize(cfg.basic_blocks.len(), HashMap::new());
  
    calculate_definitions(function, cfg, 0, &immediate_dominators, &mut reaching_defs);

    reaching_defs
}

fn calculate_definitions(
    function: &Function,
    cfg: &CFG,
    cur_block: usize,
    immediate_dominators: &Vec<Option<usize>>,
    reaching_defs: &mut Vec<HashMap<u32, u32>>) {

    println!("\n\nCalculating definitions for block {}", cur_block+1);
    let mut cur_hashmap = HashMap::new();
    
    // populate the hashmap with parent info
    let parents = cfg.get_parent_blocks(cur_block);
    for parent in parents.iter() {
            println!("Pre-populating with values from parent block {}", parent);
            for (key, value) in reaching_defs[*parent].iter() {
                cur_hashmap.insert(*key, *value);                
            }
    }

    println!("After pre-population: {:?}", cur_hashmap);

    // then replace the values with local declarations
    for i in cfg.basic_blocks[cur_block].start..cfg.basic_blocks[cur_block].end {

        match function.statements[i] {
              Statement::PhiFunction(
                Operand::SSAVariable(_, id, ssa_id), 
                _) |
            Statement::Assignment(
                _, 
                Some(Operand::SSAVariable(_, id, ssa_id)), 
                _, 
                _) => {
                println!("Found local definition {}:{} from {}", id, ssa_id, function.statements[i]);
                cur_hashmap.insert(id, ssa_id);
            },
            _ => {},
        }
    }

    println!("After update: {:?}\n\n", cur_hashmap);
    reaching_defs[cur_block] = cur_hashmap;
    // finally recursively call this with immediately dominated values
    for block in immediately_dominated_nodes(cur_block, immediate_dominators) {
        calculate_definitions(
            function, 
            cfg, 
            block, 
            immediate_dominators, 
            reaching_defs);
    }
}

fn immediately_dominated_nodes(
    block: usize,
    immediate_dominators: &Vec<Option<usize>>) -> Vec<usize> {
    let mut successor_nodes = vec![];
    for (i, opt_dominator) in immediate_dominators.iter().enumerate() {
        if let Some(dominator) = *opt_dominator {
            if block == 0 && i == 0 {
                continue;
            }
            if dominator == block {
                successor_nodes.push(i);
            }
        }
    }
    successor_nodes
}




fn remove_non_reaching_defs_from_phi(
    operands: &mut Vec<Operand>,
    reaching_defs: &Vec<HashMap<u32, u32>>,
    block_id: usize,
    var_id: u32,
    parents: &Vec<usize>) {    

    println!("\nHandling block {} for variable {}", block_id+1, var_id);
    println!("Parents: {:?}", parents.iter().map(|v| v+1).collect::<Vec<usize>>());
    println!("Reaching defs: {:?}", reaching_defs);


    operands.retain(|v|
        if let Operand::SSAVariable(_, _, ssa_id) = *v {
            for p in parents.iter() {
                if reaching_defs[*p].get(&var_id) == Some(&ssa_id) {
                    println!("Keeping ssa id: {}", ssa_id);
                    return true;
                }   
            }
            println!("Removing ssa id: {}", ssa_id);
            return false;
        } else {
            ice!("Non-SSA variable {} present in phi node", v);
        }
    );

    println!("\n");
}