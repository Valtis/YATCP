
use cfg::CFG; 
use cfg::Adj;

use std::collections::HashMap;
use std::collections::HashSet;


pub fn calculate_dominance_frontier(func_cfgs: &mut HashMap<String, CFG>) {

    for (ref name, ref cfg) in func_cfgs.iter() {
        let idom = calculate_immediate_dominator(cfg);


        let mut dominance_frontier = vec![];
        dominance_frontier.resize(cfg.basic_blocks.len(), vec![]);

        for bb in 0..cfg.basic_blocks.len() {
            let parents = get_parents(&cfg.adjancency_list, bb);
            if parents.len() >= 2 {
                for p in parents.iter() {
                    let mut runner = *p;
                    while runner != idom[bb] {
                        dominance_frontier[runner].push(bb);
                        runner = idom[runner];
                    }
                }
            }
        }        

        for i in 0..dominance_frontier.len() {
            println!("Dominance frontier for {}: {:?}", i, dominance_frontier[i]);
        } 

        // the dominators contain the list of dominators for given nodes.


        let dominators = calculate_dominators(cfg);
        println!("Dominators for {}", name);
        for i in 0..cfg.basic_blocks.len() {
            println!("    Block {}: {:?}", i, dominators[&i]);
        }
        println!("");
    }
}

// find the list of nodes that dominate the given node
fn calculate_dominators(cfg: &CFG) -> HashMap<usize, HashSet<usize>> {
    let mut dominators = HashMap::new();
    for i in 0..cfg.basic_blocks.len() {
        dominators.insert(i, HashSet::new());
    }

    // entry block dominates itself
    dominators.get_mut(&0).unwrap().insert(0);

    for i in 1..cfg.basic_blocks.len() {
        // initially other blocks are dominated by all the blocks
        dominators.get_mut(&i).unwrap().extend(0..cfg.basic_blocks.len());
    }

    let mut changes = true;

    while changes {
        changes = false;
        for i in 1..cfg.basic_blocks.len() {
            let len = dominators[&i].len();

            let parents =  get_parents(&cfg.adjancency_list, i);

            // iteratively set dominator of i as intersection of parent dominators and the i
            if parents.len() > 0 {
                let mut parent_set = dominators[&parents[0]].clone();
                for parent in parents.iter().skip(1) {
                    parent_set = parent_set.intersection(&dominators[parent]).cloned().collect();
                }
                parent_set.insert(i);
                *dominators.get_mut(&i).unwrap() = parent_set;
                
                // if any of the dominators lists have changed, new iteration
                // is required
                changes = changes || dominators[&i].len() != len;
            }
        }
    }
    dominators
}

fn calculate_immediate_dominator(cfg: &CFG) -> Vec<usize> {
    let mut opt_idom = vec![];
    let mut parents = vec![];
    for bb in 0..cfg.basic_blocks.len() {
        opt_idom.push(None);
        parents.push(get_parents(&cfg.adjancency_list, bb));
    }


    let reverse_postorder = calculate_reverse_post_order(cfg);
    // node pos stores the value when the node was visited during the post-order
    // traveral of the graph (not reverse post-order!). If there are 6 nodes in the
    // graph, the entry node would hold the value '5', one of its children the value '4'
    // etc.
    // this is used during intersect to determine which node is smaller/larger

    let mut node_pos = vec![];
    node_pos.resize(reverse_postorder.len(), 0);
    for i in 0..reverse_postorder.len() {
        node_pos[reverse_postorder[i]] = (reverse_postorder.len() - 1) - i;
    }

    println!("Rev postorder: {:?}", reverse_postorder);
    println!("node_pos: {:?}", node_pos);


    opt_idom[0] = Some(0);
    let mut changes = true;

    let mut processed_nodes = HashSet::new();
    processed_nodes.insert(0);

    while changes {
        changes = false;
        for bb in reverse_postorder.iter() {
            if *bb == 0 {
                continue;
            }

            let mut opt_new_idom = None;
            for parent in parents[*bb].iter() {
                if processed_nodes.contains(parent) {
                    opt_new_idom = Some(*parent);
                    break;
                } 
            } 
            let mut new_idom = opt_new_idom.unwrap_or_else(|| {
                ice!("No processed parent node found for bb '{}'", bb)
            });

            for parent in parents[*bb].iter() {
                if new_idom == *parent {
                    continue;    
                }

                if opt_idom[*parent] != None {
                    new_idom = intersect(
                        *parent, 
                        new_idom, 
                        &opt_idom, 
                        &node_pos);
                }
            }

            if Some(new_idom) != opt_idom[*bb] {
                opt_idom[*bb] = Some(new_idom);
                changes = true;
            }

            processed_nodes.insert(*bb);
        }
    }


    for i in 0..opt_idom.len() {
        println!("IDOM({}): {:?}", i, opt_idom[i]);
    }

    opt_idom.
    iter().
    map(|v| v.unwrap_or_else(|| ice!("Unitialized immediate dominator")) ).
    collect()
}

fn intersect(
    parent: usize, 
    new_idom: usize, 
    opt_idom: &Vec<Option<usize>>,
    node_pos: &Vec<usize>) -> usize {
    let mut finger1 = parent;
    let mut finger2 = new_idom;


    while finger1 != finger2 {        
        while node_pos[finger1] < node_pos[finger2] {
            finger1 = opt_idom[finger1].unwrap();
        }
        while node_pos[finger2] < node_pos[finger1] {
            finger2 = opt_idom[finger2].unwrap();
        }
    }
    finger1
}

// depth-first search of the graph, append nodes into 
fn calculate_reverse_post_order(cfg: &CFG) -> Vec<usize> {

    let mut visited = HashSet::new();
    let mut post_order = vec![];
    depth_first_search(0, &mut visited, &mut post_order, cfg);

    post_order.reverse();
    post_order
}
    
fn depth_first_search(node: usize, 
    visited: &mut HashSet<usize>,
    post_order: &mut Vec<usize>,
    cfg: &CFG) {
        visited.insert(node);
        for child in cfg.adjancency_list[&node].iter() {
            if let Adj::Block(id) = *child {
                if !visited.contains(&id) {
                    depth_first_search(id, visited, post_order, cfg);
                }   
            }
        }
        post_order.push(node);
}



fn get_parents(adjancency_list: &HashMap<usize, Vec<Adj>>, block: usize) -> Vec<usize> {
    let mut parents = vec![];
    for (i, ref list) in adjancency_list {
        if *i == block {
            continue;
        } 

        for b in list.iter() {
            match *b {
                Adj::Block(id) => {
                    if id == block {
                        if !parents.contains(i) {
                            parents.push(*i);
                            continue;
                        }
                    }
                },
                _ => {},
            }
            
        }        
    } 
    return parents;
}