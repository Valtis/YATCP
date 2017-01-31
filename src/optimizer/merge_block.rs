use cfg::Adj;
use cfg::CFG;
use tac_generator::Function;
use tac_generator::Statement;

pub fn merge_linear_blocks(
    function: &mut Function, 
    cfg: &mut CFG) {

    let mut changes = true;
    'outer: while changes {
        changes = false;
        for id in 0..cfg.basic_blocks.len() {
            if cfg.adjacency_list[id].len() == 1 {

                let child = if let Adj::Block(val) = cfg.adjacency_list[id][0] {
                    val
                } else {
                    continue;
                };

                if cfg.get_parent_blocks(child).len() == 1 {
                    merge_blocks(function, cfg, id, child);
                    changes = true;
                    continue 'outer; 
                }
            }
        }
    }
}

fn merge_blocks(
    function: &mut Function, 
    cfg: &mut CFG,
    mut parent: usize,
    child: usize) {
    println!("Parent: {}  Child: {}", parent, child);
    println!("CFG before merge:");
    print_cfg(function, cfg);


    println!("BBs before operations:\n") ;

    for bb in cfg.basic_blocks.iter() {
        println!("    {:?}", bb);
    }

    // copy instructions from child start - end to parent-end
    // update all blocks to take account new instructions
    // update adj list
    // erase child block
    // update all blocks to take account deletion (maybe merge with first update)

    let parent_block = cfg.basic_blocks[parent].clone();
    let child_block = cfg.basic_blocks[child].clone();

    println!("\n\nParent block: {:?}, len: {}", parent_block, function.statements.len());
    println!("Child block: {:?}", child_block);

    let mut remove_list = vec![];

    // first, remove the potential jump and label, as these are not needed after merge
    match function.statements[parent_block.end-1] {
        Statement::Jump(_) |
        Statement::JumpIfTrue(_, _) =>
            remove_list.push(parent_block.end-1),
        _ => {},
    }  

    if let Statement::Label(_) = function.statements[child_block.start] {
        remove_list.push(child_block.start);
    }

    println!("Removal: {:?}", remove_list);
    cfg.remove_statements(function, remove_list);        


    let parent_block = cfg.basic_blocks[parent].clone();
    // update child block info, as this may be out of date after the 
    // instruction removals
    let child_block = cfg.basic_blocks[child].clone();
    println!("Parent block after update: {:?}", parent_block);
    println!("Child block after update: {:?}", child_block);

    println!("Statements after jmp/label removal: {}",  function.statements.len());

    // copy the instructions from the child block and insert them after
    // the parent block. Remove child block
    // TODO: This and the removal are, again, O(n^2) in the worst case
    // -----> really should optimize this

    let mut instructions = vec![];
    for i in child_block.start..child_block.end {
        instructions.push(function.statements[i].clone());
    }

    println!("Instructions: {:?}", instructions);


    println!("Instructions before child removal: {}", function.statements.len());

    println!("BBs before removal:\n") ;

    for bb in cfg.basic_blocks.iter() {
        println!("    {:?}", bb);
    }

    let successors_of_child = cfg.adjacency_list[child].clone();

    cfg.remove_block(function, child);

    // any successor that were after the child block have their index
    // decremented by one. Update the list to account for this 
    println!("Successors before map: {:?}", successors_of_child);
    let successors_of_child = successors_of_child.
        iter().
        map(|v| { let val = if let Adj::Block(id) = *v {
            if id > child {
                Adj::Block(id-1)
            } else {
                Adj::Block(id)
            }
        } else {
            Adj::End
        }; println!("Mapped val: {:?}", val); val } ).collect::<Vec<Adj>>();

    println!("Successors after map: {:?}", successors_of_child);
    println!("BBs after removal:\n") ;

    for bb in cfg.basic_blocks.iter() {
        println!("    {:?}", bb);
    }

    if child < parent {
        parent -= 1;
    }

    println!("Instructions after removal: {}", function.statements.len());
    // update, as might be out of date now
    let parent_block = cfg.basic_blocks[parent].clone();


    // update the adjacency for the parent block to be the adjacency of
    // the child block

    cfg.adjacency_list[parent] = successors_of_child;

    // if we have single successor, it is not the next block, and 
    // there is no jump to this block, append a jump to this block
    let parent_block = cfg.basic_blocks[parent].clone();
    if cfg.adjacency_list[parent].len() == 1 && 
        cfg.adjacency_list[parent][0] != Adj::Block(parent+1) && 
        !(parent == cfg.basic_blocks.len() - 1 && cfg.adjacency_list[parent][0] == Adj::End) {

        let append_jump = if instructions.len() == 0 {
            true
        } else {
            match instructions[instructions.len() - 1].clone() {
                Statement::Jump(_) => {
                    // ok, jump present
                    false
                },
                _ => {
                    true
                }
            }
        };

        if append_jump {
            let successor = if let Adj::Block(blk) = cfg.adjacency_list[parent][0] {
                blk
            } else {
                // TODO: Handle case  where the end block is a 'end'
                // ==> need to create new bb after the last one, and insert a new label there
                // and then generate jump there
                unimplemented!();
            };

            if let Statement::Label(id) = function.statements[cfg.basic_blocks[successor].start] {
                instructions.push(Statement::Jump(id));
            } else {
                // need to insert a new label to this block

                // first find a free label id
                let mut new_label_id = 0;
                for s in function.statements.iter() {
                    if let Statement::Label(id) = *s {
                        if new_label_id > id {
                            new_label_id = id + 1;
                        } 
                    }
                }
                // generate jump to this label id
                instructions.push(Statement::Jump(new_label_id));

                { // borrow check scoping
                    // and finally insert the label to the start of the label
                    let succ = cfg.basic_blocks.get_mut(successor).unwrap();
                    function.statements.insert(succ.start, Statement::Label(new_label_id));
                    succ.end += 1;
                }
                // update the remaining basic blocks
                for i in successor+1..cfg.basic_blocks.len() {
                    let blk = cfg.basic_blocks.get_mut(i).unwrap(); 
                    blk.start += 1;
                    blk.end += 1;
                }

            }
        }
    }

    // pre-emptively update all the blocks start/end points that will be affected
    // by the instruction insertion

    for bb in cfg.basic_blocks.iter_mut() {
        if bb.start >= parent_block.end && *bb != parent_block {
            println!("Updating {} {}", bb.start, bb.end);
            bb.start += instructions.len();
            bb.end += instructions.len();
            println!("Now {} {}", bb.start, bb.end);
        }
    }

    println!("Parent block: {:?}, len: {}", parent_block, function.statements.len());
    // update, as might be out of date now
    let parent_block = cfg.basic_blocks[parent].clone();
    // and update the parent block
    cfg.basic_blocks.get_mut(parent).unwrap().end += instructions.len();




    // finally, insert the instructions

    let mut i = 0;
    println!("Parent block (old) before insertion: {:?}", parent_block);
    for inst in instructions {
        println!("Inserting into {} (len: {})", parent_block.end + i, function.statements.len());
        function.statements.insert(parent_block.end + i, inst);
        i += 1;
    }

    for (i, s) in function.statements.iter().enumerate() {
        println!("{}  {}", i, s);
    }



    println!("\nCFG After merge:");
    print_cfg(function, cfg);

    println!("\nBBs after merge:\n") ;

    for bb in cfg.basic_blocks.iter() {
        println!("    {:?}", bb);
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



