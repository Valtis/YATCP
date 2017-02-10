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

    // copy instructions from child start - end to parent-end
    // update all blocks to take account new instructions
    // update adj list
    // erase child block
    // update all blocks to take account deletion (maybe merge with first update)

    println!("BB count: {}", cfg.basic_blocks.len());
    println!("Adj list, parent ({}): {:?}", parent, cfg.adjacency_list[parent]);
    println!("Adj list, child ({}): {:?}", child, cfg.adjacency_list[child]);

    let parent_block = cfg.basic_blocks[parent].clone();
    let child_block = cfg.basic_blocks[child].clone();

    let mut remove_list = vec![];

    // first, remove the potential jump and label, as these are not needed after merge
    if parent_block.end - parent_block.start != 0 {
        match function.statements[parent_block.end-1] {
            Statement::Jump(_) |
            Statement::JumpIfTrue(_, _) =>
                remove_list.push(parent_block.end-1),
            _ => {},
        }
    }

    if child_block.end - child_block.start != 0 {
        if let Statement::Label(_) = function.statements[child_block.start] {
            remove_list.push(child_block.start);
        }
    }
    cfg.remove_statements(function, remove_list);

    // update child block info, as this may be out of date after the
    // instruction removals
    let child_block = cfg.basic_blocks[child].clone();
    // copy the instructions from the child block and insert them after
    // the parent block. Remove child block
    // TODO: This and the removal are, again, O(n^2) in the worst case
    // -----> really should optimize this

    let mut instructions = vec![];
    for i in child_block.start..child_block.end {
        instructions.push(function.statements[i].clone());
    }

    let child_adjacency = cfg.adjacency_list[child].clone();

    cfg.remove_block(function, child);

    // any successor that were after the child block have their index
    // decremented by one. Update the list to account for this
    let child_adjacency = child_adjacency.
        iter().
        map(|v| { let val = if let Adj::Block(id) = *v {
            if id > child {
                Adj::Block(id-1)
            } else {
                Adj::Block(id)
            }
        } else {
            Adj::End
        }; val } ).collect::<Vec<Adj>>();

    if child < parent {
        parent -= 1;
    }

    update_adjacency_list(
        function,
        cfg,
        &mut instructions,
        parent,
        child_adjacency);

    let parent_block = cfg.basic_blocks[parent].clone();
    // pre-emptively update all the blocks start/end points that will be affected
    // by the instruction insertion

    for bb in cfg.basic_blocks.iter_mut() {
        if bb.start >= parent_block.end && *bb != parent_block {

            bb.start += instructions.len();
            bb.end += instructions.len();
        }
    }

    // update, as might be out of date now
    let parent_block = cfg.basic_blocks[parent].clone();
    // and update the parent block
    cfg.basic_blocks.get_mut(parent).unwrap().end += instructions.len();

    // finally, insert the instructions

    let mut i = 0;
    for inst in instructions {
        function.statements.insert(parent_block.end + i, inst);
        i += 1;
    }
}

fn update_adjacency_list(
    function: &mut Function,
    cfg: &mut CFG,
    instructions: &mut Vec<Statement>,
    parent: usize,
    child_adjacency: Vec<Adj>) {
    cfg.adjacency_list[parent] = child_adjacency;

    if cfg.adjacency_list[parent].len() == 1 {
        handle_single_successor_case(function, cfg, instructions, parent);
    } else if cfg.adjacency_list[parent].len() == 2 {
        handle_two_successors(function, cfg, instructions, parent);
    } else {
       ice!("Merging a block with more than two successors");
    }
}

fn handle_single_successor_case(
    function: &mut Function,
    cfg: &mut CFG,
    child_instructions: &mut Vec<Statement>,
    parent: usize) {

    // if the successor block is the next one, or the successor block is the
    // end block and we are the last block, no need to do anything, can just
    // fall through
    if cfg.adjacency_list[parent][0] == Adj::Block(parent+1) ||
        (parent == cfg.basic_blocks.len() - 1 && cfg.adjacency_list[parent][0] == Adj::End) {
            return;
    }


    let append_jump = if child_instructions.len() == 0 {
        true
    } else {
        match child_instructions[child_instructions.len() - 1].clone() {
            Statement::Jump(_) => {
                // ok, jump present
                false
            },
            Statement::JumpIfTrue(_, _) => {
                // something has gone wrong, conditional jump means two children
                ice!("Invalid conditional jump present when unconditional jump or no jump expected");
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
            child_instructions.push(Statement::Jump(id));
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
            child_instructions.push(Statement::Jump(new_label_id));

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

fn handle_two_successors(
    function: &mut Function,
    cfg: &mut CFG,
    child_instructions: &mut Vec<Statement>,
    parent: usize) {
    // if the fallthrough block is the false branch of the conditional jump,
    // do nothing. Otherwise generate a new basic block and add a unconditional
    // jump to the false branch, adding a label if necessary
    let jump_label_id = if let Statement::JumpIfTrue(_, jump_label_id) = child_instructions[child_instructions.len() - 1] {
        jump_label_id
    } else {
        ice!("Missing conditional jump in child block when block has two successors");
    };

    if cfg.adjacency_list[parent].contains(&Adj::Block(parent+1)) {
        if let Statement::Label(follower_label) = function.statements[cfg.basic_blocks[parent+1].start] {

            // if the label the true branch jumps is not in the following branch,
            // the false branch is the following branch. We can bail out without
            // doing anything, as true branch jumps to correct label and false
            // branch correctly falls through to the following basic block.
            if follower_label != jump_label_id {
                return;
            }

        }
    }

    // if this point is reached, the false branch does not follow the merged
    // block. This means we need an unconditional jump after the conditional jump,
    // which jumps to the false branch. This also needs to go into its own basic block


    // insert the new basic block after this block. This creates a bb with size of 0
    cfg.create_block(parent+1);

    // then, we need to get the false block bb_id. This is the block that does *not*
    // have the label, that is present in the jump statement. We also need the label
    // of this block, so that we can generate the jump instruction
    let (false_block, false_label) = get_false_branch_and_label(
        function, cfg, parent, jump_label_id);

    // insert the instruction into bb. This updates bb start/ends, but does not update
    // adjacency information.
    let pos = cfg.basic_blocks[parent+1].start;
    cfg.insert_statement(function, pos, Statement::Jump(false_label));


    // remove the false branch from the merge block, as this is no longer the connected to this
    cfg.adjacency_list[parent].retain(|v| *v != Adj::Block(false_block));

    // and add the new jump block as connected node
    cfg.adjacency_list[parent].push(Adj::Block(parent+1));

    // update the new block adjacency to point towards the false branch.
    cfg.adjacency_list[parent+1].push(Adj::Block(false_block));
}

fn get_false_branch_and_label(
    function: &mut Function,
    cfg: &mut CFG,
    parent: usize,
    jump_label_id: u32) -> (usize, u32) {
    for i in 0..cfg.adjacency_list[parent].len() {
        let adj = cfg.adjacency_list[parent][i].clone();
        if let Adj::Block(adj_block) = adj {
            if let Statement::Label(id) =  function.statements[cfg.basic_blocks[adj_block].start] {
                if id != jump_label_id {
                    // id is not the same than in the true branch, must be the false branch
                    return (adj_block, id);
                }
            } else {
                // Not a label, must be the false branch, as true branch must
                // have a label for the conditional jump.

                // Insert a new label here for the unconditional jump

                let mut label_id = 0;
                // find a free label id
                for b in cfg.basic_blocks.iter() {
                    if let Statement::Label(cur_id) =  function.statements[b.start] {
                        if cur_id >= label_id {
                            label_id = cur_id + 1;
                        }
                    }
                }

                let false_block_start = cfg.basic_blocks[adj_block].start;
                cfg.insert_statement(function, false_block_start, Statement::Label(label_id));

                return (adj_block, label_id);
            }

        }
    }

    ice!("Failed to determine the false branch of conditional jump when merging block with conditional jump");
}