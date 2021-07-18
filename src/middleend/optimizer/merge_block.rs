use super::super::cfg::{Adjacency, CFG};
use crate::common::tac_code::{Function, Statement};



// TODO check that dom frontier update accordingly
pub fn merge_linear_blocks(
    function: &mut Function,
    cfg: &mut CFG) {

    let mut changes = true;
    'outer: while changes {
        changes = false;
        for id in 0..cfg.basic_blocks.len() {
            if cfg.adjacency_list[id].len() == 1 {

                let child = if let Adjacency::Block(val) = cfg.adjacency_list[id][0] {
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

    let parent_block = cfg.basic_blocks[parent].clone();
    let child_block = cfg.basic_blocks[child].clone();

    let mut remove_list = vec![];

    // first, remove the potential jump and label, as these are not needed after merge
    if parent_block.end - parent_block.start != 0 {
        match function.statements[parent_block.end-1] {
            Statement::Jump(_) |
            Statement::JumpIfTrue(_, _) |
            Statement::JumpIfFalse(_, _) =>
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
        map(|v| { let val = if let Adjacency::Block(id) = *v {
            if id > child {
                Adjacency::Block(id-1)
            } else {
                Adjacency::Block(id)
            }
        } else {
            Adjacency::End
        }; val } ).collect::<Vec<Adjacency>>();

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
    child_adjacency: Vec<Adjacency>) {
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
    if cfg.adjacency_list[parent][0] == Adjacency::Block(parent+1) ||
        (parent == cfg.basic_blocks.len() - 1 && cfg.adjacency_list[parent][0] == Adjacency::End) {
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
            Statement::JumpIfTrue(_, _) |
            Statement::JumpIfFalse(_, _) => {
                // something has gone wrong, conditional jump means two children
                ice!("Invalid conditional jump present when unconditional jump or no jump expected");
            },
            _ => {
                true
            }
        }
    };

    if append_jump {
        let successor = if let Adjacency::Block(blk) = cfg.adjacency_list[parent][0] {
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

    let jump_label_id = match child_instructions[child_instructions.len() - 1] {
        Statement::JumpIfTrue(_, jump_label_id) |
        Statement::JumpIfFalse(_, jump_label_id) => jump_label_id,
        _ => ice!("Missing conditional jump in child block when block has two successors"),
    };

    if cfg.adjacency_list[parent].contains(&Adjacency::Block(parent+1)) {
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
    cfg.adjacency_list[parent].retain(|v| *v != Adjacency::Block(false_block));

    // and add the new jump block as connected node
    cfg.adjacency_list[parent].push(Adjacency::Block(parent+1));

    // update the new block adjacency to point towards the false branch.
    cfg.adjacency_list[parent+1].push(Adjacency::Block(false_block));
}

#[allow(unreachable_code)] // ide has false positive about what's reachable and shows error, needs explicit unreachable!() to silence. This causes rustc to complain about unreachable code
fn get_false_branch_and_label(
    function: &mut Function,
    cfg: &mut CFG,
    parent: usize,
    jump_label_id: u32) -> (usize, u32) {

    for i in 0..cfg.adjacency_list[parent].len() {
        let adj = cfg.adjacency_list[parent][i].clone();
        if let Adjacency::Block(adj_block) = adj {
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
    unreachable!(); // make IDE happy, does not realize this is unreachable and complains about missing return value.
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::common::{
        tac_code::{Operand},
        node_info::{FunctionInfo, Span },
        types::Type,
    };
    use super::super::super::cfg::basic_block::BasicBlock;

    use std::rc::Rc;

    fn create_function(statements: Vec<Statement>) -> Function {
        Function {
            function_info: FunctionInfo {
                name: Rc::new("foo".to_string()),
                parameters: vec![],
                return_type: Type::Void,
                span: Span {
                    line: 1,
                    column: 1,
                    length: 3
                },
            },
            statements,
            attributes: vec![],
        }
    }

    #[test]
    fn when_merging_two_blocks_where_child_has_fall_through_to_next_block_jump_is_inserted_and_adjacency_updated_after_merge() {

        /*
            block 1 -->|
         ---block 2    |
         |  block 3 <--|
         |->block 4

            3 falls to 4

            merge 1, 3
            after merge, should be

            block 1+3 -->|
         |--block 2      |
         |->block 4 <----|

        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            Statement::Jump(1),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(6)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(7)), left_operand: None, right_operand: None },
            // block 4,
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(8)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 4,
                },
                BasicBlock{
                    start: 4,
                    end: 7,
                },
                BasicBlock{
                    start: 7,
                    end: 10,
                },
                BasicBlock{
                    start: 10,
                    end: 12,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: 2
        // 1: 2
        // 2: End

        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(6, cfg.basic_blocks[0].end);

        assert_eq!(6, cfg.basic_blocks[1].start);
        assert_eq!(9, cfg.basic_blocks[1].end);

        assert_eq!(9, cfg.basic_blocks[2].start);
        assert_eq!(11, cfg.basic_blocks[2].end);

        assert_eq!(11, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            f.statements[1]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            f.statements[2]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(6)), left_operand: None, right_operand: None },
            f.statements[3]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(7)), left_operand: None, right_operand: None },
            f.statements[4]);
        assert_eq!(
            Statement::Jump(1),
            f.statements[5]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            f.statements[6]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            f.statements[7]);
        assert_eq!(
            Statement::Jump(1),
            f.statements[8]);
        assert_eq!(
            Statement::Label(1),
            f.statements[9]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(8)), left_operand: None, right_operand: None },
            f.statements[10]);


        assert_eq!(3, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[2]);
    }

    #[test]
    fn merge_handles_case_where_successor_block_has_no_label_when_inserting_jumps() {

        /*
            block 1 -->|
         ---block 2    |
         |  block 3 <--|
         |  block 4
         |->block5
            3 falls to 4

            merge 1, 3
            after merge, should be

            block 1+3+4->|
         |--block 2      |
         |->block 5 <----|

         the interesting case really is the initial merge of blocks 1 & 2,
         as the block 4 does not have label (other optimizations may create blocks like these)

        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            Statement::Jump(1),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(6)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(7)), left_operand: None, right_operand: None },
            // block 4,
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(8)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(9)), left_operand: None, right_operand: None },
            // block 5
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(10)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 4,
                },
                BasicBlock{
                    start: 4,
                    end: 7,
                },
                BasicBlock{
                    start: 7,
                    end: 10,
                },
                BasicBlock{
                    start: 10,
                    end: 12,
                },
                BasicBlock{
                    start: 12,
                    end: 14 ,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(4)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(4)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: 2
        // 1: 2
        // 2: End

        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(8, cfg.basic_blocks[0].end);

        assert_eq!(8, cfg.basic_blocks[1].start);
        assert_eq!(11, cfg.basic_blocks[1].end);

        assert_eq!(11, cfg.basic_blocks[2].start);
        assert_eq!(13, cfg.basic_blocks[2].end);

        assert_eq!(13, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            f.statements[1]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            f.statements[2]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(6)), left_operand: None, right_operand: None },
            f.statements[3]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(7)), left_operand: None, right_operand: None },
            f.statements[4]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(8)), left_operand: None, right_operand: None },
            f.statements[5]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(9)), left_operand: None, right_operand: None },
            f.statements[6]);
        assert_eq!(
            Statement::Jump(1),
            f.statements[7]);


        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            f.statements[8]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            f.statements[9]);
        assert_eq!(
            Statement::Jump(1),
            f.statements[10]);
        assert_eq!(
            Statement::Label(1),
            f.statements[11]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(10)), left_operand: None, right_operand: None },
            f.statements[12]);


        assert_eq!(3, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[2]);
    }

    #[test]
    fn when_merging_two_blocks_where_child_has_only_a_label_and_has_fall_through_to_next_block_jump_is_inserted_and_adjacency_updated_after_merge() {

        /*
            block 1 -->|
         ---block 2    |
         |  block 3 <--|
         |->block 4

            3 falls to 4

            merge 1, 3
            after merge, should be

            block 1+3 -->|
         |--block 2      |
         |->block 4 <----|

        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            Statement::Jump(1),
            // block 3
            Statement::Label(0),
            // block 4,
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(8)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 4,
                },
                BasicBlock{
                    start: 4,
                    end: 7,
                },
                BasicBlock{
                    start: 7,
                    end: 8,
                },
                BasicBlock{
                    start: 8,
                    end: 10,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: 2
        // 1: 2
        // 2: End

        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(4, cfg.basic_blocks[0].end);

        assert_eq!(4, cfg.basic_blocks[1].start);
        assert_eq!(7, cfg.basic_blocks[1].end);

        assert_eq!(7, cfg.basic_blocks[2].start);
        assert_eq!(9, cfg.basic_blocks[2].end);

        assert_eq!(9, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            f.statements[1]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            f.statements[2]);
        assert_eq!(
            Statement::Jump(1),
            f.statements[3]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            f.statements[4]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            f.statements[5]);
        assert_eq!(
            Statement::Jump(1),
            f.statements[6]);
        assert_eq!(
            Statement::Label(1),
            f.statements[7]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(8)), left_operand: None, right_operand: None },
            f.statements[8]);


        assert_eq!(3, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[2]);
    }


    #[test]
    fn merging_two_successive_blocks_where_child_is_connected_to_end_works() {

        /*
            block 1 -->|
            block 2 <--|


            block 1+2
        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(6)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
                BasicBlock{
                    start: 2,
                    end: 4,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: End

        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);
        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(6)), left_operand: None, right_operand: None },
            f.statements[1]);

        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[0]);
    }

    #[test]
    fn merging_two_successive_blocks_where_child_is_connected_to_end_and_has_only_label_works() {

        /*
            block 1 -->|
            block 2 <--|


            block 1+2
        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Label(0),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
                BasicBlock{
                    start: 2,
                    end: 3,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: End

        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(1, cfg.basic_blocks[0].end);

        assert_eq!(1, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);

        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[0]);
    }

    #[test]
    fn merge_of_two_successive_blocks_where_child_has_conditional_jump_and_false_branch_will_follow_the_merged_block_is_correct() {
        /*
            Block 1
            Block 2--|
         |->Block 3  |
         |--Block 4<-|


        Block 1: Fallthrough to 2
        Block 2: Conditional jump to 4, fallthrough to 3,
        Block 3: Fallthrough to 4
        Block 4: Unconditional jump to 3

        After merge, should be

        Block 1 + 2 --|
     |->Block 3       |
     |--Block 4<------|

        Block 1 + 2: Fallthrough to 3, conditional jump to 4
        Block 3: Fallthrough to 4,
        Block 4: Unconditional jump to 3
        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            // block 2
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            // block 4:
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            Statement::Jump(0),
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 1,
                },
                BasicBlock{
                    start: 1,
                    end: 3,
                },
                BasicBlock{
                    start: 3,
                    end: 5,
                },
                BasicBlock{
                    start: 5,
                    end: 8,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1)],
                vec![Adjacency::Block(2), Adjacency::Block(3)],
                vec![Adjacency::Block(3), ],
                vec![Adjacency::Block(2)],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: End

        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);

        assert_eq!(3, cfg.basic_blocks[1].start);
        assert_eq!(5, cfg.basic_blocks[1].end);

        assert_eq!(5, cfg.basic_blocks[2].start);
        assert_eq!(8, cfg.basic_blocks[2].end);

        assert_eq!(8, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            f.statements[1]);

        assert_eq!(
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            f.statements[2]);

        assert_eq!(
            Statement::Label(0),
            f.statements[3]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            f.statements[4]);

        // this + next op is kinda silly, but that can/will be killed by a separate pass
        assert_eq!(
            Statement::Label(1),
            f.statements[5]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            f.statements[6]);

        assert_eq!(
            Statement::Jump(0),
            f.statements[7]);


        assert_eq!(3, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1)], cfg.adjacency_list[2]);
    }

    #[test]
    fn merge_of_block_with_conditional_jump_to_block_that_follows_the_original_child_block_is_correct() {
        /*
            Block 1--|
         |->Block 2  | --|
         |--Block 3<-|   |
            Block 4 <----|

        Block 1: Unconditional jump to 3
        Block 2: Unconditional jump to to 4
        Block 3: Conditional jump to 2
        Block 4: Connects to end

        After merge, should be

        Block 1 + 3--|
     ---Block 2      |
     |  Block 4 <----|
     |->Block 5

        Block 1 + 3: Conditional jump to 4, fallthrough to 2
        Block 2: Unconditional jump to 5
        Block 4: Fallthrough to 5
        Block 5: End

        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            Statement::Jump(2),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 4:
            Statement::Label(2),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
                BasicBlock{
                    start: 2,
                    end: 5,
                },
                BasicBlock{
                    start: 5,
                    end: 8,
                },
                BasicBlock{
                    start: 8,
                    end: 10,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(3)],
                vec![Adjacency::Block(1), Adjacency::Block(3)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: End

        assert_eq!(4, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);

        assert_eq!(3, cfg.basic_blocks[1].start);
        assert_eq!(4, cfg.basic_blocks[1].end);

        assert_eq!(4, cfg.basic_blocks[2].start);
        assert_eq!(7, cfg.basic_blocks[2].end);

        assert_eq!(7, cfg.basic_blocks[3].start);
        assert_eq!(9, cfg.basic_blocks[3].end);

        assert_eq!(9, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            f.statements[1]);

        assert_eq!(
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            f.statements[2]);

        assert_eq!(
            Statement::Jump(2),
            f.statements[3]);

        assert_eq!(
            Statement::Label(1),
            f.statements[4]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            f.statements[5]);

        // this + next op is kinda silly, but that can/will be killed by a separate pass
        assert_eq!(
            Statement::Jump(2),
            f.statements[6]);

        assert_eq!(
            Statement::Label(2),
            f.statements[7]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            f.statements[8]);


        assert_eq!(4, cfg.adjacency_list.len());
        // blocks are likely no longer in order. Sorting makes the testing easier
        cfg.adjacency_list[0].sort();
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);
    }

    #[test]
    fn merge_of_block_with_conditional_jump_where_false_branch_requires_jump_but_target_block_has_no_label_works() {
        /*
            Block 1--|
         |->Block 2  | --|
         |--Block 3<-|   |
            Block 4      |
            Block 5<-----|

        Block 1: Unconditional jump to 3
        Block 2: Unconditional jump to to 5
        Block 3: Conditional jump to 2, fallthrough to 4
        Block 4: Fallthrough to 5
        Block 5: Connects to end


        After merge, should be

        Block 1 + 3----|
     ---Block 1.5 + 4  |
     |  Block 2<-------|
     |->Block 4
        Block 5

        Block 1 + 3: Fallthrough to 1.5+4, conditional jump to 2
        Block 1.5 + 4: Unconditional jump to 5
        Block 2: Fallthrough to 5
        Block 5: Connects to end

        When initially merging 1 and 3, a label needs to be placed in block 4
        This then gets merged with 1.5 in next pass, put this is ok

        */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            Statement::Jump(2),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 4:
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            // block 5
            Statement::Label(2),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 2,
                },
                BasicBlock{
                    start: 2,
                    end: 5,
                },
                BasicBlock{
                    start: 5,
                    end: 8,
                },
                BasicBlock{
                    start: 8,
                    end: 9,
                },
                BasicBlock{
                    start: 9,
                    end: 11,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(2)],
                vec![Adjacency::Block(4)],
                vec![Adjacency::Block(1), Adjacency::Block(3)],
                vec![Adjacency::Block(4)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);

        // adjacency should now be:
        // 0: End

        assert_eq!(4, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);

        assert_eq!(3, cfg.basic_blocks[1].start);
        assert_eq!(5, cfg.basic_blocks[1].end);

        assert_eq!(5, cfg.basic_blocks[2].start);
        assert_eq!(8, cfg.basic_blocks[2].end);

        assert_eq!(8, cfg.basic_blocks[3].start);
        assert_eq!(10, cfg.basic_blocks[3].end);

        assert_eq!(10, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None },
            f.statements[1]);

        assert_eq!(
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            f.statements[2]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None },
            f.statements[3]);

        assert_eq!(
            Statement::Jump(2),
            f.statements[4]);

        assert_eq!(
            Statement::Label(1),
            f.statements[5]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None },
            f.statements[6]);

        assert_eq!(
            Statement::Jump(2),
            f.statements[7]);

        assert_eq!(
            Statement::Label(2),
            f.statements[8]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(5)), left_operand: None, right_operand: None },
            f.statements[9]);


        assert_eq!(4, cfg.adjacency_list.len());
        // blocks are likely no longer in order. Sorting makes the testing easier
        cfg.adjacency_list[0].sort();
        cfg.adjacency_list[0].sort();
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);
    }

    #[test]
    fn merge_where_parent_block_is_empty_works() {
        /*
        block 1
        block 2
        1 falls through to 2

        after merge, should be:

        block 1 + 2
    */

        let statements = vec![
            // block 1
            // empty intentionally
            // block 2
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 0,
                },
                BasicBlock{
                    start: 0,
                    end: 1,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);


        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(1, cfg.basic_blocks[0].end);

        assert_eq!(1, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);


        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[0]);
    }

    #[test]
    fn merge_where_child_block_is_empty_works() {
        /*
        block 1
        block 2
        1 falls through to 2

        after merge, should be:

        block 1 + 2
    */

        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            // block 2
            // empty intentionally
        ];

        let mut f = create_function(statements);

        let mut cfg = CFG {
            basic_blocks: vec![
                BasicBlock{
                    start: 0,
                    end: 1,
                },
                BasicBlock{
                    start: 1,
                    end: 1,
                },
            ],
            adjacency_list: vec![
                vec![Adjacency::Block(1)],
                vec![Adjacency::End],
            ],
            dominance_frontier: vec![],
            immediate_dominators: vec![],
        };

        merge_linear_blocks(&mut f, &mut cfg);


        assert_eq!(1, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(1, cfg.basic_blocks[0].end);

        assert_eq!(1, f.statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None },
            f.statements[0]);


        assert_eq!(1, cfg.adjacency_list.len());
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[0]);
    }

// TODO: child block is connected to end block -> jump generated correctly

// TODO: child before parent, combinations with above

}
