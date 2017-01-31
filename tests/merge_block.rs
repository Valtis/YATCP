extern crate compiler;

use compiler::cfg::Adj;
use compiler::cfg::basic_block::BasicBlock;
use compiler::cfg::CFG;
use compiler::cfg::generate_cfg;
use compiler::tac_generator::Function;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Operand;
use compiler::tac_generator::Operator;

use compiler::optimizer::merge_block::merge_linear_blocks;




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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
        Statement::Jump(0),
        // block 2
        Statement::Assignment(None, Some(Operand::Integer(4)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(5)), None, None),
        Statement::Jump(1),
        // block 3
        Statement::Label(0),
        Statement::Assignment(None, Some(Operand::Integer(6)), None, None),      
        Statement::Assignment(None, Some(Operand::Integer(7)), None, None),
        // block 4,
        Statement::Label(1),
        Statement::Assignment(None, Some(Operand::Integer(8)), None, None),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(2)],
            vec![Adj::Block(3)],
            vec![Adj::Block(3)],
            vec![Adj::End],
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        f.statements[0]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        f.statements[1]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
        f.statements[2]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(6)), None, None),
        f.statements[3]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(7)), None, None),
        f.statements[4]);
    assert_eq!(
        Statement::Jump(1),
        f.statements[5]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(4)), None, None),
        f.statements[6]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(5)), None, None),
        f.statements[7]);
    assert_eq!(
        Statement::Jump(1),
        f.statements[8]);
    assert_eq!(
        Statement::Label(1),
        f.statements[9]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(8)), None, None),
        f.statements[10]);


    assert_eq!(3, cfg.adjacency_list.len());
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[2]);
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
        Statement::Jump(0),
        // block 2
        Statement::Assignment(None, Some(Operand::Integer(4)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(5)), None, None),
        Statement::Jump(1),
        // block 3
        Statement::Label(0),
        Statement::Assignment(None, Some(Operand::Integer(6)), None, None),      
        Statement::Assignment(None, Some(Operand::Integer(7)), None, None),
        // block 4,        
        Statement::Assignment(None, Some(Operand::Integer(8)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(9)), None, None),
        // block 5       
        Statement::Label(1),
        Statement::Assignment(None, Some(Operand::Integer(10)), None, None),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(2)],
            vec![Adj::Block(4)],
            vec![Adj::Block(3)],
            vec![Adj::Block(4)],
            vec![Adj::End],
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        f.statements[0]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        f.statements[1]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
        f.statements[2]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(6)), None, None),
        f.statements[3]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(7)), None, None),
        f.statements[4]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(8)), None, None),
        f.statements[5]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(9)), None, None),
        f.statements[6]);
    assert_eq!(
        Statement::Jump(1),
        f.statements[7]);


    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(4)), None, None),
        f.statements[8]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(5)), None, None),
        f.statements[9]);
    assert_eq!(
        Statement::Jump(1),
        f.statements[10]);
    assert_eq!(
        Statement::Label(1),
        f.statements[11]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(10)), None, None),
        f.statements[12]);


    assert_eq!(3, cfg.adjacency_list.len());
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[2]);
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
        Statement::Jump(0),
        // block 2
        Statement::Assignment(None, Some(Operand::Integer(4)), None, None),
        Statement::Assignment(None, Some(Operand::Integer(5)), None, None),
        Statement::Jump(1),
        // block 3
        Statement::Label(0),
        // block 4,
        Statement::Label(1),
        Statement::Assignment(None, Some(Operand::Integer(8)), None, None),
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(2)],
            vec![Adj::Block(3)],
            vec![Adj::Block(3)],
            vec![Adj::End],
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        f.statements[0]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(2)), None, None),
        f.statements[1]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(3)), None, None),
        f.statements[2]);
    assert_eq!(
        Statement::Jump(1),
        f.statements[3]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(4)), None, None),
        f.statements[4]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(5)), None, None),
        f.statements[5]);
    assert_eq!(
        Statement::Jump(1),
        f.statements[6]);
    assert_eq!(
        Statement::Label(1),
        f.statements[7]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(8)), None, None),
        f.statements[8]);


    assert_eq!(3, cfg.adjacency_list.len());
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[0]);
    assert_eq!(vec![Adj::Block(2)], cfg.adjacency_list[1]);
    assert_eq!(vec![Adj::End], cfg.adjacency_list[2]);
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::Jump(0),
        // block 2
        Statement::Label(0),
        Statement::Assignment(None, Some(Operand::Integer(6)), None, None),      
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(1)],
            vec![Adj::End],
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        f.statements[0]);
    assert_eq!(
        Statement::Assignment(None, Some(Operand::Integer(6)), None, None),
        f.statements[1]);

    assert_eq!(1, cfg.adjacency_list.len());
    assert_eq!(vec![Adj::End], cfg.adjacency_list[0]);
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        Statement::Jump(0),
        // block 2
        Statement::Label(0),   
    ];

    let mut f = Function {
        name: "foo".to_string(),
        statements: statements,
    };

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
            vec![Adj::Block(1)],
            vec![Adj::End],
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
        Statement::Assignment(None, Some(Operand::Integer(1)), None, None),
        f.statements[0]);

    assert_eq!(1, cfg.adjacency_list.len());
    assert_eq!(vec![Adj::End], cfg.adjacency_list[0]);
}








// TODO: Test cases for when the child block has conditional jump to another block
// ---> check this is handled correctly after merge

// TODO: child block is connected to end block -> jump generated correctly