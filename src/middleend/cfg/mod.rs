use crate::common::tac_code::{Function, Statement};

pub mod basic_block;
pub mod dom_front;
pub mod check_cfg;

use self::basic_block::BasicBlock;
use self::dom_front::calculate_dominance_frontier;

use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::cmp::Ordering;
use std::collections::HashSet;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Adjacency {
    End,
    Block(usize),
}

impl Display for Adjacency {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Adjacency::End => "End".to_string(),
            Adjacency::Block(id) => (id+1).to_string(),
        })
    }
}

impl Ord for Adjacency {
    fn cmp(&self, other: &Adjacency) -> Ordering {
        match (self, other) {
            (&Adjacency::End, &Adjacency::End) => Ordering::Equal,
            (&Adjacency::Block(_), &Adjacency::End) => Ordering::Less,
            (&Adjacency::End, &Adjacency::Block(_)) => Ordering::Greater,
            (&Adjacency::Block(val1), &Adjacency::Block(val2)) => val1.cmp(&val2),
        }
    }
}

impl PartialOrd for Adjacency {
    fn partial_cmp(&self, other: &Adjacency) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
#[derive(Clone, Debug)]
pub struct CFG {
    pub basic_blocks: Vec<BasicBlock>,
    pub adjacency_list: Vec<Vec<Adjacency>>,
    pub dominance_frontier: Vec<Vec<usize>>,
    pub immediate_dominators: Vec<usize>,
}

impl CFG {
    fn new(
        basic_blocks: Vec<BasicBlock>,
        adjacency_list: Vec<Vec<Adjacency>>) -> CFG {

        let mut dominance_frontier = vec![];
        dominance_frontier.resize(basic_blocks.len(), vec![]);

        CFG {
            basic_blocks,
            adjacency_list,
            dominance_frontier,
            immediate_dominators: vec![],
        }
    }

    pub fn get_parent_blocks(&self, block: usize) -> Vec<usize> {
        get_parents(&self.adjacency_list, block)
    }

    pub fn insert_statement(&mut self,
        function: &mut Function,
        position: usize,
        statement: Statement) {

        function.statements.insert(position, statement);

        for i in 0..self.basic_blocks.len() {
            // insertion into this block -> increment end. For blocks after this, increment start & end
            if (self.basic_blocks[i].start <= position && self.basic_blocks[i].end > position) ||
                (self.basic_blocks[i].start == self.basic_blocks[i].end && self.basic_blocks[i].start == position) {

                self.basic_blocks[i].end += 1;
                for j in i+1..self.basic_blocks.len() {
                    self.basic_blocks[j].start += 1;
                    self.basic_blocks[j].end += 1;
                }
                return;
            }
        }
    }

    pub fn remove_statements(&mut self,
        function: &mut Function,
        mut remove_list: Vec<usize>) {
        for i in 0..remove_list.len() {


            function.statements.remove(remove_list[i]);

            for bb in self.basic_blocks.iter_mut() {
                if bb.start > remove_list[i] {
                    bb.start -= 1;
                    bb.end -= 1;
                } else if bb.start <= remove_list[i] && bb.end > remove_list[i] {
                    bb.end -= 1;
                }
            }

            for j in (i+1)..remove_list.len() {
                remove_list[j] -= 1;
            }
        }
    }

    pub fn create_block(&mut self, position: usize) {
        let (start, end) = if position == 0 {
            (0, 0)
        } else if position <= self.basic_blocks.len() {
            let prev_end = self.basic_blocks[position - 1].end;
            (prev_end, prev_end)
        } else {
            ice!("Out of bounds basic block insertion: Insertion at position {} when only {} basic blocks exists (one past end is allowed)", position, self.basic_blocks.len());
        };

        self.basic_blocks.insert(position, BasicBlock {
            start: start,
            end: end,
        });
        self.adjacency_list.insert(position, vec![]);

        for adj_list in self.adjacency_list.iter_mut() {
            *adj_list = adj_list.iter().map(|v|
                if let Adjacency::Block(block_id) = *v {
                    if block_id >= position {
                        Adjacency::Block(block_id+1)
                    } else {
                        v.clone()
                    }
                } else {
                    v.clone()
                }).collect();
        }
    }

    pub fn remove_block(
        &mut self,
        function: &mut Function,
        removed_block_id: usize) {

        let bb = self.basic_blocks[removed_block_id].clone();
        let bb_len = bb.end - bb.start;

        // remove instructions belonging to this block
        for _ in bb.start..bb.end {
            function.statements.remove(bb.start);
        }

        // update block starts and ends for removed instructions
        self.basic_blocks.remove(removed_block_id);
        for remaining_bb in self.basic_blocks.iter_mut() {
            if remaining_bb.start >= bb.end {
                remaining_bb.start -= bb_len;
                remaining_bb.end -= bb_len;
            }
        }


        // remove the erased block's adjacency_list
        self.adjacency_list.remove(removed_block_id);

        // remove the erased block from other adjacency lists
        for vec in self.adjacency_list.iter_mut() {

             vec.retain(|v| *v != Adjacency::Block(removed_block_id));
            *vec = vec.iter_mut().map(|v|
                // adjust IDs to account for removal of an earlier block
                match v {
                    Adjacency::Block(blk) if *blk > removed_block_id => {
                        Adjacency::Block(*blk - 1)
                    },
                    block => block.clone(),
                }).collect();
        }
    }

    pub fn blocks_not_connected_to_entry(&self) -> Vec<usize> {

        let mut all_blocks = HashSet::new();

        for bb in 0..self.basic_blocks.len() {
            all_blocks.insert(bb);
        }

        let mut visited = HashSet::new();
        depth_first_search(0, &mut visited, self);


        fn depth_first_search(
            node: usize,
            visited: &mut HashSet<usize>,
            cfg: &CFG) {

            visited.insert(node);
            for child in cfg.adjacency_list[node].iter() {
                if let Adjacency::Block(id) = *child {
                    if !visited.contains(&id) {
                        depth_first_search(id, visited, cfg);
                    }
                }
            }
        }

        all_blocks.difference(&visited).cloned().collect()
    }


    // ensure that the graph does not have orphan blocks.
    // Orphan blocks may have been created during tac generation after peephole optimizations.
    // Also might exist naturally, if there is code after return statement
    fn remove_dead_blocks(&mut self, function: &mut Function) {

        let mut unconnected_blocks = self.blocks_not_connected_to_entry();
        unconnected_blocks.sort();
        // delete in reverse order, so that indices are not invalidated when earlier block is removed
        unconnected_blocks = unconnected_blocks.into_iter().rev().collect();

        for block in unconnected_blocks {
            self.remove_block(function, block);
        }

        ice_if!(
            self.basic_blocks.len() != self.adjacency_list.len(),
            "Adjacency list length does not match with basic block count: {} vs {} ",
            self.adjacency_list.len(),
            self.basic_blocks.len());
    }

}

fn get_parents(adjacency_list: &Vec<Vec<Adjacency>>, block: usize) -> Vec<usize> {
    let mut parents = vec![];
    for (i, bb) in adjacency_list.iter().enumerate() {
        if i == block {
            continue;
        }

        for b in bb.iter() {
            match *b {
                Adjacency::Block(id) => {
                    if id == block {
                        if !parents.contains(&i) {
                            parents.push(i);
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

pub fn generate_cfg(functions: &mut Vec<Function>) -> HashMap<Rc<String>, CFG> {
    let mut cfgs = HashMap::new();
    for f in functions.iter_mut() {
        let basic_blocks = BasicBlock::construct_basic_blocks(&f);
        let adjacency_list = create_adj_list(&basic_blocks, f);

        let mut cfg = CFG::new(basic_blocks, adjacency_list);
        cfg.remove_dead_blocks(f);
        cfgs.insert(f.function_info.name.clone(), cfg);
    }

    calculate_dominance_frontier(&mut cfgs);
    cfgs
}

fn create_adj_list(basic_blocks: &Vec<BasicBlock>, f: &Function) -> Vec<Vec<Adjacency>> {

    let mut label_id_to_bb = HashMap::new();

    let mut pos = 0;
    for bb in basic_blocks.iter() {
        if let Statement::Label(id) = f.statements[bb.start] {
            label_id_to_bb.insert(id, pos);
        }
        pos += 1;
    }


    let mut adjacency_list = vec![];
    adjacency_list.resize(basic_blocks.len(), vec![]);

    pos = 0;

    for bb in basic_blocks.iter() {
        let vec = &mut adjacency_list[pos];
        match f.statements[bb.end-1] {
            Statement::Jump(id) => {
                let id = label_id_to_bb[&id];
                ice_if!(
                    id >= basic_blocks.len(),
                    "Unconditional jump: Attempting to set adjacency to basic block {}, when only {} block(s) exist",
                    id,
                    basic_blocks.len()-1
                );

                insert_if_not_present(
                    vec,
                    Adjacency::Block(id));
            },
            Statement::JumpIfTrue(_, id) |
            Statement::JumpIfFalse(_, id) => {

                let id = label_id_to_bb[&id];

                ice_if!(
                    id >= basic_blocks.len(),
                    "Conditional jump, branch taken: Attempting to set adjacency to basic block {}, when only {} block(s) exist",
                    id,
                    basic_blocks.len()-1);

                insert_if_not_present(
                    vec,
                    Adjacency::Block(id));



                if pos == basic_blocks.len() - 1 {
                    insert_if_not_present(vec, Adjacency::End);
                } else {
                    ice_if!(
                        pos + 1 >= basic_blocks.len(),
                        "Conditional jump, branch not taken: Attempting to set adjacency to basic block {}, when only {} block(s) exist",
                        pos+1,
                        basic_blocks.len()-1);

                    insert_if_not_present(
                        vec,
                        Adjacency::Block(pos + 1));
                }
            },
            Statement::Return(_) => {
                insert_if_not_present(
                    vec,
                    Adjacency::End);
            },
            _ => {
                if pos == basic_blocks.len() - 1 {
                    insert_if_not_present(
                        vec,
                        Adjacency::End);
                } else {
                    ice_if!(
                      pos + 1 >= basic_blocks.len(),
                        "Unconditional jump: Attempting to set adjacency to basic block {}, when only {} block(s) exist",
                        pos+1,
                        basic_blocks.len()-1
                     );

                    insert_if_not_present(
                        vec,
                        Adjacency::Block(pos + 1));
                }
            },
        }


        vec.sort();
        pos += 1;
    }
    adjacency_list
}

fn insert_if_not_present(vec: &mut Vec<Adjacency>, val: Adjacency) {

    if !vec.contains(&val) {
        vec.push(val);
    }
}




#[cfg(test)]
mod tests {

    use super::*;
    use crate::common::{
        node_info::{FunctionInfo, Span},
        types::Type,
        tac_code::{Statement, Function, Operator, Operand},
    };
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
    fn removing_statement_from_the_beginning_of_function_updates_cfg_and_function_correctly() {

        let statements = vec![
            Statement::Label(1),
            Statement::Assignment{
                operator: None,
                destination: None,
                left_operand: None,
                right_operand: None
            },
            Statement::Assignment{
                operator: Some(Operator::Plus),
                destination: None,
                left_operand: None,
                right_operand: None
            },
            Statement::Assignment{
                operator: None,
                destination: None,
                left_operand: None,
                right_operand: None
            },
            Statement::Label(4),
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            Statement::Assignment{
                operator: None,
                destination: None,
                left_operand: None,
                right_operand: None
            },
            Statement::Assignment{
                operator: None,
                destination: None,
                left_operand: None,
                right_operand: None
            },
        ];

        let function = create_function(statements);

        let mut functions = vec![function];
        println!("{:?} ", functions[0].statements);

        let mut cfgs = generate_cfg(&mut functions);
        let cfg = &mut cfgs.get_mut(&Rc::new("foo".to_string())).unwrap();


        let remove_list = vec![0];
        cfg.remove_statements(&mut functions[0], remove_list);

        assert_eq!(7, functions[0].statements.len());

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: Some(Operator::Plus), destination: None, left_operand: None, right_operand: None },
            functions[0].statements[1]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[2]);

        assert_eq!(
            Statement::Label(4),
            functions[0].statements[3]);

        assert_eq!(
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            functions[0].statements[4]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[5]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[6]);

        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);

        assert_eq!(3, cfg.basic_blocks[1].start);
        assert_eq!(5, cfg.basic_blocks[1].end);

        assert_eq!(5, cfg.basic_blocks[2].start);
        assert_eq!(7, cfg.basic_blocks[2].end);

    }

    #[test]
    fn removing_statements_from_multiple_basic_blocks_updates_cfg_and_function_correctly() {

        let statements = vec![
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: Some(Operator::Plus), destination: None, left_operand: None, right_operand: None},
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Label(4),
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
        ];

        let function = create_function(statements);

        let mut functions = vec![function];
        let mut cfgs = generate_cfg(&mut functions);
        let cfg = &mut cfgs.get_mut(&Rc::new("foo".to_string())).unwrap();

        let remove_list = vec![2, 5, 6];
        cfg.remove_statements(&mut functions[0], remove_list);

        assert_eq!(5, functions[0].statements.len());

        assert_eq!(
            Statement::Label(1),
            functions[0].statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[1]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[2]);

        assert_eq!(
            Statement::Label(4),
            functions[0].statements[3]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[4]);


        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);

        assert_eq!(3, cfg.basic_blocks[1].start);
        assert_eq!(4, cfg.basic_blocks[1].end);

        assert_eq!(4, cfg.basic_blocks[2].start);
        assert_eq!(5, cfg.basic_blocks[2].end);
    }

    #[test]
    fn removing_statements_from_multiple_basic_blocks_boundaries_updates_cfg_and_function_correctly() {

        let statements = vec![
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: Some(Operator::Plus), destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Label(4),
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
        ];

        let function = create_function(statements);

        let mut functions = vec![function];
        let mut cfgs = generate_cfg(&mut functions);
        let cfg = &mut cfgs.get_mut(&Rc::new("foo".to_string())).unwrap();

        let remove_list = vec![3, 4, 6];
        cfg.remove_statements(&mut functions[0], remove_list);

        assert_eq!(5, functions[0].statements.len());

        assert_eq!(
            Statement::Label(1),
            functions[0].statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[1]);

        assert_eq!(
            Statement::Assignment{ operator: Some(Operator::Plus), destination: None, left_operand: None, right_operand: None },
            functions[0].statements[2]);

        assert_eq!(
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            functions[0].statements[3]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[4]);

        assert_eq!(3, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(3, cfg.basic_blocks[0].end);

        assert_eq!(3, cfg.basic_blocks[1].start);
        assert_eq!(4, cfg.basic_blocks[1].end);

        assert_eq!(4, cfg.basic_blocks[2].start);
        assert_eq!(5, cfg.basic_blocks[2].end);
    }

    #[test]
    fn removing_statements_from_block_with_size_of_one_updates_cfg_and_function_correctly() {
        let statements = vec![
            Statement::Label(1),
            Statement::Label(2),
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: Some(Operator::Plus), destination: None, left_operand: None, right_operand: None } ,
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Label(4),
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
        ];

        let function = create_function(statements);

        let mut functions = vec![function];
        let mut cfgs = generate_cfg(&mut functions);
        let cfg = &mut cfgs.get_mut(&Rc::new("foo".to_string())).unwrap();

        let remove_list = vec![0];
        cfg.remove_statements(&mut functions[0], remove_list);

        assert_eq!(8, functions[0].statements.len());

        assert_eq!(
            Statement::Label(2),
            functions[0].statements[0]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[1]);

        assert_eq!(
            Statement::Assignment{ operator: Some(Operator::Plus), destination: None, left_operand: None, right_operand: None },
            functions[0].statements[2]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[3]);

        assert_eq!(
            Statement::Label(4),
            functions[0].statements[4]);

        assert_eq!(
            Statement::JumpIfTrue(Operand::Boolean(false), 1),
            functions[0].statements[5]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[6]);

        assert_eq!(
            Statement::Assignment{ operator: None, destination: None, left_operand: None, right_operand: None },
            functions[0].statements[7]);

        assert_eq!(4, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(0, cfg.basic_blocks[0].end);

        assert_eq!(0, cfg.basic_blocks[1].start);
        assert_eq!(4, cfg.basic_blocks[1].end);

        assert_eq!(4, cfg.basic_blocks[2].start);
        assert_eq!(6, cfg.basic_blocks[2].end);

        assert_eq!(6, cfg.basic_blocks[3].start);
        assert_eq!(8, cfg.basic_blocks[3].end);

    }

    /*
    #[test]
    fn removing_empty_block_updates_cfg_and_function_correctly() {
        unimplemented!();
    }

    #[test]
    fn removing_first_block_updates_cfg_and_function_correctly() {
        unimplemented!();
    }

    #[test]
    fn removing_last_block_updates_cfg_and_function_correctly() {
        unimplemented!();
    }

    #[test]
    fn removing_block_from_the_middle_updates_cfg_and_function_correctly() {
        unimplemented!();
    }
    */

    #[test]
    fn creating_new_block_after_a_block_inserts_new_zero_sized_block() {

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

        cfg.create_block(2);

        assert_eq!(5, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, cfg.basic_blocks[1].start);
        assert_eq!(5, cfg.basic_blocks[1].end);

        assert_eq!(5, cfg.basic_blocks[2].start);
        assert_eq!(5, cfg.basic_blocks[2].end);

        assert_eq!(5, cfg.basic_blocks[3].start);
        assert_eq!(8, cfg.basic_blocks[3].end);

        assert_eq!(8, cfg.basic_blocks[4].start);
        assert_eq!(10, cfg.basic_blocks[4].end);


        assert_eq!(5, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(4)], cfg.adjacency_list[1]);
        assert_eq!(vec![] as Vec<Adjacency>, cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(4)], cfg.adjacency_list[3]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[4]);
    }

    #[test]
    fn creating_new_block_as_the_first_block_inserts_new_zero_sized_block() {

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

        cfg.create_block(0);


        assert_eq!(5, cfg.basic_blocks.len());


        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(0, cfg.basic_blocks[0].end);


        assert_eq!(0, cfg.basic_blocks[1].start);
        assert_eq!(2, cfg.basic_blocks[1].end);

        assert_eq!(2, cfg.basic_blocks[2].start);
        assert_eq!(5, cfg.basic_blocks[2].end);

        assert_eq!(5, cfg.basic_blocks[3].start);
        assert_eq!(8, cfg.basic_blocks[3].end);

        assert_eq!(8, cfg.basic_blocks[4].start);
        assert_eq!(10, cfg.basic_blocks[4].end);

        assert_eq!(5, cfg.adjacency_list.len());

        assert_eq!(vec![] as Vec<Adjacency>, cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(4)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::Block(2), Adjacency::Block(4)], cfg.adjacency_list[3]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[4]);
    }

    #[test]
    fn creating_new_block_as_the_last_block_inserts_new_zero_sized_block() {


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

        cfg.create_block(4);

        assert_eq!(5, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, cfg.basic_blocks[1].start);
        assert_eq!(5, cfg.basic_blocks[1].end);

        assert_eq!(5, cfg.basic_blocks[2].start);
        assert_eq!(8, cfg.basic_blocks[2].end);

        assert_eq!(8, cfg.basic_blocks[3].start);
        assert_eq!(10, cfg.basic_blocks[3].end);

        assert_eq!(10, cfg.basic_blocks[4].start);
        assert_eq!(10, cfg.basic_blocks[4].end);


        assert_eq!(5, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);
        assert_eq!(vec![] as Vec<Adjacency>, cfg.adjacency_list[4]);
    }

    #[test]
    fn inserting_statement_into_nonzero_block_updates_bb_info_correctly_and_inserts_the_statement() {
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

        cfg.insert_statement(&mut f, 3, Statement::Label(25));

        assert_eq!(11, f.statements.len());

        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None }, f.statements[0]);
        assert_eq!(Statement::Jump(0), f.statements[1]);
        assert_eq!(Statement::Label(1), f.statements[2]);
        assert_eq!(Statement::Label(25), f.statements[3]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None }, f.statements[4]);
        assert_eq!(Statement::Jump(2), f.statements[5]);
        assert_eq!(Statement::Label(0), f.statements[6]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None }, f.statements[7]);
        assert_eq!(Statement::JumpIfTrue(Operand::Boolean(true), 1), f.statements[8]);
        assert_eq!(Statement::Label(2), f.statements[9]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None}, f.statements[10]);



        assert_eq!(4, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, cfg.basic_blocks[1].start);
        assert_eq!(6, cfg.basic_blocks[1].end);

        assert_eq!(6, cfg.basic_blocks[2].start);
        assert_eq!(9, cfg.basic_blocks[2].end);

        assert_eq!(9, cfg.basic_blocks[3].start);
        assert_eq!(11, cfg.basic_blocks[3].end);


        assert_eq!(4, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);
    }

    #[test]
    fn inserting_statement_into_start_of_nonzero_block_updates_bb_info_correctly_and_inserts_the_statement() {
        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None},
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None},
            Statement::Jump(2),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None},
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 4:
            Statement::Label(2),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None},
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

        cfg.insert_statement(&mut f, 2, Statement::Label(25));

        assert_eq!(11, f.statements.len());

        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None}, f.statements[0]);
        assert_eq!(Statement::Jump(0), f.statements[1]);
        assert_eq!(Statement::Label(25), f.statements[2]);
        assert_eq!(Statement::Label(1), f.statements[3]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None}, f.statements[4]);
        assert_eq!(Statement::Jump(2), f.statements[5]);
        assert_eq!(Statement::Label(0), f.statements[6]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None}, f.statements[7]);
        assert_eq!(Statement::JumpIfTrue(Operand::Boolean(true), 1), f.statements[8]);
        assert_eq!(Statement::Label(2), f.statements[9]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None}, f.statements[10]);



        assert_eq!(4, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, cfg.basic_blocks[1].start);
        assert_eq!(6, cfg.basic_blocks[1].end);

        assert_eq!(6, cfg.basic_blocks[2].start);
        assert_eq!(9, cfg.basic_blocks[2].end);

        assert_eq!(9, cfg.basic_blocks[3].start);
        assert_eq!(11, cfg.basic_blocks[3].end);


        assert_eq!(4, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);
    }

    #[test]
    fn inserting_statement_into_end_of_nonzero_block_updates_bb_info_correctly_and_inserts_the_statement() {
        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None},
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None},
            Statement::Jump(2),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None},
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 4:
            Statement::Label(2),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None},
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

        cfg.insert_statement(&mut f, 4, Statement::Label(25));

        assert_eq!(11, f.statements.len());

        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None}, f.statements[0]);
        assert_eq!(Statement::Jump(0), f.statements[1]);
        assert_eq!(Statement::Label(1), f.statements[2]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None}, f.statements[3]);
        assert_eq!(Statement::Label(25), f.statements[4]);
        assert_eq!(Statement::Jump(2), f.statements[5]);
        assert_eq!(Statement::Label(0), f.statements[6]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None}, f.statements[7]);
        assert_eq!(Statement::JumpIfTrue(Operand::Boolean(true), 1), f.statements[8]);
        assert_eq!(Statement::Label(2), f.statements[9]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None}, f.statements[10]);



        assert_eq!(4, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, cfg.basic_blocks[1].start);
        assert_eq!(6, cfg.basic_blocks[1].end);

        assert_eq!(6, cfg.basic_blocks[2].start);
        assert_eq!(9, cfg.basic_blocks[2].end);

        assert_eq!(9, cfg.basic_blocks[3].start);
        assert_eq!(11, cfg.basic_blocks[3].end);


        assert_eq!(4, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(3)], cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[3]);
    }


    #[test]
    fn inserting_statement_into_empty_block_updates_bb_info_correctly_and_inserts_the_statement() {
        let statements = vec![
            // block 1
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None},
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None},
            Statement::Jump(2),
            // block 3
            Statement::Label(0),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None},
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
            // block 4:
            Statement::Label(2),
            Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None},
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

        cfg.create_block(2);

        cfg.insert_statement(&mut f, 5, Statement::Label(25));

        assert_eq!(11, f.statements.len());

        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(1)), left_operand: None, right_operand: None}, f.statements[0]);
        assert_eq!(Statement::Jump(0), f.statements[1]);
        assert_eq!(Statement::Label(1), f.statements[2]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(2)), left_operand: None, right_operand: None}, f.statements[3]);
        assert_eq!(Statement::Jump(2), f.statements[4]);
        assert_eq!(Statement::Label(25), f.statements[5]);
        assert_eq!(Statement::Label(0), f.statements[6]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(3)), left_operand: None, right_operand: None}, f.statements[7]);
        assert_eq!(Statement::JumpIfTrue(Operand::Boolean(true), 1), f.statements[8]);
        assert_eq!(Statement::Label(2), f.statements[9]);
        assert_eq!(Statement::Assignment{ operator: None, destination: Some(Operand::Integer(4)), left_operand: None, right_operand: None}, f.statements[10]);



        assert_eq!(5, cfg.basic_blocks.len());

        assert_eq!(0, cfg.basic_blocks[0].start);
        assert_eq!(2, cfg.basic_blocks[0].end);

        assert_eq!(2, cfg.basic_blocks[1].start);
        assert_eq!(5, cfg.basic_blocks[1].end);

        assert_eq!(5, cfg.basic_blocks[2].start);
        assert_eq!(6, cfg.basic_blocks[2].end);

        assert_eq!(6, cfg.basic_blocks[3].start);
        assert_eq!(9, cfg.basic_blocks[3].end);

        assert_eq!(9, cfg.basic_blocks[4].start);
        assert_eq!(11, cfg.basic_blocks[4].end);


        assert_eq!(5, cfg.adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(3)], cfg.adjacency_list[0]);
        assert_eq!(vec![Adjacency::Block(4)], cfg.adjacency_list[1]);
        assert_eq!(vec![] as Vec<Adjacency>, cfg.adjacency_list[2]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::Block(4)], cfg.adjacency_list[3]);
        assert_eq!(vec![Adjacency::End], cfg.adjacency_list[4]);
    }


    #[test]
    fn conditional_jump_sets_branch_not_taken_block_if_jump_is_in_last_block() {
        let statements = vec![
            // block 1
            Statement::Jump(0),
            // block 2
            Statement::Label(1),
            Statement::Return(None),
            // block 3
            Statement::Label(0),
            Statement::JumpIfTrue(Operand::Boolean(true), 1),
        ];

        let f = create_function(statements);

        let basic_blocks = vec![
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
        ];

        let adjacency_list = create_adj_list(&basic_blocks, &f);

        assert_eq!(3, adjacency_list.len());

        assert_eq!(vec![Adjacency::Block(2)], adjacency_list[0]);
        assert_eq!(vec![Adjacency::End], adjacency_list[1]);
        assert_eq!(vec![Adjacency::Block(1), Adjacency::End], adjacency_list[2]);
    }
}