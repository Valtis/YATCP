use tac_generator::Function;
use tac_generator::Statement;

pub mod basic_block;
pub mod dom_front;

use self::basic_block::BasicBlock;
use self::dom_front::calculate_dominance_frontier;

use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

use std::cmp::Ordering;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Adj {
    End,
    Block(usize),
}

impl Display for Adj {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        write!(formatter, "{}", match *self {
            Adj::End => "End".to_string(),
            Adj::Block(id) => (id+1).to_string(), 
        })
    }
}

impl Ord for Adj {
    fn cmp(&self, other: &Adj) -> Ordering {
        match (self, other) {
            (&Adj::End, &Adj::End) => Ordering::Equal,
            (&Adj::Block(_), &Adj::End) => Ordering::Less,
            (&Adj::End, &Adj::Block(_)) => Ordering::Greater,
            (&Adj::Block(val1), &Adj::Block(val2)) => val2.cmp(&val1),
        }
    }
}

impl PartialOrd for Adj {
    fn partial_cmp(&self, other: &Adj) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub struct CFG {
    pub basic_blocks: Vec<BasicBlock>,
    pub adjancency_list: Vec<Vec<Adj>>,
    pub dominance_frontier: Vec<Vec<usize>>,
    pub immediate_dominators: Vec<usize>,
}

impl CFG {
    fn new(
        basic_blocks: Vec<BasicBlock>, 
        adjancency_list: Vec<Vec<Adj>>) -> CFG {
        
        let mut dominance_frontier = vec![];
        dominance_frontier.resize(basic_blocks.len(), vec![]);

        CFG {
            basic_blocks: basic_blocks,
            adjancency_list: adjancency_list,
            dominance_frontier: dominance_frontier,
            immediate_dominators: vec![],
        }        
    }

    pub fn get_parent_blocks(&self, block: usize) -> Vec<usize> {
        get_parents(&self.adjancency_list, block)
    }

    pub fn remove_statements(&mut self, 
        function: &mut Function, 
        mut remove_list: Vec<usize>) {
        for i in 0..remove_list.len() {
            
            
            function.statements.remove(remove_list[i]);

            let mut blocks = 0;
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
    
    pub fn remove_block(
        &mut self, 
        function: &mut Function,
        id: usize) {

        let bb = self.basic_blocks[id].clone();
        let bb_len = bb.end - bb.start; 

        // remove instructions belonging to this block        
        for _ in bb.start..bb.end {
            function.statements.remove(bb.start);
        }

        self.basic_blocks.remove(id);
        for remaining_bb in self.basic_blocks.iter_mut() {
            if remaining_bb.start >= bb.end {                                    
                remaining_bb.start -= bb_len;
                remaining_bb.end -= bb_len;
            }
        }


        // update adjancency_list
        self.adjancency_list.remove(id);
        
        for vec in self.adjancency_list.iter_mut() {
            vec.retain(|v| *v != Adj::Block(id));
            *vec = vec.iter().map(|v| 
                    match *v {             
                        Adj::Block(blk) => {
                            if blk > id { 
                                Adj::Block(blk - 1)
                            } else { 
                                Adj::Block(blk)
                            }
                        }, 
                        Adj::End => Adj::End,
                    }).collect();
        } 
    }    
}

fn get_parents(adjancency_list: &Vec<Vec<Adj>>, block: usize) -> Vec<usize> {
    let mut parents = vec![];
    for (i, bb) in adjancency_list.iter().enumerate() {
        if i == block {
            continue;
        } 

        for b in bb.iter() {
            match *b {
                Adj::Block(id) => {
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

fn insert_if_not_present(vec: &mut Vec<Adj>, val: Adj) {
    if !vec.contains(&val) {
        vec.push(val);
    }
}

pub fn generate_cfg(functions: &mut Vec<Function>) -> HashMap<String, CFG> {
    let mut cfgs = HashMap::new();
    for f in functions.iter_mut() {
        let mut basic_blocks = BasicBlock::construct_basic_blocks(&f);
        let mut adjancency_list = create_adj_list(&basic_blocks, f);

        remove_dead_blocks(&mut basic_blocks, &mut adjancency_list, f);
        cfgs.insert(f.name.clone(), CFG::new(basic_blocks, adjancency_list));
    }

    calculate_dominance_frontier(&mut cfgs);
    cfgs
}

fn create_adj_list(basic_blocks: &Vec<BasicBlock>, f: &Function) -> Vec<Vec<Adj>> {

    let mut label_id_to_bb = HashMap::new();

    let mut pos = 0; 
    for bb in basic_blocks.iter() {
        if let Statement::Label(id) = f.statements[bb.start] {
            label_id_to_bb.insert(id, pos); 
        } 
        pos += 1;
    }


    let mut adjancency_list = vec![];
    adjancency_list.resize(basic_blocks.len(), vec![]);

    pos = 0;

    for bb in basic_blocks.iter() {
        let vec = &mut adjancency_list[pos];
        match f.statements[bb.end-1] {
            Statement::Jump(id) => {
                insert_if_not_present(
                    vec, 
                    Adj::Block(label_id_to_bb[&id]));
            },
            Statement::JumpIfTrue(_, id) => {
                insert_if_not_present(
                    vec, 
                    Adj::Block(label_id_to_bb[&id]));

                insert_if_not_present(
                    vec, 
                    Adj::Block(pos + 1));
            },
            Statement::Return(_) => {
                insert_if_not_present(
                    vec, 
                    Adj::End);
            },
            _ => { 
                if pos == basic_blocks.len() - 1 {
                    insert_if_not_present(
                        vec, 
                        Adj::End);
                } else {
                    insert_if_not_present(
                        vec, 
                        Adj::Block(pos + 1));
                }                         
            },
        } 

        vec.sort();
        pos += 1;
    }
    adjancency_list
}


// ensure that the graph does not have parentless blocks. 
// Parentless blocks may have been created during tac generation
// after peephole optimizations.
fn remove_dead_blocks(
    basic_blocks: &mut Vec<BasicBlock>,
    adjancency_list: &mut Vec<Vec<Adj>>,
    function: &mut Function) {
    let mut changes = true; 
    while changes {
        let mut block_to_kill = None;
        changes = false;
        println!("BB len: {}", basic_blocks.len());

        for (bb_id, _) in basic_blocks.iter().enumerate() {
            if get_parents(adjancency_list, bb_id).is_empty() && bb_id != 0 {
                block_to_kill = Some(bb_id);
                changes = true;
                break;
            }
        }

        if let Some(bb_id) = block_to_kill {

            // TODO: Optimize. 
            // Potentially O(n^2), as we may end up removing lots of instructions
            // from the beginning of the vector
            // Could just copy from statement[bb.end+iter] -> statement[bb.start+iter]
            // and resize
            let bb = basic_blocks[bb_id].clone();
            let bb_len = bb.end - bb.start; 
            for _ in bb.start..bb.end {
                function.statements.remove(bb.start);
            } 

            basic_blocks.remove(bb_id);
            for remaining_bb in basic_blocks.iter_mut() {
                if remaining_bb.start >= bb.end {                                    
                    remaining_bb.start -= bb_len;
                    remaining_bb.end -= bb_len;
                }
            }
            adjancency_list.remove(bb_id);

            // update adjacencies by decrementing ids that are greater than current block id
            for vec in adjancency_list.iter_mut() {
                for adj in vec.iter_mut() {
                    match *adj {
                        Adj::Block(ref mut id) => {
                            if *id > bb_id {
                                *id -= 1;
                            }
                        },
                        _ => {},
                    }
                } 
            }            
        }
    }
}   