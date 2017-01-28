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

#[derive(Debug, PartialEq, Eq)]
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
    pub adjancency_list: HashMap<usize, Vec<Adj>>,
    pub dominance_frontier: HashMap<usize, Vec<usize>>,
}

impl CFG {
    fn new(
        basic_blocks: Vec<BasicBlock>, 
        adjancency_list: HashMap<usize, Vec<Adj>>) -> CFG {
        
        let mut dominance_frontier = HashMap::new();
        for i in 0..basic_blocks.len() {
            dominance_frontier.insert(i, vec![]);
        }

        CFG {
            basic_blocks: basic_blocks,
            adjancency_list: adjancency_list,
            dominance_frontier: dominance_frontier,
        }        
    }
    
}

fn insert_if_not_present(vec: &mut Vec<Adj>, val: Adj) {
    if !vec.contains(&val) {
        vec.push(val);
    }
}

pub fn generate_cfg(functions: &Vec<Function>) -> HashMap<String, CFG> {
    let mut cfgs = HashMap::new();
    for f in functions.iter() {
        let basic_blocks = BasicBlock::construct_basic_blocks(&f);

        let adjancency_list = create_adj_list(&basic_blocks, f);

        cfgs.insert(f.name.clone(), CFG::new(basic_blocks, adjancency_list));
    }

    calculate_dominance_frontier(&mut cfgs);
    cfgs
}

fn create_adj_list(basic_blocks: &Vec<BasicBlock>, f: &Function) -> HashMap<usize, Vec<Adj>> {

    let mut label_id_to_bb = HashMap::new();

    let mut pos = 0; 
    for bb in basic_blocks.iter() {
        if let Statement::Label(id) = f.statements[bb.start] {
            label_id_to_bb.insert(id, pos); 
        } 
        pos += 1;
    }


    let mut adjancency_list = HashMap::new();
    for i in 0..basic_blocks.len() {
        adjancency_list.insert(i, vec![]);
    }

    pos = 0;

    for bb in basic_blocks.iter() {
        let vec = adjancency_list.get_mut(&pos).unwrap();
        match f.statements[bb.end-1] {
            Statement::Jump(id) => {
                insert_if_not_present(
                    vec, 
                    Adj::Block(label_id_to_bb[&id]));
            },
            Statement::JumpIfTrue(_, id) => {
                ; 
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
