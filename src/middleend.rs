use crate::tac_generator::Function;
use crate::ssa_generator::convert_to_ssa;
use crate::ssa_generator::destroy_ssa;

use crate::cfg::generate_cfg;
use crate::cfg::CFG;

use crate::optimizer::optimize;


use std::rc::Rc;
use std::collections::HashMap;

pub fn run_middleend(
    mut functions: Vec<Function>,
    print_tac: bool,
    print_cfg: bool) -> Vec<Function> {

    let mut cfg = generate_cfg(&mut functions);

    if print_cfg {
        println!("Unoptimized cfg:");
        print_function_cfgs(&cfg, &functions);
    }
    convert_to_ssa(&mut functions, &mut cfg);
    optimize(&mut functions, &mut cfg);


    destroy_ssa(&mut functions, &mut cfg);

    println!("\nAfter SSA destruction\n");

    if print_cfg {
        println!("Optimized cfg:");
        print_function_cfgs(&cfg, &functions);
    }

    if print_tac {
        println!("Optimized three address code");
        for f in functions.iter() {
            f.print();
        }
    }

    functions
}




fn print_function_cfgs(cfg: &HashMap<Rc<String>, CFG>, functions: &Vec<Function>) {

    for f in functions {
        let mut counter = 1;
        println!("Function {}", f.name);
        for bb in cfg[&f.name].basic_blocks.iter() {
            println!("<BB {}>", counter);
            for i in bb.start..bb.end {
                println!("    {}", f.statements[i])
            }
            counter += 1;
        }

        println!("adjacency:\n");

        for i in 0..cfg[&f.name].basic_blocks.len()  {
            let mut adj_str = cfg[&f.name].adjacency_list[i].
                iter().
                fold(String::new(), |acc, ref val| format!("{}, {}", val, acc));

            adj_str.pop(); adj_str.pop(); // remove last comma + space
            if adj_str.is_empty() {
                adj_str = "<None>".to_string();
            }
            println!("{}: {}", i+1, adj_str);

        }
        println!("\nDominance frontiers\n");
        for i in 0..cfg[&f.name].dominance_frontier.len() {
            println!("{}: {:?}", i+1, cfg[&f.name].dominance_frontier[i].iter().map(|v| v + 1).collect::<Vec<usize>>());
        }


        println!("\n");
    }
}
