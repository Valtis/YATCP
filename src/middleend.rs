use crate::tac_generator::Function;
use crate::ssa_generator::convert_to_ssa;
use crate::ssa_generator::destroy_ssa;

use crate::cfg::generate_cfg;
use crate::cfg::check_cfg::check_cfg;
use crate::cfg::CFG;

use crate::error_reporter::ErrorReporter;

use crate::optimizer::optimize;

use took::Timer;

use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

pub fn run_middleend(
    mut functions: Vec<Function>,
    run_optimizer: bool,
    print_tac: bool,
    print_cfg: bool,
    print_timings: bool,
    error_reporter: Rc<RefCell<dyn ErrorReporter>>) -> Option<Vec<Function>> {


    let timer = Timer::new();
    let mut cfg = generate_cfg(&mut functions);
    if print_timings {
        println!("CFG generation took {}", timer.took());
    }

    let timer = Timer::new();
    let mut functions = check_cfg(functions, &mut cfg, error_reporter.clone());
    if print_timings {
        println!("CFG analysis took {}", timer.took());
    }

    if error_reporter.borrow().has_reports() {
        error_reporter.borrow().print_errors();
        if error_reporter.borrow().has_errors() {
            return None;
        }
    }


    if print_cfg {
        println!("Unoptimized cfg:");
        print_function_cfgs(&cfg, &functions);
    }

    if run_optimizer {
        let timer = Timer::new();
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

        println!("Optimization passes took {}", timer.took());
    }
    Some(functions)
}




fn print_function_cfgs(cfg: &HashMap<Rc<String>, CFG>, functions: &Vec<Function>) {

    for f in functions {
        let mut counter = 1;
        println!("Function {}", f.function_info.name);
        for bb in cfg[&f.function_info.name].basic_blocks.iter() {
            println!("<BB {}>", counter);
            for i in bb.start..bb.end {
                println!("    {}", f.statements[i])
            }
            counter += 1;
        }

        println!("adjacency:\n");

        for i in 0..cfg[&f.function_info.name].basic_blocks.len()  {
            let mut adj_str = cfg[&f.function_info.name].adjacency_list[i].
                iter().
                fold(String::new(), |acc, ref val| format!("{}, {}", val, acc));

            adj_str.pop(); adj_str.pop(); // remove last comma + space
            if adj_str.is_empty() {
                adj_str = "<None>".to_string();
            }
            println!("{}: {}", i+1, adj_str);

        }
        println!("\nDominance frontiers\n");
        for i in 0..cfg[&f.function_info.name].dominance_frontier.len() {
            println!("{}: {:?}", i+1, cfg[&f.function_info.name].dominance_frontier[i].iter().map(|v| v + 1).collect::<Vec<usize>>());
        }


        println!("\n");
    }
}
