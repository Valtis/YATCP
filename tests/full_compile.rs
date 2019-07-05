use compiler::frontend::run_frontend;
use compiler::backend::run_backend;

use std::env;
use std::sync::atomic::{AtomicI32, Ordering};

use std::fs::File;
use std::io::prelude::*;
use std::Command;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);

fn compile_and_run_no_opt(program : &str) -> String {
    let mut input_file = env::temp_dir();
    let ctr = FILE_COUNTER.fetch_add(1, Ordering::Relaxed);
    input_file.push(format!("yatcp_test_input_{}.txt", ctr));
    let input_file_str = input_file.to_str().unwrap();

    let mut f = File::create(&input_file)
        .unwrap_or_else(
            |err| panic!("Could not create file {}, err: {}", input_file_str, err));

    f.write_all(program.as_bytes()).expect("Failed to write file contents to file");

    let functions = run_frontend(
        input_file_str.to_string(),
        false,
        false).unwrap();

    let mut output_file = env::temp_dir();
    output_file.push(format!("yatcp_test_output_{}.o", ctr));
    output_file_str = output_file.to_str().unwrap().to_string();
    run_backend(output_file_str, functions, false);





    //Command::new
}

#[test]
fn empty_function_compiles() {
    compile_and_run_no_opt(r#"fn test() : void {} "#);

    assert_eq!(23, 3);
}

