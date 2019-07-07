use compiler::frontend::run_frontend;
use compiler::backend::run_backend;

use std::env;
use std::sync::atomic::{AtomicI32, Ordering};

use std::fs::File;
use std::io::prelude::*;
use std::process::Command;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);

enum FunctionKind {
    VOID(String),
    INT(String),
}

impl FunctionKind {
    fn to_define(&self) -> &str {
        match self {
            FunctionKind::VOID(_) => "VOID_FUNCTION",
            FunctionKind::INT(_) => "INT_FUNCTION"
        }
    }

    fn name(&self) -> &String {
        match self {
            FunctionKind::VOID(ref name) |
            FunctionKind::INT(ref name) => name,
        }
    }
}

fn compile_and_run_no_opt(program: &str, kind: FunctionKind) -> String {

    let ctr = FILE_COUNTER.fetch_add(1, Ordering::Relaxed);

    let input_file_str = write_program_into_tmp_file(program, ctr);
    let output_file_str = create_obj_file(ctr, &input_file_str);

    let mut binary_out = env::temp_dir();
    binary_out.push(format!("yatcp_test_binary_{}", ctr));
    let binary_out_str = binary_out.to_str().unwrap().to_string();

    compile_test_binary(kind, output_file_str.clone(), &binary_out_str);
    run_test_binary(&output_file_str, &binary_out_str)
}

fn write_program_into_tmp_file(program: &str, ctr: i32) -> String {
    let mut input_file = env::temp_dir();
    input_file.push(format!("yatcp_test_input_{}.txt", ctr));
    let input_file_str = input_file.to_str().unwrap();
    let mut f = File::create(&input_file)
        .unwrap_or_else(
            |err| panic!("Could not create file {}, err: {}", input_file_str, err));
    f.write_all(program.as_bytes()).expect("Failed to write file contents to file");
    input_file_str.to_owned()
}

fn create_obj_file(ctr: i32, input_file_str: &String) -> String {

    let functions = run_frontend(
        input_file_str.to_string(),
        false,
        false).unwrap();
    let mut output_file = env::temp_dir();
    output_file.push(format!("yatcp_test_output_{}.o", ctr));
    let output_file_str = output_file.to_str().unwrap().to_string();
    run_backend(output_file_str.clone(), functions, false);
    output_file_str
}

fn compile_test_binary(kind: FunctionKind, output_file_str: String, binary_out_str: &String) {
    let output = Command::new("./wrapper.sh")
        .arg(format!("-D{}={}", kind.to_define(), kind.name()))
        .arg("tests/files/support/support.c")
        .arg(output_file_str)
        .arg(format!("-o {}", binary_out_str))
        .output()
        .unwrap_or_else(|err| {
            panic!("Failed to compile the test binary: {}", err);
        });

    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    println!("{}", stdout);


    if !output.status.success() {
        let mut stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let indent = &" ".repeat(8);
        stderr.insert_str(0, indent);
        panic!("Failed to compile the test binary:\n\n{}",
               stderr
                   .replace("\n", &format!("\n{}", indent)));
    }
}

fn run_test_binary(object: &String, binary_out_str: &String) -> String {
    let output = Command::new(binary_out_str)
        .output()
        .unwrap_or_else(|err| {
            panic!("Failed to run the test binary {}: {}", binary_out_str, err);
        });


    if !output.status.success() {

        let indent = &" ".repeat(8);

        let mut stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        stdout = stdout.replace("\n", &format!("\n{}", indent));
        stdout.insert_str(0, indent);

        let mut stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        stderr = stderr.replace("\n", &format!("\n{}", indent));
        stderr.insert_str(0, indent);

        eprintln!("Failed to run the test binary {}:\n\nStdout:{}\n\nStderr:\n{}",
               binary_out_str,
               stdout,
               stderr);

        objdump_binary(object);

        panic!();
    }

    let cow = String::from_utf8_lossy(&output.stdout);
    cow.into_owned()
}

fn objdump_binary(binary: &String) {
    let output = Command::new("objdump")
        .arg("-d")
        .arg("-Mintel")
        .arg(binary)
        .output()
        .unwrap_or_else(|err| {
            panic!("Failed to objdump test binary {}: {}", binary, err);
        });

    let indent = &" ".repeat(8);
    let mut stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    stdout = stdout.replace("\n", &format!("\n{}", indent));
    stdout.insert_str(0, indent);

    eprintln!("Disassembly output: \n\n{}", stdout);

    if !output.status.success() {



        let mut stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        stderr = stderr.replace("\n", &format!("\n{}", indent));
        stderr.insert_str(0, indent);

        panic!("Failed to obj dump the object file {}:\n\nStderr:\n{}",
                  binary,
                  stderr);
    }
}

#[test]
fn empty_function_compiles() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : void {
        } "#,
        FunctionKind::VOID("test".to_owned())
    );
    assert_eq!("", output);
}

#[test]
fn should_return_constant_value() {

    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 1;
            }
        ",
    FunctionKind::INT("foo".to_owned()));

    assert_eq!("1\n", output);
}

#[test]
fn should_return_addition_result() {

    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 1+5;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("6\n", output);
}

#[test]
fn should_return_subtraction_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 3-5;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("-2\n", output);
}

#[test]
fn should_return_multiplication_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 6*7;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("42\n", output);
}

#[test]
fn should_return_division_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 10/5;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("2\n", output);
}

#[test]
fn should_truncate_division_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 29/10;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("2\n", output);
}

#[test]
fn should_work_when_variables_used() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a : int = 30;
                let b : int = 3;
                return a*b;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("90\n", output);
}

#[test]
fn should_work_with_complex_arithmetics_when_variables_used() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a : int = 30;
                let b : int = 3;
                let c : int = (a-b)*2; // 27*2 = 54
                let d : int = c/b; // 54/3 = 18
                return (d + 2)/2+4; // 20/2+4 = 14
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("14\n", output);
}

