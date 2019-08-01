use compiler::frontend::run_frontend;
use compiler::middleend::run_middleend;
use compiler::backend::run_backend;

use compiler::error_reporter::file_reporter::FileErrorReporter;

use std::sync::atomic::{AtomicI32, Ordering};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;
use std::os::unix::prelude::ExitStatusExt;



use std::rc::Rc;
use std::cell::RefCell;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);

pub enum FunctionKind {
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

pub fn compile_and_run_no_opt(program: &str, kind: FunctionKind) -> String {

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

    let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(input_file_str)));

    let functions = run_frontend(
        input_file_str.to_string(),
        false,
        false,
        error_reporter.clone()).unwrap();
    let mut output_file = env::temp_dir();
    output_file.push(format!("yatcp_test_output_{}.o", ctr));
    let output_file_str = output_file.to_str().unwrap().to_string();

    let functions = run_middleend(
        functions,
        false,
        false,
        false,
        error_reporter.clone()).unwrap();

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

fn run_test_binary(object: &str, binary_out_str: &str) -> String {

    // Need to wrap the execution with { } in order to grab SIGSEVS & friends
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

        eprintln!("Failed to run the test binary {}:\n\nExit code: {}\nStdout:{}\n\nStderr:\n{}",
                  binary_out_str,
                  match output.status.code() {
                     Some(code) => {
                         format!("{}", code)
                     },
                     None => {
                         let signal = match output.status.signal() {
                             Some(1) => "SIGHUP - Hanged up".to_owned(),
                             Some(2) => "SIGINT - Interrupted".to_owned(),
                             Some(3) => "SIGQUIT - Quit".to_owned(),
                             Some(4) => "SIGILL - Illegal instruction".to_owned(),
                             Some(5) => "SIGTRAP - Trapped".to_owned(),
                             Some(6) => "SIGABRT - Abort".to_owned(),
                             Some(7) => "SIGBUS - Bus error".to_owned(),
                             Some(8) => "SIGFPE - Floating point/arithmetic error".to_owned(),
                             Some(9) => "SIGKILL - Killed".to_owned(),
                             Some(11) => "SIGSEGV - Segmentation fault".to_owned(),
                             Some(15) => "SIGTERM - Terminated".to_owned(),
                             Some(16) => "SIGSTKFLT - Stack fault".to_owned(),
                             None => "Unknown signal".to_owned(),
                             Some(x) => format!("Unknown signal: {}", x),
                         };
                         let signal = ansi_term::Colour::Red.bold().paint(signal);
                         format!("No exit code - terminated by signal: {}", signal)
                     },
                  },
                  stdout,
                  stderr,
        );

        objdump_binary(object);

        panic!();
    }

    let cow = String::from_utf8_lossy(&output.stdout);
    cow.into_owned()
}

fn objdump_binary(binary: &str) {
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