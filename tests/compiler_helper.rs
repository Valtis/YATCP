use compiler::frontend::run_frontend;
use compiler::middleend::run_middleend;
use compiler::backend::run_backend;

use compiler::error_reporter::file_reporter::FileErrorReporter;

use ansi_term::Colour;

use std::sync::atomic::{AtomicI32, Ordering};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;
use std::os::unix::prelude::ExitStatusExt;

use std::path::PathBuf;
use std::rc::Rc;
use std::cell::RefCell;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);

#[derive(Debug)]
pub enum FunctionKind {
    VOID(String),
    INT(String),
}

#[derive(Debug)]
pub struct CompileData {
    pub program: String,
    pub callable: Option<FunctionKind>,
    pub expected_stdout: String,
    pub expected_stderr: String,
    pub link_with: Vec<PathBuf>,
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


fn indent(input: &str) -> String {
    let indent = " ".repeat(8);

    format!("{}{}", indent,  input.replace("\n", &format!("\n{}", indent)))
}

pub fn compile_and_run(compile_data: CompileData, optimize: bool) -> Result<(), String> {

    let ctr = FILE_COUNTER.fetch_add(1, Ordering::Relaxed);

    let input_file_str = write_program_into_tmp_file(&compile_data.program, ctr);
    let output_file_str = create_obj_file(ctr, &input_file_str, optimize)?;

    let mut binary_out = env::temp_dir();
    binary_out.push(format!("yatcp_test_binary_{}", ctr));
    let binary_out_str = binary_out.to_str().unwrap().to_string();

    compile_test_binary(&compile_data, output_file_str.clone(), &binary_out_str)?;
    let (stdout, stderr) = run_test_binary(&output_file_str, &binary_out_str);



    fn format_message(name: &str, expected: &str, actual: &str) -> String {
        let mut err_str = format!("{}: \nExpected:\n{}\nActual:\n{}",
                          Colour::Yellow.bold().paint(format!("{0} does not match the expected {0}", name)),
                          indent(expected),
                          indent(actual));

        if expected.trim() == actual.trim() {

            err_str += &format!("\n{}: \nExpected:\n{}\nActual:\n{}",
                                Colour::Blue.bold().paint("Note: Whitespace differs"),
                                indent(&expected
                                    .replace(" ", ".")
                                    .replace("\t", "⇄")
                                    .replace("\n", "↵")
                                ),
                                indent(&actual
                                    .replace(" ", ".")
                                    .replace("\t", "--->")
                                    .replace("\n", "↵")
                                ));
        }

        err_str
    }

    let mut err_str = String::new();
    if stdout != compile_data.expected_stdout {
        err_str = format_message("stdout", &compile_data.expected_stdout, &stdout);
    }

    if stderr != compile_data.expected_stderr {
        err_str += &format_message("stderr", &compile_data.expected_stderr, &stderr);
    }

    if err_str.is_empty() {
        Ok(())
    } else {
        err_str += &format!("\nExecutable: {}\n", binary_out_str);
        Err(err_str)
    }
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

fn create_obj_file(ctr: i32, input_file_str: &String, optimize: bool) -> Result<String, String> {

    let error_reporter = Rc::new(RefCell::new(FileErrorReporter::new(input_file_str)));

    let functions = run_frontend(
        input_file_str.to_string(),
        false,
        false,
        error_reporter.clone()).ok_or("Frontend error (syntax/semantics)".to_owned())?;

    let mut output_file = env::temp_dir();
    output_file.push(format!("yatcp_test_output_{}.o", ctr));
    let output_file_str = output_file.to_str().unwrap().to_string();

    let functions = run_middleend(
        functions,
        optimize,
        false,
        false,
        error_reporter.clone()).ok_or("Middle-end error (cfg)".to_owned())?;

    run_backend(output_file_str.clone(), functions, false);
    Ok(output_file_str)
}

fn compile_test_binary(compile_data: &CompileData, output_file_str: String, binary_out_str: &String) -> Result<(), String> {

    let callable_define = if let Some(ref kind ) = compile_data.callable {
        format!("-D{}={}", kind.to_define(), kind.name())
    } else {
        "-Dno_callable".to_owned()
    };

    let output = Command::new("./tests/programs/wrapper.sh")
        .arg(callable_define)
        .arg(output_file_str)
        .args(compile_data.link_with.clone())
        .arg(format!("-o {}", binary_out_str))
        .output()
        .unwrap_or_else(|err| {
            panic!("Failed to compile the test binary: {}", err);
        });

    if output.status.success() {
        Ok(())
    } else {
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        Err(format!("Failed to compile the test binary:\n\nStdout:\n{}\nStderr:\n{}",
               indent(&stdout),
               indent(&stderr)))
    }
}

fn run_test_binary(object: &str, binary_out_str: &str) -> (String, String) {

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

    let stdout_cow = String::from_utf8_lossy(&output.stdout);
    let stderr_cow = String::from_utf8_lossy(&output.stderr);
    (stdout_cow.into_owned(), stderr_cow.into_owned())
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

    let mut stdout = String::from_utf8_lossy(&output.stdout).into_owned();
    stdout = indent(&stdout);

    eprintln!("Disassembly output: \n\n{}", stdout);

    if !output.status.success() {



        let mut stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        stderr = indent(&stderr);

        panic!("Failed to obj dump the object file {}:\n\nStderr:\n{}",
               binary,
               stderr);
    }
}