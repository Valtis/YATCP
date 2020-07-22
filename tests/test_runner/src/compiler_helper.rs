use std::sync::atomic::{AtomicI32, Ordering};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::process::Command;
use std::os::unix::prelude::ExitStatusExt;

use std::path::PathBuf;

use crate::helper::indent;

static FILE_COUNTER: AtomicI32 = AtomicI32::new(0);

// FIXME Hardcoded path to test binary, assumes that it is compiled
// preferably all paths are given as inputs, so we can test arbitrary binaries
const COMPILER_BINARY:&'static str = "../../target/debug/compiler";

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
    pub expect_compile_failure: bool,
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




pub fn compile_and_run(compile_data: &CompileData, optimize: bool) -> Result<(String, String, String), String> {

    let ctr = FILE_COUNTER.fetch_add(1, Ordering::Relaxed);

    let input_file_str = write_program_into_tmp_file(&compile_data.program, ctr);
    let output_file_str = match create_obj_file(ctr, &input_file_str, optimize) {
        Ok(file) => {
            if !compile_data.expect_compile_failure {
                file
            } else {
                return Err(indent("Unexpected passing build when failure was expected"));
            }
        }
        Err((message, stdout , stderr)) => {
          if compile_data.expect_compile_failure {
              return Ok((stdout, stderr, "<Compile failure>".to_owned()));
          } else {
              return Err(message);
          }
        }
    };

    let mut binary_out = env::temp_dir();
    binary_out.push(format!("yatcp_test_binary_{}", ctr));
    let binary_out_str = binary_out.to_str().unwrap().to_string();

    compile_test_binary(&compile_data, output_file_str.clone(), &binary_out_str)?;



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

fn create_obj_file(ctr: i32, input_file_str: &String, optimize: bool) -> Result<String, (String, String, String)> {


    let mut output_file = env::temp_dir();
    output_file.push(format!("yatcp_test_output_{}.o", ctr));
    let output_file_str = output_file.to_str().unwrap().to_string();


    let mut args = vec![];
    args.push("-o".to_owned());
    args.push(output_file_str.to_owned());

    if optimize {
        args.push("-O".to_owned());
    }

    args.push(input_file_str.to_owned());

    let output = Command::new(COMPILER_BINARY)
        .args(args)
        .output()
        .unwrap_or_else(|err| {
            panic!("Failed to create object file: {}", err)
        });

    if output.status.success() {
        Ok(output_file_str)
    } else {
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        Err((format!("Failed to create the object file:\n\nStdout:\n{}\nStderr:\n{}",
                    indent(&stdout),
                    indent(&stderr)), stdout, stderr))
    }
}

fn compile_test_binary(compile_data: &CompileData, output_file_str: String, binary_out_str: &String) -> Result<(), String> {

    let callable_define = if let Some(ref kind ) = compile_data.callable {
        format!("-D{}={}", kind.to_define(), kind.name())
    } else {
        "-Dno_callable".to_owned()
    };

    let output = Command::new("tests/programs/wrapper.sh")
        .arg(callable_define)
        .arg(output_file_str)
        .args(compile_data.link_with.clone())
        .arg(format!("-o {}", binary_out_str))
        .current_dir("../../") // compatibility hack; links_with directory path is wrong after code reorg
        .output()
        .unwrap_or_else(|err| {
            panic!("Failed to link the test binary: {}", err);
        });

    if output.status.success() {
        Ok(())
    } else {
        let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        Err(format!("Failed to link the test binary:\n\nStdout:\n{}\nStderr:\n{}",
               indent(&stdout),
               indent(&stderr)))
    }
}

fn run_test_binary(object: &str, binary_out_str: &str) -> Result<(String, String, String), String> {

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

        let mut err_str =
            format!("Failed to run the test binary {}:\n\nExit code: {}\nStdout:{}\n\nStderr:\n{}",
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

        err_str = format!("{}\n\n{}", err_str, objdump_binary(object)?);

        return Err(err_str)
    }

    let stdout_cow = String::from_utf8_lossy(&output.stdout);
    let stderr_cow = String::from_utf8_lossy(&output.stderr);

    Ok((stdout_cow.into_owned(), stderr_cow.into_owned(), binary_out_str.to_owned()))

}

fn objdump_binary(binary: &str) -> Result<String, String> {
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


    if !output.status.success() {
        let mut stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        stderr = indent(&stderr);

        return Err(format!("Failed to obj dump the object file {}:\n\nStderr:\n{}",
               binary,
               stderr));
    }

    Ok(format!("Disassembly output: \n\n{}", stdout))
}