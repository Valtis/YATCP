extern crate yaml_rust;

use yaml_rust::YamlLoader;
use rayon::prelude::*;
use ansi_term::Colour;

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::panic;
use crate::compiler_helper::{compile_and_run, CompileData, FunctionKind};
use crate::helper::indent;
use regex::Regex;

const TEST_FILE_EXTENSION: &'static str= "yaml";


#[derive(Clone, Debug)]
enum TestResult {
    Passed,
    Skipped,
    Failed{ testcase: String, message: String},
}


pub fn run_tests(name_contains: &str) -> i32  {
    let programs = find_all_test_programs();
    println!("Discovered {} test cases\n", programs.len());

    let results = programs.par_iter().map(|program| run_test(program, name_contains)).collect::<Vec<_>>();


    let mut failures = vec![];
    let mut passes = 0;
    let mut skipped = 0;
    for result in results {
        match result {
            TestResult::Passed => passes +=1,
            TestResult::Skipped => skipped += 1,
            TestResult::Failed{ testcase, message} => {
                failures.push((testcase, message));
            }
        }
    }


    for f in failures.iter() {
        println!("{}:\n{}", Colour::Cyan.paint(&f.0), &f.1);
    }

    let stars = Colour::Red.bold().paint("************************");
    if failures.len() == 1 {
        println!("{0}\nThere was {1} failure\n{0}\n\n", stars, Colour::Red.paint(format!("{}", failures.len())));
    } else if failures.len() > 1{
        println!("{0}\nThere were {1} failures\n{0}\n\n", stars, Colour::Red.paint(format!("{}", failures.len())));
    }

    println!("\nRan {} test, {} passed, {} skipped, {} failed", programs.len(), passes, skipped, failures.len());

    // Note: Unix uses 8 bit return codes. Technically we could just return the failure count,
    // but 'failures MOD 256' is interpreted as zeros for multiples of 256
    if failures.is_empty() {
        return 0;
    }

    return 1;
}

fn find_all_test_programs() -> Vec<PathBuf> {
    find_files_recursively(Path::new("../programs"))
}

fn find_files_recursively(path: &Path) -> Vec<PathBuf> {
    let mut paths = vec![];

    for entry in fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.is_dir() {
            let mut res = find_files_recursively(path.as_path());
            paths.append(&mut res);
        } else if path.is_file() && path.extension().is_some() && path.extension().unwrap() == TEST_FILE_EXTENSION {
            paths.push(path.as_path().to_owned());
        }
    }
    paths
}

fn run_test(path: &Path, name_contains: &str) -> TestResult {
    let testcase = format!("{}/{}",
                           path.components().rev().skip(1).take(1).collect::<Vec<_>>()[0].as_os_str().to_str().unwrap(),
                           path.file_stem().unwrap().to_str().unwrap());
    if !testcase.contains(name_contains) {
        return TestResult::Skipped;
    }
    let result = panic::catch_unwind(||do_run(path));

    // error due to panic
    let result = result.unwrap_or_else(|cause|
                                           Err(format!("Unexpected panic: {}",
                                                       cause.downcast::<String>().unwrap_or_else(
                                                           |cause| {
                                                               let str_box = cause.downcast::<&'static str>().unwrap_or(
                                                                Box::new("Cause omitted: Not string"));
                                                                Box::new((*str_box).to_owned())
                                                           })
                                           )));



    // test failure
    let result = result.map_err(|err| (testcase.to_owned(), err));


    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    let stderr = std::io::stderr();
    let mut _handle = stderr.lock(); // lock stderr as well, to prevent stdout/err interleaving

    let result = match &result {
       Ok(()) => {
           writeln!(&mut handle, "{}: {}",
                    Colour::Green.bold().paint("PASS"),
                    testcase
           ).unwrap();
           TestResult::Passed
       },
       Err((_, err)) => {
           writeln!(&mut handle, "{}: {}\n\n{}",
                    Colour::Red.paint("FAIL"),
                    testcase,
                    err,
                    ).unwrap();
           TestResult::Failed{ testcase: testcase.to_owned(), message: err.clone() }
       }

    };
    result
}

fn do_run(path: &Path) -> Result<(), String> {
    let data = load_values_from_yaml(path);
    let (stdout, stderr, exec) = compile_and_run(&data, false)?;

    let stdout = remove_color_codes(&stdout).trim().to_owned();
    let stderr = remove_color_codes(&stderr).trim().to_owned();

    let mut err_str = String::new();
    if stdout != data.expected_stdout.trim() {
        err_str = format_message("stdout", data.expected_stdout.trim(), &stdout);
    }

    if stderr != data.expected_stderr.trim() {
        err_str += &format_message("stderr", data.expected_stderr.trim(), &stderr);
    }

    if err_str.is_empty() {
        Ok(())
    } else {
        err_str += &format!("\nExecutable: {}\n", exec);
        Err(err_str)
    }
}

fn format_message(name: &str, expected: &str, actual: &str) -> String {
    let mut err_str =
        format!(
            "{}: \n{}:\n{}\n{}\n{}",
            Colour::Yellow.bold().paint(format!("{0} does not match the expected {0}", name)),
            Colour::Fixed(105).paint("Expected"),
            indent(expected),
            Colour::Fixed(198).paint("Actual"),
            indent(actual));

    let whitespace_note = if expected.split("\n")
        .zip(actual.split("\n"))
        .any(|(exp, act)| exp != act && exp.trim() == act.trim()) {
        format!(" ({})", Colour::Cyan.paint("Note: Whitespace differences present!"))
    } else {
        "".to_owned()
    };

    let expected = indent(&expected);
    let actual =  indent(&actual);

    let changeset = difference::Changeset::new(&expected, &actual, "\n");

    err_str += &format!(
        "\n{}{}\n\n{}",
        Colour::Fixed(84).paint("Diff"),
        whitespace_note,
        changeset);
    err_str

}

fn remove_color_codes(formatted_string: &str) -> String {
    let escape_code = 0x1B as char;

    let regex  = Regex::new(
        &format!("{}{}",
        escape_code,
        r"\[.*?m")).unwrap();

    regex.replace_all(formatted_string, "").to_string()
}

fn load_values_from_yaml(path: &Path) -> CompileData {

    let content = fs::read_to_string(path).unwrap();
    let yaml = YamlLoader::load_from_str(&content)
        .unwrap_or_else(|err| panic!("Malformed test file {}: {}", path.to_str().unwrap(), err));

    if yaml.len() != 1 {
        panic!("Unexpected yaml document count for file {}: {}", path.to_str().unwrap(), yaml.len());
    }
    let doc = &yaml[0];

    let mandatory_fields = vec!["program"];

    for field in mandatory_fields.iter() {
        if doc[*field].is_badvalue() {
            panic!("Mandatory field '{}' not present in file {}", *field, path.to_str().unwrap());
        }
    }

    // make sure we have a trailing newline, error reporter does not like it when one is not present
    let program = format!("{}\n", doc["program"].as_str().unwrap().to_owned());

    let expected_stdout = doc["expect_stdout"].as_str().unwrap_or("").to_owned();
    let expected_stderr = doc["expect_stderr"].as_str().unwrap_or("").to_owned();
    let callable_function = doc["callable"].as_str().unwrap_or("").to_owned();
    let return_type = doc["returns"].as_str().unwrap_or("").to_owned();
    let expect_compile_failure = doc["expect_compile_failure"].as_bool().unwrap_or(false).to_owned();

    let link_with: Vec<PathBuf> = doc["link_with"].as_vec()
        .map(|vec|
            vec
                .iter()
                .map(|yaml_obj|
                    PathBuf::from(yaml_obj.as_str().unwrap())).collect()).unwrap_or(vec![]);

    if (callable_function.trim().is_empty() && !return_type.trim().is_empty()) ||
        (!callable_function.trim().is_empty() && return_type.trim().is_empty()) {
        panic!("Both callable and return type must be provided, or neither must be provided {}", path.to_str().unwrap());
    }


    let callable = if !return_type.is_empty() {
        match return_type.as_str() {
            "long" => Some(FunctionKind::LONG(callable_function)),
            "int" => Some(FunctionKind::INT(callable_function)),
            "byte" => Some(FunctionKind::BYTE(callable_function)),
            "void" => Some(FunctionKind::VOID(callable_function)),
            _ => panic!("Unexpected return type in {}: {} ", path.to_str().unwrap(), return_type),
        }
    } else {
        None
    };

    CompileData {
        program,
        callable,
        expected_stdout,
        expected_stderr,
        expect_compile_failure,
        link_with,
    }
}


