extern crate yaml_rust;
mod compiler_helper;

use compiler_helper::{FunctionKind, CompileData};

use yaml_rust::YamlLoader;
use rayon::prelude::*;
use ansi_term::Colour;

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::panic;

const TEST_FILE_EXTENSION: &'static str= "yaml";

fn run_tests() {
    let programs = find_all_test_programs();
    let results: Vec<Result<(), (String, String)>> = programs.par_iter().map(|program| run_test(program)).collect();

    let mut failures = vec![];

    for result in results {
        match result {
            Ok(_) => (), // don't care
            Err(fail) => {
                failures.push(fail);
            }
        }
    }

    for f in failures.iter() {
        println!("{}:\n{}", Colour::Cyan.paint(&f.0), f.1);
    }

    let stars = Colour::Red.bold().paint("************************");
    if failures.len() == 1 {
        println!("{0}\nThere was {1} failure\n{0}\n\n", stars, Colour::Red.paint(format!("{}", failures.len())));
    } else if failures.len() > 1{
        println!("{0}\nThere were {1} failures\n{0}\n\n", stars, Colour::Red.paint(format!("{}", failures.len())));
    }

    assert_eq!(0, failures.len());
}

fn find_all_test_programs() -> Vec<PathBuf> {
    find_files_recursively(Path::new("tests/programs"))
}

fn find_files_recursively(path: &Path) -> Vec<PathBuf> {
    let mut paths = vec![];

    for entry in fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();

        if path.is_dir() {
            let mut res =find_files_recursively(path.as_path());
            paths.append(&mut res);
        } else if path.is_file() && path.extension().is_some() && path.extension().unwrap() == TEST_FILE_EXTENSION {
            paths.push(path.as_path().to_owned());
        }
    }
    paths
}

fn run_test(path: &Path) -> Result<(), (String, String)> {
    let testcase = path.file_stem().unwrap().to_str().unwrap();
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

    match &result {
       Ok(()) => {
           writeln!(&mut handle, "{}: {}",
                    Colour::Green.bold().paint("PASS"),
                    testcase
           ).unwrap();
       },
       Err((_, err)) => {
           writeln!(&mut handle, "\n{}\n{}: {}",
                    err,
                    Colour::Red.paint("FAIL"),
                    testcase,
                    ).unwrap();
       }

    }
    result
}

fn do_run(path: &Path) -> Result<(), String> {
    let data = load_values_from_yaml(path);
    compiler_helper::compile_and_run(data, false)
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

    let program = doc["program"].as_str().unwrap().to_owned();
    let expected_stdout = doc["expect_stdout"].as_str().unwrap_or("").to_owned();
    let expected_stderr = doc["expect_stderr"].as_str().unwrap_or("").to_owned();
    let callable_function = doc["callable"].as_str().unwrap_or("").to_owned();
    let return_type = doc["returns"].as_str().unwrap_or("").to_owned();

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
            "int" => Some(FunctionKind::INT(callable_function)),
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
        link_with,
    }
}

#[test]
fn run_e2e_tests() {
    run_tests();
}