extern crate compiler;
use compiler::semcheck::SemanticsCheck;
use compiler::error_reporter::ErrorReporter;
use compiler::error_reporter::Error;

use std::rc::Rc;
use std::cell::RefCell;

struct ReportedError {
    error_type: Error,
    line: i32,
    column: i32,
    token_length: i32,
    error_string: String,
}


struct TestReporter {
    errors: Vec<ReportedError>
} 

impl ErrorReporter for TestReporter {
    fn report_error(
        &mut self, 
        error_type: Error, 
        line: i32, 
        column: i32, 
        token_length : i32, 
        error_string: String) {

        self.errors.push(ReportedError {
            error_type: error_type,
            line: line,
            column: column,
            token_length: token_length,
            error_string: error_string,
        });
    }
}

#[cfg(test)]
fn create_sem_checker() -> (Rc<RefCell<TestReporter>>, SemanticsCheck) {
    let reporter = Rc::new(RefCell::new(TestReporter {
        errors: vec![],
    }));

    (reporter.clone(), SemanticsCheck::new(reporter))
}

#[test]
fn undeclared_variable_is_reported() {
    let (reporter, checker) = create_sem_checker();

    
}

#[test]
fn using_function_as_variable_is_reported() {

}

#[test]
fn type_error_when_variable_is_declared_is_reported() {

}

#[test]
fn type_error_when_assigning_into_variable_is_reported() {

}

#[test]
fn multiple_errors_are_reported() {

}