extern crate compiler;
use compiler::error_reporter::ErrorReporter;
use compiler::error_reporter::Error;

pub struct ReportedError {
    pub error_type: Error,
    pub line: i32,
    pub column: i32,
    pub token_length: i32,
    pub error_string: String,
}

pub struct TestReporter {
    errors: Vec<ReportedError>
} 

impl TestReporter {
    pub fn new() -> TestReporter {
        TestReporter {
            errors: vec![],
        }
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn errors(&self) -> &Vec<ReportedError> {
        &self.errors
    }
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
