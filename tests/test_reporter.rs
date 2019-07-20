use compiler::error_reporter::ErrorReporter;
use compiler::error_reporter::ReportKind;

pub struct ReportedError {
    pub error_type: ReportKind,
    pub line: i32,
    pub column: i32,
    pub token_length: i32,
    pub error_string: String,
}

#[macro_use]
macro_rules! assert_eq_error {
    ($error: expr, $e_type:expr, $line:expr, $column:expr, $length:expr) => (
        {
            assert_eq!($error.error_type, $e_type);
            assert_eq!($error.line, $line);
            assert_eq!($error.column, $column);
            assert_eq!($error.token_length, $length);
        }
    )
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
        error_type: ReportKind,
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


    // ignoring operator for now
    fn report_error_with_expression(
        &mut self,
        error_type: ReportKind,
        line: i32,
        expression_start: i32,
        expression_end: i32,
        operator_start:i32,
        operator_length: i32,
        message: String) {

        self.errors.push(ReportedError {
            error_type: error_type,
            line: line,
            column: expression_start,
            token_length: expression_end - expression_start,
            error_string: message,
        });
    }

    fn print_errors(&self) {
        unimplemented!()
    }

    fn has_errors(&self) -> bool {
        unimplemented!();
    }

    fn has_reports(&self) -> bool {
        unimplemented!();
    }

    fn errors(&self) -> i32 {
        unimplemented!();
    }
}
