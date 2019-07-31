use super::{ErrorReporter, ReportKind, Message, HighlightMessage };

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;



/*
Reports errors by printing/highlighting content from the given file
*/
pub struct FileErrorReporter {
    file_path: String,
    errors: i32,
    messages: Vec<Box<dyn Message>>
}

impl FileErrorReporter {
    pub fn new(file: &str) -> FileErrorReporter {
        FileErrorReporter {
            file_path: file.to_owned(),
            errors: 0,
            messages: vec![],
        }
    }

    fn read_lines(&self) -> Vec<String> {
        let mut lines = vec![];
        // Todo: Replace with something sligtly saner
        let f = match File::open(&self.file_path) {
            Ok(file) => file,
            Err(e) => panic!("Failed to open file {}: {}", self.file_path, e),
        };

        let reader = BufReader::new(f);
        for line in reader.lines()  {
            match line {
                Ok(content) => lines.push(content),
                Err(e) => panic!("IO error: {}", e),
            }
        }
        lines
    }

    fn update_error_count(&mut self, error_type: &ReportKind) {
        match error_type {
            ReportKind::TokenError |
            ReportKind::TypeError |
            ReportKind::NameError |
            ReportKind::SyntaxError |
            ReportKind::DataFlowError => self.errors += 1,
            ReportKind::Note | ReportKind::Warning => (),
        }
    }
}

impl ErrorReporter for FileErrorReporter {
    fn report_error(&mut self, error_type: ReportKind, line: i32, column: i32, token_length : i32, message : String) {

        self.update_error_count(&error_type);

        self.messages.push(
            Box::new(HighlightMessage::new(
                line,
                column,
                token_length,
                error_type,
                message)));

    }

    fn has_errors(&self) -> bool {
        self.errors != 0
    }

    fn has_reports(&self) -> bool {
        self.messages.len() > 0
    }

    fn errors(&self) -> i32 {
        self.errors
    }

    fn print_errors(&self) {
        let lines = self.read_lines();

        for msg in self.messages.iter() {
            msg.write_message(&lines);
        }

    }
}

