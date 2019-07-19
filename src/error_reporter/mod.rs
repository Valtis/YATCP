use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

use std::cmp;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter;

use ansi_term::Colour::{Red, Cyan, Yellow};
use ansi_term;

#[derive(Debug, PartialEq)]
pub enum ReportKind {
    Note,
    Warning,
    TokenError,
    SyntaxError,
    TypeError,
    NameError,
    DataFlowError,
}

impl ReportKind {
    fn get_color(&self) -> ansi_term::Colour {
        match *self {
            ReportKind::Note => Cyan,
            ReportKind::Warning => Yellow,
            ReportKind::TokenError |
            ReportKind::SyntaxError |
            ReportKind::TypeError |
            ReportKind::NameError |
            ReportKind::DataFlowError => Red,
        }
    }
}

impl Display for ReportKind {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        let color = self.get_color();
        let str = match *self {
            ReportKind::Note => color.bold().paint("Note").to_string(),
            ReportKind::Warning => color.paint("Warning").to_string(),
            ReportKind::TokenError => color.bold().paint("Token error").to_string(),
            ReportKind::SyntaxError => color.bold().paint("Syntax error").to_string(),
            ReportKind::TypeError => color.bold().paint("Type error").to_string(),
            ReportKind::NameError => color.bold().paint("Name error").to_string(),
            ReportKind::DataFlowError => color.bold().paint("Data flow error").to_string(),
        };

        write!(formatter, "{}", str)
    }
}

pub trait ErrorReporter {
    fn report_error(&mut self, error_type: ReportKind, line: i32, column: i32, token_length : i32, error_string: String);
    fn report_error_with_expression(&mut self, error_type: ReportKind, line: i32, expression_start: i32, expression_end: i32, operator_start:i32, operator_length: i32, message: String);

    fn has_errors(&self) -> bool;
    fn has_reports(&self) -> bool;
    fn errors(&self) -> i32;
    fn print_errors(&self);

}

fn write_stderr(txt: String) {
    match write!(&mut ::std::io::stderr(), "{}", txt) {
        Ok(_) => {},
        Err(x) => panic!("Unable to write to stderr: {}", x),
    }

}

trait Message {
    fn write_message(&self, lines: &Vec<String>);
}


struct UnderScoreMessage  {
    line: i32,
    column: i32,
    length: i32,
    error: ReportKind,
    message: String,
}

impl UnderScoreMessage {
    fn new(
        line: i32,
        column: i32,
        length: i32,
        error: ReportKind,
        message: String) -> UnderScoreMessage {

        UnderScoreMessage {
            line: line,
            column: column,
            length: length,
            error: error,
            message: message
        }
    }
}


impl Message for UnderScoreMessage {
    fn write_message(&self, lines: &Vec<String>) {
        if self.error != ReportKind::Note {
            write_stderr("\n".to_string());
        }

        write_stderr(
            format!(
                "{}:{} {}: {}\n",
                self.line,
                self.column,
                self.error,
                self.message));

        let line = &lines[(self.line-1) as usize];

        write_stderr(format!("{}", line));
        if !line.ends_with("\n") {
            write_stderr("\n".to_string());
        }

        write_stderr(
            iter::repeat(" ").
            take(cmp::max(self.column-1, 0) as usize).
            collect::<String>());
        let color = self.error.get_color();

        for _ in 0..self.length {
            write_stderr(color.bold().paint("^").to_string());
        }
        write_stderr("\n".to_string());
    }
}

struct ExpressionMessage {
    line: i32,
    expression_start: i32,
    expression_end: i32,
    operator_start: i32,
    operator_length: i32,
    error: ReportKind,
    message: String,
}

impl ExpressionMessage {
    fn new(
        line: i32,
        expression_start: i32,
        expression_end: i32,
        operator_start: i32,
        operator_length: i32,
        error: ReportKind,
        message: String,
        ) -> ExpressionMessage {
        ExpressionMessage {
            line,
            expression_start,
            expression_end,
            operator_start,
            operator_length,
            error,
            message,
        }
    }
}

impl Message for ExpressionMessage {
    fn write_message(&self, lines: &Vec<String>) {

        if self.error != ReportKind::Note {
            write_stderr("\n".to_string());
        }

        write_stderr(
            format!(
                "{}:{} {}: {}\n",
                self.line,
                self.operator_start,
                self.error,
                self.message));

        let line = &lines[(self.line-1) as usize];

        write_stderr(format!("{}", line));
        if !line.ends_with("\n") {
            write_stderr("\n".to_string());
        }

        write_stderr(
            iter::repeat(" ").
            take(cmp::max(self.expression_start-1, 0) as usize).
            collect::<String>());
        let color = self.error.get_color();

        for _ in self.expression_start..self.operator_start {
            write_stderr(color.bold().paint("~").to_string());
        }

        for _ in 0..self.operator_length {
            write_stderr(color.bold().paint("^").to_string());
        }

        for _ in (self.operator_start + self.operator_length)..self.expression_end {
            write_stderr(color.bold().paint("~").to_string());
        }
        write_stderr("\n".to_string());

    }
}

pub struct FileErrorReporter {
    file_path: String,
    errors: i32,
    messages: Vec<Box<dyn Message>>

}

impl FileErrorReporter {
    pub fn new(file: &str) -> FileErrorReporter {
        FileErrorReporter {
            file_path: file.to_string(),
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
            Box::new(UnderScoreMessage::new(
                line,
                column,
                token_length,
                error_type,
                message)));

    }

    fn report_error_with_expression(
        &mut self,
        error_type: ReportKind,
        line: i32,
        expression_start: i32,
        expression_end: i32,
        operator_start: i32,
        operator_length: i32,
        message: String) {
        self.update_error_count(&error_type);

        self.messages.push(
            Box::new(ExpressionMessage::new(
                line,
                expression_start,
                expression_end,
                operator_start,
                operator_length,
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

