use crate::ast::NodeInfo as Span;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

use std::cmp;
use std::iter;

use ansi_term::Colour::{Red, Cyan, Yellow};
use ansi_term;
use crate::ast::NodeInfo;

pub mod file_reporter;


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


struct HighlightMessage {
    span: Span,
    report: ReportKind,
    message: String,
}

impl HighlightMessage {
    fn new(
        line: i32,
        column: i32,
        length: i32,
        report: ReportKind,
        message: String) -> HighlightMessage {

        HighlightMessage {
            span: Span {
                line: line,
                column: column,
                length: length,
            },
            report,
            message
        }
    }
}


impl Message for HighlightMessage {
    fn write_message(&self, lines: &Vec<String>) {
        if self.report != ReportKind::Note {
            write_stderr("\n".to_string());
        }

        write_stderr(
            format!(
                "{}:{} {}: {}\n",
                self.span.line,
                self.span.column,
                self.report,
                self.message));

        let line = &lines[(self.span.line-1) as usize];

        write_stderr(format!("{}", line));
        if !line.ends_with("\n") {
            write_stderr("\n".to_string());
        }

        write_stderr(
            iter::repeat(" ").
            take(cmp::max(self.span.column-1, 0) as usize).
            collect::<String>());
        let color = self.report.get_color();

        for _ in 0..self.span.length {
            write_stderr(color.bold().paint("^").to_string());
        }
        write_stderr("\n".to_string());
    }
}

struct HighlightWithOperatorMessage {
    line: i32,
    expression_start: i32,
    expression_end: i32,
    operator_start: i32,
    operator_length: i32,
    error: ReportKind,
    message: String,
}

impl HighlightWithOperatorMessage {
    fn new(
        line: i32,
        expression_start: i32,
        expression_end: i32,
        operator_start: i32,
        operator_length: i32,
        error: ReportKind,
        message: String,
        ) -> HighlightWithOperatorMessage {
        HighlightWithOperatorMessage {
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

impl Message for HighlightWithOperatorMessage {
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
