use crate::ast::NodeInfo as Span;

use ansi_term::Colour::{Red, Cyan, Yellow};
use ansi_term;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

pub mod file_reporter;


#[derive(Debug, Clone, Copy, PartialEq)]
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
    fn report_error(&mut self, error_type: ReportKind, span: Span, error_string: String);

    fn has_errors(&self) -> bool;
    fn has_reports(&self) -> bool;
    fn errors(&self) -> i32;
    fn print_errors(&self);

}


enum Message {
    HighlightMessage {
        span: Span,
        report: ReportKind,
        message: String,
    }
}



