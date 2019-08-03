use crate::ast::NodeInfo as Span;

use ansi_term::Colour::{Red, Cyan, Yellow};
use ansi_term;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;

pub mod file_reporter;
pub mod null_reporter;

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

    fn is_error(&self) -> bool {
        match self {
            ReportKind::Note |
            ReportKind::Warning => false,
            ReportKind::TokenError |
            ReportKind::SyntaxError |
            ReportKind::TypeError |
            ReportKind::NameError |
            ReportKind::DataFlowError => true,
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
    fn reports(&self) -> i32;
    fn print_errors(&self);
    fn clear_reports(&mut self);

}

#[derive(Debug, Clone)]
pub enum Message {
    HighlightMessage {
        span: Span,
        report_kind: ReportKind,
        message: String,
    }
}

impl Message {
    pub fn is_error(&self) -> bool {
        match self {
            Message::HighlightMessage { span: _, report_kind, message: _ } => {
                report_kind.is_error()
            }
        }
    }

    pub fn highlight_message(
        report_kind: ReportKind,
        span: Span,
        message: String
    ) -> Message {
        Message::HighlightMessage {
            span,
            report_kind,
            message
        }
    }
}

impl PartialEq for Message {

    #[allow(unreachable_patterns)] // for default case, lint complains as HighlightMessage is currently only variant
    fn eq(&self, other: &Message) -> bool {
        match (self, other) {
            (Message::HighlightMessage{
                span: my_span,
                report_kind: my_kind,
                message: _,
            },
            Message::HighlightMessage {
                span: other_span,
                report_kind: other_kind,
                message: _
            }) => {
                my_span == other_span && my_kind == other_kind
            }
            _ => false
        }
    }
}
