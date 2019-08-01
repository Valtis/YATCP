use super::{ErrorReporter, Message, ReportKind};

use crate::ast::NodeInfo as Span;

pub struct NullReporter {
    messages: Vec<Message>
}

impl NullReporter {
    pub fn new() -> NullReporter {
        NullReporter {
            messages: vec![],
        }
    }

    pub fn get_messages(&self) -> &Vec<Message> {
        &self.messages
    }
}

impl ErrorReporter for NullReporter {

    fn report_error(
        &mut self,
        report_kind: ReportKind,
        span: Span,
        message: String) {

        self.messages.push(
            Message::HighlightMessage{
                span,
                report_kind,
                message
        });
    }

    fn has_errors(&self) -> bool {
        self.messages.iter().any(|msg| msg.is_error())
    }

    fn has_reports(&self) -> bool {
        !self.messages.is_empty()
    }

    fn errors(&self) -> i32 {
        self.messages.iter().filter(|msg| msg.is_error()).count() as i32
    }

    fn reports(&self) -> i32 {
        self.messages.len() as i32
    }

    fn print_errors(&self) {
        () // do nothing
    }
}
