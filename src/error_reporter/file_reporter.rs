use super::{ErrorReporter, ReportKind, Message };

use crate::ast::NodeInfo as Span;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::cmp;
use std::iter;



/*
Reports errors by printing/highlighting content from the given file
*/
pub struct FileErrorReporter {
    file_path: String,
    errors: i32,
    messages: Vec<Message>,
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
        // Todo: Replace with something slightly saner
        // Works for small files, but can be expensive memory wise in case the source file is huge
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
    fn report_error(&mut self, report_kind: ReportKind, span: Span, message : String) {

        self.update_error_count(&report_kind);

        self.messages.push(
            Message::HighlightMessage {
                span,
                report_kind,
                message
            });

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

    fn reports(&self) -> i32 {
        self.messages.len() as i32
    }

    fn print_errors(&self) {
        let lines = self.read_lines();

        for msg in self.messages.iter() {
            match msg {
                Message::HighlightMessage{
                    span,
                    report_kind,
                    message,
                } => {
                    write_highlight_message(span, *report_kind, message, lines.as_slice())
                }
            }
        }

    }
}

fn write_highlight_message(span: &Span, report_kind: ReportKind, message: &String, lines: &[String]) {

    // group notes with the warning/error, otherwise add a newline
    if report_kind != ReportKind::Note {
        eprintln!();
    }

    // main error/warning/note print
    eprintln!("{}:{} {}: {}",
            span.line,
            span.column,
            report_kind,
            message);

    // print line
    let line = &lines[(span.line-1) as usize];
    eprint!("{}", line);
    if !line.ends_with("\n") {
        eprintln!();
    }

    // indentation for highlighting line
    eprint!("{}",
        iter::repeat(" ").
            take(cmp::max(span.column-1, 0) as usize).
            collect::<String>());


    // highlighting
    let color = report_kind.get_color();
    for _ in 0..span.length {
        eprint!("{}", color.bold().paint("^").to_string());
    }
    eprintln!();
}

