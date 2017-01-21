use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

use std::cmp;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::iter;

use ansi_term::Colour::Red;
use ansi_term::Colour::Cyan;
use ansi_term;

#[derive(Debug, PartialEq)]
pub enum Error {
    Note,
    TokenError,
    SyntaxError,
    TypeError,
    NameError,
}

impl Error {
    fn get_color(&self) -> ansi_term::Colour {
        match *self {
            Error::Note => Cyan,
            Error::TokenError 
                | Error::SyntaxError
                | Error::TypeError 
                | Error::NameError=> Red,
        }
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        let color = self.get_color();
        let str = match *self {
            Error::Note => color.bold().paint("Note").to_string(),
            Error::TokenError => color.bold().paint("Token error").to_string(),
            Error::SyntaxError => color.bold().paint("Syntax error").to_string(),
            Error::TypeError => color.bold().paint("Type error").to_string(),
            Error::NameError => color.bold().paint("Name error").to_string(),           
        };
   
        write!(formatter, "{}", str)
    }
}

pub trait ErrorReporter {
    fn report_error(&mut self, error_type: Error, line: i32, column: i32, token_length : i32, error_string: String);
}

struct Message {
    line: i32,
    column: i32,
    length: i32,
    error: Error,
    message: String,
}

pub struct FileErrorReporter {
    file_path: String,
    errors: i32,
    messages: Vec<Message>
}

impl FileErrorReporter {
    pub fn new(file: &str) -> FileErrorReporter {
        FileErrorReporter {
            file_path: file.to_string(),
            errors: 0,
            messages: vec![],
        }
    }

    pub fn has_errors(&self) -> bool {
        self.errors != 0
    }

    pub fn errors(&self) -> i32 {
        self.errors
    }

    pub fn print_errors(&self) {
        let lines = self.read_lines();

        fn write_stderr(txt: String) {
            match write!(&mut ::std::io::stderr(), "{}", txt) {
                Ok(_) => {},
                Err(x) => panic!("Unable to write to stderr: {}", x),
            }

        }
        for msg in self.messages.iter() {

            if msg.error != Error::Note {
                write_stderr("\n".to_string());
            }

            write_stderr(
                format!(
                    "{}:{} {}: {}\n", 
                    msg.line, 
                    msg.column, 
                    msg.error, 
                    msg.message));

            let line = &lines[(msg.line-1) as usize];
            
            write_stderr(format!("{}", line));
            if !line.ends_with("\n") {
                write_stderr("\n".to_string());
            }

            write_stderr(
                iter::repeat(" ").
                take(cmp::max(msg.column-1, 0) as usize).
                collect::<String>());
            let color = msg.error.get_color();

            write_stderr(color.bold().paint("^").to_string());
            for _ in 0..(msg.length-1) {
                write_stderr(color.bold().paint("~").to_string());
            }
            write_stderr("\n".to_string());
        }

    }

    fn read_lines(&self) -> Vec<String> {
        let mut lines = vec![];
        // Todo: Replace with something sligtly saner
        let f = match File::open(&self.file_path) {
            Ok(file) => file,
            Err(e) => panic!("Failed to open file {}: {}", self.file_path, e),
        };

        let mut reader = BufReader::new(f);
        let mut buffer = String::new();    
        for line in reader.lines()  {
            match line {
                Ok(content) => lines.push(content), 
                Err(e) => panic!("IO error: {}", e),
            }
        }
        lines
    }
}

impl ErrorReporter for FileErrorReporter {
    fn report_error(&mut self, error_type: Error, line: i32, column: i32, token_length : i32, message : String) {
     
        match error_type {
            Error::TokenError 
            | Error::TypeError 
            | Error::NameError
            | Error::SyntaxError => self.errors += 1,
            Error::Note => {},
        }

        self.messages.push(
            Message {
                line: line,
                column: column,
                length: token_length,
                error: error_type,
                message: message
            });

    }

}

