

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use ansi_term::Colour::Red;
use ansi_term::Colour::Cyan;
use ansi_term;

#[derive(Debug, PartialEq)]
pub enum Error {
    Note,
    TokenError,
    TypeError,
    NameError,
}

impl Error {
    fn get_color(&self) -> ansi_term::Colour {
        match *self {
            Error::Note => Cyan,
            Error::TokenError => Red,
            Error::TypeError => Red,
            Error::NameError => Red,
        }
    }
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        let color = self.get_color();
        let str = match *self {
            Error::Note => color.bold().paint("Note").to_string(),
            Error::TokenError => color.bold().paint("Token error").to_string(),
            Error::TypeError => color.bold().paint("Type error").to_string(),
            Error::NameError => color.bold().paint("Name error").to_string(),           
        };
   
        write!(formatter, "{}", str)
    }
}

pub trait ErrorReporter {
    fn report_error(&mut self, error_type: Error, line: i32, column: i32, token_length : i32, error_string: String);
}

pub struct FileErrorReporter {
    file_path: String,
}

impl FileErrorReporter {
    pub fn new(file: &str) -> FileErrorReporter {
        FileErrorReporter {
            file_path: file.to_string(),
        }
    }
}

impl ErrorReporter for FileErrorReporter {
    fn report_error(&mut self, error_type: Error, line: i32, column: i32, token_length : i32, error : String) {
        match writeln!(&mut ::std::io::stderr(), "{}:{} {}: {}", line, column, error_type, error) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }

        // Todo: Replace with something sligtly saner
        let f = match File::open(&self.file_path) {
            Ok(file) => file,
            Err(e) => panic!("Failed to open file {}: {}", self.file_path, e),
        };

        let mut reader = BufReader::new(f);
        
        let mut buffer = String::new();    
        for _ in 0..(line)  {
            buffer = String::new();             
            match reader.read_line(&mut buffer) {
                Ok(_) => { }, 
                Err(e) => panic!("IO error: {}", e),
            }
        }

        print!("{}", buffer);
        if !buffer.ends_with("\n") {
            println!("");
        }
        for _ in 0..(column-1) {
            print!(" ");
        }

        let color = error_type.get_color();

        print!("{}", color.bold().paint("^").to_string());
        for _ in 0..(token_length-1) {
            print!("{}", color.bold().paint("~").to_string());
        }
        println!("")

    }
}

