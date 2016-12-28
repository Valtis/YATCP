

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Result;
use std::io::Write;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

use ansi_term::Colour::Red;
use ansi_term::Colour::Cyan;

pub enum Error {
    Note,
    TypeError,
    NameError,
}

impl Display for Error {
    fn fmt(&self, formatter: &mut Formatter) -> Result {
        let str = match *self {
            Error::Note => Cyan.bold().paint("Note").to_string(),
            Error::TypeError => Red.bold().paint("Type error").to_string(),
            Error::NameError => Red.bold().paint("Name error").to_string(),           
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
        for _ in 0..(column-1) {
            print!(" ");
        }

        let color = Red;

        print!("{}", color.bold().paint("^").to_string());
        for _ in 0..(token_length-1) {
            print!("{}", color.bold().paint("~").to_string());
        }
        println!("")

    }
}

