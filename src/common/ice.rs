use std::io::Write;
use backtrace::*;
use std::path::Component::Normal;

#[track_caller]
pub fn print_and_panic(msg: &str, file: &str, column: u32) -> !  {
    let stdout = std::io::stdout();
    let stderr = std::io::stderr();
    let _stdout_handle = stdout.lock();
    let mut stderr_handle = stderr.lock();



    stderr_handle.write(format!("\n{}: {}\n\n",
                                ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
                                msg).as_bytes()).unwrap();

    stderr_handle.write(format!("Issue occurred in file {} at line {}\n", file, column).as_bytes()).unwrap();
    stderr_handle.write("This is a bug in the compiler, not in the source file.\n\n".as_bytes()).unwrap();

    stderr_handle.write("Stacktrace:\n".as_bytes()).unwrap();

    let bt = Backtrace::new();

    // filter out non-compiler frames - don't care about Rust stdlib internals
    // also skip this function
    'out:
    for (i, frame) in bt.frames().iter().skip(1).enumerate() {

        for symbol in frame.symbols().iter() {
            let name = if let Some(name) = symbol.name() {
                let name = name.to_string();

                if !name.starts_with("compiler") {
                    break 'out;
                }

                let mut split_name = name.split("::").peekable();

                let mut filtered_name = String::new();
                while split_name.peek().is_some() {
                    let value = split_name.next();

                    // filter last element out
                    if split_name.peek().is_none() {
                        break;
                    }

                    if !filtered_name.is_empty() {
                        filtered_name += "::";
                    }
                    filtered_name += value.unwrap();
                }
                filtered_name
            } else {
                "<No symbol name>".to_owned()
            };

            let file = if let Some(file) = symbol.filename() {
                let mut filtered_path = String::new();
                let mut append = false;
                for c in file.components() {
                    if let Normal(c) = c {
                        let utf8_str = c.to_string_lossy().to_string();
                        if utf8_str == *"src" {
                            append = true;
                        }
                        if append {
                            if !filtered_path.is_empty() {
                                filtered_path += "/";
                            }
                            filtered_path += &utf8_str;
                        }
                    }
                }

                filtered_path
            } else {
                "<No file name>".to_owned()
            };

            let line = if let Some(line) = symbol.lineno() {
                line
            } else {
                0
            };

            let column = if let Some(column) = symbol.colno() {
                column
            } else {
                0
            };

            stderr_handle.write(format!("   {}: {}\n", i, name).as_bytes()).unwrap();
            stderr_handle.write(format!("\t     at {}:{}:{}\n", file, line, column).as_bytes()).unwrap();
        }
    };

    std::process::exit(255);
}


#[macro_export]
macro_rules! ice {
    ($fmt:expr) => (
      {
        $crate::common::ice::print_and_panic($fmt, file!(), line!())
      }
    );
    ($fmt:expr, $($args:tt)+) => (
      {
        let msg = format!($fmt, $($args)+);
        $crate::common::ice::print_and_panic(&msg, file!(), line!())
      }
    )
}


#[macro_export]
macro_rules! ice_if {
    ($condition:expr, $fmt:expr) => (
      {
       if $condition {
            $crate::common::ice::print_and_panic($fmt, file!(), line!())
        }
      }
    );
    ($condition:expr, $fmt:expr, $($args:tt)+) => (
      {
        if $condition {
            let msg = format!($fmt, $($args)+);
            $crate::common::ice::print_and_panic(&msg, file!(), line!())
        }
      }
    )
}

