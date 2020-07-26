use std::io::Write;


pub fn print_and_panic(msg: &str, file: &str, column: u32) -> !  {
    let stdout = std::io::stdout();
    let stderr = std::io::stderr();
    let _stdout_handle = stdout.lock();
    let mut stderr_handle = stderr.lock();



    stderr_handle.write(format!("\n{}: {}\n\n",
                                ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
                                msg).as_bytes()).unwrap();

    stderr_handle.write(format!("Issue occurred in file {} at line {}\n", file, column).as_bytes()).unwrap();
    stderr_handle.write("This is a bug in the compiler, not in the source file.\n\n\n".as_bytes()).unwrap();
    panic!()
}


#[macro_export]
macro_rules! ice {
    ($fmt:expr) => (
      {
        $crate::ice::print_and_panic($fmt, file!(), line!())
      }
    );
    ($fmt:expr, $($args:tt)+) => (
      {
        let msg = format!($fmt, $($args)+);
        $crate::ice::print_and_panic(&msg, file!(), line!())
      }
    )
}


#[macro_export]
macro_rules! ice_if {
    ($condition:expr, $fmt:expr) => (
      {
       if $condition {
            $crate::ice::print_and_panic($fmt, file!(), line!())
        }
      }
    );
    ($condition:expr, $fmt:expr, $($args:tt)+) => (
      {
        if $condition {
            let msg = format!($fmt, $($args)+);
            $crate::ice::print_and_panic(&msg, file!(), line!())
        }
      }
    )
}

