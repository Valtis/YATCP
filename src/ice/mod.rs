use ansi_term::Colour::Red;
use std::io::Write;


// stupid workarounds for macro export/visibility issues
pub fn red_string(s: &str) -> String {
    Red.bold().paint(s).to_string()
}

pub fn writeln_proxy(s: String) {
  writeln!(&mut ::std::io::stderr(), "{}", s).expect("Failed to write into stderr");
}

macro_rules! println_stderr(
    ($($arg:tt)*) => { {
        ::ice::writeln_proxy(format!($($arg)*));
    } }
);


#[macro_use]
macro_rules! ice {
    ($fmt:expr) => (
      {
        println_stderr!(
            "\n{}: {}\n\nThis is a bug in the compiler, not in the source file.\n",
            ::ice::red_string("Internal compiler error"), $fmt);
          panic!("Stop");
      }
    );
    ($fmt:expr, $($args:tt)+) => (
      {
        println_stderr!(
          "\n{}: {}\n\nThis is a bug in the compiler, not in the source file.\n",
          ::ice::red_string("Internal compiler error"),
          format!($fmt, $($args)+));
        panic!("Stop");
      }
    )
}
