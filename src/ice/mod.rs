#[macro_use]
macro_rules! ice {
    ($fmt:expr) => (
      {
       eprintln!(
            "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
            ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
            $fmt,
            file!(),
            line!());
          panic!("Stop");
      }
    );
    ($fmt:expr, $($args:tt)+) => (
      {
        eprintln!(
          "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
          ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
          format!($fmt, $($args)+),
          file!(),
          line!(),
          );
        panic!("Stop");
      }
    )
}


#[macro_use]
macro_rules! ice_if {
    ($condition:expr, $fmt:expr) => (
      {
       if $condition {
           eprintln!(
                "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
                ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
                $fmt,
                file!(),
                line!(),
                );
              panic!("Stop");
        }
      }
    );
    ($condition:expr, $fmt:expr, $($args:tt)+) => (
      {
        if $condition {
            eprintln!(
              "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
              ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
              format!($fmt, $($args)+),
              file!(),
              line!(),
              );
            panic!("Stop");
        }
      }
    )
}

