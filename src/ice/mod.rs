#[macro_use]
macro_rules! ice {
    ($fmt:expr) => (
      {
       let error = format!(
            "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
            ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
            $fmt,
            file!(),
            line!());
          panic!(error)
      }
    );
    ($fmt:expr, $($args:tt)+) => (
      {
        let error = format!(
          "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
          ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
          format!($fmt, $($args)+),
          file!(),
          line!(),
          );
        panic!(error);
      }
    )
}


#[macro_use]
macro_rules! ice_if {
    ($condition:expr, $fmt:expr) => (
      {
       if $condition {
           let error = format!(
                "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
                ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
                $fmt,
                file!(),
                line!(),
                );
              panic!(error);
        }
      }
    );
    ($condition:expr, $fmt:expr, $($args:tt)+) => (
      {
        if $condition {
            let error = format!(
              "\n{}: {}\n\nIssue occurred in file {} at line {}\n\nThis is a bug in the compiler, not in the source file.\n",
              ansi_term::Colour::Red.bold().paint("Internal compiler error").to_string(),
              format!($fmt, $($args)+),
              file!(),
              line!(),
              );
            panic!(error);
        }
      }
    )
}

