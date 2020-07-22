mod test_harness;
mod compiler_helper;
mod helper;

fn main() {

    // FIXME: implement proper arg parsing

    let test_name_includes = if std::env::args().len() > 1 {
        let args = std::env::args();
        args.skip(1).take(1).collect::<_>()
    } else {
        "".to_owned()
    };

    std::process::exit(test_harness::run_tests(&test_name_includes));
}
