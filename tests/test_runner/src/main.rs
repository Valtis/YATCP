mod test_harness;
mod compiler_helper;

fn main() {
    std::process::exit(test_harness::run_tests());
}
