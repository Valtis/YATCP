mod compiler_helper;
use self::compiler_helper::{compile_and_run_no_opt, FunctionKind};

#[test]
fn call_to_simple_int_function_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            return bar();
        }

        fn bar() : int {
            return 5;
        }

        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("5\n", output);
}

#[test]
fn calling_function_that_calls_function_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            return bar();
        }

        fn bar() : int {
            return baz();
        }

        fn baz() : int {
            return 3543;
        }

        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("3543\n", output);
}

#[test]
fn calling_function_that_calls_function_that_calls_function_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            return bar();
        }

        fn bar() : int {
            return baz();
        }

        fn baz() : int {
            return qux();
        }

        fn qux() : int {
            return 3543;
        }

        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("3543\n", output);
}
