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

#[test]
fn can_assign_function_return_value_to_variable() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = bar();
            return a;
        }

        fn bar() : int {
            return 12345;
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("12345\n", output);
}

#[test]
fn can_use_functions_in_expressions() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = bar()*3 + bar()/2-2;
            return a*bar()-bar();
        }

        fn bar() : int {
            return 9;
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("252\n", output);
}

#[test]
fn can_call_function_with_constant_integer_parameter() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            return bar(5);
        }

        fn bar(a: int ) : int {
            return 3*a;
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("15\n", output);
}

#[test]
fn can_call_function_with_multiple_constant_integer_parameter() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            return bar(1, 2, 3, 4, 5, 6 );
        }

        fn bar(a: int, b: int, c: int, d: int, e: int, f: int) : int {
            return (a + b + c - d - e)*f;
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("-18\n", output);
}

#[test]
fn can_call_function_with_multiple_constant_integer_parameter_which_calls_another_function_with_constant_arguments() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            return bar(5, 2);
        }

        fn bar(a: int, b: int) : int {
            return baz(3, 4)*a+b;
        }

        fn baz(a: int, b: int) : int {
            return a-b;
        }

        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("-3\n", output);
}

#[test]
fn can_call_function_with_variables() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            let b: int = 6;
            return bar(a, b);
        }

        fn bar(a: int, b: int) : int {
            return b/a;
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("2\n", output);
}

#[test]
fn can_call_function_with_variables_and_expressions() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            let b: int = 6;
            return bar(a+2, b*7, 9-3);
        }

        fn bar(a: int, b: int, c: int) : int {
            return b*a/c;
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("35\n", output);
}

#[test]
fn recursive_functions_work() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {

            return factorial(8);
        }

        fn factorial(n: int) : int {
            if n <= 1 {
                return 1;
            }

            return n*factorial(n-1);
        }
        "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("40320\n", output);
}
