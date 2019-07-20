mod compiler_helper;
use self::compiler_helper::{compile_and_run_no_opt, FunctionKind};

#[test]
fn constant_constant_less_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 3 < 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
assert_eq!("52\n", output);
}

#[test]
fn constant_constant_less_than_evaluates_false_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 < 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_less_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 < 3 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn else_block_is_used() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 < 1 {
                return 23;
            } else {
                return 52;
            }

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_less_or_equal_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 3 <= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_less_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 <= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_constant_less_or_equal_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 1 <= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_constant_equal_to_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 3 == 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_equal_to_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 == 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_constant_equal_to_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 1 == 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_greater_or_equal_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 3 >= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_constant_greater_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 >= 2 {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_constant_greater_or_equal_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 1 >= 2 {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_greater_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 3 > 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_constant_greater_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 2 > 2 {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_greater_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 1 > 2 {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}


#[test]
fn variable_constant_less_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            if a < 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_less_than_evaluates_false_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if a < 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_less_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if a < 3 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_less_or_equal_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            if a <= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_less_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if a <= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_less_or_equal_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            if a <= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_equal_to_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            if a == 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_equal_to_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if a == 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_equal_to_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            if a == 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_greater_or_equal_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            if a >= 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_greater_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if a >= 2 {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_greater_or_equal_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            if a >= 2 {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_greater_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            if a > 2 {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_constant_greater_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if a > 2 {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_constant_greater_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            if a > 2 {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}


#[test]
fn constant_variable_less_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 3 < a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_less_than_evaluates_false_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 2 < a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_less_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            if 2 < a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_less_or_equal_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 3 <= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_less_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 2 <= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_less_or_equal_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 1 <= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_equal_to_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 3 == a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_equal_to_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 2 == a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_equal_to_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 1 == a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_greater_or_equal_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 3 >= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_greater_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 2 >= a {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_greater_or_equal_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 1 >= a {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_greater_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 3 > a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn constant_variable_greater_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 2 > a {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_variable_greater_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            if 1 > a {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}


#[test]
fn variable_variable_less_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 3;
            if b < a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_less_than_evaluates_false_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 2;
            if b < a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_less_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 3;
            let b: int = 2;
            if b < a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_less_or_equal_than_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 3;
            if b <= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_less_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b : int = 2;
            if b <= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_less_or_equal_than_evaluates_true_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 1;
            if b <= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_equal_to_evaluates_false_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 3;
            if b == a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_equal_to_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 2;
            if b == a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_equal_to_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 1;
            if b == a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_greater_or_equal_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 3;
            if b >= a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_greater_or_equal_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 2;
            if b >= a {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_greater_or_equal_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 1;
            if b >= a {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_greater_than_evaluates_true_when_greater() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 3;
            if b > a {
                return 23;
            }

            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn variable_variable_greater_than_evaluates_true_when_equal() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 2;
            if b > a {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn variable_variable_greater_than_evaluates_false_when_less() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 2;
            let b: int = 1;
            if b > a {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn constant_constant_order_of_operation_is_respected() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if 1 + 3 > 2*6 {
                return 23;
            }
            return 52;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}
