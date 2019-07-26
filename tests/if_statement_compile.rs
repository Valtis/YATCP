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

#[test]
fn if_statement_is_taken_with_literal_true() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if true {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn if_statement_is_not_taken_with_literal_false() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            if false {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn if_statement_taken_if_boolean_variable_is_true() {

    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let my_var: bool = true;
            if my_var {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("23\n", output);
}

#[test]
fn if_statement_taken_if_boolean_variable_is_false() {

    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let my_var: bool = false;
            if my_var {
                return 23;
            }
            return 52;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("52\n", output);
}

#[test]
fn if_inside_if_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let my_var: bool = true;
            let count: int = 0;
            let x: int = 50;
            if my_var {
                count = count + 1;
                if 3 < 2 {
                    count = count + 2;
                } else if x < 20 {
                    count = count + 4;
                } else {
                    count = count + 8;
                }
            } else {
                count = count + 16;
            }

            return count;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("9\n", output);

}

/*
    Test case for a bug where successive stores ended up mangling the results; in this case value of
    'b' would be used for when 'a' is needed
*/
#[test]
fn successive_boolean_stores_do_not_overwrite_comparison_results() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: bool = 2 < 3;
            let b: bool = 4 < 3;

            if b {
                return 4;
            } else if a {
                return 2;
            }
            return 0;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("2\n", output);
}



#[test]
fn multiple_successive_ifs_work() {

    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 10;
            let true1: bool = 2 < 3;
            let true2: bool = a == 10;
            let false1: bool = a > 10;
            let false2: bool = 5 >= 6;

            let ret: int = 0;

            if true1 {
                ret = ret + 1;
            }

            if true2 {
                ret = ret + 2;
            }

            if false1 {
                ret = ret + 4;
            }

            if false2 {
                ret = ret + 8;
            }

            return ret;
        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("3\n", output);

}
