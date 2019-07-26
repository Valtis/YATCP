mod compiler_helper;
use self::compiler_helper::{compile_and_run_no_opt, FunctionKind};

#[test]
fn constant_constant_while_loop_not_taken_if_condition_is_false() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 0;
            while 3 < 2 {
                a = a + 1;
            }

            return a;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("0\n", output);
}

#[test]
fn variable_constant_while_loop_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            let i: int = 0;
            while i < 5 {
                a = a*2;
                i = i + 1;
            }

            return a;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("32\n", output);
}

#[test]
fn constant_variable_while_loop_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            let i: int = 0;
            while 16 > i {
                a = a*2;
                i = i + 1;
            }

            return a;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("65536\n", output);
}

#[test]
fn variable_variable_while_loop_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            let i: int = 0;
            let j: int = 7;
            while i <= j {
                a = a*2;
                i = i + 1;
            }

            return a;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("256\n", output);
}

#[test]
fn order_of_operation_is_respected_in_while_loop_condition() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let a: int = 1;
            let i: int = 0;
            let j: int = 7;
            while i + 1 <= j*2-8 {
                a = a*2;
                i = i + 1;
            }

            return a;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("64\n", output);
}

#[test]
fn while_loop_inside_while_loop_works() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : int {
            let i: int = 0;
            let j: int = 7;
            let cnt: int = 0;
            while i < 10 {
                while j >= 0 {
                   cnt = cnt + 1;
                   j = j - 1;
                }
                i = i + 1;
                cnt = cnt+2;
                j = 7;
            }

            return cnt;

        } "#,
        FunctionKind::INT("test".to_owned())
    );
    assert_eq!("100\n", output);

}
