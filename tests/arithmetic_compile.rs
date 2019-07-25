mod compiler_helper;
use self::compiler_helper::{compile_and_run_no_opt, FunctionKind};

#[test]
fn empty_function_compiles() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : void {
            return;
        } "#,
        FunctionKind::VOID("test".to_owned())
    );
    assert_eq!("", output);
}

#[test]
fn empty_void_function_without_return_compiles() {
    let output = compile_and_run_no_opt(
        r#"
        fn test() : void {
        } "#,
        FunctionKind::VOID("test".to_owned())
    );
    assert_eq!("", output);
}

#[test]
fn should_return_constant_value() {

    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 1;
            }
        ",
    FunctionKind::INT("foo".to_owned()));

    assert_eq!("1\n", output);
}

#[test]
fn should_return_constant_through_variable() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a : int = 210;
                return a;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("210\n", output);
}

#[test]
fn should_work_with_lots_of_locals() {

    let program = r"
    fn foo() : int {
        let a : int = 4;
        let b : int = 6;
        let c1 : int = 8;
        let c2 : int = 8;
        let c3 : int = 8;
        let c4 : int = 8;
        let c5 : int = 8;
        let c6 : int = 8;
        let c7 : int = 8;
        let c8 : int = 8;
        let c9 : int = 8;
        let ca : int = 8;
        let cb : int = 8;
        let cc : int = 8;
        let cd : int = 8;
        let ce : int = 8;
        let cf : int = 8;
        let cg : int = 8;
        let ch : int = 8;
        let ci : int = 8;
        let cj : int = 8;
        let ck : int = 8;
        let cl : int = 8;
        let cm : int = 8;
        let cn : int = 8;
        let co : int = 8;
        let cp : int = 8;
        let cq : int = 8;
        let cr : int = 8;
        let cs : int = 8;
        let ct : int = 8;
        let cu : int = 8;
        let cv : int = 8;
        let cx : int = 8;
        let cy : int = 8;
        let cz : int = 8;
        let caa : int = 8;
        let cab : int = 8;
        let cac : int = 8;
        let cad : int = 8;
        let cae : int = 8;
        let caf : int = 8;
        let cag : int = 8;
        let cah : int = 8;
        let cai : int = 8;
        let caj : int = 8;
        let cak : int = 8;
        let cal : int = 8;
        let cam : int = 8;
        let can : int = 8;
        let cao : int = 8;
        let cap : int = 8;
        let caq : int = 8;
        let car : int = 8;
        let cas : int = 8;
        let cat : int = 8;
        let cau : int = 8;
        let cav : int = 8;
        let caw : int = 8;
        let cax : int = 8;
        let cay : int = 8;
        let caz : int = 8;
        let cba : int = 8;
        let cbb : int = 8;
        let cbc : int = 8;
        let cbd : int = 8;
        let cbe : int = 8;
        let cbf : int = 8;
        let cbg : int = 8;
        let cbh : int = 8;
        let cbi : int = 8;
        let cbj : int = 8;
        let cbk : int = 8;
        let cbl : int = 8;
        let cbm : int = 8;
        let cbn : int = 8;
        let cbo : int = 8;
        let cbp : int = 8;
        let cbq : int = 8;
        let cbr : int = 8;
        let cbs : int = 8;
        let cbt : int = 8;
        let cbu : int = 8;
        let cbv : int = 8;
        let cbw : int = 8;
        let cbx : int = 8;
        let cby : int = 8;
        let cbz : int = 8;
        let cca : int = 8;
        let ccb : int = 8;
        let ccc : int = 8;
        let ccd : int = 8;
        let cce : int = 8;
        let ccf : int = 8;
        let ccg : int = 8;
        let cch : int = 8;
        let cci : int = 8;
        let ccj : int = 8;
        let cck : int = 8;
        let ccl : int = 8;
        let ccm : int = 8;
        let ccn : int = 8;
        let cco : int = 8;
        let ccp : int = 8;
        let ccq : int = 8;
        let ccr : int = 8;
        let ccs : int = 8;
        let cct : int = 8;
        let ccu : int = 8;
        let ccv : int = 8;
        let ccw : int = 8;
        let ccx : int = 8;
        let ccy : int = 8;
        let ccz : int = 8;
        let cda : int = 8;
        let cdb : int = 8;
        let cdc : int = 8;
        let cdd : int = 8;
        let cde : int = 8;
        let cdf : int = 8;
        let cdg : int = 8;
        let cdh : int = 8;
        let cdi : int = 8;
        let cdj : int = 8;
        let cdk : int = 8;
        let cdl : int = 8;
        let cdm : int = 8;
        let cdn : int = 8;
        let cdo : int = 8;
        let cdp : int = 8;
        let cdq : int = 8;
        let cdr : int = 8;
        let cds : int = 8;
        let cdt : int = 8;
        let cdu : int = 8;
        let cdv : int = 8;
        let cdw : int = 8;
        let cdx : int = 8;
        let cdy : int = 8;
        let cdz : int = 8;
        let cea : int = 8;
        let ceb : int = 8;
        let cec : int = 8;
        let ced : int = 8;
        let cee : int = 8;
        let cef : int = 8;
        let ceg : int = 8;
        let ceh : int = 8;
        let cei : int = 18;
        let cej : int = 17;
        let cek : int = 16;
        let cel : int = 15;
        let cem : int = 14;
        let cen : int = 13;
        let ceo : int = 12;
        let cep : int = 11;
        let ceq : int = 10;
        let cer : int = 0;
        let ces : int = 8;
        let cet : int = 7;
        let ceu : int = 6;
        let cev : int = 5;
        let cew : int = 4;
        let cex : int = 3;
        let cey : int = 2;
        let cez : int = 102;
        return cez;
    }";

    let output = compile_and_run_no_opt(
        program,
       FunctionKind::INT("foo".to_owned()));

    assert_eq!("102\n", output);
}

#[test]
fn should_return_addition_result() {

    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 1+5;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("6\n", output);
}

#[test]
fn should_return_more_complex_addition_result() {

    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 5;
                let b: int = 6;
                let c: int = a + 1; // = 6
                c = 2 + c; // = 8
                c = c + a; // = 13
                c = a + c; // = 18

                return a + b + c + 4;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("33\n", output);
}

#[test]
fn should_return_subtraction_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 3-5;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("-2\n", output);
}

#[test]
fn should_return_complex_subtraction_result() {
    let output = compile_and_run_no_opt(
r"
            fn foo() : int {
                let a: int = 4;
                let b: int = 6;
                let c: int = 3 - a; // 3 - 4 = -1
                let d: int = c - 6; // -1 - 6 = -7
                let e: int = 3 - 6; // -3
                e = e - a; // -7
                e = a - e; // 11

                return a - b - c - d - e - 20;
            }
        ",

FunctionKind::INT("foo".to_owned()));
    assert_eq!("-25\n", output);
}

#[test]
fn should_return_multiplication_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 6*7;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("42\n", output);
}

#[test]
fn should_return_variable_constant_multiplication() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 7;
                return a*7;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("49\n", output);
}

#[test]
fn should_return_constant_variable_multiplication() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 30;
                return 7*a;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("210\n", output);
}

#[test]
fn should_return_same_variable_variable_multiplication() {
    let output = compile_and_run_no_opt(
        r"
                fn foo() : int {
                    let a: int = 9;
                    return a*a;
                }
            ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("81\n", output);
}
#[test]
fn should_return_variable_variable_multiplication() {
    let output = compile_and_run_no_opt(
    r"
                fn foo() : int {
                    let a: int = 7;
                    let b: int  = 8;
                    return a*b;
                }
            ",
    FunctionKind::INT("foo".to_owned()));

    assert_eq!("56\n", output);
}
// 6 * 8 * 48 * 24 * 432
#[test]
fn should_return_complex_multiplication_result() {
    let output = compile_and_run_no_opt(
        r"
                fn foo() : int {
                    let a: int = 2*3;
                    let b: int = 8;
                    let c: int = a * b;
                    let d: int = a*2;
                    d = 2*d;
                    let e: int = 2*a;
                    c = c*a;
                    c = a*c;

                    return a*b*c*d*e*3;
                }
            ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("71663616\n", output);
}

#[test]
fn should_return_division_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 10/5;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("2\n", output);
}

#[test]
fn should_truncate_division_result() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return 29/10;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("2\n", output);
}

#[test]
fn should_divide_when_using_variable_and_constant() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 30;
                return a/10;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("3\n", output);
}

#[test]
fn should_divide_when_using_constant_and_variable() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 3;
                return 10/a;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("3\n", output);
}

#[test]
fn should_divide_when_using_two_variables() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 3;
                let b: int = 18;
                return b/a;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("6\n", output);
}

#[test]
fn should_work_with_complex_arithmetics_when_variables_used() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a : int = 30;
                let b : int = 3;
                let c : int = (a-b)*2; // 27*2 = 54
                let d : int = c/b; // 54/3 = 18
                return (d + 2)/2+4; // 20/2+4 = 14
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("14\n", output);
}

#[test]
fn should_respect_order_of_operation() {

    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                return (2+3)*5-6/3+6;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("29\n", output);
}

#[test]
fn can_represent_int_max() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 2147483647; // INTMAX
                return a;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("2147483647\n", output);
}

#[test]
fn can_represent_int_min() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = -2147483648; // INTMIN
                return a;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("-2147483648\n", output);
}

#[test]
fn integer_addition_overflows() {
    let output = compile_and_run_no_opt(
        r"
            fn foo() : int {
                let a: int = 2147483647; // INTMAX
                return a + 1;
            }
        ",
        FunctionKind::INT("foo".to_owned()));

    assert_eq!("-2147483648\n", output);
}