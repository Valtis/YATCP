BYTE_MAX = 127
BYTE_MIN = -128

"""
    Tuples of:
        * Test file name without extension
        * Initialization expression(s) (e.g. variable declarations)
        * Expression for return statement
        * Expected answer
        """
test_tuples = [

    # constant
    ("positive_constant_expression_works",
        "",
        "4",
        4),
    ("negative_constant_expression_works",
        "",
        "-4",
        -4),

    # # addition
    ("constant_constant_addition_works",
        "",
        "2 + 9",
        11),
    ("constant_constant_addition_with_negative_number_works",
        "",
        "10 + (-5)",
        5),
    ("variable_constant_addition_works",
        "let a: byte = 24;",
        "a + 9",
        "33"),
    ("constant_variable_addition_works",
        "let a: byte = 49;",
        "5 + a",
        54),
    ("variable_variable_addition_works",
        "let a: byte = 50; let b: byte = 47;",
        "a + b",
        97),
    ("complex_addition_works",
        """
        let a: byte = 20;
        let b: byte = 8;
        let c: byte = 5 + a + b;
        let d: byte = c + 4;
        """,
        "0+a+b+c+d+1",
       99),
    ("can_add_up_to_byte_max",
        f"let a: byte = {BYTE_MAX-1};",
        "a + 1",
        BYTE_MAX),
    ("addition_overflows_when_adding_to_byte_max",
        f"let a: byte = {BYTE_MAX};",
        "a + 1",
        BYTE_MIN),


    # # subtraction
    ("constant_constant_subtraction_works_when_result_is_positive",
       "",
       "3-1",
       2),
    ("constant_constant_subtraction_works_when_result_is_negative",
       "",
       "3-9",
       -6),
    ("constant_constant_subtraction_works_when_rhs_operand_is_negated",
       "",
       "3-(-2)",
       5),
    ("variable_constant_subtraction_works",
        "let a: byte = 8;",
        "a - 6",
        2),
    ("constant_variable_subtraction_works",
        "let a: byte = 8;",
        "5 - a",
        -3),
    ("variable_variable_subtraction_works",
        "let a: byte = 8; let b: byte = 38;",
        "a - b",
        -30),
    ("complex_subtraction_works",
        """
        let a: byte = 20;
        let b: byte = 8;
        let c: byte = 5 - a - b;
        let d: byte = c - 4;
        """,
        "0-a-b-c-d-1",
       21),
    ("can_subtract_to_byte_min",
        f"let a: byte = {BYTE_MIN+1};",
        "a - 1",
        BYTE_MIN,
        ),
    ("subtraction_overflows_when_subtracting_from_byte_min",
        f"let a: byte = {BYTE_MIN};",
        "a - 1",
        BYTE_MAX),


    # # multiplication
    ("constant_constant_multiplication_works_when_result_is_positive",
       "",
       "3*7",
       21),
    ("constant_constant_multiplication_works_when_lhs_operand_is_negative",
       "",
       "-3*9",
       -27),
    ("constant_constant_multiplication_works_when_rhs_operand_is_negated",
       "",
       "3*(-2)",
       -6),
    ("variable_constant_multiplication_works",
        "let a: byte = 8;",
        "a*6",
        48),
    ("constant_variable_multiplication_works",
        "let a: byte = 8;",
        "5*a",
        40),
    ("variable_variable_multiplication_works",
        "let a: byte = 8; let b: byte = -1;",
        "a*b",
        -8),
    ("complex_multiplication_works",
        """
        let a: byte = 1;
        let b: byte = 2;
        let c: byte = 2*a;
        let d: byte = c*2;
        """,
        "a*b*c*d*2",
       32),
    ("positive_multiplication_result_can_overflow",
        f"let a: byte = 100;",
        "a*2",
        -56),
    ("negative_multiplication_result_can_overflow",
        f"let a: byte = {BYTE_MIN};",
        "a*(-1)",
        BYTE_MIN,
        ),


    # # division
    ("constant_constant_division_works_when_result_is_positive",
       "",
       "7/2",
       3),
    ("constant_constant_division_works_when_lhs_operand_is_negative",
       "",
       "-12/4",
       -3),
    ("constant_constant_division_works_when_rhs_operand_is_negated",
       "",
       "5/(-2)",
       -2),
    ("variable_constant_division_works",
        "let a: byte = 20;",
        "a/5",
        4),
    ("constant_variable_division_works",
        "let a: byte = 8;",
        "24/a",
        3),
    ("variable_variable_division_works",
        "let a: byte = 8; let b: byte = -1;",
        "a/b",
        -8),
    ("complex_division_works",
        """
        let a: byte = 3;
        let b: byte = 2;
        let c: byte = a/b;
        let d: byte = 8/2;
        """,
        "127/a/b/c/d",
       5),


    # # modulo 
    ("constant_constant_modulo_works_when_result_is_positive",
       "",
       "7 % 2",
       1),
    ("constant_constant_modulo_works_when_lhs_operand_is_negative",
       "",
       "-12%5",
       -2),

    ("constant_constant_modulo_works_when_rhs_operand_is_negated",
       "",
       "5%(-2)",
       1),

    ("variable_constant_modulo_works",
        "let a: byte = 20;",
        "a%3",
        2),

    ("constant_variable_modulo_works",
        "let a: byte = 9;",
        "24 % a",
        6),
    ("variable_variable_modulo_works",
        "let a: byte = 13; let b: byte = 5;",
        "a%b",
        3),

    # # negation 

    ("can_negate_variable",
        """
        let a: byte = 4;
        a = -a;
        """,
        "a",
        -4),
    ("can_negate_negative_variable",
        """
        let a: byte = -4;
        a = -a;
        """,
        "a",
        4),
    ("can_negate_variable_when_assigning_to_another_variable",
        """
        let a: byte = 4;
        let b: byte = -a;
        """,
        "b",
        -4),
    ("can_negate_negative_variable_when_assigning_to_another_variable",
        """
        let a: byte = -4;
        let b: byte = -a;
        """,
        "b",
        4),
    ("can_negate_byte_max",
        f"""
        let a: byte = {BYTE_MAX};
        let b: byte = -a;
        """,
        "b",
        -BYTE_MAX),
    ("negating_byte_min_overflows_to_byte_min",
        f"""
        let a: byte = {BYTE_MIN};
        let b: byte = -a;
        """,
        "b",
        BYTE_MIN),


    # # +=
    ("self_addition_shorthand_works",
        f"""
        let a: byte = 4;
        a += 6;
        """,
        "a",
        10),

    # # -=
    ("self_subtraction_shorthand_works",
        f"""
        let a: byte = 4;
        a -= 6;
        """,
        "a",
        -2),



    # # *=
    ("self_multiplication_shorthand_works",
        f"""
        let a: byte = 4;
        a *= 6;
        """,
        "a",
        24),



    # # /=
    ("self_division_shorthand_works",
        f"""
        let a: byte = 9;
        a /= 2;
        """,
        "a",
        4),


    # # %=
    ("self_modulo_shorthand_works",
        f"""
        let a: byte = 14;
        a %= 5;
        """,
        "a",
        4),

]

for t in test_tuples:

    out = f'''
program: |
    fn test_function() : byte {{
        {t[1]}
        return {t[2]};
    }}
link_with:
    - tests/files/support/support.c
callable: test_function
returns: byte 
expect_stdout: |
    {t[3]}
    '''
    with open(f"{t[0]}.yaml", "w") as f:
        f.write(out)



