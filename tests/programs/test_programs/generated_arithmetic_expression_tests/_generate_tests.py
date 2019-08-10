INT_MAX = 2**31-1
INT_MIN = -INT_MAX-1

"""
    Tuples of:
        * Test file name without extension
        * Initialization expression(s) (e.g. variable declarations)
        * Expression for return statement
        * Expected answer
        """
condition_answer_tuples = [

    # constant
    ("positive_constant_expression_works",
        "",
        "4",
        4),
    ("negative_constant_expression_works",
        "",
        "-4",
        -4),


    # addition
    ("constant_constant_addition_works",
        "",
        "2 + 9",
        11),
    ("constant_constant_addition_with_negative_number_works",
        "",
        "10 + (-5)",
        5),
    ("variable_constant_addition_works",
        "let a: int = 24;",
        "a + 9",
        "33"),
    ("constant_variable_addition_works",
        "let a: int = 49;",
        "5 + a",
        54),
    ("variable_variable_addition_works",
        "let a: int = 123; let b: int = 47;",
        "a + b",
        170),
    ("complex_addition_works",
        """
        let a: int = 20;
        let b: int = 8;
        let c: int = 5 + a + b;
        let d: int = c + 4;
        """,
        "0+a+b+c+d+1",
       99),
    ("can_add_up_to_int_max",
        f"let a: int = {INT_MAX-1};",
        "a + 1",
        INT_MAX),
    ("addition_overflows_when_adding_to_int_max",
        f"let a: int = {INT_MAX};",
        "a + 1",
        INT_MIN),


    # subtraction
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
        "let a: int = 8;",
        "a - 6",
        2),
    ("constant_variable_subtraction_works",
        "let a: int = 8;",
        "5 - a",
        -3),
    ("variable_variable_subtraction_works",
        "let a: int = 8; let b: int = 5402;",
        "a - b",
        -5394),
    ("complex_subtraction_works",
        """
        let a: int = 20;
        let b: int = 8;
        let c: int = 5 - a - b;
        let d: int = c - 4;
        """,
        "0-a-b-c-d-1",
       21),
    ("can_subtract_to_int_min",
        f"let a: int = {INT_MIN+1};",
        "a - 1",
        INT_MIN,
        ),
    ("subtraction_overflows_when_subtracting_from_int_min",
        f"let a: int = {INT_MIN};",
        "a - 1",
        INT_MAX),



    # multiplication
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
        "let a: int = 8;",
        "a*6",
        48),
    ("constant_variable_multiplication_works",
        "let a: int = 8;",
        "5*a",
        40),
    ("variable_variable_multiplication_works",
        "let a: int = 8; let b: int = -1;",
        "a*b",
        -8),
    ("complex_multiplication_works",
        """
        let a: int = 1;
        let b: int = 2;
        let c: int = 3*a*b;
        let d: int = c*4;
        """,
        "a*b*c*d*10",
       2880),
    ("positive_multiplication_result_can_overflow",
        f"let a: int = {INT_MAX};",
        "a*2",
        -2),
    ("negative_multiplication_result_can_overflow",
        f"let a: int = {INT_MIN};",
        "a*(-1)",
        INT_MIN,
        ),


    # division
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
        "let a: int = 20;",
        "a/5",
        4),
    ("constant_variable_division_works",
        "let a: int = 8;",
        "24/a",
        3),
    ("variable_variable_division_works",
        "let a: int = 8; let b: int = -1;",
        "a/b",
        -8),
    ("complex_division_works",
        """
        let a: int = 10;
        let b: int = 2;
        let c: int = a/b;
        let d: int = c/2;
        """,
        "500/a/b/c/d",
       2),

    ("can_negate_variable",
        """
        let a: int = 4;
        a = -a;
        """,
        "a",
        -4),
    ("can_negate_negative_variable",
        """
        let a: int = -4;
        a = -a;
        """,
        "a",
        4),
    ("can_negate_variable_when_assigning_to_another_variable",
        """
        let a: int = 4;
        let b: int = -a;
        """,
        "b",
        -4),
    ("can_negate_negative_variable_when_assigning_to_another_variable",
        """
        let a: int = -4;
        let b: int = -a;
        """,
        "b",
        4),
    ("can_negate_int_max",
        f"""
        let a: int = {INT_MAX};
        let b: int = -a;
        """,
        "b",
        -INT_MAX),
    ("negating_int_min_overflows_to_int_min",
        f"""
        let a: int = {INT_MIN};
        let b: int = -a;
        """,
        "b",
        INT_MIN),
]

for t in condition_answer_tuples:

    out = f'''
program: |
    fn test_function() : int {{
        {t[1]}
        return {t[2]};
    }}
link_with:
    - tests/files/support/support.c
callable: test_function
returns: int
expect_stdout: |
    {t[3]}
    '''
    with open(f"{t[0]}.yaml", "w") as f:
        f.write(out)




