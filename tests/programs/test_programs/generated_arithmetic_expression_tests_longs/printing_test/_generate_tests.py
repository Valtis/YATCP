LONG_MAX = 2**63-1
LONG_MIN = -LONG_MAX-1


"""
    Tuples of:
        * Test file name without extension
        * Initialization expression(s) (e.g. variable declarations)
        * Expression for return statement
        * Expected answer
        """
test_tuples = [

    # constant
    ("large_long_positive_constant_expression_works",
        "",
        "5000000000000l",
        5000000000000),
    ("large_long_negative_constant_expression_works",
        "",
        "-5000000000000l",
        -5000000000000),

    # addition
    ("large_long_constant_constant_addition_works",
        "let a = 5000000000000l + 7000000000000l;",
        "a",
        12000000000000),

    ("large_long_variable_constant_addition_works",
        """
        let a = 5000000000000l;
        let b = a + 7000000000000l;""",
        "b",
        12000000000000),
    ("large_long_variable_constant_self_addition_works",
        """
        let a = 5000000000000l;
        a = a + 7000000000000l;""",
        "a",
        12000000000000),


    ("large_long_constant_variable_addition_works",
        """
        let a = 5000000000000l;
        let b = 7000000000000l + a;""",
        "b",
        12000000000000),
    ("large_long_constant_variable_self_addition_works",
        """
        let a = 5000000000000l;
        a = 7000000000000l + a;""",
        "a",
        12000000000000),


    ("large_long_varible_variable_addition_works",
        """
        let a = 5000000000000l;
        let b = 7000000000000l;
        let c = a + b;
        """,
        "c",
        12000000000000),
    ("large_long_variable_variable_self_addition_works",
        """
        let a = 5000000000000l;
        let b = 7000000000000l;
        a = a + b;""",
        "a",
        12000000000000),

    # subtraction
    ("large_long_constant_constant_subtraction_works",
        "let a = 5000000000000l - 7000000000000l;",
        "a",
        -2000000000000),

    ("large_long_variable_constant_subtraction_works",
        """
        let a = 5000000000000l;
        let b = a - 7000000000000l;""",
        "b",
        -2000000000000),
    ("large_long_variable_constant_self_subtraction_works",
        """
        let a = 5000000000000l;
        a = a - 7000000000000l;""",
        "a",
        -2000000000000),

    ("large_long_constant_variable_subtraction_works",
        """
        let a = 5000000000000l;
        let b = 7000000000000l - a;""",
        "b",
        2000000000000),
    ("large_long_constant_variable_self_subtraction_works",
        """
        let a = 5000000000000l;
        a = 7000000000000l - a;""",
        "a",
        2000000000000),


    ("large_long_varible_variable_subtraction_works",
        """
        let a = 5000000000000l;
        let b = 7000000000000l;
        let c = a - b;
        """,
        "c",
        -2000000000000),
    ("large_long_variable_variable_self_subtraction_works",
        """
        let a = 5000000000000l;
        let b = 7000000000000l;
        a = a - b;""",
        "a",
        -2000000000000),



    # multiplication
    ("large_long_constant_constant_multiplication_works",
        "let a = 2l*7000000000000l;",
        "a",
        14000000000000),

    ("large_long_variable_constant_multiplication_works",
        """
        let a = 2l;
        let b = a*7000000000000l;""",
        "b",
        14000000000000),
    ("large_long_variable_constant_self_multiplication_works",
        """
        let a = 2l;
        a = a*7000000000000l;""",
        "a",
        14000000000000),

    ("large_long_constant_variable_multiplication_works",
        """
        let a = 2l;
        let b = 7000000000000l*a;""",
        "b",
        14000000000000),
    ("large_long_constant_variable_self_multiplication_works",
        """
        let a = 2l;
        a = 7000000000000l*a;""",
        "a",
        14000000000000),


    ("large_long_varible_variable_multiplication_works",
        """
        let a = 2l;
        let b = 7000000000000l;
        let c = a*b;
        """,
        "c",
        14000000000000),
    ("large_long_variable_variable_self_multiplication_works",
        """
        let a = 2l;
        let b = 7000000000000l;
        a = a*b;""",
        "a",
        14000000000000),


    # division
    ("large_long_constant_constant_division_works",
        "let a = 14000000000000l/7000000000000l;",
        "a",
        2),

    ("large_long_variable_constant_division_works",
        """
        let a = 14000000000000l;
        let b = a/7000000000000l;""",
        "b",
        2),
    ("large_long_variable_constant_self_division_works",
        """
        let a = 14000000000000l;
        a = a/7000000000000l;""",
        "a",
        2),

    ("large_long_constant_variable_division_works",
        """
        let a = 7000000000000l;
        let b = 14000000000000/a;""",
        "b",
        2),
    ("large_long_constant_variable_self_division_works",
        """
        let a = 7000000000000l;
        a = 14000000000000/a;""",
        "a",
        2),


    ("large_long_varible_variable_division_works",
        """
        let a = 14000000000000l;
        let b = 7000000000000l;
        let c = a/b;
        """,
        "c",
        2),
    ("large_long_variable_variable_self_division_works",
        """
        let a = 14000000000000l;
        let b = 7000000000000l;
        a = a/b;""",
        "a",
        2),

    # modulo
    ("large_long_constant_constant_modulo_works",
        "let a = 14000000000001l%7000000000000l;",
        "a",
        1),

    ("large_long_variable_constant_modulo_works",
        """
        let a = 14000000000001l;
        let b = a%7000000000000l;""",
        "b",
        1),
    ("large_long_variable_constant_self_modulo_works",
        """
        let a = 14000000000001l;
        a = a%7000000000000l;""",
        "a",
        1),

    ("large_long_constant_variable_modulo_works",
        """
        let a = 7000000000000l;
        let b = 14000000000001l%a;""",
        "b",
        1),
    ("large_long_constant_variable_self_modulo_works",
        """
        let a = 7000000000000l;
        a = 14000000000001l%a;""",
        "a",
        1),


    ("large_long_varible_variable_modulo_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        let c = a%b;
        """,
        "c",
        1),
    ("large_long_variable_variable_self_modulo_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        a = a%b;""",
        "a",
        1),

    # bitwise AND

    ("large_long_constant_constant_bitwise_and_works",
        "let a = 14000000000001l & 7000000000000l;",
        "a",
        4507568332800),

    ("large_long_variable_constant_bitwise_and_works",
        """
        let a = 14000000000001l;
        let b = a & 7000000000000l;""",
        "b",
        4507568332800),
    ("large_long_variable_constant_self_bitwise_and_works",
        """
        let a = 14000000000001l;
        a = a & 7000000000000l;""",
        "a",
        4507568332800),

    ("large_long_constant_variable_bitwise_and_works",
        """
        let a = 7000000000000l;
        let b = 14000000000001l & a;""",
        "b",
        4507568332800),
    ("large_long_constant_variable_self_bitwise_and_works",
        """
        let a = 7000000000000l;
        a = 14000000000001l & a;""",
        "a",
        4507568332800),


    ("large_long_varible_variable_bitwise_and_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        let c = a & b;
        """,
        "c",
        4507568332800),
    ("large_long_variable_variable_self_bitwise_and_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        a = a & b;""",
        "a",
        4507568332800),

    # bitwise OR

    ("large_long_constant_constant_bitwise_or_works",
        "let a = 14000000000001l | 7000000000000l;",
        "a",
        16492431667201),

    ("large_long_variable_constant__bitwise_or_works",
        """
        let a = 14000000000001l;
        let b = a | 7000000000000l;""",
        "b",
        16492431667201),
    ("large_long_variable_constant_self_bitwise_or_works",
        """
        let a = 14000000000001l;
        a = a | 7000000000000l;""",
        "a",
        16492431667201),

    ("large_long_constant_variable_bitwise_or_works",
        """
        let a = 7000000000000l;
        let b = 14000000000001l | a;""",
        "b",
        16492431667201),
    ("large_long_constant_variable_self_bitwise_or_works",
        """
        let a = 7000000000000l;
        a = 14000000000001l | a;""",
        "a",
        16492431667201),


    ("large_long_varible_variable_bitwise_or_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        let c = a | b;
        """,
        "c",
        16492431667201),
    ("large_long_variable_variable_self_bitwise_or_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        a = a | b;""",
        "a",
        16492431667201),



    # bitwise XOR

    ("large_long_constant_constant_bitwise_xor_works",
        "let a = 14000000000001l ^ 7000000000000l;",
        "a",
        11984863334401),

    ("large_long_variable_constant_bitwise_xor_works",
        """
        let a = 14000000000001l;
        let b = a ^ 7000000000000l;""",
        "b",
        11984863334401),
    ("large_long_variable_constant_self_bitwise_xor_works",
        """
        let a = 14000000000001l;
        a = a ^ 7000000000000l;""",
        "a",
        11984863334401),

    ("large_long_constant_variable_bitwise_xor_works",
        """
        let a = 7000000000000l;
        let b = 14000000000001l ^ a;""",
        "b",
        11984863334401),
    ("large_long_constant_variable_self_bitwise_xor_works",
        """
        let a = 7000000000000l;
        a = 14000000000001l ^ a;""",
        "a",
        11984863334401),


    ("large_long_varible_variable_bitwise_xor_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        let c = a ^ b;
        """,
        "c",
        11984863334401),
    ("large_long_variable_variable_self_bitwise_xor_works",
        """
        let a = 14000000000001l;
        let b = 7000000000000l;
        a = a ^ b;""",
        "a",
        11984863334401),


    # bitwise NOT
    ("can_bitwise_not_constant",
        """
        let a: long = ~14000000000001l;
        """,
        "a",
        -14000000000002),
    ("can_bitwise_not_variable",
        """
        let a: long = 14000000000001l;
        a = ~a;
        """,
        "a",
        -14000000000002),
    ("can_bitwise_not_negative_variable",
        """
        let a: long = -14000000000001l;
        a = ~a;
        """,
        "a",
        14000000000000),
    ("can_bitwise_not_variable_when_assigning_to_another_variable",
        """
        let a: long = 14000000000001l;
        let b: long = ~a;
        """,
        "b",
        -14000000000002),
    ("can_bitwise_not_negative_variable_when_assigning_to_another_variable",
        """
        let a: long = -14000000000001l;
        let b: long = ~a;
        """,
        "b",
        14000000000000),
    ("can_bitwise_not_constant_variable",
        f"""
        const X: long = 14000000000001l;
        let b: long = ~X;
        """,
        "b",
        -14000000000002),


]

for t in test_tuples:

    out = f'''
program: |

    extern fn long_printer(value: long);
    fn main() : int {{
        {t[1]}
        long_printer({t[2]});
        return 0;
    }}
link_with:
    - tests/files/support/support.c
expect_stdout: |
    {t[3]}
    '''
    with open(f"{t[0]}.yaml", "w") as f:
        f.write(out)
