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
        "let a: byte = 24;",
        "a + 9",
        "33"),
    ("variable_constant_self_assignment_addition_works",
        """
        let a: byte = 24;
        a = a + 9;
        """,
        "a",
        "33"),
    ("constant_variable_addition_works",
        "let a: byte = 49;",
        "5 + a",
        54),
    ("constant_variable_self_assignment_addition_works",
        """
        let a: byte = 49;
        a = 5 + a;
        """,
        "a",
        54),
    ("variable_variable_addition_works",
        "let a: byte = 50; let b: byte = 47;",
        "a + b",
        97),
    ("variable_variable_self_assignment_addition_works",
        """
        let a: byte = 50;
        let b: byte = 47;
        a = a + b;
        """,
        "a",
        97),

    ("variable_variable_self_assignment_rhs_addition_works",
        """
        let a: byte = 50;
        let b: byte = 47;
        b = a + b;
        """,
        "b",
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
        "let a: byte = 8;",
        "a - 6",
        2),
    ("variable_constant_self_assignment_subtraction_works",
        "let a: byte = 8;",
        "a - 6",
        2),
    ("constant_variable_subtraction_works",
        "let a: byte = 8;",
        "5 - a",
        -3),
    ("constant_variable_self_assignment_subtraction_works",
        """
        let a: byte = 8;
        a = 5 - a;
        """,
        "a",
        -3),
    ("variable_variable_subtraction_works",
        "let a: byte = 8; let b: byte = 38;",
        "a - b",
        -30),
    ("variable_variable_self_assignment_subtraction_works",
        """
        let a: byte = 8;
        let b: byte = 38;
        a = a - b;
        """,
        "a",
        -30),
    ("variable_variable_self_assignment_rhs_subtraction_works",
        """
        let a: byte = 8;
        let b: byte = 38;
        b = a - b;
        """,
        "b",
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
        "let a: byte = 8;",
        "a*6",
        48),
    ("variable_constant_self_assignment_multiplication_works",
        """
        let a: byte = 8;
        a = a*6;
        """,
        "a",
        48),
    ("constant_variable_multiplication_works",
        "let a: byte = 8;",
        "5*a",
        40),
    ("constant_variable_self_assignment_multiplication_works",
        """
        let a: byte = 8;
        a = 5*a;
        """,
        "a",
        40),
    ("variable_variable_multiplication_works",
        "let a: byte = 8; let b: byte = -1;",
        "a*b",
        -8),
    ("variable_variable_self_assignment_multiplication_works",
        """
        let a: byte = 8;
        let b: byte = -1;
        a = a*b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_multiplication_works",
        """
        let a: byte = 8;
        let b: byte = -1;
        b = a*b;
        """,
        "b",
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
        "let a: byte = 20;",
        "a/5",
        4),
    ("variable_constant_self_assignment_division_works",
        """
        let a: byte = 20;
        a = a /5;
        """,
        "a",
        4),
    ("constant_variable_division_works",
        "let a: byte = 8;",
        "24/a",
        3),
    ("constant_variable_self_assignment_division_works",
        """
        let a: byte = 8;
        a = 24 /a;
        """,
        "a",
        3),
    ("variable_variable_division_works",
        "let a: byte = 8; let b: byte = -1;",
        "a/b",
        -8),

    ("variable_variable__self_assignment_division_works",
        """
        let a: byte = 8;
        let b: byte = -1;
        a = a / b;
        """,
        "a",
        -8),

    ("variable_variable__self_assignment_rhs_division_works",
        """
        let a: byte = 8;
        let b: byte = -1;
        b = a / b;
        """,
        "b",
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

    # modulo
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

    # negation

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

    ("can_negate_constant_variable",
        f"""
        const X: byte = 5;
        let b: byte = -X;
        """,
        "b",
        -5),


    # logical shift left
    ("constant_constant_shl_works",
        """
        let a: byte = 4b << 2;
        """,
        "a",
        16),

    ("variable_constant_shl_works",
        """
        let a: byte = 4;
        let b: byte = a << 2b;
        """,
        "b",
        16),

    ("variable_constant_self_assignment_shl_works",
        """
        let a: byte = 4;
        a = a << 2;
        """,
        "a",
        16),

    ("constant_variable_shl_works",
        """
        let a: byte = 2b;
        let b: byte = 4b << a;
        """,
        "b",
        16),

    ("constant_variable_self_assignment_shl_works",
        """
        let a: byte = 2;
        a = 4b << a;
        """,
        "a",
        16),

    ("variable_variable_shl_works",
        """
        let a: byte = 4;
        let b: byte = 2;
        let c: byte = a << b;
        """,
        "c",
        16),

    ("variable_variable_self_assignment_shl_works",
        """
        let a: byte = 4;
        let b: byte = 2;
         a = a << b;
        """,
        "a",
        16),

    ("variable_variable_self_assignment_rhs_shl_works",
        """
        let a: byte = 4;
        let b: byte = 2;
        b = a << b;
        """,
        "b",
        16),

    # logical shift right

    ("constant_constant_shr_works",
        """
        let a: byte = -128b >>> 1b;
        """,
        "a",
        64),

    ("variable_constant_shr_works",
        """
        let a: byte = -128;
        let b: byte = a >>> 1;
        """,
        "b",
        64),

    ("variable_constant_self_assignment_shr_works",
        """
        let a: byte = -128;
        a = a >>> 1;
        """,
        "a",
        64),

    ("constant_variable_shr_works",
        """
        let a: byte = 1;
        let b: byte = -128b >>> a;
        """,
        "b",
        64),

    ("constant_variable_self_assignment_shr_works",
        """
        let a: byte = 1;
        a = -128b >>> a;
        """,
        "a",
        64),

    ("variable_variable_shr_works",
        """
        let a: byte = -128;
        let b: byte = 1;
        let c: byte = a >>> b;
        """,
        "c",
        64),

    ("variable_variable_self_assignment_shr_works",
        """
        let a: byte = -128;
        let b: byte = 1;
        a = a >>> b;
        """,
        "a",
        64),

    ("variable_variable_self_assignment_rhs_shr_works",
        """
        let a: byte = -128;
        let b: byte = 1;
        b = a >>> b;
        """,
        "b",
        64),


    # arithmetic shift right

    ("constant_constant_sar_works",
        """
        let a: byte = -128b >> 1b;
        """,
        "a",
        -64),

    ("variable_constant_sar_works",
        """
        let a: byte = -128;
        let b: byte = a >> 1;
        """,
        "b",
        -64),

    ("variable_constant_self_assignment_sar_works",
        """
        let a: byte = -128;
        a = a >> 1;
        """,
        "a",
        -64),

    ("constant_variable_sar_works",
        """
        let a: byte = 1;
        let b: byte = -128b >> a;
        """,
        "b",
        -64),

    ("constant_variable_self_assignment_sar_works",
        """
        let a: byte = 1;
        a = -128b >> a;
        """,
        "a",
        -64),

    ("variable_variable_sar_works",
        """
        let a: byte = -128;
        let b: byte = 1;
        let c: byte = a >> b;
        """,
        "c",
        -64),

    ("variable_variable_self_assignment_sar_works",
        """
        let a: byte = -128;
        let b: byte = 1;
         a = a >> b;
        """,
        "a",
        -64),

    ("variable_variable_self_assignment_rhs_sar_works",
        """
        let a: byte = -128;
        let b: byte = 1;
        b = a >> b;
        """,
        "b",
        -64),

    # bitwise AND
    ("constant_constant_bitwise_and_works",
        """
        let a: byte = 101 & 87;
        """,
        "a",
        69),

    ("variable_constant_bitwise_and_works",
        """
        let a: byte = 101;
        let b: byte = a & 87;
        """,
        "b",
        69),

    ("variable_constant_self_assignment_bitwise_and_works",
        """
        let a: byte = 101;
        a = a & 87;
        """,
        "a",
        69),

    ("constant_variable_bitwise_and_works",
        """
        let a: byte = 87;
        let b: byte = 101 & a;
        """,
        "b",
        69),

    ("constant_variable_self_assignment_bitwise_and_works",
        """
        let a: byte = 101;
        a = 87 & a;
        """,
        "a",
        69),

    ("variable_variable_bitwise_and_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        let c: byte = a & b;
        """,
        "c",
        69),

    ("variable_variable_self_assignment_bitwise_and_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        a = a & b;
        """,
        "a",
        69),

    ("variable_variable_self_assignment_rhs_bitwise_and_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        b = a & b;
        """,
        "b",
        69),

    # bitwise OR
    ("constant_constant_bitwise_or_works",
        """
        let a: byte = 101 | 87;
        """,
        "a",
        119),

    ("variable_constant_bitwise_or_works",
        """
        let a: byte = 101;
        let b: byte = a | 87;
        """,
        "b",
        119),

    ("variable_constant_self_assignment_bitwise_or_works",
        """
        let a: byte = 101;
        a = a | 87;
        """,
        "a",
        119),

    ("constant_variable_bitwise_or_works",
        """
        let a: byte = 87;
        let b: byte = 101 | a;
        """,
        "b",
        119),

    ("constant_variable_self_assignment_bitwise_or_works",
        """
        let a: byte = 101;
        a = 87 | a;
        """,
        "a",
        119),

    ("variable_variable_bitwise_or_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        let c: byte = a | b;
        """,
        "c",
        119),

    ("variable_variable_self_assignment_bitwise_or_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        a = a | b;
        """,
        "a",
        119),

    ("variable_variable_self_assignment_rhs_bitwise_or_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        b = a | b;
        """,
        "b",
        119),


    # bitwise XOR
    ("constant_constant_bitwise_xor_works",
        """
        let a: byte = 101 ^ 87;
        """,
        "a",
        50),

    ("variable_constant_bitwise_xor_works",
        """
        let a: byte = 101;
        let b: byte = a ^ 87;
        """,
        "b",
        50),

    ("variable_constant_self_assignment_bitwise_xor_works",
        """
        let a: byte = 101;
        a = a ^ 87;
        """,
        "a",
        50),

    ("constant_variable_bitwise_xor_works",
        """
        let a: byte = 87;
        let b: byte = 101 ^ a;
        """,
        "b",
        50),

    ("constant_variable_self_assignment_bitwise_xor_works",
        """
        let a: byte = 101;
        a = 87 ^ a;
        """,
        "a",
        50),

    ("variable_variable_bitwise_xor_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        let c: byte = a ^ b;
        """,
        "c",
        50),

    ("variable_variable_self_assignment_bitwise_xor_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        a = a ^ b;
        """,
        "a",
        50),

    ("variable_variable_self_assignment_rhs_bitwise_xor_works",
        """
        let a: byte = 101;
        let b: byte = 87;
        b = a ^ b;
        """,
        "b",
        50),

    # bitwise NOT
    ("can_bitwise_not_constant",
        """
        let a: byte = ~105;
        """,
        "a",
        -106),
    ("can_bitwise_not_variable",
        """
        let a: byte = 105;
        a = ~a;
        """,
        "a",
        -106),
    ("can_bitwise_not_negative_variable",
        """
        let a: byte = -106;
        a = ~a;
        """,
        "a",
        105),
    ("can_bitwise_not_variable_when_assigning_to_another_variable",
        """
        let a: byte = 105;
        let b: byte = ~a;
        """,
        "b",
        -106),
    ("can_bitwise_not_negative_variable_when_assigning_to_another_variable",
        """
        let a: byte = -106;
        let b: byte = ~a;
        """,
        "b",
        105),
    ("can_bitwise_not_int_max",
        f"""
        let a: byte = {BYTE_MAX};
        let b: byte = ~a;
        """,
        "b",
        BYTE_MIN),
    ("can_bitwise_not_int_min",
        f"""
        let a: byte = {BYTE_MIN};
        let b: byte = ~a;
        """,
        "b",
        BYTE_MAX),
    ("can_bitwise_not_constant_variable",
        f"""
        const X: byte = 105;
        let b: byte = ~X;
        """,
        "b",
        -106),

    # +=
    ("self_addition_shorthand_works",
        f"""
        let a: byte = 4;
        a += 6;
        """,
        "a",
        10),

    ("self_addition_shorthand_works_with_arrays",
        f"""
        let a: byte[4] = 4;
        a[0] += 6;
        """,
        "a[0]",
        10),

    # -=
    ("self_subtraction_shorthand_works",
        f"""
        let a: byte = 4;
        a -= 6;
        """,
        "a",
        -2),

    ("self_subtraction_shorthand_works_with_arrays",
        f"""
        let a: byte[7] = 4;
        a[0] -= 6;
        """,
        "a[0]",
        -2),

    # *=
    ("self_multiplication_shorthand_works",
        f"""
        let a: byte = 4;
        a *= 6;
        """,
        "a",
        24),

    ("self_multiplication_shorthand_works_with_arrays",
        f"""
        let a: byte[4] = 4;
        a[0] *= 6;
        """,
        "a[0]",
        24),



    # /=
    ("self_division_shorthand_works",
        f"""
        let a: byte = 9;
        a /= 2;
        """,
        "a",
        4),

    ("self_division_shorthand_works_with_arrays",
        f"""
        let a: byte[4] = 9;
        a[0] /= 2;
        """,
        "a[0]",
        4),



    # # %=
    ("self_modulo_shorthand_works",
        f"""
        let a: byte = 14;
        a %= 5;
        """,
        "a",
        4),

    ("self_modulo_shorthand_works_with_arrays",
        f"""
        let a: byte[7] = 14;
        a[0] %= 5;
        """,
        "a[0]",
        4),


    # <<=

    ("self_shl_shorthand_works",
        """
        let a: byte = 16;
        a <<= 2;
        """,
        "a",
       64),


    ("self_shl_shorthand_works_with_arrays",
        """
        let a: byte[4] = 16;
        a[0] <<= 2;
        """,
        "a[0]",
       64),

    # >>=

    ("self_sar_shorthand_works",
        """
        let a: byte = -16;
        a >>= 2;
        """,
        "a",
        -4),

    ("self_sar_shorthand_works_with_arrays",
        """
        let a: byte[4] = -16;
        a[0] >>= 2;
        """,
        "a[0]",
        -4),

    # >>>=

    ("self_shr_shorthand_works",
        """
        let a: byte = -16;
        a >>>= 2;
        """,
        "a",
        60),


    ("self_shr_shorthand_works_with_arrays",
        """
        let a: byte[4] = -16;
        a[0] >>>= 2;
        """,
        "a[0]",
        60),

    # &=

    ("self_bitwise_and_shorthand_works_with_self",
        """
        let a: byte = 101;
        let b: byte = 87;
        a &= b;
        """,
        "a",
        69),

    ("self_bitwise_and_shorthand_works_with_arrays",
        """
        let a: byte[4] = 101;
        let b: byte = 87;
        a[0] &= b;
        """,
        "a[0]",
        69),

    # |=

    ("self_bitwise_or_shorthand_works_with_self",
        """
        let a: byte = 101;
        let b: byte = 87;
        a |= b;
        """,
        "a",
        119),

    ("self_bitwise_or_shorthand_works_with_arrays",
        """
        let a: byte[4] = 101;
        let b: byte = 87;
        a[0] |= b;
        """,
        "a[0]",
        119),


    # ^=

    ("self_bitwise_xor_shorthand_works_with_self",
        """
        let a: byte = 101;
        let b: byte = 87;
        a ^= b;
        """,
        "a",
        50),

    ("self_bitwise_xor_shorthand_works_with_arrays",
        """
        let a: byte[4] = 101;
        let b: byte = 87;
        a[0] ^= b;
        """,
        "a[0]",
        50),
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




