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
    ("positive_constant_expression_works",
        "",
        "4l",
        4),
    ("negative_constant_expression_works",
        "",
        "-4l",
        -4),


    # addition
    ("constant_constant_addition_works",
        "",
        "2l + 9l",
        11),
    ("constant_constant_addition_with_negative_number_works",
        "",
        "10l + (-5l)",
        5),
    ("variable_constant_addition_works",
        "let a: long = 24;",
        "a + 9",
        "33"),
    ("constant_variable_addition_works",
        "let a: long = 49;",
        "5 + a",
        54),
    ("constant_variable__self_assignment_addition_works",
        """
        let a: long = 49;
        a = 5 + a;
        """,
        "a",
        54),
     ("constant_variable_self_assignment_addition_works",
        """
        let a: long = 49;
        a = a + 5;
        """,
        "a",
        54),
    ("variable_variable_addition_works",
        "let a: long = 123; let b: long = 47;",
        "a + b",
        170),
    ("variable_variable_self_assignment_addition_works",
        """
        let a: long = 123;
        let b: long = 47;
        a = a + b;
        """,
        "a",
        170),
    ("variable_variable_self_assignment_rhs_addition_works",
        """
        let a: long = 123;
        let b: long = 47;
        a = b + a;
        """,
        "a",
        170),
    ("complex_addition_works",
        """
        let a: long = 20;
        let b: long = 8;
        let c: long = 5 + a + b;
        let d: long = c + 4;
        """,
        "0+a+b+c+d+1",
       99),
    ("can_add_up_to_LONG_Max",
        f"let a: long = {LONG_MAX-1};",
        "a + 1",
        LONG_MAX),
    ("addition_overflows_when_adding_to_LONG_Max",
        f"let a: long = {LONG_MAX};",
        "a + 1",
        LONG_MIN),


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
        "let a: long = 8;",
        "a - 6",
        2),
    ("variable_constant_self_assignment_subtraction_works",
        """
        let a: long = 8;
        a = a - 6;
        """,
        "a",
        2),
    ("constant_variable_subtraction_works",
        "let a: long = 8;",
        "5 - a",
        -3),
    ("constant_variable_self_assignment_subtraction_works",
        """
        let a: long = 8;
        a = 5 - a;
        """,
        "a",
        -3),
    ("variable_variable_subtraction_works",
        "let a: long = 8; let b: long = 5402;",
        "a - b",
        -5394),
    ("variable_variable_self_assignment_subtraction_works",
        """
        let a: long = 8;
        let b: long = 5402;
        a = a - b;
        """,
        "a",
        -5394),
    ("variable_variable_self_assignment_rhs_subtraction_works",
        """
        let a: long = 8;
        let b: long = 5402;
        b = a - b;
        """,
        "b",
        -5394),
    ("complex_subtraction_works",
        """
        let a: long = 20;
        let b: long = 8;
        let c: long = 5 - a - b;
        let d: long = c - 4;
        """,
        "0-a-b-c-d-1",
       21),
    ("can_subtract_to_LONG_Min",
        f"let a: long = {LONG_MIN+1};",
        "a - 1",
        LONG_MIN,
        ),
    ("subtraction_overflows_when_subtracting_from_LONG_Min",
        f"let a: long = {LONG_MIN};",
        "a - 1",
        LONG_MAX),

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
        "let a: long = 8;",
        "a*6",
        48),
    ("variable_constant_self_assignment_multiplication_works",
        """
        let a: long = 8;
        a = a * 6;
        """,
        "a",
        48),
    ("constant_variable_multiplication_works",
        "let a: long = 8;",
        "5*a",
        40),
    ("constant_variable_self_assignment_multiplication_works",
        """
        let a: long = 8;
        a = 5*a;
        """,
        "a",
        40),
    ("variable_variable_multiplication_works",
        "let a: long = 8; let b: long = -1;",
        "a*b",
        -8),
    ("variable_variable_self_assignment_multiplication_works",
        """
        let a: long = 8;
        let b: long = -1;
        a = a * b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_multiplication_works",
        """
        let a: long = 8;
        let b: long = -1;
        b = a * b;
        """,
        "b",
        -8),
    ("complex_multiplication_works",
        """
        let a: long = 1;
        let b: long = 2;
        let c: long = 3*a*b;
        let d: long = c*4;
        """,
        "a*b*c*d*10",
       2880),
    ("positive_multiplication_result_can_overflow",
        f"let a: long = {LONG_MAX};",
        "a*2",
        -2),
    ("negative_multiplication_result_can_overflow",
        f"let a: long = {LONG_MIN};",
        "a*(-1)",
        LONG_MIN,
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
        "let a: long = 20;",
        "a/5",
        4),
    ("variable_constant_self_assignment_division_works",
        """
        let a: long = 20;
        a = a / 5;
        """,
        "a",
        4),
    ("constant_variable_division_works",
        "let a: long = 8;",
        "24/a",
        3),
    ("constant_variable_self_assignment_division_works",
        """
        let a: long = 8;
        a = 24 / a;
        """,
        "a",
        3),
    ("variable_variable_division_works",
        "let a: long = 8; let b: long = -1;",
        "a/b",
        -8),
    ("variable_variable_self_assignment_division_works",
        """
        let a: long = 8; let b: long = -1;
        a = a / b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_division_works",
        """
        let a: long = 8; let b: long = -1;
        b = a / b;
        """,
        "b",
        -8),
    ("complex_division_works",
        """
        let a: long = 10;
        let b: long = 2;
        let c: long = a/b;
        let d: long = c/2;
        """,
        "500/a/b/c/d",
       2),


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
        "let a: long = 20;",
        "a%3",
        2),

    ("constant_variable_modulo_works",
        "let a: long = 9;",
        "24 % a",
        6),
    ("variable_variable_modulo_works",
        "let a: long = 13; let b: long = 5;",
        "a%b",
        3),

    # negation

    ("can_negate_variable",
        """
        let a: long = 4;
        a = -a;
        """,
        "a",
        -4),
    ("can_negate_negative_variable",
        """
        let a: long = -4;
        a = -a;
        """,
        "a",
        4),
    ("can_negate_variable_when_assigning_to_another_variable",
        """
        let a: long = 4;
        let b: long = -a;
        """,
        "b",
        -4),
    ("can_negate_negative_variable_when_assigning_to_another_variable",
        """
        let a: long = -4;
        let b: long = -a;
        """,
        "b",
        4),
    ("can_negate_LONG_Max",
        f"""
        let a: long = {LONG_MAX};
        let b: long = -a;
        """,
        "b",
        -LONG_MAX),
    ("negating_LONG_Min_overflows_to_LONG_Min",
        f"""
        let a: long = {LONG_MIN};
        let b: long = -a;
        """,
        "b",
        LONG_MIN),
    ("can_negate_constant_variable",
        f"""
        const X: long = 12345;
        let b: long = -X;
        """,
        "b",
        -12345),

    # logical shift left

    ("constant_constant_shl_works",
        """
        let a: long = 4l << 2l;
        """,
        "a",
        16),

    ("variable_constant_shl_works",
        """
        let a: long = 4;
        let b: long = a << 2;
        """,
        "b",
        16),

    ("variable_constant_self_assignment_shl_works",
        """
        let a: long = 4;
        a = a << 2;
        """,
        "a",
        16),

    ("variable_long_constant_shl_works",
        """
        let a: long = 4;
        let b: long = a << 2l;
        """,
        "b",
        16),

    ("variable_long_constant_self_assignment_shl_works",
        """
        let a: long = 4;
        a = a << 2l;
        """,
        "a",
        16),
    ("variable_byte_constant_shl_works",
        """
        let a: long = 4;
        let b: long = a << 2b;
        """,
        "b",
        16),

    ("variable_byte_constant_self_assignment_shl_works",
        """
        let a: long = 4;
        a = a << 2b;
        """,
        "a",
        16),

    ("constant_variable_shl_long_works",
        """
        let a: long = 2;
        let b: long = 4l << a;
        """,
        "b",
        16),

    ("constant_variable_shl_int_works",
        """
        let a: int = 2;
        let b: long = 4l << a;
        """,
        "b",
        16),

    ("constant_variable_shl_byte_works",
        """
        let a: int = 2;
        let b: long = 4l << a;
        """,
        "b",
        16),

    ("long_constant_variable_self_assignment_shl_works",
        """
        let a: long = 2;
        a = 4l << a;
        """,
        "a",
        16),

    ("constant_variable_self_assignment_shl_works",
        """
        let a: long = 2;
        a = (4 << a) as long;
        """,
        "a",
        16),

    ("byte_constant_variable_self_assignment_shl_works",
        """
        let a: long = 2;
        a = (4b << a) as long;
        """,
        "a",
        16),

    ("variable_variable_shl_works",
        """
        let a: long = 4;
        let b: long = 2;
        let c: long = a << b;
        """,
        "c",
        16),

    ("variable_variable_self_assignment_shl_works",
        """
        let a: long = 4;
        let b: long = 2;
         a = a << b;
        """,
        "a",
        16),

    ("variable_variable_self_assignment_rhs_shl_works",
        """
        let a: long = 4;
        let b: long = 2;
        b = a << b;
        """,
        "b",
        16),

    # logical shift right

    ("constant_constant_shr_works",
        f"""
        let a: long =  {LONG_MIN}l >>> 1;
        """,
        "a",
        LONG_MAX//2+1),

    ("variable_constant_shr_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = a >>> 1;
        """,
        "b",
        LONG_MAX//2+1),

    ("variable_constant_self_assignment_shr_works",
        f"""
        let a: long = {LONG_MIN}l;
        a = a >>> 1;
        """,
        "a",
        LONG_MAX//2+1),

    ("constant_variable_shr_works",
        f"""
        let a: long = 1;
        let b: long = {LONG_MIN}l >>> a;
        """,
        "b",
        LONG_MAX//2+1),

    ("constant_variable_self_assignment_shr_works",
        f"""
        let a: long = 1;
        a = {LONG_MIN}l >>> a;
        """,
        "a",
        LONG_MAX//2+1),

    ("variable_variable_shr_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = 1;
        let c: long = a >>> b;
        """,
        "c",
        LONG_MAX//2+1),

    ("variable_variable_self_assignment_shr_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = 1;
         a = a >>> b;
        """,
        "a",
        LONG_MAX//2+1),

    ("variable_variable_self_assignment_rhs_shr_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = 1;
        b = a >>> b;
        """,
        "b",
        LONG_MAX//2+1),


    # arithmetic shift right

    ("constant_constant_sar_works",
        f"""
        let a: long = {LONG_MIN}l >> 1;
        """,
        "a",
        LONG_MIN//2),

    ("variable_constant_sar_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = a >> 1;
        """,
        "b",
        LONG_MIN//2),

    ("variable_constant_self_assignment_sar_works",
        f"""
        let a: long = {LONG_MIN}l;
        a = a >> 1;
        """,
        "a",
        LONG_MIN//2),

    ("constant_variable_sar_works",
        f"""
        let a: long = 1;
        let b: long = {LONG_MIN}l >> a;
        """,
        "b",
        LONG_MIN//2),

    ("constant_variable_self_assignment_sar_works",
        f"""
        let a: long = 1;
        a = {LONG_MIN}l >> a;
        """,
        "a",
         LONG_MIN//2),

    ("variable_variable_sar_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = 1;
        let c: long = a >> b;
        """,
        "c",
         LONG_MIN//2),

    ("variable_variable_self_assignment_sar_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = 1;
         a = a >> b;
        """,
        "a",
         LONG_MIN//2),

    ("variable_variable_self_assignment_rhs_sar_works",
        f"""
        let a: long = {LONG_MIN}l;
        let b: long = 1;
        b = a >> b;
        """,
        "b",
         LONG_MIN//2),

    # bitwise AND
    ("constant_constant_bitwise_and_works",
        """
        let a: long = 167 & 181;
        """,
        "a",
        165),

    ("variable_constant_bitwise_and_works",
        """
        let a: long = 167;
        let b: long = a & 181;
        """,
        "b",
        165),

    ("variable_constant_self_assignment_bitwise_and_works",
        """
        let a: long = 167;
        a = a & 181;
        """,
        "a",
        165),

    ("constant_variable_bitwise_and_works",
        """
        let a: long = 181;
        let b: long = 167 & a;
        """,
        "b",
        165),

    ("constant_variable_self_assignment_bitwise_and_works",
        """
        let a: long = 167;
        a = 181 & a;
        """,
        "a",
        165),

    ("variable_variable_bitwise_and_works",
        """
        let a: long = 167;
        let b: long = 181;
        let c: long = a & b;
        """,
        "c",
        165),

    ("variable_variable_self_assignment_bitwise_and_works",
        """
        let a: long = 167;
        let b: long = 181;
        a = a & b;
        """,
        "a",
        165),

    ("variable_variable_self_assignment_rhs_bitwise_and_works",
        """
        let a: long = 167;
        let b: long = 181;
        b = a & b;
        """,
        "b",
        165),


    # bitwise OR
    ("constant_constant_bitwise_or_works",
        """
        let a: long = 167 | 181;
        """,
        "a",
        183),

    ("variable_constant_bitwise_or_works",
        """
        let a: long = 167;
        let b: long = a | 181;
        """,
        "b",
        183),

    ("variable_constant_self_assignment_bitwise_or_works",
        """
        let a: long = 167;
        a = a | 181;
        """,
        "a",
        183),

    ("constant_variable_bitwise_or_works",
        """
        let a: long = 181;
        let b: long = 167 | a;
        """,
        "b",
        183),

    ("constant_variable_self_assignment_bitwise_or_works",
        """
        let a: long = 167;
        a = 181 | a;
        """,
        "a",
        183),

    ("variable_variable_bitwise_or_works",
        """
        let a: long = 167;
        let b: long = 181;
        let c: long = a | b;
        """,
        "c",
        183),

    ("variable_variable_self_assignment_bitwise_or_works",
        """
        let a: long = 167;
        let b: long = 181;
        a = a | b;
        """,
        "a",
        183),

    ("variable_variable_self_assignment_rhs_bitwise_or_works",
        """
        let a: long = 167;
        let b: long = 181;
        b = a | b;
        """,
        "b",
        183),

    # bitwise XOR
    ("constant_constant_bitwise_xor_works",
        """
        let a: long = 167 ^ 181;
        """,
        "a",
        18),

    ("variable_constant_bitwise_xor_works",
        """
        let a: long = 167;
        let b: long = a ^ 181;
        """,
        "b",
        18),

    ("variable_constant_self_assignment_bitwise_xor_works",
        """
        let a: long = 167;
        a = a ^ 181;
        """,
        "a",
        18),

    ("constant_variable_bitwise_xor_works",
        """
        let a: long = 181;
        let b: long = 167 ^ a;
        """,
        "b",
        18),

    ("constant_variable_self_assignment_bitwise_xor_works",
        """
        let a: long = 167;
        a = 181 ^ a;
        """,
        "a",
        18),

    ("variable_variable_bitwise_xor_works",
        """
        let a: long = 167;
        let b: long = 181;
        let c: long = a ^ b;
        """,
        "c",
        18),

    ("variable_variable_self_assignment_bitwise_xor_works",
        """
        let a: long = 167;
        let b: long = 181;
        a = a ^ b;
        """,
        "a",
        18),

    ("variable_variable_self_assignment_rhs_bitwise_xor_works",
        """
        let a: long = 167;
        let b: long = 181;
        b = a ^ b;
        """,
        "b",
        18),


    # bitwise NOT
    ("can_bitwise_not_constant",
        """
        let a: long = ~684;
        """,
        "a",
        -685),
    ("can_bitwise_not_variable",
        """
        let a: long = 684;
        a = ~a;
        """,
        "a",
        -685),
    ("can_bitwise_not_negative_variable",
        """
        let a: long = -685;
        a = ~a;
        """,
        "a",
        684),
    ("can_bitwise_not_variable_when_assigning_to_another_variable",
        """
        let a: long = 684;
        let b: long = ~a;
        """,
        "b",
        -685),
    ("can_bitwise_not_negative_variable_when_assigning_to_another_variable",
        """
        let a: long = -685;
        let b: long = ~a;
        """,
        "b",
        684),
    ("can_bitwise_not_LONG_Max",
        f"""
        let a: long = {LONG_MAX};
        let b: long = ~a;
        """,
        "b",
        LONG_MIN),
    ("can_bitwise_not_LONG_Min",
        f"""
        let a: long = {LONG_MIN};
        let b: long = ~a;
        """,
        "b",
        LONG_MAX),
    ("can_bitwise_not_constant_variable",
        f"""
        const X: long = 684;
        let b: long = ~X;
        """,
        "b",
        -685),

    # +=
    ("self_addition_shorthand_works",
        f"""
        let a: long = 4;
        a += 6;
        """,
        "a",
        10),

    ("self_addition_shorthand_works_with_arrays",
        f"""
        let a: long[4] = 4;
        a[0] += 6;
        """,
        "a[0]",
        10),


    # -=
    ("self_subtraction_shorthand_works",
        f"""
        let a: long = 4;
        a -= 6;
        """,
        "a",
        -2),

    ("self_subtraction_shorthand_works_with_arrays",
        f"""
        let a: long[4] = 4;
        a[0] -= 6;
        """,
        "a[0]",
        -2),


    # *=
    ("self_multiplication_shorthand_works",
        f"""
        let a: long = 4;
        a *= 6;
        """,
        "a",
        24),

    ("self_multiplication_shorthand_works_with_arrays",
        f"""
        let a: long[4] = 4;
        a[0] *= 6;
        """,
        "a[0]",
        24),


    # /=
    ("self_division_shorthand_works",
        f"""
        let a: long = 9;
        a /= 2;
        """,
        "a",
        4),

    ("self_division_shorthand_works_with_arrays",
        f"""
        let a: long[4] = 9;
        a[0] /= 2;
        """,
        "a[0]",
        4),


    # %=
    ("self_modulo_shorthand_works",
        f"""
        let a: long = 14;
        a %= 5;
        """,
        "a",
        4),

    ("self_modulo_shorthand_works_with_arrays",
        f"""
        let a: long[4] = 14;
        a[0] %= 5;
        """,
        "a[0]",
        4),

    # <<=

    ("self_shl_shorthand_works",
        """
        let a: long = 16;
        a <<= 2;
        """,
        "a",
       64),


    ("self_shl_shorthand_works_with_arrays",
        """
        let a: long[4] = 16;
        a[0] <<= 2;
        """,
        "a[0]",
       64),

    # >>=

    ("self_sar_shorthand_works",
        """
        let a: long = -16;
        a >>= 2;
        """,
        "a",
        -4),

    ("self_sar_shorthand_works_with_arrays",
        """
        let a: long[4] = -16;
        a[0] >>= 2;
        """,
        "a[0]",
        -4),

    # >>>=

    ("self_shr_shorthand_works",
        """
        let a: long = -16;
        a >>>= 2;
        """,
        "a",
        4611686018427387900),


    ("self_shr_shorthand_works_with_arrays",
        """
        let a: long[4] = -16;
        a[0] >>>= 2;
        """,
        "a[0]",
        4611686018427387900),

    # &=

    ("self_bitwise_and_shorthand_works_with_self",
        """
        let a: long = 167;
        let b: long = 181;
        a &= b;
        """,
        "a",
        165),

    ("self_bitwise_and_shorthand_works_with_arrays",
        """
        let a: long[4] = 167;
        let b: long = 181;
        a[0] &= b;
        """,
        "a[0]",
        165),

    # |=

    ("self_bitwise_or_shorthand_works_with_self",
        """
        let a: long = 167;
        let b: long = 181;
        a |= b;
        """,
        "a",
        183),

    ("self_bitwise_or_shorthand_works_with_arrays",
        """
        let a: long[4] = 167;
        let b: long = 181;
        a[0] |= b;
        """,
        "a[0]",
        183),


    # ^=
    ("self_bitwise_xor_shorthand_works_with_self",
        """
        let a: long = 167;
        let b: long = 181;
        a ^= b;
        """,
        "a",
        18),

    ("self_bitwise_xor_shorthand_works_with_arrays",
        """
        let a: long[4] = 167;
        let b: long = 181;
        a[0] ^= b;
        """,
        "a[0]",
        18),
]

for t in test_tuples:

    out = f'''
program: |
    fn test_function() : long {{
        {t[1]}
        return {t[2]};
    }}
link_with:
    - tests/files/support/support.c
callable: test_function
returns: long
expect_stdout: |
    {t[3]}
    '''
    with open(f"{t[0]}.yaml", "w") as f:
        f.write(out)
