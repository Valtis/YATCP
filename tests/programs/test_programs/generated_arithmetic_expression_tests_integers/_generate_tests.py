INT_MAX = 2**31-1
INT_MIN = -INT_MAX-1


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
        "let a: int = 24;",
        "a + 9",
        "33"),
    ("constant_variable_addition_works",
        "let a: int = 49;",
        "5 + a",
        54),
    ("constant_variable__self_assignment_addition_works",
        """
        let a: int = 49;
        a = 5 + a;
        """,
        "a",
        54),
     ("constant_variable_self_assignment_addition_works",
        """
        let a: int = 49;
        a = a + 5;
        """,
        "a",
        54),
    ("variable_variable_addition_works",
        "let a: int = 123; let b: int = 47;",
        "a + b",
        170),
    ("variable_variable_self_assignment_addition_works",
        """
        let a: int = 123;
        let b: int = 47;
        a = a + b;
        """,
        "a",
        170),
    ("variable_variable_self_assignment_rhs_addition_works",
        """
        let a: int = 123;
        let b: int = 47;
        a = b + a;
        """,
        "a",
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
    ("variable_constant_self_assignment_subtraction_works",
        """
        let a: int = 8;
        a = a - 6;
        """,
        "a",
        2),
    ("constant_variable_subtraction_works",
        "let a: int = 8;",
        "5 - a",
        -3),
    ("constant_variable_self_assignment_subtraction_works",
        """
        let a: int = 8;
        a = 5 - a;
        """,
        "a",
        -3),
    ("variable_variable_subtraction_works",
        "let a: int = 8; let b: int = 5402;",
        "a - b",
        -5394),
    ("variable_variable_self_assignment_subtraction_works",
        """
        let a: int = 8;
        let b: int = 5402;
        a = a - b;
        """,
        "a",
        -5394),
    ("variable_variable_self_assignment_rhs_subtraction_works",
        """
        let a: int = 8;
        let b: int = 5402;
        b = a - b;
        """,
        "b",
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
    ("variable_constant_self_assignment_multiplication_works",
        """
        let a: int = 8;
        a = a * 6;
        """,
        "a",
        48),
    ("constant_variable_multiplication_works",
        "let a: int = 8;",
        "5*a",
        40),
    ("constant_variable_self_assignment_multiplication_works",
        """
        let a: int = 8;
        a = 5*a;
        """,
        "a",
        40),
    ("variable_variable_multiplication_works",
        "let a: int = 8; let b: int = -1;",
        "a*b",
        -8),
    ("variable_variable_self_assignment_multiplication_works",
        """
        let a: int = 8;
        let b: int = -1;
        a = a * b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_multiplication_works",
        """
        let a: int = 8;
        let b: int = -1;
        b = a * b;
        """,
        "b",
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
    ("variable_constant_self_assignment_division_works",
        """
        let a: int = 20;
        a = a / 5;
        """,
        "a",
        4),
    ("constant_variable_division_works",
        "let a: int = 8;",
        "24/a",
        3),
    ("constant_variable_self_assignment_division_works",
        """
        let a: int = 8;
        a = 24 / a;
        """,
        "a",
        3),
    ("variable_variable_division_works",
        "let a: int = 8; let b: int = -1;",
        "a/b",
        -8),
    ("variable_variable_self_assignment_division_works",
        """
        let a: int = 8; let b: int = -1;
        a = a / b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_division_works",
        """
        let a: int = 8; let b: int = -1;
        b = a / b;
        """,
        "b",
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
        "let a: int = 20;",
        "a%3",
        2),

    ("constant_variable_modulo_works",
        "let a: int = 9;",
        "24 % a",
        6),
    ("variable_variable_modulo_works",
        "let a: int = 13; let b: int = 5;",
        "a%b",
        3),

    # negation

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
    ("can_negate_constant_variable",
        f"""
        const X: int = 12345;
        let b: int = -X;
        """,
        "b",
        -12345),

    # logical shift left

    ("constant_constant_shl_works",
        """
        let a: int = 4 << 2;
        """,
        "a",
        16),

    ("variable_constant_shl_works",
        """
        let a: int = 4;
        let b: int = a << 2;
        """,
        "b",
        16),

    ("variable_constant_self_assignment_shl_works",
        """
        let a: int = 4;
        a = a << 2;
        """,
        "a",
        16),

    ("variable_byte_constant_shl_works",
        """
        let a: int = 4;
        let b: int = a << 2b;
        """,
        "b",
        16),

    ("variable_byte_constant_self_assignment_shl_works",
        """
        let a: int = 4b;
        a = a << 2;
        """,
        "a",
        16),

    ("variable_long_constant_shl_works",
        """
        let a: int = 4;
        let b: int = a << 2l;
        """,
        "b",
        16),

    ("variable_long_constant_self_assignment_shl_works",
        """
        let a: int = 4;
        a = a << 2l;
        """,
        "a",
        16),


    ("constant_variable_shl_works",
        """
        let a: int = 2;
        let b: int = 4 << a;
        """,
        "b",
        16),

    ("constant_variable_self_assignment_shl_works",
        """
        let a: int = 2;
        a = 4 << a;
        """,
        "a",
        16),
    ("byte_constant_variable_shl_works",
        """
        let a: int = 2;
        let b: int = (4b << a) as int;
        """,
        "b",
        16),

    ("byte_constant_variable_self_assignment_shl_works",
        """
        let a: int = 2;
        a = (4b << a) as int;
        """,
        "a",
        16),
    ("long_constant_variable_shl_works",
        """
        let a: int = 2;
        let b: int = (4l << a) as int;
        """,
        "b",
        16),

    ("long_constant_variable_self_assignment_shl_works",
        """
        let a: int = 2;
        a = (4l << a) as int;
        """,
        "a",
        16),

    ("variable_variable_shl_works",
        """
        let a: int = 4;
        let b: int = 2;
        let c: int = a << b;
        """,
        "c",
        16),

    ("variable_variable_self_assignment_shl_works",
        """
        let a: int = 4;
        let b: int = 2;
         a = a << b;
        """,
        "a",
        16),

    ("variable_variable_self_assignment_rhs_shl_works",
        """
        let a: int = 4;
        let b: int = 2;
        b = a << b;
        """,
        "b",
        16),

    # logical shift right

    ("constant_constant_shr_works",
        """
        let a: int = -2147483648 >>> 1;
        """,
        "a",
        1073741824),

    ("variable_constant_shr_works",
        """
        let a: int = -2147483648;
        let b: int = a >>> 1;
        """,
        "b",
        1073741824),

    ("variable_constant_self_assignment_shr_works",
        """
        let a: int = -2147483648;
        a = a >>> 1;
        """,
        "a",
        1073741824),

    ("variable_byte_constant_shr_works",
        """
        let a: int = -2147483648;
        let b: int = a >>> 1b;
        """,
        "b",
        1073741824),

    ("variable_byte_constant_self_assignment_shr_works",
        """
        let a: int = -2147483648;
        a = a >>> 1b;
        """,
        "a",
        1073741824),
    ("variable_long_constant_shr_works",
        """
        let a: int = -2147483648;
        let b: int = a >>> 1l;
        """,
        "b",
        1073741824),

    ("variable_long_constant_self_assignment_shr_works",
        """
        let a: int = -2147483648;
        a = a >>> 1l;
        """,
        "a",
        1073741824),


    ("constant_variable_shr_works",
        """
        let a: int = 1;
        let b: int = -2147483648 >>> a;
        """,
        "b",
        1073741824),

    ("constant_variable_self_assignment_shr_works",
        """
        let a: int = 1;
        a = -2147483648 >>> a;
        """,
        "a",
        1073741824),

    ("byte_constant_variable_shr_works",
        """
        let a: int = 1;
        let b: int = (-128b >>> a) as int;
        """,
        "b",
        64),

    ("byte_constant_variable_self_assignment_shr_works",
        """
        let a: int = 1;
        a =  (-128b >>> a) as int;
        """,
        "a",
        64),

    ("long_constant_variable_shr_works",
        """
        let a: int = 1;
        let b: int = (-2147483648l >>> a) as int;
        """,
        "b",
        -1073741824),

    ("long_constant_variable_self_assignment_shr_works",
        """
        let a: int = 1;
        a = (-2147483648l >>> a) as int;
        """,
        "a",
        -1073741824),

    ("variable_variable_shr_works",
        """
        let a: int = -2147483648;
        let b: int = 1;
        let c: int = a >>> b;
        """,
        "c",
        1073741824),

    ("variable_variable_self_assignment_shr_works",
        """
        let a: int = -2147483648;
        let b: int = 1;
         a = a >>> b;
        """,
        "a",
        1073741824),

    ("variable_variable_self_assignment_rhs_shr_works",
        """
        let a: int = -2147483648;
        let b: int = 1;
        b = a >>> b;
        """,
        "b",
        1073741824),


    # arithmetic shift right

    ("constant_constant_sar_works",
        """
        let a: int = -2147483648 >> 1;
        """,
        "a",
        -1073741824),

    ("variable_constant_sar_works",
        """
        let a: int = -2147483648;
        let b: int = a >> 1;
        """,
        "b",
        -1073741824),

    ("variable_constant_self_assignment_sar_works",
        """
        let a: int = -2147483648;
        a = a >> 1;
        """,
        "a",
        -1073741824),

    ("variable_byte_constant_sar_works",
        """
        let a: int = -2147483648;
        let b: int = a >> 1b;
        """,
        "b",
        -1073741824),

    ("variable_byte_constant_self_assignment_sar_works",
        """
        let a: int = -2147483648;
        a = a >> 1b;
        """,
        "a",
        -1073741824),

    ("variable_long_constant_sar_works",
        """
        let a: int = -2147483648;
        let b: int = a >> 1l;
        """,
        "b",
        -1073741824),

    ("variable_long_constant_self_assignment_sar_works",
        """
        let a: int = -2147483648;
        a = a >> 1l;
        """,
        "a",
        -1073741824),

    ("constant_variable_sar_works",
        """
        let a: int = 1;
        let b: int = -2147483648 >> a;
        """,
        "b",
        -1073741824),

    ("constant_variable_self_assignment_sar_works",
        """
        let a: int = 1;
        a = -2147483648 >> a;
        """,
        "a",
        -1073741824),


    ("byte_constant_variable_sar_works",
        """
        let a: int = 1;
        let b: int = (-128b >> a) as int;
        """,
        "b",
        -64),

    ("byte_constant_variable_self_assignment_sar_works",
        """
        let a: int = 1;
        a = (-128b >> a) as int;
        """,
        "a",
        -64),

    ("long_constant_variable_sar_works",
        """
        let a: int = 1;
        let b: int = (-2147483648l >> a) as int;
        """,
        "b",
        -1073741824),

    ("long_constant_variable_self_assignment_sar_works",
        """
        let a: int = 1;
        a = (-2147483648l >> a) as int;
        """,
        "a",
        -1073741824),

    ("variable_variable_sar_works",
        """
        let a: int = -2147483648;
        let b: int = 1;
        let c: int = a >> b;
        """,
        "c",
        -1073741824),

    ("variable_variable_self_assignment_sar_works",
        """
        let a: int = -2147483648;
        let b: int = 1;
         a = a >> b;
        """,
        "a",
        -1073741824),

    ("variable_variable_self_assignment_rhs_sar_works",
        """
        let a: int = -2147483648;
        let b: int = 1;
        b = a >> b;
        """,
        "b",
        -1073741824),

    # bitwise AND
    ("constant_constant_bitwise_and_works",
        """
        let a: int = 167 & 181;
        """,
        "a",
        165),

    ("variable_constant_bitwise_and_works",
        """
        let a: int = 167;
        let b: int = a & 181;
        """,
        "b",
        165),

    ("variable_constant_self_assignment_bitwise_and_works",
        """
        let a: int = 167;
        a = a & 181;
        """,
        "a",
        165),

    ("constant_variable_bitwise_and_works",
        """
        let a: int = 181;
        let b: int = 167 & a;
        """,
        "b",
        165),

    ("constant_variable_self_assignment_bitwise_and_works",
        """
        let a: int = 167;
        a = 181 & a;
        """,
        "a",
        165),

    ("variable_variable_bitwise_and_works",
        """
        let a: int = 167;
        let b: int = 181;
        let c: int = a & b;
        """,
        "c",
        165),

    ("variable_variable_self_assignment_bitwise_and_works",
        """
        let a: int = 167;
        let b: int = 181;
        a = a & b;
        """,
        "a",
        165),

    ("variable_variable_self_assignment_rhs_bitwise_and_works",
        """
        let a: int = 167;
        let b: int = 181;
        b = a & b;
        """,
        "b",
        165),


    # bitwise OR
    ("constant_constant_bitwise_or_works",
        """
        let a: int = 167 | 181;
        """,
        "a",
        183),

    ("variable_constant_bitwise_or_works",
        """
        let a: int = 167;
        let b: int = a | 181;
        """,
        "b",
        183),

    ("variable_constant_self_assignment_bitwise_or_works",
        """
        let a: int = 167;
        a = a | 181;
        """,
        "a",
        183),

    ("constant_variable_bitwise_or_works",
        """
        let a: int = 181;
        let b: int = 167 | a;
        """,
        "b",
        183),

    ("constant_variable_self_assignment_bitwise_or_works",
        """
        let a: int = 167;
        a = 181 | a;
        """,
        "a",
        183),

    ("variable_variable_bitwise_or_works",
        """
        let a: int = 167;
        let b: int = 181;
        let c: int = a | b;
        """,
        "c",
        183),

    ("variable_variable_self_assignment_bitwise_or_works",
        """
        let a: int = 167;
        let b: int = 181;
        a = a | b;
        """,
        "a",
        183),

    ("variable_variable_self_assignment_rhs_bitwise_or_works",
        """
        let a: int = 167;
        let b: int = 181;
        b = a | b;
        """,
        "b",
        183),

    # bitwise XOR
    ("constant_constant_bitwise_xor_works",
        """
        let a: int = 167 ^ 181;
        """,
        "a",
        18),

    ("variable_constant_bitwise_xor_works",
        """
        let a: int = 167;
        let b: int = a ^ 181;
        """,
        "b",
        18),

    ("variable_constant_self_assignment_bitwise_xor_works",
        """
        let a: int = 167;
        a = a ^ 181;
        """,
        "a",
        18),

    ("constant_variable_bitwise_xor_works",
        """
        let a: int = 181;
        let b: int = 167 ^ a;
        """,
        "b",
        18),

    ("constant_variable_self_assignment_bitwise_xor_works",
        """
        let a: int = 167;
        a = 181 ^ a;
        """,
        "a",
        18),

    ("variable_variable_bitwise_xor_works",
        """
        let a: int = 167;
        let b: int = 181;
        let c: int = a ^ b;
        """,
        "c",
        18),

    ("variable_variable_self_assignment_bitwise_xor_works",
        """
        let a: int = 167;
        let b: int = 181;
        a = a ^ b;
        """,
        "a",
        18),

    ("variable_variable_self_assignment_rhs_bitwise_xor_works",
        """
        let a: int = 167;
        let b: int = 181;
        b = a ^ b;
        """,
        "b",
        18),


    # bitwise NOT
    ("can_bitwise_not_constant",
        """
        let a: int = ~684;
        """,
        "a",
        -685),
    ("can_bitwise_not_variable",
        """
        let a: int = 684;
        a = ~a;
        """,
        "a",
        -685),
    ("can_bitwise_not_negative_variable",
        """
        let a: int = -685;
        a = ~a;
        """,
        "a",
        684),
    ("can_bitwise_not_variable_when_assigning_to_another_variable",
        """
        let a: int = 684;
        let b: int = ~a;
        """,
        "b",
        -685),
    ("can_bitwise_not_negative_variable_when_assigning_to_another_variable",
        """
        let a: int = -685;
        let b: int = ~a;
        """,
        "b",
        684),
    ("can_bitwise_not_int_max",
        f"""
        let a: int = {INT_MAX};
        let b: int = ~a;
        """,
        "b",
        INT_MIN),
    ("can_bitwise_not_int_min",
        f"""
        let a: int = {INT_MIN};
        let b: int = ~a;
        """,
        "b",
        INT_MAX),
    ("can_bitwise_not_constant_variable",
        f"""
        const X: int = 684;
        let b: int = ~X;
        """,
        "b",
        -685),

    # +=
    ("self_addition_shorthand_works",
        f"""
        let a: int = 4;
        a += 6;
        """,
        "a",
        10),

    ("self_addition_shorthand_works_with_arrays",
        f"""
        let a: int[4] = 4;
        a[0] += 6;
        """,
        "a[0]",
        10),


    # -=
    ("self_subtraction_shorthand_works",
        f"""
        let a: int = 4;
        a -= 6;
        """,
        "a",
        -2),

    ("self_subtraction_shorthand_works_with_arrays",
        f"""
        let a: int[4] = 4;
        a[0] -= 6;
        """,
        "a[0]",
        -2),


    # *=
    ("self_multiplication_shorthand_works",
        f"""
        let a: int = 4;
        a *= 6;
        """,
        "a",
        24),

    ("self_multiplication_shorthand_works_with_arrays",
        f"""
        let a: int[4] = 4;
        a[0] *= 6;
        """,
        "a[0]",
        24),


    # /=
    ("self_division_shorthand_works",
        f"""
        let a: int = 9;
        a /= 2;
        """,
        "a",
        4),

    ("self_division_shorthand_works_with_arrays",
        f"""
        let a: int[4] = 9;
        a[0] /= 2;
        """,
        "a[0]",
        4),


    # %=
    ("self_modulo_shorthand_works",
        f"""
        let a: int = 14;
        a %= 5;
        """,
        "a",
        4),

    ("self_modulo_shorthand_works_with_arrays",
        f"""
        let a: int[4] = 14;
        a[0] %= 5;
        """,
        "a[0]",
        4),

    # <<=

    ("self_shl_shorthand_works",
        """
        let a: int = 16;
        a <<= 2;
        """,
        "a",
       64),


    ("self_shl_shorthand_works_with_arrays",
        """
        let a: int[4] = 16;
        a[0] <<= 2;
        """,
        "a[0]",
       64),

    # >>=

    ("self_sar_shorthand_works",
        """
        let a: int = -16;
        a >>= 2;
        """,
        "a",
        -4),

    ("self_sar_shorthand_works_with_arrays",
        """
        let a: int[4] = -16;
        a[0] >>= 2;
        """,
        "a[0]",
        -4),

    # >>>=

    ("self_shr_shorthand_works",
        """
        let a: int = -16;
        a >>>= 2;
        """,
        "a",
        1073741820),


    ("self_shr_shorthand_works_with_arrays",
        """
        let a: int[4] = -16;
        a[0] >>>= 2;
        """,
        "a[0]",
        1073741820),

    # &=

    ("self_bitwise_and_shorthand_works_with_self",
        """
        let a: int = 167;
        let b: int = 181;
        a &= b;
        """,
        "a",
        165),

    ("self_bitwise_and_shorthand_works_with_arrays",
        """
        let a: int[4] = 167;
        let b: int = 181;
        a[0] &= b;
        """,
        "a[0]",
        165),

    # |=

    ("self_bitwise_or_shorthand_works_with_self",
        """
        let a: int = 167;
        let b: int = 181;
        a |= b;
        """,
        "a",
        183),

    ("self_bitwise_or_shorthand_works_with_arrays",
        """
        let a: int[4] = 167;
        let b: int = 181;
        a[0] |= b;
        """,
        "a[0]",
        183),


    # ^=

    ("self_bitwise_xor_shorthand_works_with_self",
        """
        let a: int = 167;
        let b: int = 181;
        a ^= b;
        """,
        "a",
        18),

    ("self_bitwise_xor_shorthand_works_with_arrays",
        """
        let a: int[4] = 167;
        let b: int = 181;
        a[0] ^= b;
        """,
        "a[0]",
        18),
]

for t in test_tuples:

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




