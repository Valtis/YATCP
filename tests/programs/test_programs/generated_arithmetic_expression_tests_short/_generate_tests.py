SHORT_MAX = 2**15-1
SHORT_MIN = -SHORT_MAX-1


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
        "4s",
        4),
    ("negative_constant_expression_works",
        "",
        "-4s",
        -4),


    # addition
    ("constant_constant_addition_works",
        "",
        "2s + 9s",
        11),
    ("constant_constant_addition_with_negative_number_works",
        "",
        "10s + (-5s)",
        5),
    ("variable_constant_addition_works",
        "let a: short = 24;",
        "a + 9s",
        "33"),
    ("constant_variable_addition_works",
        "let a: short = 49;",
        "5s + a",
        54),
    ("constant_variable__self_assignment_addition_works",
        """
        let a: short = 49;
        a = 5s + a;
        """,
        "a",
        54),
     ("constant_variable_self_assignment_addition_works",
        """
        let a: short = 49;
        a = a + 5s;
        """,
        "a",
        54),
    ("variable_variable_addition_works",
        "let a: short = 123s; let b: short = 47;",
        "a + b",
        170),
    ("variable_variable_self_assignment_addition_works",
        """
        let a: short = 123;
        let b: short = 47;
        a = a + b;
        """,
        "a",
        170),
    ("variable_variable_self_assignment_rhs_addition_works",
        """
        let a: short = 123s;
        let b: short = 47s;
        a = b + a;
        """,
        "a",
        170),
    ("complex_addition_works",
        """
        let a: short = 20;
        let b: short = 8;
        let c: short = 5s + a + b;
        let d: short = c + 4;
        """,
        "0+a+b+c+d+1",
       99),
    ("can_add_up_to_short_max",
        f"let a: short = {SHORT_MAX-1};",
        "a + 1",
        SHORT_MAX),
    ("addition_overflows_when_adding_to_short_max",
        f"let a: short = {SHORT_MAX};",
        "a + 1",
        SHORT_MIN),


    # subtraction
    ("constant_constant_subtraction_works_when_result_is_positive",
       "",
       "3s-1s",
       2),
    ("constant_constant_subtraction_works_when_result_is_negative",
       "",
       "3s-9s",
       -6),
    ("constant_constant_subtraction_works_when_rhs_operand_is_negated",
       "",
       "3s-(-2s)",
       5),
    ("variable_constant_subtraction_works",
        "let a: short = 8;",
        "a - 6s",
        2),
    ("variable_constant_self_assignment_subtraction_works",
        """
        let a: short = 8s;
        a = a - 6s;
        """,
        "a",
        2),
    ("constant_variable_subtraction_works",
        "let a: short = 8s;",
        "5s - a",
        -3),
    ("constant_variable_self_assignment_subtraction_works",
        """
        let a: short = 8s;
        a = 5s - a;
        """,
        "a",
        -3),
    ("variable_variable_subtraction_works",
        "let a: short = 8s; let b: short = 5402;",
        "a - b",
        -5394),
    ("variable_variable_self_assignment_subtraction_works",
        """
        let a: short = 8;
        let b: short = 5402;
        a = a - b;
        """,
        "a",
        -5394),
    ("variable_variable_self_assignment_rhs_subtraction_works",
        """
        let a: short = 8;
        let b: short = 5402;
        b = a - b;
        """,
        "b",
        -5394),
    ("complex_subtraction_works",
        """
        let a: short = 20;
        let b: short = 8;
        let c: short = 5 - a - b;
        let d: short = c - 4s;
        """,
        "0-a-b-c-d-1",
       21),
    ("can_subtract_to_short_min",
        f"let a: short = {SHORT_MIN+1};",
        "a - 1s",
        SHORT_MIN,
        ),
    ("subtraction_overflows_when_subtracting_from_short_min",
        f"let a: short = {SHORT_MIN};",
        "a - 1s",
        SHORT_MAX),

    # multiplication
    ("constant_constant_multiplication_works_when_result_is_positive",
       "",
       "3s*7s",
       21),
    ("constant_constant_multiplication_works_when_lhs_operand_is_negative",
       "",
       "-3s*9s",
       -27),
    ("constant_constant_multiplication_works_when_rhs_operand_is_negated",
       "",
       "3s*(-2s)",
       -6),
    ("variable_constant_multiplication_works",
        "let a: short = 8;",
        "a*6",
        48),
    ("variable_constant_self_assignment_multiplication_works",
        """
        let a: short = 8;
        a = a * 6;
        """,
        "a",
        48),
    ("constant_variable_multiplication_works",
        "let a: short = 8;",
        "5*a",
        40),
    ("constant_variable_self_assignment_multiplication_works",
        """
        let a: short = 8;
        a = 5*a;
        """,
        "a",
        40),
    ("variable_variable_multiplication_works",
        "let a: short = 8; let b: short = -1;",
        "a*b",
        -8),
    ("variable_variable_self_assignment_multiplication_works",
        """
        let a: short = 8;
        let b: short = -1;
        a = a * b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_multiplication_works",
        """
        let a: short = 8;
        let b: short = -1;
        b = a * b;
        """,
        "b",
        -8),
    ("complex_multiplication_works",
        """
        let a: short = 1;
        let b: short = 2;
        let c: short = 3*a*b;
        let d: short = c*4;
        """,
        "a*b*c*d*10",
       2880),
    ("positive_multiplication_result_can_overflow",
        f"let a: short = {SHORT_MAX};",
        "a*2",
        -2),
    ("negative_multiplication_result_can_overflow",
        f"let a: short = {SHORT_MIN};",
        "a*(-1)",
        SHORT_MIN,
        ),


    # division
    ("constant_constant_division_works_when_result_is_positive",
       "",
       "7s/2s",
       3),
    ("constant_constant_division_works_when_lhs_operand_is_negative",
       "",
       "-12s/4s",
       -3),
    ("constant_constant_division_works_when_rhs_operand_is_negated",
       "",
       "5s/(-2s)",
       -2),
    ("variable_constant_division_works",
        "let a: short = 20;",
        "a/5",
        4),
    ("variable_constant_self_assignment_division_works",
        """
        let a: short = 20;
        a = a / 5;
        """,
        "a",
        4),
    ("constant_variable_division_works",
        "let a: short = 8;",
        "24/a",
        3),
    ("constant_variable_self_assignment_division_works",
        """
        let a: short = 8;
        a = 24 / a;
        """,
        "a",
        3),
    ("variable_variable_division_works",
        "let a: short = 8; let b: short = -1;",
        "a/b",
        -8),
    ("variable_variable_self_assignment_division_works",
        """
        let a: short = 8; let b: short = -1;
        a = a / b;
        """,
        "a",
        -8),
    ("variable_variable_self_assignment_rhs_division_works",
        """
        let a: short = 8; let b: short = -1;
        b = a / b;
        """,
        "b",
        -8),
    ("complex_division_works",
        """
        let a: short = 10;
        let b: short = 2;
        let c: short = a/b;
        let d: short = c/2;
        """,
        "500/a/b/c/d",
       2),


    # modulo
    ("constant_constant_modulo_works_when_result_is_positive",
       "",
       "7s % 2s",
       1),
    ("constant_constant_modulo_works_when_lhs_operand_is_negative",
       "",
       "-12s%5s",
       -2),

    ("constant_constant_modulo_works_when_rhs_operand_is_negated",
       "",
       "5%(-2)",
       1),

    ("variable_constant_modulo_works",
        "let a: short = 20;",
        "a%3",
        2),

    ("constant_variable_modulo_works",
        "let a: short = 9;",
        "24 % a",
        6),
    ("variable_variable_modulo_works",
        "let a: short = 13; let b: short = 5;",
        "a%b",
        3),

    # negation

    ("can_negate_variable",
        """
        let a: short = 4;
        a = -a;
        """,
        "a",
        -4),
    ("can_negate_negative_variable",
        """
        let a: short = -4s;
        a = -a;
        """,
        "a",
        4),
    ("can_negate_variable_when_assigning_to_another_variable",
        """
        let a: short = 4;
        let b: short = -a;
        """,
        "b",
        -4),
    ("can_negate_negative_variable_when_assigning_to_another_variable",
        """
        let a: short = -4;
        let b: short = -a;
        """,
        "b",
        4),
    ("can_negate_short_max",
        f"""
        let a: short = {SHORT_MAX};
        let b: short = -a;
        """,
        "b",
        -SHORT_MAX),
    ("negating_short_min_overflows_to_short_min",
        f"""
        let a: short = {SHORT_MIN};
        let b: short = -a;
        """,
        "b",
        SHORT_MIN),
    ("can_negate_constant_variable",
        f"""
        const X: short = 12345s;
        let b: short = -X;
        """,
        "b",
        -12345),

    # logical shift left

    ("constant_constant_shl_works",
        """
        let a: short = 4s << 2;
        """,
        "a",
        16),

    ("variable_constant_shl_works",
        """
        let a: short = 4;
        let b: short = a << 2;
        """,
        "b",
        16),

    ("variable_constant_self_assignment_shl_works",
        """
        let a: short = 4;
        a = a << 2;
        """,
        "a",
        16),

    ("variable_byte_constant_shl_works",
        """
        let a: short = 4;
        let b: short = a << 2b;
        """,
        "b",
        16),

    ("variable_byte_constant_self_assignment_shl_works",
        """
        let a: short = 4b;
        a = a << 2;
        """,
        "a",
        16),

    ("variable_long_constant_shl_works",
        """
        let a: short = 4;
        let b: short = a << 2l;
        """,
        "b",
        16),

    ("variable_long_constant_self_assignment_shl_works",
        """
        let a: short = 4;
        a = a << 2l;
        """,
        "a",
        16),


    ("constant_variable_shl_works",
        """
        let a: short = 2;
        let b: short = 4s << a;
        """,
        "b",
        16),

    ("constant_variable_self_assignment_shl_works",
        """
        let a: short = 2;
        a = 4S << a;
        """,
        "a",
        16),
    ("byte_constant_variable_shl_works",
        """
        let a: short = 2;
        let b: short = (4b << a) as short;
        """,
        "b",
        16),

    ("byte_constant_variable_self_assignment_shl_works",
        """
        let a: short = 2;
        a = (4b << a) as short;
        """,
        "a",
        16),
    ("long_constant_variable_shl_works",
        """
        let a: short = 2;
        let b: short = (4l << a) as short;
        """,
        "b",
        16),

    ("long_constant_variable_self_assignment_shl_works",
        """
        let a: short = 2;
        a = (4l << a) as short;
        """,
        "a",
        16),

    ("variable_variable_shl_works",
        """
        let a: short = 4;
        let b: short = 2;
        let c: short = a << b;
        """,
        "c",
        16),

    ("variable_variable_self_assignment_shl_works",
        """
        let a: short = 4;
        let b: short = 2;
         a = a << b;
        """,
        "a",
        16),

    ("variable_variable_self_assignment_rhs_shl_works",
        """
        let a: short = 4;
        let b: short = 2;
        b = a << b;
        """,
        "b",
        16),

    # logical shift right

    ("constant_constant_shr_works",
        f"""
        let a: short = {SHORT_MIN}s >>> 1;
        """,
        "a",
        16384),

    ("variable_constant_shr_works",
        f"""
        let a: short = {SHORT_MIN}s;
        let b: short = a >>> 1;
        """,
        "b",
        16384),

    ("variable_constant_self_assignment_shr_works",
        f"""
        let a: short = {SHORT_MIN}s;
        a = a >>> 1;
        """,
        "a",
        16384),

    ("variable_byte_constant_shr_works",
        f"""
        let a: short = {SHORT_MIN}s;
        let b: short = a >>> 1b;
        """,
        "b",
        16384),

    ("variable_byte_constant_self_assignment_shr_works",
        f"""
        let a: short = {SHORT_MIN}s;
        a = a >>> 1b;
        """,
        "a",
        16384),
    ("variable_long_constant_shr_works",
        f"""
        let a: short = {SHORT_MIN}s;
        let b: short = a >>> 1l;
        """,
        "b",
        16384),

    ("variable_long_constant_self_assignment_shr_works",
        f"""
        let a: short = {SHORT_MIN}s;
        a = a >>> 1l;
        """,
        "a",
        16384),


    ("constant_variable_shr_works",
        f"""
        let a: short = 1;
        let b: short = {SHORT_MIN}s >>> a;
        """,
        "b",
        16384),

    ("constant_variable_self_assignment_shr_works",
        f"""
        let a: short = 1;
        a = {SHORT_MIN}s >>> a;
        """,
        "a",
        16384),

    ("byte_constant_variable_shr_works",
        """
        let a: short = 1;
        let b: short = (-128b >>> a) as short;
        """,
        "b",
        64),

    ("byte_constant_variable_self_assignment_shr_works",
        """
        let a: short = 1;
        a =  (-128b >>> a) as short;
        """,
        "a",
        64),

    ("long_constant_variable_shr_works",
        f"""
        let a: short = 1;
        let b: short = ({SHORT_MIN}L >>> a) as short;
        """,
        "b",
        -16384),

    ("long_constant_variable_self_assignment_shr_works",
        f"""
        let a: short = 1;
        a = ({SHORT_MIN}l >>> a) as short;
        """,
        "a",
        -16384),

    ("variable_variable_shr_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = 1;
        let c: short = a >>> b;
        """,
        "c",
        16384),

    ("variable_variable_self_assignment_shr_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = 1;
         a = a >>> b;
        """,
        "a",
        16384),

    ("variable_variable_self_assignment_rhs_shr_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = 1;
        b = a >>> b;
        """,
        "b",
        16384),


    # arithmetic shift right

    ("constant_constant_sar_works",
        f"""
        let a: short = {SHORT_MIN}S >> 1;
        """,
        "a",
        -16384),

    ("variable_constant_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = a >> 1;
        """,
        "b",
        -16384),

    ("variable_constant_self_assignment_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        a = a >> 1;
        """,
        "a",
        -16384),

    ("variable_byte_constant_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = a >> 1b;
        """,
        "b",
        -16384),

    ("variable_byte_constant_self_assignment_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        a = a >> 1b;
        """,
        "a",
        -16384),

    ("variable_long_constant_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = a >> 1l;
        """,
        "b",
        -16384),

    ("variable_long_constant_self_assignment_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        a = a >> 1l;
        """,
        "a",
        -16384),

    ("constant_variable_sar_works",
        f"""
        let a: short = 1;
        let b: short = {SHORT_MIN}S >> a;
        """,
        "b",
        -16384),

    ("constant_variable_self_assignment_sar_works",
        f"""
        let a: short = 1;
        a = {SHORT_MIN}S >> a;
        """,
        "a",
        -16384),


    ("byte_constant_variable_sar_works",
        """
        let a: short = 1;
        let b: short = (-128b >> a) as short;
        """,
        "b",
        -64),

    ("byte_constant_variable_self_assignment_sar_works",
        """
        let a: short = 1;
        a = (-128b >> a) as short;
        """,
        "a",
        -64),

    ("long_constant_variable_sar_works",
        f"""
        let a: short = 1;
        let b: short = ({SHORT_MIN}l >> a) as short;
        """,
        "b",
        -16384),

    ("long_constant_variable_self_assignment_sar_works",
        f"""
        let a: short = 1;
        a = ({SHORT_MIN}l >> a) as short;
        """,
        "a",
        -16384),

    ("variable_variable_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = 1;
        let c: short = a >> b;
        """,
        "c",
        -16384),

    ("variable_variable_self_assignment_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = 1;
         a = a >> b;
        """,
        "a",
        -16384),

    ("variable_variable_self_assignment_rhs_sar_works",
        f"""
        let a: short = {SHORT_MIN}S;
        let b: short = 1;
        b = a >> b;
        """,
        "b",
        -16384),

    # bitwise AND
    ("constant_constant_bitwise_and_works",
        """
        let a: short = 167s & 181s;
        """,
        "a",
        165),

    ("variable_constant_bitwise_and_works",
        """
        let a: short = 167;
        let b: short = a & 181;
        """,
        "b",
        165),

    ("variable_constant_self_assignment_bitwise_and_works",
        """
        let a: short = 167;
        a = a & 181;
        """,
        "a",
        165),

    ("constant_variable_bitwise_and_works",
        """
        let a: short = 181;
        let b: short = 167 & a;
        """,
        "b",
        165),

    ("constant_variable_self_assignment_bitwise_and_works",
        """
        let a: short = 167;
        a = 181 & a;
        """,
        "a",
        165),

    ("variable_variable_bitwise_and_works",
        """
        let a: short = 167;
        let b: short = 181;
        let c: short = a & b;
        """,
        "c",
        165),

    ("variable_variable_self_assignment_bitwise_and_works",
        """
        let a: short = 167;
        let b: short = 181;
        a = a & b;
        """,
        "a",
        165),

    ("variable_variable_self_assignment_rhs_bitwise_and_works",
        """
        let a: short = 167;
        let b: short = 181;
        b = a & b;
        """,
        "b",
        165),


    # bitwise OR
    ("constant_constant_bitwise_or_works",
        """
        let a: short = 167s | 181s;
        """,
        "a",
        183),

    ("variable_constant_bitwise_or_works",
        """
        let a: short = 167;
        let b: short = a | 181;
        """,
        "b",
        183),

    ("variable_constant_self_assignment_bitwise_or_works",
        """
        let a: short = 167;
        a = a | 181;
        """,
        "a",
        183),

    ("constant_variable_bitwise_or_works",
        """
        let a: short = 181;
        let b: short = 167 | a;
        """,
        "b",
        183),

    ("constant_variable_self_assignment_bitwise_or_works",
        """
        let a: short = 167;
        a = 181 | a;
        """,
        "a",
        183),

    ("variable_variable_bitwise_or_works",
        """
        let a: short = 167;
        let b: short = 181;
        let c: short = a | b;
        """,
        "c",
        183),

    ("variable_variable_self_assignment_bitwise_or_works",
        """
        let a: short = 167;
        let b: short = 181;
        a = a | b;
        """,
        "a",
        183),

    ("variable_variable_self_assignment_rhs_bitwise_or_works",
        """
        let a: short = 167;
        let b: short = 181;
        b = a | b;
        """,
        "b",
        183),

    # bitwise XOR
    ("constant_constant_bitwise_xor_works",
        """
        let a: short = 167s ^ 181s;
        """,
        "a",
        18),

    ("variable_constant_bitwise_xor_works",
        """
        let a: short = 167;
        let b: short = a ^ 181;
        """,
        "b",
        18),

    ("variable_constant_self_assignment_bitwise_xor_works",
        """
        let a: short = 167;
        a = a ^ 181;
        """,
        "a",
        18),

    ("constant_variable_bitwise_xor_works",
        """
        let a: short = 181;
        let b: short = 167 ^ a;
        """,
        "b",
        18),

    ("constant_variable_self_assignment_bitwise_xor_works",
        """
        let a: short = 167;
        a = 181 ^ a;
        """,
        "a",
        18),

    ("variable_variable_bitwise_xor_works",
        """
        let a: short = 167;
        let b: short = 181;
        let c: short = a ^ b;
        """,
        "c",
        18),

    ("variable_variable_self_assignment_bitwise_xor_works",
        """
        let a: short = 167;
        let b: short = 181;
        a = a ^ b;
        """,
        "a",
        18),

    ("variable_variable_self_assignment_rhs_bitwise_xor_works",
        """
        let a: short = 167;
        let b: short = 181;
        b = a ^ b;
        """,
        "b",
        18),


    # bitwise NOT
    ("can_bitwise_not_constant",
        """
        let a: short = ~684s;
        """,
        "a",
        -685),
    ("can_bitwise_not_variable",
        """
        let a: short = 684;
        a = ~a;
        """,
        "a",
        -685),
    ("can_bitwise_not_negative_variable",
        """
        let a: short = -685;
        a = ~a;
        """,
        "a",
        684),
    ("can_bitwise_not_variable_when_assigning_to_another_variable",
        """
        let a: short = 684;
        let b: short = ~a;
        """,
        "b",
        -685),
    ("can_bitwise_not_negative_variable_when_assigning_to_another_variable",
        """
        let a: short = -685;
        let b: short = ~a;
        """,
        "b",
        684),
    ("can_bitwise_not_short_max",
        f"""
        let a: short = {SHORT_MAX};
        let b: short = ~a;
        """,
        "b",
        SHORT_MIN),
    ("can_bitwise_not_short_min",
        f"""
        let a: short = {SHORT_MIN};
        let b: short = ~a;
        """,
        "b",
        SHORT_MAX),
    ("can_bitwise_not_constant_variable",
        f"""
        const X: short = 684s;
        let b: short = ~X;
        """,
        "b",
        -685),

    # +=
    ("self_addition_shorthand_works",
        f"""
        let a: short = 4;
        a += 6s;
        """,
        "a",
        10),

    ("self_addition_shorthand_works_with_arrays",
        f"""
        let a: short[4] = 4;
        a[0] += 6s;
        """,
        "a[0]",
        10),


    # -=
    ("self_subtraction_shorthand_works",
        f"""
        let a: short = 4;
        a -= 6s;
        """,
        "a",
        -2),

    ("self_subtraction_shorthand_works_with_arrays",
        f"""
        let a: short[4] = 4;
        a[0] -= 6s;
        """,
        "a[0]",
        -2),


    # *=
    ("self_multiplication_shorthand_works",
        f"""
        let a: short = 4;
        a *= 6s;
        """,
        "a",
        24),

    ("self_multiplication_shorthand_works_with_arrays",
        f"""
        let a: short[4] = 4;
        a[0] *= 6s;
        """,
        "a[0]",
        24),


    # /=
    ("self_division_shorthand_works",
        f"""
        let a: short = 9;
        a /= 2s;
        """,
        "a",
        4),

    ("self_division_shorthand_works_with_arrays",
        f"""
        let a: short[4] = 9;
        a[0] /= 2s;
        """,
        "a[0]",
        4),


    # %=
    ("self_modulo_shorthand_works",
        f"""
        let a: short = 14;
        a %= 5s;
        """,
        "a",
        4),

    ("self_modulo_shorthand_works_with_arrays",
        f"""
        let a: short[4] = 14;
        a[0] %= 5s;
        """,
        "a[0]",
        4),

    # <<=

    ("self_shl_shorthand_works",
        """
        let a: short = 16;
        a <<= 2s;
        """,
        "a",
       64),


    ("self_shl_shorthand_works_with_arrays",
        """
        let a: short[4] = 16;
        a[0] <<= 2s;
        """,
        "a[0]",
       64),

    # >>=

    ("self_sar_shorthand_works",
        """
        let a: short = -16;
        a >>= 2s;
        """,
        "a",
        -4),

    ("self_sar_shorthand_works_with_arrays",
        """
        let a: short[4] = -16;
        a[0] >>= 2s;
        """,
        "a[0]",
        -4),

    # >>>=

    ("self_shr_shorthand_works",
        """
        let a: short = -16;
        a >>>= 2s;
        """,
        "a",
        16380),


    ("self_shr_shorthand_works_with_arrays",
        """
        let a: short[4] = -16;
        a[0] >>>= 2s;
        """,
        "a[0]",
        16380),

    # &=

    ("self_bitwise_and_shorthand_works_with_self",
        """
        let a: short = 167;
        let b: short = 181;
        a &= b;
        """,
        "a",
        165),

    ("self_bitwise_and_shorthand_works_with_arrays",
        """
        let a: short[4] = 167;
        let b: short = 181;
        a[0] &= b;
        """,
        "a[0]",
        165),

    # |=

    ("self_bitwise_or_shorthand_works_with_self",
        """
        let a: short = 167;
        let b: short = 181;
        a |= b;
        """,
        "a",
        183),

    ("self_bitwise_or_shorthand_works_with_arrays",
        """
        let a: short[4] = 167;
        let b: short = 181;
        a[0] |= b;
        """,
        "a[0]",
        183),


    # ^=

    ("self_bitwise_xor_shorthand_works_with_self",
        """
        let a: short = 167;
        let b: short = 181;
        a ^= b;
        """,
        "a",
        18),

    ("self_bitwise_xor_shorthand_works_with_arrays",
        """
        let a: short[4] = 167;
        let b: short = 181;
        a[0] ^= b;
        """,
        "a[0]",
        18),
]

for t in test_tuples:

    out = f'''
program: |
    fn test_function() : short {{
        {t[1]}
        return {t[2]};
    }}
link_with:
    - tests/files/support/support.c
callable: test_function
returns: short
expect_stdout: |
    {t[3]}
    '''
    with open(f"{t[0]}.yaml", "w") as f:
        f.write(out)




