

"""
    Tuples of:
        * Test file name without extension
        * Initialization expression(s) (e.g. variable declarations)
        * Expression for return statement
        * Expected answer
        """
test_tuples = [

    # addition
        ############## byte ##############

            ("struct_const_byte_addition_works",
                "byte",
                "let out = test_variable.a + 2b;",
                "out",
                102),

            ("struct_const_byte_self_addition_works",
                "byte",
                "test_variable.a = test_variable.a + 2b;",
                "test_variable.a",
                102),

            ("const_struct_byte_addition_works",
                "byte",
                "let out = 5b + test_variable.a;",
                "out",
                105),

            ("struct_variable_byte_addition_works",
                "byte",
                "let x = 8b; let out = test_variable.a + x;",
                "out",
                108),

            ("struct_variable_byte_self_addition_works",
                "byte",
                "let x = 8b; test_variable.a = test_variable.a + x;",
                "test_variable.a",
                108),

            ("variable_struct_byte_addition_works",
                "byte",
                "let x = 15b; let out = x + test_variable.a;",
                "out",
                115),

            ("struct_array_variable_byte_addition_works",
                "byte",
                "let x: byte[3] = 8b; let out = test_variable.a + x[1];",
                "out",
                108),

            ("struct_array_variable_byte_self_addition_works",
                "byte",
                "let x: byte[3] = 8b; test_variable.a = test_variable.a + x[1];",
                "test_variable.a",
                108),

            ("array_variable_struct_byte_addition_works",
                "byte",
                "let x: byte[4] = 15b; let out = x[2] + test_variable.a;",
                "out",
                115),


        ############## short ##############

            ("struct_const_short_addition_works",
                "short",
                "let out = test_variable.a + 2s;",
                "out",
                102),

            ("struct_const_short_self_addition_works",
                "short",
                "test_variable.a = test_variable.a + 2s;",
                "test_variable.a",
                102),

            ("const_struct_short_addition_works",
                "short",
                "let out = 5s + test_variable.a;",
                "out",
                105),

            ("struct_variable_short_addition_works",
                "short",
                "let x = 8s; let out = test_variable.a + x;",
                "out",
                108),

            ("struct_variable_short_self_addition_works",
                "short",
                "let x = 8s; test_variable.a = test_variable.a + x;",
                "test_variable.a",
                108),

            ("variable_struct_short_addition_works",
                "short",
                "let x = 15s; let out = x + test_variable.a;",
                "out",
                115),

            ("struct_array_variable_short_addition_works",
                "short",
                "let x: short[3] = 8s; let out = test_variable.a + x[1];",
                "out",
                108),


            ("struct_array_variable_short_self_addition_works",
                "short",
                "let x: short[3] = 8s; test_variable.a = test_variable.a + x[1];",
                "test_variable.a",
                108),

            ("array_variable_struct_short_addition_works",
                "short",
                "let x: short[4] = 15s; let out = x[2] + test_variable.a;",
                "out",
                115),

        ############## int ##############

            ("struct_const_int_addition_works",
                "int",
                "let out = test_variable.a + 2i;",
                "out",
                102),

            ("struct_const_int_self_addition_works",
                "int",
                "test_variable.a = test_variable.a + 2i;",
                "test_variable.a",
                102),

            ("const_struct_int_addition_works",
                "int",
                "let out = 5i + test_variable.a;",
                "out",
                105),

            ("struct_variable_int_addition_works",
                "int",
                "let x = 8i; let out = test_variable.a + x;",
                "out",
                108),

            ("struct_variable_int_self_addition_works",
                "int",
                "let x = 8i; test_variable.a = test_variable.a + x;",
                "test_variable.a",
                108),

            ("variable_struct_int_addition_works",
                "int",
                "let x = 15i; let out = x + test_variable.a;",
                "out",
                115),

            ("struct_array_variable_int_addition_works",
                "int",
                "let x: int[3] = 8i; let out = test_variable.a + x[1];",
                "out",
                108),

            ("struct_array_variable_int_self_addition_works",
                "int",
                "let x: int[3] = 8i; test_variable.a = test_variable.a + x[1];",
                "test_variable.a",
                108),

            ("array_variable_struct_int_addition_works",
                "int",
                "let x: int[4] = 15i; let out = x[2] + test_variable.a;",
                "out",
                115),

        ############## long ##############

            ("struct_const_long_addition_works",
                "long",
                "let out = test_variable.a + 2l;",
                "out",
                102),

            ("struct_const_long_addition_works_with_large_immediate_works",
                "long",
                "let out = test_variable.a + 8000000000000l;",
                "out",
                8000000000100),

            ("struct_const_long_self_addition_works",
                "long",
                "test_variable.a = test_variable.a + 2l;",
                "test_variable.a",
                102),

            ("const_struct_long_addition_works",
                "long",
                "let out = 5l + test_variable.a;",
                "out",
                105),

            ("struct_variable_long_addition_works",
                "long",
                "let x = 8l; let out = test_variable.a + x;",
                "out",
                108),

            ("struct_variable_long_self_addition_works",
                "long",
                "let x = 8l; test_variable.a = test_variable.a + x;",
                "test_variable.a",
                108),

            ("variable_struct_long_addition_works",
                "long",
                "let x = 15l; let out = x + test_variable.a;",
                "out",
                115),

            ("struct_array_variable_long_addition_works",
                "long",
                "let x: long[3] = 8l; let out = test_variable.a + x[1];",
                "out",
                108),

            ("struct_array_variable_long_self_addition_works",
                "long",
                "let x: long[3] = 8l; test_variable.a = test_variable.a + x[1];",
                "test_variable.a",
                108),

            ("array_variable_struct_long_addition_works",
                "long",
                "let x: long[4] = 15l; let out = x[2] + test_variable.a;",
                "out",
                115),
















    # subtraction
        ############## byte ##############

            ("struct_const_byte_subtraction_works",
                "byte",
                "let out = test_variable.a - 2b;",
                "out",
                98),

            ("struct_const_byte_self_subtraction_works",
                "byte",
                "test_variable.a = test_variable.a - 2b;",
                "test_variable.a",
                98),

            ("const_struct_byte_subtraction_works",
                "byte",
                "let out = 5b - test_variable.a;",
                "out",
                -95),

            ("struct_variable_byte_subtraction_works",
                "byte",
                "let x = 8b; let out = test_variable.a - x;",
                "out",
                92),

            ("struct_variable_byte_self_subtraction_works",
                "byte",
                "let x = 8b; test_variable.a = test_variable.a - x;",
                "test_variable.a",
                92),

            ("variable_struct_byte_subtraction_works",
                "byte",
                "let x = 15b; let out = x - test_variable.a;",
                "out",
                -85),

            ("struct_array_variable_byte_subtraction_works",
                "byte",
                "let x: byte[3] = 8b; let out = test_variable.a - x[1];",
                "out",
                92),

            ("struct_array_variable_byte_self_subtraction_works",
                "byte",
                "let x: byte[3] = 8b; test_variable.a = test_variable.a - x[1];",
                "test_variable.a",
                92),

            ("array_variable_struct_byte_subtraction_works",
                "byte",
                "let x: byte[4] = 15b; let out = x[2] - test_variable.a;",
                "out",
                -85),


        ############## short ##############

            ("struct_const_short_subtraction_works",
                "short",
                "let out = test_variable.a - 2s;",
                "out",
                98),


            ("struct_const_short_self_subtraction_works",
                "short",
                "test_variable.a = test_variable.a - 2s;",
                "test_variable.a",
                98),

            ("const_struct_short_subtraction_works",
                "short",
                "let out = 5s - test_variable.a;",
                "out",
                -95),

            ("struct_variable_short_subtraction_works",
                "short",
                "let x = 8s; let out = test_variable.a - x;",
                "out",
                92),

            ("struct_variable_short_self_subtraction_works",
                "short",
                "let x = 8s; test_variable.a = test_variable.a - x;",
                "test_variable.a",
                92),

            ("variable_struct_short_subtraction_works",
                "short",
                "let x = 15s; let out = x - test_variable.a;",
                "out",
                -85),

            ("struct_array_variable_short_subtraction_works",
                "short",
                "let x: short[3] = 8s; let out = test_variable.a - x[1];",
                "out",
                92),

            ("struct_array_variable_short_self_subtraction_works",
                "short",
                "let x: short[3] = 8s; test_variable.a = test_variable.a - x[1];",
                "test_variable.a",
                92),

            ("array_variable_struct_short_subtraction_works",
                "short",
                "let x: short[4] = 15s; let out = x[2] - test_variable.a;",
                "out",
                -85),

        ############## int ##############

            ("struct_const_int_subtraction_works",
                "int",
                "let out = test_variable.a - 2i;",
                "out",
                98),

            ("struct_const_int_self_subtraction_works",
                "int",
                "test_variable.a = test_variable.a - 2i;",
                "test_variable.a",
                98),

            ("const_struct_int_subtraction_works",
                "int",
                "let out = 5i - test_variable.a;",
                "out",
                -95),

            ("struct_variable_int_subtraction_works",
                "int",
                "let x = 8i; let out = test_variable.a - x;",
                "out",
                92),

            ("struct_variable_int_self_subtraction_works",
                "int",
                "let x = 8i; test_variable.a = test_variable.a - x;",
                "test_variable.a",
                92),


            ("variable_struct_int_subtraction_works",
                "int",
                "let x = 15i; let out = x - test_variable.a;",
                "out",
                -85),

            ("struct_array_variable_int_subtraction_works",
                "int",
                "let x: int[3] = 8i; let out = test_variable.a - x[1];",
                "out",
                92),

            ("struct_array_variable_int_self_subtraction_works",
                "int",
                "let x: int[3] = 8i; test_variable.a = test_variable.a - x[1];",
                "test_variable.a",
                92),

            ("array_variable_struct_int_subtraction_works",
                "int",
                "let x: int[4] = 15i; let out = x[2] - test_variable.a;",
                "out",
                -85),

        ############## long ##############

            ("struct_const_long_subtraction_works",
                "long",
                "let out = test_variable.a - 2l;",
                "out",
                98),

            ("struct_const_long_self_subtraction_works",
                "long",
                "test_variable.a = test_variable.a - 2l;",
                "test_variable.a",
                98),


            ("struct_const_long_subtraction_works_with_large_immediate",
                "long",
                "let out = test_variable.a - 2000000000000l;",
                "out",
                -1999999999900),


            ("const_struct_long_subtraction_works",
                "long",
                "let out = 5l - test_variable.a;",
                "out",
                -95),

            ("struct_variable_long_subtraction_works",
                "long",
                "let x = 8l; let out = test_variable.a - x;",
                "out",
                92),

            ("struct_variable_long_self_subtraction_works",
                "long",
                "let x = 8l; test_variable.a = test_variable.a - x;",
                "test_variable.a",
                92),

            ("variable_struct_long_subtraction_works",
                "long",
                "let x = 15l; let out = x - test_variable.a;",
                "out",
                -85),

            ("struct_array_variable_long_subtraction_works",
                "long",
                "let x: long[3] = 8l; let out = test_variable.a - x[1];",
                "out",
                92),
            
            ("struct_array_variable_long_self_subtraction_works",
                "long",
                "let x: long[3] = 8l; test_variable.a = test_variable.a - x[1];",
                "test_variable.a",
                92),

            ("array_variable_struct_long_subtraction_works",
                "long",
                "let x: long[4] = 15l; let out = x[2] - test_variable.a;",
                "out",
                -85),
















    # multiplication
        ############## byte ##############

            ("struct_const_byte_multiplication_works",
                "byte",
                "let out = test_variable.a*2b;",
                "out",
                -56), # overflow

            ("struct_const_byte_self_multiplication_works",
                "byte",
                "test_variable.a = test_variable.a*2b;",
                "test_variable.a",
                -56),

            ("const_struct_byte_multiplication_works",
                "byte",
                "let out = 2b*test_variable.a;",
                "out",
                -56),

            ("struct_variable_byte_multiplication_works",
                "byte",
                "let x = 8b; let out = test_variable.a*x;",
                "out",
                32),

            ("struct_variable_byte_self_multiplication_works",
                "byte",
                "let x = 8b; test_variable.a = test_variable.a*x;",
                "test_variable.a",
                32),

            ("variable_struct_byte_multiplication_works",
                "byte",
                "let x = 8b; let out = x*test_variable.a;",
                "out",
                32),

            ("struct_array_variable_byte_multiplication_works",
                "byte",
                "let x: byte[3] = 8b; let out = test_variable.a*x[1];",
                "out",
                32),

            ("struct_array_variable_byte_self_multiplication_works",
                "byte",
                "let x: byte[3] = 8b; test_variable.a = test_variable.a*x[1];",
                "test_variable.a",
                32),

            ("array_variable_struct_byte_multiplication_works",
                "byte",
                "let x: byte[4] = 8b; let out = x[2]*test_variable.a;",
                "out",
                32),


        ############## short ##############

            ("struct_const_short_multiplication_works",
                "short",
                "let out = test_variable.a*2s;",
                "out",
                200),


            ("struct_const_short_self_multiplication_works",
                "short",
                "test_variable.a = test_variable.a*2s;",
                "test_variable.a",
                200),

            ("const_struct_short_multiplication_works",
                "short",
                "let out = 5s*test_variable.a;",
                "out",
                500),

            ("struct_variable_short_multiplication_works",
                "short",
                "let x = 8s; let out = test_variable.a*x;",
                "out",
                800),

            ("struct_variable_short_self_multiplication_works",
                "short",
                "let x = 8s; test_variable.a = test_variable.a*x;",
                "test_variable.a",
                800),

            ("variable_struct_short_multiplication_works",
                "short",
                "let x = 15s; let out = x*test_variable.a;",
                "out",
                1500),

            ("struct_array_variable_short_multiplication_works",
                "short",
                "let x: short[3] = 8s; let out = test_variable.a*x[1];",
                "out",
                800),

            ("struct_array_variable_short_self_multiplication_works",
                "short",
                "let x: short[3] = 8s; test_variable.a = test_variable.a*x[1];",
                "test_variable.a",
                800),

            ("array_variable_struct_short_multiplication_works",
                "short",
                "let x: short[4] = 15s; let out = x[2]*test_variable.a;",
                "out",
                1500),

        ############## int ##############

            ("struct_const_int_multiplication_works",
                "int",
                "let out = test_variable.a*2i;",
                "out",
                200),

            ("struct_const_int_self_multiplication_works",
                "int",
                "test_variable.a = test_variable.a*2i;",
                "test_variable.a",
                200),

            ("const_struct_int_multiplication_works",
                "int",
                "let out = 5i*test_variable.a;",
                "out",
                500),

            ("struct_variable_int_multiplication_works",
                "int",
                "let x = 8i; let out = test_variable.a*x;",
                "out",
                800),

            ("struct_variable_int_self_multiplication_works",
                "int",
                "let x = 8i; test_variable.a = test_variable.a*x;",
                "test_variable.a",
                800),


            ("variable_struct_int_multiplication_works",
                "int",
                "let x = 15i; let out = x*test_variable.a;",
                "out",
                1500),

            ("struct_array_variable_int_multiplication_works",
                "int",
                "let x: int[3] = 8i; let out = test_variable.a*x[1];",
                "out",
                800),

            ("struct_array_variable_int_self_multiplication_works",
                "int",
                "let x: int[3] = 8i; test_variable.a = test_variable.a*x[1];",
                "test_variable.a",
                800),

            ("array_variable_struct_int_multiplication_works",
                "int",
                "let x: int[4] = 15i; let out = x[2]*test_variable.a;",
                "out",
                1500),

        ############## long ##############

            ("struct_const_long_multiplication_works",
                "long",
                "let out = test_variable.a*2l;",
                "out",
                200),

            ("struct_const_long_self_multiplication_works",
                "long",
                "test_variable.a = test_variable.a*2l;",
                "test_variable.a",
                200),


            ("struct_const_long_multiplication_works_with_large_immediate",
                "long",
                "let out = test_variable.a*2000000000000l;",
                "out",
                200000000000000),


            ("const_struct_long_multiplication_works",
                "long",
                "let out = 5l*test_variable.a;",
                "out",
                500),

            ("struct_variable_long_multiplication_works",
                "long",
                "let x = 8l; let out = test_variable.a*x;",
                "out",
                800),

            ("struct_variable_long_self_multiplication_works",
                "long",
                "let x = 8l; test_variable.a = test_variable.a*x;",
                "test_variable.a",
                800),

            ("variable_struct_long_multiplication_works",
                "long",
                "let x = 15l; let out = x*test_variable.a;",
                "out",
                1500),

            ("struct_array_variable_long_multiplication_works",
                "long",
                "let x: long[3] = 8l; let out = test_variable.a*x[1];",
                "out",
                800),
            
            ("struct_array_variable_long_self_multiplication_works",
                "long",
                "let x: long[3] = 8l; test_variable.a = test_variable.a*x[1];",
                "test_variable.a",
                800),

            ("array_variable_struct_long_multiplication_works",
                "long",
                "let x: long[4] = 15l; let out = x[2]*test_variable.a;",
                "out",
                1500),



        # array indexing
        # array refs in expressions

]

for t in test_tuples:

    out = f'''
program: |

    struct Foo {{
        a: {t[1]};
    }}
    
    extern fn {t[1]}_printer(value: {t[1]});

    fn main() : int {{
        let test_variable = Foo {{
            a = 100;
        }};

        {t[2]}
        {t[1]}_printer({t[3]});

        return 0;
    }}
link_with:
    - tests/files/support/support.c
expect_stdout: |
    {t[4]}
'''   
    with open(f"{t[0]}.yaml", "w") as f:
        f.write(out)





