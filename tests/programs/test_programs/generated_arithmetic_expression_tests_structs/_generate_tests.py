

"""
    Tuples of:
        * Test file name without extension
        * Initialization expression(s) (e.g. variable declarations)
        * Expression for return statement
        * Expected answer
        """
test_tuples = [


# negation 
    ############## byte ##############

        ("struct_const_byte_negation_works",
            "byte",
            "let out = -test_variable.a;",
            "out",
            -100),

        ("struct_const_byte_self_negation_works",
            "byte",
            "test_variable.a = -test_variable.a;",
            "test_variable.a",
            -100),

    ############## short ##############

        ("struct_const_short_negation_works",
            "short",
            "let out = -test_variable.a;",
            "out",
            -100),

        ("struct_const_short_self_negation_works",
            "short",
            "test_variable.a = -test_variable.a;",
            "test_variable.a",
            -100),


    ############## int ##############

        ("struct_const_int_negation_works",
            "int",
            "let out = -test_variable.a;",
            "out",
            -100),

        ("struct_const_int_self_negation_works",
            "int",
            "test_variable.a = -test_variable.a;",
            "test_variable.a",
            -100),


    ############## long ##############

        ("struct_const_long_negation_works",
            "long",
            "let out = -test_variable.a;",
            "out",
            -100),

        ("struct_const_long_self_negation_works",
            "long",
            "test_variable.a = -test_variable.a;",
            "test_variable.a",
            -100),



























 







   # not 
        ############## byte ##############

            ("struct_const_byte_not_works",
                "byte",
                "let out = ~test_variable.a;",
                "out",
                -101),

            ("struct_const_byte_self_not_works",
                "byte",
                "test_variable.a = ~test_variable.a;",
                "test_variable.a",
                -101),

        ############## short ##############

            ("struct_const_short_not_works",
                "short",
                "let out = ~test_variable.a;",
                "out",
                -101),

            ("struct_const_short_self_not_works",
                "short",
                "test_variable.a = ~test_variable.a;",
                "test_variable.a",
                -101),


        ############## int ##############

            ("struct_const_int_not_works",
                "int",
                "let out = ~test_variable.a;",
                "out",
                -101),

            ("struct_const_int_self_not_works",
                "int",
                "test_variable.a = ~test_variable.a;",
                "test_variable.a",
                -101),


        ############## long ##############

            ("struct_const_long_not_works",
                "long",
                "let out = ~test_variable.a;",
                "out",
                -101),

            ("struct_const_long_self_not_works",
                "long",
                "test_variable.a = ~test_variable.a;",
                "test_variable.a",
                -101),







































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

            ("struct_struct_byte_addition_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a + test_variable.a;",
                "out",
                108),

            ("struct_struct_byte_self_addition_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a + test_variable.a;",
                "test_variable.a",
                108),


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

            ("struct_struct_short_addition_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a + test_variable.a;",
                "out",
                108),

            ("struct_struct_short_self_addition_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a + test_variable.a;",
                "test_variable.a",
                108),


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

            ("struct_struct_int_addition_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a + test_variable.a;",
                "out",
                108),

            ("struct_struct_int_self_addition_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a + test_variable.a;",
                "test_variable.a",
                108),


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

            ("struct_struct_long_addition_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a + test_variable.a;",
                "out",
                108),

            ("struct_struct_long_self_addition_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a + test_variable.a;",
                "test_variable.a",
                108),

























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

            ("struct_struct_byte_subtraction_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a - test_variable.a;",
                "out",
                -92),

            ("struct_struct_byte_self_subtraction_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a - test_variable.a;",
                "test_variable.a",
                -92),



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

            ("struct_struct_short_subtraction_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a - test_variable.a;",
                "out",
                -92),

            ("struct_struct_short_self_subtraction_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a - test_variable.a;",
                "test_variable.a",
                -92),


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

            ("struct_struct_int_subtraction_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a - test_variable.a;",
                "out",
                -92),

            ("struct_struct_int_self_subtraction_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a - test_variable.a;",
                "test_variable.a",
                -92),


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

            ("struct_struct_long_subtraction_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a - test_variable.a;",
                "out",
                -92),

            ("struct_struct_long_self_subtraction_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a - test_variable.a;",
                "test_variable.a",
                -92),

























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

            ("struct_struct_byte_multiplication_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a * test_variable.a;",
                "out",
                32),

            ("struct_struct_byte_self_multiplication_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a * test_variable.a;",
                "test_variable.a",
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


            ("struct_struct_short_multiplication_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a * test_variable.a;",
                "out",
                800),

            ("struct_struct_short_self_multiplication_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a * test_variable.a;",
                "test_variable.a",
                800),



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

            ("struct_struct_int_multiplication_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a * test_variable.a;",
                "out",
                800),

            ("struct_struct_int_self_multiplication_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a * test_variable.a;",
                "test_variable.a",
                800),


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

            ("struct_struct_long_multiplication_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a * test_variable.a;",
                "out",
                800),

            ("struct_struct_long_self_multiplication_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a * test_variable.a;",
                "test_variable.a",
                800),





























    # division
        ############## byte ##############

            ("struct_const_byte_division_works",
                "byte",
                "let out = test_variable.a/2b;",
                "out",
                50), 

            ("struct_const_byte_self_division_works",
                "byte",
                "test_variable.a = test_variable.a/2b;",
                "test_variable.a",
                50),

            ("const_struct_byte_division_works",
                "byte",
                "let out = 2b/test_variable.a;",
                "out",
                0),

            ("struct_variable_byte_division_works",
                "byte",
                "let x = 8b; let out = test_variable.a/x;",
                "out",
                12),

            ("struct_variable_byte_self_division_works",
                "byte",
                "let x = 8b; test_variable.a = test_variable.a/x;",
                "test_variable.a",
                12),

            ("variable_struct_byte_division_works",
                "byte",
                "let x = 8b; let out = x/test_variable.a;",
                "out",
                0),

            ("struct_array_variable_byte_division_works",
                "byte",
                "let x: byte[3] = 8b; let out = test_variable.a/x[1];",
                "out",
                12),

            ("struct_array_variable_byte_self_division_works",
                "byte",
                "let x: byte[3] = 8b; test_variable.a = test_variable.a/x[1];",
                "test_variable.a",
                12),

            ("array_variable_struct_byte_division_works",
                "byte",
                "let x: byte[4] = 8b; let out = x[2]/test_variable.a;",
                "out",
                0),

            ("struct_struct_byte_division_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = test_variable.a/x.a;",
                "out",
                12),

            ("struct_struct_byte_self_division_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a / test_variable.a;",
                "test_variable.a",
                0),



        ############## short ##############

            ("struct_const_short_division_works",
                "short",
                "let out = test_variable.a/2s;",
                "out",
                50),


            ("struct_const_short_self_division_works",
                "short",
                "test_variable.a = test_variable.a/2s;",
                "test_variable.a",
                50),

            ("const_struct_short_division_works",
                "short",
                "let out = 500s/test_variable.a;",
                "out",
                5),

            ("struct_variable_short_division_works",
                "short",
                "let x = 8s; let out = test_variable.a/x;",
                "out",
                12),

            ("struct_variable_short_self_division_works",
                "short",
                "let x = 8s; test_variable.a = test_variable.a/x;",
                "test_variable.a",
                12),

            ("variable_struct_short_division_works",
                "short",
                "let x = 1500s; let out = x/test_variable.a;",
                "out",
                15),

            ("struct_array_variable_short_division_works",
                "short",
                "let x: short[3] = 8s; let out = test_variable.a/x[1];",
                "out",
                12),

            ("struct_array_variable_short_self_division_works",
                "short",
                "let x: short[3] = 8s; test_variable.a = test_variable.a/x[1];",
                "test_variable.a",
                12),

            ("array_variable_struct_short_division_works",
                "short",
                "let x: short[4] = 1500s; let out = x[2]/test_variable.a;",
                "out",
                15),


            ("struct_struct_short_division_works",
                "short",
                "let x = Foo { a = 800;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; let out = x.a / test_variable.a;",
                "out",
                8),

            ("struct_struct_short_self_division_works",
                "short",
                "let x = Foo { a = 800;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; test_variable.a = x.a / test_variable.a;",
                "test_variable.a",
                8),


        ############## int ##############

            ("struct_const_int_division_works",
                "int",
                "let out = test_variable.a/2i;",
                "out",
                50),

            ("struct_const_int_self_division_works",
                "int",
                "test_variable.a = test_variable.a/2i;",
                "test_variable.a",
                50),

            ("const_struct_int_division_works",
                "int",
                "let out = 500i/test_variable.a;",
                "out",
                5),

            ("struct_variable_int_division_works",
                "int",
                "let x = 8i; let out = test_variable.a/x;",
                "out",
                12),

            ("struct_variable_int_self_division_works",
                "int",
                "let x = 8i; test_variable.a = test_variable.a/x;",
                "test_variable.a",
                12),


            ("variable_struct_int_division_works",
                "int",
                "let x = 1500i; let out = x/test_variable.a;",
                "out",
                15),

            ("struct_array_variable_int_division_works",
                "int",
                "let x: int[3] = 8i; let out = test_variable.a/x[1];",
                "out",
                12),

            ("struct_array_variable_int_self_division_works",
                "int",
                "let x: int[3] = 8i; test_variable.a = test_variable.a/x[1];",
                "test_variable.a",
                12),

            ("array_variable_struct_int_division_works",
                "int",
                "let x: int[4] = 1500i; let out = x[2]/test_variable.a;",
                "out",
                15),

            ("struct_struct_int_division_works",
                "int",
                "let x = Foo { a = 800;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; let out = x.a / test_variable.a;",
                "out",
                8),

            ("struct_struct_int_self_division_works",
                "int",
                "let x = Foo { a = 800;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; test_variable.a = x.a / test_variable.a;",
                "test_variable.a",
                8),


        ############## long ##############

            ("struct_const_long_division_works",
                "long",
                "let out = test_variable.a/2l;",
                "out",
                50),

            ("struct_const_long_self_division_works",
                "long",
                "test_variable.a = test_variable.a/2l;",
                "test_variable.a",
                50),


            ("struct_const_long_division_works_with_large_immediate",
                "long",
                "test_variable.a = 6000000000000l; let out = test_variable.a/2000000000000l;",
                "out",
                3),


            ("const_struct_long_division_works",
                "long",
                "let out = 500l/test_variable.a;",
                "out",
                5),

            ("struct_variable_long_division_works",
                "long",
                "let x = 8l; let out = test_variable.a/x;",
                "out",
                12),

            ("struct_variable_long_self_division_works",
                "long",
                "let x = 8l; test_variable.a = test_variable.a/x;",
                "test_variable.a",
                12),

            ("variable_struct_long_division_works",
                "long",
                "let x = 1500l; let out = x/test_variable.a;",
                "out",
                15),

            ("struct_array_variable_long_division_works",
                "long",
                "let x: long[3] = 8l; let out = test_variable.a/x[1];",
                "out",
                12),
            
            ("struct_array_variable_long_self_division_works",
                "long",
                "let x: long[3] = 8l; test_variable.a = test_variable.a/x[1];",
                "test_variable.a",
                12),

            ("array_variable_struct_long_division_works",
                "long",
                "let x: long[4] = 1500l; let out = x[2]/test_variable.a;",
                "out",
                15),

            ("struct_struct_long_division_works",
                "long",
                "let x = Foo { a = 800;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; let out = x.a / test_variable.a;",
                "out",
                8),

            ("struct_struct_long_self_division_works",
                "long",
                "let x = Foo { a = 800;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; test_variable.a = x.a / test_variable.a;",
                "test_variable.a",
                8),





























    # modulo
        ############## byte ##############

            ("struct_const_byte_modulo_works",
                "byte",
                "let out = test_variable.a%2b;",
                "out",
                0), 

            ("struct_const_byte_self_modulo_works",
                "byte",
                "test_variable.a = test_variable.a%3b;",
                "test_variable.a",
                1),

            ("const_struct_byte_modulo_works",
                "byte",
                "let out = 2b%test_variable.a;",
                "out",
                2),

            ("struct_variable_byte_modulo_works",
                "byte",
                "let x = 8b; let out = test_variable.a%x;",
                "out",
                4),

            ("struct_variable_byte_self_modulo_works",
                "byte",
                "let x = 8b; test_variable.a = test_variable.a%x;",
                "test_variable.a",
                4),

            ("variable_struct_byte_modulo_works",
                "byte",
                "let x = 8b; let out = x%test_variable.a;",
                "out",
                8),

            ("struct_array_variable_byte_modulo_works",
                "byte",
                "let x: byte[3] = 8b; let out = test_variable.a%x[1];",
                "out",
                4),

            ("struct_array_variable_byte_self_modulo_works",
                "byte",
                "let x: byte[3] = 8b; test_variable.a = test_variable.a%x[1];",
                "test_variable.a",
                4),

            ("array_variable_struct_byte_modulo_works",
                "byte",
                "let x: byte[4] = 8b; let out = x[2]%test_variable.a;",
                "out",
                8),

            ("struct_struct_byte_modulo_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = test_variable.a%x.a;",
                "out",
                4),

            ("struct_struct_byte_self_modulo_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a % test_variable.a;",
                "test_variable.a",
                8),



        ############## short ##############

            ("struct_const_short_modulo_works",
                "short",
                "let out = test_variable.a%2s;",
                "out",
                0),


            ("struct_const_short_self_modulo_works",
                "short",
                "test_variable.a = test_variable.a%3s;",
                "test_variable.a",
                1),

            ("const_struct_short_modulo_works",
                "short",
                "let out = 500s%test_variable.a;",
                "out",
                0),

            ("struct_variable_short_modulo_works",
                "short",
                "let x = 8s; let out = test_variable.a%x;",
                "out",
                4),

            ("struct_variable_short_self_modulo_works",
                "short",
                "let x = 8s; test_variable.a = test_variable.a%x;",
                "test_variable.a",
                4),

            ("variable_struct_short_modulo_works",
                "short",
                "let x = 1500s; let out = x%test_variable.a;",
                "out",
                0),

            ("struct_array_variable_short_modulo_works",
                "short",
                "let x: short[3] = 8s; let out = test_variable.a%x[1];",
                "out",
                4),

            ("struct_array_variable_short_self_modulo_works",
                "short",
                "let x: short[3] = 8s; test_variable.a = test_variable.a%x[1];",
                "test_variable.a",
                4),

            ("array_variable_struct_short_modulo_works",
                "short",
                "let x: short[4] = 1500s; let out = x[2]%test_variable.a;",
                "out",
                0),


            ("struct_struct_short_modulo_works",
                "short",
                "let x = Foo { a = 8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; let out = x.a % test_variable.a;",
                "out",
                8),

            ("struct_struct_short_self_modulo_works",
                "short",
                "let x = Foo { a = 8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; test_variable.a = x.a % test_variable.a;",
                "test_variable.a",
                8),


        ############## int ##############

            ("struct_const_int_modulo_works",
                "int",
                "let out = test_variable.a%2i;",
                "out",
                0),

            ("struct_const_int_self_modulo_works",
                "int",
                "test_variable.a = test_variable.a%3i;",
                "test_variable.a",
                1),

            ("const_struct_int_modulo_works",
                "int",
                "let out = 500i%test_variable.a;",
                "out",
                0),

            ("struct_variable_int_modulo_works",
                "int",
                "let x = 8i; let out = test_variable.a%x;",
                "out",
                4),

            ("struct_variable_int_self_modulo_works",
                "int",
                "let x = 8i; test_variable.a = test_variable.a%x;",
                "test_variable.a",
                4),


            ("variable_struct_int_modulo_works",
                "int",
                "let x = 1500i; let out = x%test_variable.a;",
                "out",
                0),

            ("struct_array_variable_int_modulo_works",
                "int",
                "let x: int[3] = 8i; let out = test_variable.a%x[1];",
                "out",
                4),

            ("struct_array_variable_int_self_modulo_works",
                "int",
                "let x: int[3] = 8i; test_variable.a = test_variable.a%x[1];",
                "test_variable.a",
                4),

            ("array_variable_struct_int_modulo_works",
                "int",
                "let x: int[4] = 1500i; let out = x[2]%test_variable.a;",
                "out",
                0),

            ("struct_struct_int_modulo_works",
                "int",
                "let x = Foo { a = 8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; let out = x.a % test_variable.a;",
                "out",
                8),

            ("struct_struct_int_self_modulo_works",
                "int",
                "let x = Foo { a = 8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; test_variable.a = x.a % test_variable.a;",
                "test_variable.a",
                8),


        ############## long ##############

            ("struct_const_long_modulo_works",
                "long",
                "let out = test_variable.a%2l;",
                "out",
                0),

            ("struct_const_long_self_modulo_works",
                "long",
                "test_variable.a = test_variable.a%3l;",
                "test_variable.a",
                1),


            ("struct_const_long_modulo_works_with_large_immediate",
                "long",
                "test_variable.a = 6000000000000l; let out = test_variable.a%2000000000000l;",
                "out",
                0),


            ("const_struct_long_modulo_works",
                "long",
                "let out = 500l%test_variable.a;",
                "out",
                0),

            ("struct_variable_long_modulo_works",
                "long",
                "let x = 8l; let out = test_variable.a%x;",
                "out",
                4),

            ("struct_variable_long_self_modulo_works",
                "long",
                "let x = 8l; test_variable.a = test_variable.a%x;",
                "test_variable.a",
                4),

            ("variable_struct_long_modulo_works",
                "long",
                "let x = 1500l; let out = x%test_variable.a;",
                "out",
                0),

            ("struct_array_variable_long_modulo_works",
                "long",
                "let x: long[3] = 8l; let out = test_variable.a%x[1];",
                "out",
                4),
            
            ("struct_array_variable_long_self_modulo_works",
                "long",
                "let x: long[3] = 8l; test_variable.a = test_variable.a%x[1];",
                "test_variable.a",
                4),

            ("array_variable_struct_long_modulo_works",
                "long",
                "let x: long[4] = 1500l; let out = x[2]%test_variable.a;",
                "out",
                0),

            ("struct_struct_long_modulo_works",
                "long",
                "let x = Foo { a = 8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; let out = x.a % test_variable.a;",
                "out",
                8),

            ("struct_struct_long_self_modulo_works",
                "long",
                "let x = Foo { a = 8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0; }; test_variable.a = x.a % test_variable.a;",
                "test_variable.a",
                8),
































    # bitwise_and
        ############## byte ##############

            ("struct_const_byte_bitwise_and_works",
                "byte",
                "let out = test_variable.a & 2b;",
                "out",
                0),

            ("struct_const_byte_self_bitwise_and_works",
                "byte",
                "test_variable.a = test_variable.a & 4b;",
                "test_variable.a",
                4),

            ("const_struct_byte_bitwise_and_works",
                "byte",
                "let out = 5b & test_variable.a;",
                "out",
                4),

            ("struct_variable_byte_bitwise_and_works",
                "byte",
                "let x = 8b; let out = test_variable.a & x;",
                "out",
                0),

            ("struct_variable_byte_self_bitwise_and_works",
                "byte",
                "let x = 32b; test_variable.a = test_variable.a & x;",
                "test_variable.a",
                32),

            ("variable_struct_byte_bitwise_and_works",
                "byte",
                "let x = 97b; let out = x & test_variable.a;",
                "out",
                96),

            ("struct_array_variable_byte_bitwise_and_works",
                "byte",
                "let x: byte[3] = 96b; let out = test_variable.a & x[1];",
                "out",
                96),

            ("struct_array_variable_byte_self_bitwise_and_works",
                "byte",
                "let x: byte[3] = 98b; test_variable.a = test_variable.a & x[1];",
                "test_variable.a",
                96),

            ("array_variable_struct_byte_bitwise_and_works",
                "byte",
                "let x: byte[4] = 15b; let out = x[2] & test_variable.a;",
                "out",
                4),

            ("struct_struct_byte_bitwise_and_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a & test_variable.a;",
                "out",
                0),

            ("struct_struct_byte_self_bitwise_and_works",
                "byte",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a & test_variable.a;",
                "test_variable.a",
                32),


        ############## short ##############

            ("struct_const_short_bitwise_and_works",
                "short",
                "let out = test_variable.a & 2s;",
                "out",
                0),

            ("struct_const_short_self_bitwise_and_works",
                "short",
                "test_variable.a = test_variable.a & 4s;",
                "test_variable.a",
                4),

            ("const_struct_short_bitwise_and_works",
                "short",
                "let out = 5s & test_variable.a;",
                "out",
                4),

            ("struct_variable_short_bitwise_and_works",
                "short",
                "let x = 8s; let out = test_variable.a & x;",
                "out",
                0),

            ("struct_variable_short_self_bitwise_and_works",
                "short",
                "let x = 32s; test_variable.a = test_variable.a & x;",
                "test_variable.a",
                32),

            ("variable_struct_short_bitwise_and_works",
                "short",
                "let x = 97s; let out = x & test_variable.a;",
                "out",
                96),

            ("struct_array_variable_short_bitwise_and_works",
                "short",
                "let x: short[3] = 96s; let out = test_variable.a & x[1];",
                "out",
                96),


            ("struct_array_variable_short_self_bitwise_and_works",
                "short",
                "let x: short[3] = 98s; test_variable.a = test_variable.a & x[1];",
                "test_variable.a",
                96),

            ("array_variable_struct_short_bitwise_and_works",
                "short",
                "let x: short[4] = 15s; let out = x[2] & test_variable.a;",
                "out",
                4),

            ("struct_struct_short_bitwise_and_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a & test_variable.a;",
                "out",
                0),

            ("struct_struct_short_self_bitwise_and_works",
                "short",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a & test_variable.a;",
                "test_variable.a",
                32),


        ############## int ##############

            ("struct_const_int_bitwise_and_works",
                "int",
                "let out = test_variable.a & 2i;",
                "out",
                0),

            ("struct_const_int_self_bitwise_and_works",
                "int",
                "test_variable.a = test_variable.a & 4i;",
                "test_variable.a",
                4),

            ("const_struct_int_bitwise_and_works",
                "int",
                "let out = 5i & test_variable.a;",
                "out",
                4),

            ("struct_variable_int_bitwise_and_works",
                "int",
                "let x = 8i; let out = test_variable.a & x;",
                "out",
                0),

            ("struct_variable_int_self_bitwise_and_works",
                "int",
                "let x = 32i; test_variable.a = test_variable.a & x;",
                "test_variable.a",
                32),

            ("variable_struct_int_bitwise_and_works",
                "int",
                "let x = 97i; let out = x & test_variable.a;",
                "out",
                96),

            ("struct_array_variable_int_bitwise_and_works",
                "int",
                "let x: int[3] = 96i; let out = test_variable.a & x[1];",
                "out",
                96),

            ("struct_array_variable_int_self_bitwise_and_works",
                "int",
                "let x: int[3] = 98i; test_variable.a = test_variable.a & x[1];",
                "test_variable.a",
                96),

            ("array_variable_struct_int_bitwise_and_works",
                "int",
                "let x: int[4] = 15i; let out = x[2] & test_variable.a;",
                "out",
                4),

            ("struct_struct_int_bitwise_and_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a & test_variable.a;",
                "out",
                0),

            ("struct_struct_int_self_bitwise_and_works",
                "int",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a & test_variable.a;",
                "test_variable.a",
                32),


        ############## long ##############

            ("struct_const_long_bitwise_and_works",
                "long",
                "let out = test_variable.a & 2l;",
                "out",
                0),

            ("struct_const_long_bitwise_and_works_with_large_immediate_works",
                "long",
                "let out = test_variable.a & 8000000000004l;",
                "out",
                4),

            ("struct_const_long_self_bitwise_and_works",
                "long",
                "test_variable.a = test_variable.a & 4l;",
                "test_variable.a",
                4),

            ("const_struct_long_bitwise_and_works",
                "long",
                "let out = 5l & test_variable.a;",
                "out",
                4),

            ("struct_variable_long_bitwise_and_works",
                "long",
                "let x = 8l; let out = test_variable.a & x;",
                "out",
                0),

            ("struct_variable_long_self_bitwise_and_works",
                "long",
                "let x = 32l; test_variable.a = test_variable.a & x;",
                "test_variable.a",
                32),

            ("variable_struct_long_bitwise_and_works",
                "long",
                "let x = 97l; let out = x & test_variable.a;",
                "out",
                96),

            ("struct_array_variable_long_bitwise_and_works",
                "long",
                "let x: long[3] = 96l; let out = test_variable.a & x[1];",
                "out",
                96),

            ("struct_array_variable_long_self_bitwise_and_works",
                "long",
                "let x: long[3] = 98l; test_variable.a = test_variable.a & x[1];",
                "test_variable.a",
                96),

            ("array_variable_struct_long_bitwise_and_works",
                "long",
                "let x: long[4] = 15l; let out = x[2] & test_variable.a;",
                "out",
                4),

            ("struct_struct_long_bitwise_and_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a & test_variable.a;",
                "out",
                0),

            ("struct_struct_long_self_bitwise_and_works",
                "long",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a & test_variable.a;",
                "test_variable.a",
                32),




































    # bitwise_or
        ############## byte ##############

            ("struct_const_byte_bitwise_or_works",
                "byte",
                "let out = test_variable.a | 2b;",
                "out",
                102),

            ("struct_const_byte_self_bitwise_or_works",
                "byte",
                "test_variable.a = test_variable.a | 4b;",
                "test_variable.a",
                100),

            ("const_struct_byte_bitwise_or_works",
                "byte",
                "let out = 5b | test_variable.a;",
                "out",
                101),

            ("struct_variable_byte_bitwise_or_works",
                "byte",
                "let x = 8b; let out = test_variable.a | x;",
                "out",
                108),

            ("struct_variable_byte_self_bitwise_or_works",
                "byte",
                "let x = 32b; test_variable.a = test_variable.a | x;",
                "test_variable.a",
                100),

            ("variable_struct_byte_bitwise_or_works",
                "byte",
                "let x = 97b; let out = x | test_variable.a;",
                "out",
                101),

            ("struct_array_variable_byte_bitwise_or_works",
                "byte",
                "let x: byte[3] = 96b; let out = test_variable.a | x[1];",
                "out",
                100),

            ("struct_array_variable_byte_self_bitwise_or_works",
                "byte",
                "let x: byte[3] = 98b; test_variable.a = test_variable.a | x[1];",
                "test_variable.a",
                102),

            ("array_variable_struct_byte_bitwise_or_works",
                "byte",
                "let x: byte[4] = 15b; let out = x[2] | test_variable.a;",
                "out",
                111),

            ("struct_struct_byte_bitwise_or_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a | test_variable.a;",
                "out",
                108),

            ("struct_struct_byte_self_bitwise_or_works",
                "byte",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a | test_variable.a;",
                "test_variable.a",
                101),


        ############## short ##############

            ("struct_const_short_bitwise_or_works",
                "short",
                "let out = test_variable.a | 2s;",
                "out",
                102),

            ("struct_const_short_self_bitwise_or_works",
                "short",
                "test_variable.a = test_variable.a | 4s;",
                "test_variable.a",
                100),

            ("const_struct_short_bitwise_or_works",
                "short",
                "let out = 5s | test_variable.a;",
                "out",
                101),

            ("struct_variable_short_bitwise_or_works",
                "short",
                "let x = 8s; let out = test_variable.a | x;",
                "out",
                108),

            ("struct_variable_short_self_bitwise_or_works",
                "short",
                "let x = 32s; test_variable.a = test_variable.a | x;",
                "test_variable.a",
                100),

            ("variable_struct_short_bitwise_or_works",
                "short",
                "let x = 97s; let out = x | test_variable.a;",
                "out",
                101),

            ("struct_array_variable_short_bitwise_or_works",
                "short",
                "let x: short[3] = 96s; let out = test_variable.a | x[1];",
                "out",
                100),


            ("struct_array_variable_short_self_bitwise_or_works",
                "short",
                "let x: short[3] = 98s; test_variable.a = test_variable.a | x[1];",
                "test_variable.a",
                102),

            ("array_variable_struct_short_bitwise_or_works",
                "short",
                "let x: short[4] = 15s; let out = x[2] | test_variable.a;",
                "out",
                111),

            ("struct_struct_short_bitwise_or_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a | test_variable.a;",
                "out",
                108),

            ("struct_struct_short_self_bitwise_or_works",
                "short",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a | test_variable.a;",
                "test_variable.a",
                101),


        ############## int ##############

            ("struct_const_int_bitwise_or_works",
                "int",
                "let out = test_variable.a | 2i;",
                "out",
                102),

            ("struct_const_int_self_bitwise_or_works",
                "int",
                "test_variable.a = test_variable.a | 4i;",
                "test_variable.a",
                100),

            ("const_struct_int_bitwise_or_works",
                "int",
                "let out = 5i | test_variable.a;",
                "out",
                101),

            ("struct_variable_int_bitwise_or_works",
                "int",
                "let x = 8i; let out = test_variable.a | x;",
                "out",
                108),

            ("struct_variable_int_self_bitwise_or_works",
                "int",
                "let x = 32i; test_variable.a = test_variable.a | x;",
                "test_variable.a",
                100),

            ("variable_struct_int_bitwise_or_works",
                "int",
                "let x = 97i; let out = x | test_variable.a;",
                "out",
                101),

            ("struct_array_variable_int_bitwise_or_works",
                "int",
                "let x: int[3] = 96i; let out = test_variable.a | x[1];",
                "out",
                100),

            ("struct_array_variable_int_self_bitwise_or_works",
                "int",
                "let x: int[3] = 98i; test_variable.a = test_variable.a | x[1];",
                "test_variable.a",
                102),

            ("array_variable_struct_int_bitwise_or_works",
                "int",
                "let x: int[4] = 15i; let out = x[2] | test_variable.a;",
                "out",
                111),

            ("struct_struct_int_bitwise_or_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a | test_variable.a;",
                "out",
                108),

            ("struct_struct_int_self_bitwise_or_works",
                "int",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a | test_variable.a;",
                "test_variable.a",
                101),


        ############## long ##############

            ("struct_const_long_bitwise_or_works",
                "long",
                "let out = test_variable.a | 2l;",
                "out",
                102),

            ("struct_const_long_bitwise_or_works_with_large_immediate_works",
                "long",
                "let out = test_variable.a | 8000000000004l;",
                "out",
                8000000000100),

            ("struct_const_long_self_bitwise_or_works",
                "long",
                "test_variable.a = test_variable.a | 4l;",
                "test_variable.a",
                100),

            ("const_struct_long_bitwise_or_works",
                "long",
                "let out = 5l | test_variable.a;",
                "out",
                101),

            ("struct_variable_long_bitwise_or_works",
                "long",
                "let x = 8l; let out = test_variable.a | x;",
                "out",
                108),

            ("struct_variable_long_self_bitwise_or_works",
                "long",
                "let x = 32l; test_variable.a = test_variable.a | x;",
                "test_variable.a",
                100),

            ("variable_struct_long_bitwise_or_works",
                "long",
                "let x = 97l; let out = x | test_variable.a;",
                "out",
                101),

            ("struct_array_variable_long_bitwise_or_works",
                "long",
                "let x: long[3] = 96l; let out = test_variable.a | x[1];",
                "out",
                100),

            ("struct_array_variable_long_self_bitwise_or_works",
                "long",
                "let x: long[3] = 98l; test_variable.a = test_variable.a | x[1];",
                "test_variable.a",
                102),

            ("array_variable_struct_long_bitwise_or_works",
                "long",
                "let x: long[4] = 15l; let out = x[2] | test_variable.a;",
                "out",
                111),

            ("struct_struct_long_bitwise_or_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a | test_variable.a;",
                "out",
                108),

            ("struct_struct_long_self_bitwise_or_works",
                "long",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a | test_variable.a;",
                "test_variable.a",
                101),


































    # bitwise_xor
        ############## byte ##############

            ("struct_const_byte_bitwise_xor_works",
                "byte",
                "let out = test_variable.a ^ 2b;",
                "out",
                102),

            ("struct_const_byte_self_bitwise_xor_works",
                "byte",
                "test_variable.a = test_variable.a ^ 4b;",
                "test_variable.a",
                96),

            ("const_struct_byte_bitwise_xor_works",
                "byte",
                "let out = 5b ^ test_variable.a;",
                "out",
                97),

            ("struct_variable_byte_bitwise_xor_works",
                "byte",
                "let x = 8b; let out = test_variable.a ^ x;",
                "out",
                108),

            ("struct_variable_byte_self_bitwise_xor_works",
                "byte",
                "let x = 32b; test_variable.a = test_variable.a ^ x;",
                "test_variable.a",
                68),

            ("variable_struct_byte_bitwise_xor_works",
                "byte",
                "let x = 97b; let out = x ^ test_variable.a;",
                "out",
                5),

            ("struct_array_variable_byte_bitwise_xor_works",
                "byte",
                "let x: byte[3] = 96b; let out = test_variable.a ^ x[1];",
                "out",
                4),

            ("struct_array_variable_byte_self_bitwise_xor_works",
                "byte",
                "let x: byte[3] = 98b; test_variable.a = test_variable.a ^ x[1];",
                "test_variable.a",
                6),

            ("array_variable_struct_byte_bitwise_xor_works",
                "byte",
                "let x: byte[4] = 15b; let out = x[2] ^ test_variable.a;",
                "out",
                107),

            ("struct_struct_byte_bitwise_xor_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a ^ test_variable.a;",
                "out",
                108),

            ("struct_struct_byte_self_bitwise_xor_works",
                "byte",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a ^ test_variable.a;",
                "test_variable.a",
                69),


        ############## short ##############

            ("struct_const_short_bitwise_xor_works",
                "short",
                "let out = test_variable.a ^ 2s;",
                "out",
                102),

            ("struct_const_short_self_bitwise_xor_works",
                "short",
                "test_variable.a = test_variable.a ^ 4s;",
                "test_variable.a",
                96),

            ("const_struct_short_bitwise_xor_works",
                "short",
                "let out = 5s ^ test_variable.a;",
                "out",
                97),

            ("struct_variable_short_bitwise_xor_works",
                "short",
                "let x = 8s; let out = test_variable.a ^ x;",
                "out",
                108),

            ("struct_variable_short_self_bitwise_xor_works",
                "short",
                "let x = 32s; test_variable.a = test_variable.a ^ x;",
                "test_variable.a",
                68),

            ("variable_struct_short_bitwise_xor_works",
                "short",
                "let x = 97s; let out = x ^ test_variable.a;",
                "out",
                5),

            ("struct_array_variable_short_bitwise_xor_works",
                "short",
                "let x: short[3] = 96s; let out = test_variable.a ^ x[1];",
                "out",
                4),


            ("struct_array_variable_short_self_bitwise_xor_works",
                "short",
                "let x: short[3] = 98s; test_variable.a = test_variable.a ^ x[1];",
                "test_variable.a",
                6),

            ("array_variable_struct_short_bitwise_xor_works",
                "short",
                "let x: short[4] = 15s; let out = x[2] ^ test_variable.a;",
                "out",
                107),

            ("struct_struct_short_bitwise_xor_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a ^ test_variable.a;",
                "out",
                108),

            ("struct_struct_short_self_bitwise_xor_works",
                "short",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a ^ test_variable.a;",
                "test_variable.a",
                69),


        ############## int ##############

            ("struct_const_int_bitwise_xor_works",
                "int",
                "let out = test_variable.a ^ 2i;",
                "out",
                102),

            ("struct_const_int_self_bitwise_xor_works",
                "int",
                "test_variable.a = test_variable.a ^ 4i;",
                "test_variable.a",
                96),

            ("const_struct_int_bitwise_xor_works",
                "int",
                "let out = 5i ^ test_variable.a;",
                "out",
                97),

            ("struct_variable_int_bitwise_xor_works",
                "int",
                "let x = 8i; let out = test_variable.a ^ x;",
                "out",
                108),

            ("struct_variable_int_self_bitwise_xor_works",
                "int",
                "let x = 32i; test_variable.a = test_variable.a ^ x;",
                "test_variable.a",
                68),

            ("variable_struct_int_bitwise_xor_works",
                "int",
                "let x = 97i; let out = x ^ test_variable.a;",
                "out",
                5),

            ("struct_array_variable_int_bitwise_xor_works",
                "int",
                "let x: int[3] = 96i; let out = test_variable.a ^ x[1];",
                "out",
                4),

            ("struct_array_variable_int_self_bitwise_xor_works",
                "int",
                "let x: int[3] = 98i; test_variable.a = test_variable.a ^ x[1];",
                "test_variable.a",
                6),

            ("array_variable_struct_int_bitwise_xor_works",
                "int",
                "let x: int[4] = 15i; let out = x[2] ^ test_variable.a;",
                "out",
                107),

            ("struct_struct_int_bitwise_xor_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a ^ test_variable.a;",
                "out",
                108),

            ("struct_struct_int_self_bitwise_xor_works",
                "int",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a ^ test_variable.a;",
                "test_variable.a",
                69),


        ############## long ##############

            ("struct_const_long_bitwise_xor_works",
                "long",
                "let out = test_variable.a ^ 2l;",
                "out",
                102),

            ("struct_const_long_bitwise_xor_works_with_large_immediate_works",
                "long",
                "let out = test_variable.a ^ 8000000000004l;",
                "out",
                8000000000096),

            ("struct_const_long_self_bitwise_xor_works",
                "long",
                "test_variable.a = test_variable.a ^ 4l;",
                "test_variable.a",
                96),

            ("const_struct_long_bitwise_xor_works",
                "long",
                "let out = 5l ^ test_variable.a;",
                "out",
                97),

            ("struct_variable_long_bitwise_xor_works",
                "long",
                "let x = 8l; let out = test_variable.a ^ x;",
                "out",
                108),

            ("struct_variable_long_self_bitwise_xor_works",
                "long",
                "let x = 32l; test_variable.a = test_variable.a ^ x;",
                "test_variable.a",
                68),

            ("variable_struct_long_bitwise_xor_works",
                "long",
                "let x = 97l; let out = x ^ test_variable.a;",
                "out",
                5),

            ("struct_array_variable_long_bitwise_xor_works",
                "long",
                "let x: long[3] = 96l; let out = test_variable.a ^ x[1];",
                "out",
                4),

            ("struct_array_variable_long_self_bitwise_xor_works",
                "long",
                "let x: long[3] = 98l; test_variable.a = test_variable.a ^ x[1];",
                "test_variable.a",
                6),

            ("array_variable_struct_long_bitwise_xor_works",
                "long",
                "let x: long[4] = 15l; let out = x[2] ^ test_variable.a;",
                "out",
                107),

            ("struct_struct_long_bitwise_xor_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = x.a ^ test_variable.a;",
                "out",
                108),

            ("struct_struct_long_self_bitwise_xor_works",
                "long",
                "let x = Foo {a=33;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; test_variable.a = x.a ^ test_variable.a;",
                "test_variable.a",
                69),













































    # comparison, less than
        ############## byte ##############

            ("struct_const_byte_less_than_comparison_works",
                "byte",
                "let out = 0b; if test_variable.a < 2b { out = 4; }",
                "out",
                0),


            ("const_struct_byte_less_than_comparison_works",
                "byte",
                "let out = 0b; if 5b < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_variable_byte_less_than_comparison_works",
                "byte",
                "let x = 8b; let out = 0b; if test_variable.a < x { out = 4; }",
                "out",
                0),

            ("variable_struct_byte_less_than_comparison_works",
                "byte",
                "let x = 8b; let out = 0b; if x < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_array_variable_byte_less_than_comparison_works",
                "byte",
                "let x: byte[3] = 96b; let out = 0b; if test_variable.a < x[2] { out = 4; }",
                "out",
                0),

            ("array_variable_struct_byte_less_than_comparison_works",
                "byte",
                "let x: byte[4] = 15b; let out = 0b; if x[3] < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_struct_byte_less_than_comparison_works",
                "byte",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = 0b; if x.a < test_variable.a { out = 4; }",
                "out",
                4),

        ############## short ##############

            ("struct_const_short_less_than_comparison_works",
                "short",
                "let out = 0s; if test_variable.a < 2 { out = 4; }",
                "out",
                0),


            ("const_struct_short_less_than_comparison_works",
                "short",
                "let out = 0s; if 5 < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_variable_short_less_than_comparison_works",
                "short",
                "let x = 8s; let out = 0s; if test_variable.a < x { out = 4; }",
                "out",
                0),

            ("variable_struct_short_less_than_comparison_works",
                "short",
                "let x = 8s; let out = 0s; if x < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_array_variable_short_less_than_comparison_works",
                "short",
                "let x: short[3] = 96s; let out = 0s; if test_variable.a < x[2] { out = 4; }",
                "out",
                0),

            ("array_variable_struct_short_less_than_comparison_works",
                "short",
                "let x: short[4] = 15s; let out = 0s; if x[3] < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_struct_short_less_than_comparison_works",
                "short",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = 0s; if x.a < test_variable.a { out = 4; }",
                "out",
                4),

        ############## int ##############

            ("struct_const_int_less_than_comparison_works",
                "int",
                "let out = 0i; if test_variable.a < 2 { out = 4; }",
                "out",
                0),


            ("const_struct_int_less_than_comparison_works",
                "int",
                "let out = 0i; if 5 < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_variable_int_less_than_comparison_works",
                "int",
                "let x = 8i; let out = 0i; if test_variable.a < x { out = 4; }",
                "out",
                0),

            ("variable_struct_int_less_than_comparison_works",
                "int",
                "let x = 8i; let out = 0i; if x < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_array_variable_int_less_than_comparison_works",
                "int",
                "let x: int[3] = 96i; let out = 0i; if test_variable.a < x[2] { out = 4; }",
                "out",
                0),

            ("array_variable_struct_int_less_than_comparison_works",
                "int",
                "let x: int[4] = 15i; let out = 0i; if x[3] < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_struct_int_less_than_comparison_works",
                "int",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = 0i; if x.a < test_variable.a { out = 4; }",
                "out",
                4),

        ############## long ##############

            ("struct_const_long_less_than_comparison_works",
                "long",
                "let out = 0l; if test_variable.a < 2 { out = 4; }",
                "out",
                0),


            ("const_struct_long_less_than_comparison_works",
                "long",
                "let out = 0l; if 5 < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_variable_long_less_than_comparison_works",
                "long",
                "let x = 8l; let out = 0l; if test_variable.a < x { out = 4; }",
                "out",
                0),

            ("variable_struct_long_less_than_comparison_works",
                "long",
                "let x = 8l; let out = 0l; if x < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_array_variable_long_less_than_comparison_works",
                "long",
                "let x: long[3] = 96l; let out = 0l; if test_variable.a < x[2] { out = 4; }",
                "out",
                0),

            ("array_variable_struct_long_less_than_comparison_works",
                "long",
                "let x: long[4] = 15l; let out = 0l; if x[3] < test_variable.a { out = 4; }",
                "out",
                4),

            ("struct_struct_long_less_than_comparison_works",
                "long",
                "let x = Foo {a=8;unused1=0;unused2=0;unused3=0;bool_field=false;unused5=0;}; let out = 0l; if x.a < test_variable.a { out = 4; }",
                "out",
                4),














































  # boolean fields
        ############## boolean ##############

            ("struct_bool_field_can_be_used_in_if_statement_and_not_taken_if_false",
                "int", # unused in this test - don't care
                "let out = 4; if test_variable.bool_field { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_used_in_if_statement_and_taken_if_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if test_variable.bool_field { out = 8; }",
                "out",
                8),

           ("struct_bool_field_can_be_used_in_if_statement_and_taken_if_false_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; if !test_variable.bool_field { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_used_in_if_statement_and_not_taken_if_true_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if !test_variable.bool_field { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_and_branch_not_taken_when_false",
                "int", # unused in this test - don't care
                "let out = 4; let cond = test_variable.bool_field; if cond { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_and_branch_taken_if_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = test_variable.bool_field; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_and_branch_taken_when_false_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; let cond = !test_variable.bool_field; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_and_branch_not_taken_if_true_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = !test_variable.bool_field; if cond { out = 8; }",
                "out",
                4),

        ############## boolean and ##############

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_and_operator_and_not_taken_if_false",
                "int", # unused in this test - don't care
                "let out = 4; if test_variable.bool_field && true { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_and_operator_and_taken_if_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if test_variable.bool_field && true { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_and_operator_and_not_taken_if_true_but_other_op_false",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if test_variable.bool_field && false { out = 8; }",
                "out",
                4),

           ("struct_bool_field_can_be_used_in_if_statement_with_boolean_and_operator_and_taken_if_false_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; if !test_variable.bool_field && true { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_and_operator_and_not_taken_if_true_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if !test_variable.bool_field && true { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_and_operator_and_branch_not_taken_when_false",
                "int", # unused in this test - don't care
                "let out = 4; let cond = test_variable.bool_field && true; if cond { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_and_operator_and_branch_taken_if_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = test_variable.bool_field && true; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_and_operator_and_branch_not_taken_if_true_but_other_op_false",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = test_variable.bool_field && false; if cond { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_and_operator_and_branch_taken_when_false_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; let cond = !test_variable.bool_field && true; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_and_operator_and_branch_not_taken_if_true_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = !test_variable.bool_field && true; if cond { out = 8; }",
                "out",
                4),


        ############## boolean or ##############

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_or_operator_and_not_taken_if_false",
                "int", # unused in this test - don't care
                "let out = 4; if test_variable.bool_field || false { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_or_operator_and_taken_if_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if test_variable.bool_field || false { out = 8; }",
                "out",
                8),

           ("struct_bool_field_can_be_used_in_if_statement_with_boolean_or_operator_and_taken_if_other_op_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if test_variable.bool_field || true { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_or_operator_and_not_taken_if_false_and_other_op_false",
                "int", # unused in this test - don't care
                "let out = 4; if test_variable.bool_field || false { out = 8; }",
                "out",
                4),

           ("struct_bool_field_can_be_used_in_if_statement_with_boolean_or_operator_and_taken_if_false_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; if !test_variable.bool_field || false { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_used_in_if_statement_with_boolean_or_operator_and_not_taken_if_true_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; if !test_variable.bool_field || false { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_or_operator_and_branch_not_taken_when_false",
                "int", # unused in this test - don't care
                "let out = 4; let cond = test_variable.bool_field || false; if cond { out = 8; }",
                "out",
                4),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_or_operator_and_branch_taken_if_true",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = test_variable.bool_field || false; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_or_operator_and_branch_taken_if_true_and_other_op_false",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = test_variable.bool_field || false; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_or_operator_and_branch_taken_when_false_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; let cond = !test_variable.bool_field || false; if cond { out = 8; }",
                "out",
                8),

            ("struct_bool_field_can_be_assigned_to_variable_with_boolean_or_operator_and_branch_not_taken_if_true_but_flipped",
                "int", # unused in this test - don't care
                "let out = 4; test_variable.bool_field = true; let cond = !test_variable.bool_field || false; if cond { out = 8; }",
                "out",
                4),


































]

for t in test_tuples:


    out = f'''
program: |

    struct Foo {{
        // add some unused fields to make sure inner indexing works
        unused1: long;
        unused2: byte;
        unused3: int;
        a: {t[1]};
        bool_field: bool;
        unused5: long;
    }}
    
    extern fn {t[1]}_printer(value: {t[1]});

    fn main() : int {{
        let test_variable = Foo {{
            a = 100;
            unused1 = 0;
            unused2 = 0;
            unused3 = 0;
            bool_field = false;
            unused5 = 0;
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





