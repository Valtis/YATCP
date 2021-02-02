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
