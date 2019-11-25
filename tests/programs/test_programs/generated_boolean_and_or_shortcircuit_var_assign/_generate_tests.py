IF_STATEMENT_TAKEN = 101
IF_STATEMENT_NOT_TAKEN = 20

"""
	Tuples of:
		* Test file name without extension
        * First function return value
		* Second function return value
		* Expected stdout
"""
condition_answer_pairs = [


	("both_and_conditions_should_be_evaluated",
		"true",
    "true",
		f"External C function call: 123\nExternal C function call: 987\n{IF_STATEMENT_TAKEN}"),
  ("should_only_evaluate_first_argument_if_it_returns_false",
    "false",
    "true",
    f"External C function call: 123\n{IF_STATEMENT_NOT_TAKEN}"),

]


for pair in condition_answer_pairs:

  expect_stdout = pair[3].replace("\n", "\n  ")
  out = f'''program: |
  extern fn c_printer(integer: int) : void;

  fn first() : bool {{
    c_printer(123);
    return {pair[1]};
  }}

  fn second() : bool {{
    c_printer(987);
    return {pair[2]};
  }}

  fn test_function() : int {{
      let cond: bool = first() && second();
      if cond {{
      	return {IF_STATEMENT_TAKEN};
      }}
      return {IF_STATEMENT_NOT_TAKEN};
  }}
link_with:
  - tests/files/support/support.c
callable: test_function
returns: int
expect_stdout: |
  {expect_stdout}
'''
  with open(f"{pair[0]}.yaml", "w") as f:
    f.write(out)




