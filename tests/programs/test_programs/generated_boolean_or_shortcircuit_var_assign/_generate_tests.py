IF_STATEMENT_TAKEN = 101
IF_STATEMENT_NOT_TAKEN = 20

"""
	Tuples of:
		* Test file name without extension
        * First function return value
		* Second function return value
		* Expected stdotu
"""
condition_answer_pairs = [


	("all_or_expressions_should_be_evaluated_if_all_return_false",
		"false",
    "false",
    "false",
		f"External C function call: 123\nExternal C function call: 987\nExternal C function call: 443\n{IF_STATEMENT_NOT_TAKEN}"),

  ("should_only_evaluate_first_expression_if_it_returns_true",
    "true",
    "false",
    "true",
    f"External C function call: 123\n{IF_STATEMENT_TAKEN}"),

   ("should_evaluate_first_two_expressions_second_returns_true",
    "false",
    "true",
    "true",
    f"External C function call: 123\nExternal C function call: 987\n{IF_STATEMENT_TAKEN}"),

   ("should_evaluate_all_expressions_if_last_returns_true",
    "false",
    "false",
    "true",
    f"External C function call: 123\nExternal C function call: 987\nExternal C function call: 443\n{IF_STATEMENT_TAKEN}"),
]


for pair in condition_answer_pairs:

  expect_stdout = pair[4].replace("\n", "\n  ")
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

  fn third() : bool {{
    c_printer(443);
    return {pair[3]};
  }}

  fn test_function() : int {{
      let cond: bool = first() || second() || third();
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




