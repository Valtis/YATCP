IF_STATEMENT_TAKEN = 101
IF_STATEMENT_NOT_TAKEN = 20

"""
	Tuples of:
		* Test file name without extension
    * Condition, which is assigned
		* If condition
		* Expected answer
"""
condition_answer_pairs = [

  # and, using boolean literals

  ("and_should_be_true_if_both_conditions_true_using_literals",
    "true && true",
    "c",
    IF_STATEMENT_TAKEN),

  ("and_should_be_false_if_first_condition_is_false_and_second_condition_true_using_literals",
      "false && true",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("and_should_be_false_if_first_condition_is_true_and_second_condition_false_using_literals",
      "true && false",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("and_should_be_false_if_both_conditions_false_using_literals",
    "false && false",
    "c",
    IF_STATEMENT_NOT_TAKEN),
  # and, using comparisons

	("and_should_be_true_if_both_conditions_true",
		"a > 3 && b == 6",
		"c",
		IF_STATEMENT_TAKEN),

  ("and_should_be_false_if_first_condition_is_false_and_second_condition_true",
      "a > 4 && b == 6",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("and_should_be_false_if_first_condition_is_true_and_second_condition_false",
      "a > 3 && b == 7",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("and_should_be_false_if_both_conditions_false",
      "a > 4 && a < 2",
      "c",
      IF_STATEMENT_NOT_TAKEN),
]

for pair in condition_answer_pairs:

	out = f'''program: |
  fn test_function() : int {{
      let a: int = 4;
      let b: int = 6;
      let c: bool = {pair[1]};

      if {pair[2]} {{
      	return {IF_STATEMENT_TAKEN};
      }}
      return {IF_STATEMENT_NOT_TAKEN};
  }}
link_with:
  - tests/files/support/support.c
callable: test_function
returns: int
expect_stdout: |
  {pair[3]}
'''
	with open(f"{pair[0]}.yaml", "w") as f:
		f.write(out)




