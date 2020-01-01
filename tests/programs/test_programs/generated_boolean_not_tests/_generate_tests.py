IF_STATEMENT_TAKEN = 101
IF_STATEMENT_NOT_TAKEN = 20

"""
	Tuples of:
		* Test file name without extension
    * Condition, which is assigned
		* If condition
		* Expected answer
"""
condition_answer_tuples = [

  ("boolean_not_should_work_with_literal_true",
    "!true",
    "c",
    IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_should_work_with_literal_false",
      "!false",
      "c",
      IF_STATEMENT_TAKEN),

  ("boolean_not_should_work_with_variable_that_is_true",
      "!true_var",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_should_work_with_variable_that_is_false",
    "!false_var",
    "c",
    IF_STATEMENT_TAKEN),

	("boolean_not_should_work_with_function_that_returns_true",
		"!return_true()",
		"c",
		IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_should_work_with_function_that_returns_false",
      "!return_false()",
      "c",
      IF_STATEMENT_TAKEN),

  ("boolean_not_should_work_with_parenthesis_when_inner_expression_is_false",
      "!(a > 4)",
      "c",
      IF_STATEMENT_TAKEN),

  ("boolean_not_should_work_with_parenthesis_when_inner_expression_is_true",
      "!(a == 4)",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_should_work_as_part_of_expression_containing_boolean_or_that_evaluates_to_false",
      "!true_var || false_var",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_should_work_as_part_of_expression_containing_boolean_or_that_evaluates_to_true_due_to_another_operand",
      "!true_var || true_var",
      "c",
      IF_STATEMENT_TAKEN),


  ("boolean_not_should_work_as_part_of_expression_containing_boolean_or_that_evaluates_to_true",
      "!false_var || false",
      "c",
      IF_STATEMENT_TAKEN),


  ("boolean_not_should_work_as_part_of_expression_containing_boolean_and_that_evaluates_to_false",
      "!true_var && true",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_should_work_as_part_of_expression_containing_boolean_and_that_evaluates_to_true",
      "!false_var && true",
      "c",
      IF_STATEMENT_TAKEN),






 ("boolean_not_twice_should_work_with_literal_true",
    "!!true",
    "c",
    IF_STATEMENT_TAKEN),

  ("boolean_not_twice_should_work_with_literal_false",
      "!!false",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_twice_should_work_with_variable_that_is_true",
      "!!true_var",
      "c",
      IF_STATEMENT_TAKEN),

  ("boolean_not_twice_should_work_with_variable_that_is_false",
    "!!false_var",
    "c",
    IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_twice_should_work_with_function_that_returns_true",
    "!!return_true()",
    "c",
    IF_STATEMENT_TAKEN),

  ("boolean_not_twice_should_work_with_function_that_returns_false",
      "!!return_false()",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_twice_should_work_with_parenthesis_when_inner_expression_is_false",
      "!!(a > 4)",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_twice_should_work_with_parenthesis_when_inner_expression_is_true",
      "!!(a == 4)",
      "c",
      IF_STATEMENT_TAKEN),

  ("boolean_not_twice_should_work_as_part_of_expression_containing_boolean_or_that_evaluates_to_false",
      "!!false_var || false",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_twice_should_work_as_part_of_expression_containing_boolean_or_that_evaluates_to_true_due_to_another_operand",
      "!!false_var || true_var",
      "c",
      IF_STATEMENT_TAKEN),


  ("boolean_not_twice_should_work_as_part_of_expression_containing_boolean_or_that_evaluates_to_true",
      "!!true_var || false",
      "c",
      IF_STATEMENT_TAKEN),

  ("boolean_not_twice_should_work_as_part_of_expression_containing_boolean_and_that_evaluates_to_false",
      "!!false && true",
      "c",
      IF_STATEMENT_NOT_TAKEN),

  ("boolean_not_twice_should_work_as_part_of_expression_containing_boolean_and_that_evaluates_to_true",
      "!!true_var && true",
      "c",
      IF_STATEMENT_TAKEN),



]

for cond_tuple in condition_answer_tuples:

	out = f'''program: |

  fn return_true() : bool {{
    return true;
  }}

  fn return_false() : bool {{
    return false;
  }}


  fn test_function() : int {{
      let a: int = 4;
      let true_var: bool = true;
      let false_var: bool = false;
      let c: bool = {cond_tuple[1]};

      if {cond_tuple[2]} {{
      	return {IF_STATEMENT_TAKEN};
      }}
      return {IF_STATEMENT_NOT_TAKEN};
  }}
link_with:
  - tests/files/support/support.c
callable: test_function
returns: int
expect_stdout: |
  {cond_tuple[3]}
'''
	with open(f"{cond_tuple[0]}.yaml", "w") as f:
		f.write(out)




