IF_STATEMENT_TAKEN = 101
IF_STATEMENT_NOT_TAKEN = 20

"""
	Tuples of:
		* Test file name without extension
		* Pre-if statement code (if any)
		* If condition
		* Expected answer
"""
condition_answer_pairs = [

	# boolean boolean expression assigned to variable
	("if_statement_with_variable_expression_false_is_not_taken",
		"let condition: bool = 9b > 27b;",
		"condition",
		IF_STATEMENT_NOT_TAKEN),
	("if_statement_with_variable_expression_true_is_taken",
		"let condition: bool = 96b <= 120b;",
		"condition",
		IF_STATEMENT_TAKEN),


	# constant CMP constant
	("const_const_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"",
		"1b < 2b",
		 IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"",
		"2b < 2b",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"",
		"3b < 2b",
		IF_STATEMENT_NOT_TAKEN),


	("const_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"",
		"1b <= 2b",
		IF_STATEMENT_TAKEN),
	("const_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal", "",
		"2b <= 2b",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"",
		"3b <= 2b",
		IF_STATEMENT_NOT_TAKEN),


	("const_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"",
		"1b == 2b",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"",
		"2b == 2b",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"",
		"3b == 2b",
		IF_STATEMENT_NOT_TAKEN),



	("const_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"",
		"3b != 2b",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"",
		"2b != 2b",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"",
		"3b != 2b",
		IF_STATEMENT_TAKEN),


	("const_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"",
		"1b >= 2b",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"",
		"2b >= 2b",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"",
		"3b >= 2b",
		IF_STATEMENT_TAKEN),


	("const_const_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"",
		"1b > 2b",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"",
		"2b > 2b",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"",
		"3b > 2b",
		IF_STATEMENT_TAKEN),










	# variable CMP constant
	("variable_const_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 1b;",
		"value < 2b",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"value  < 2b",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value = 3b; ",
		"value < 2b",
		 IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value = 1b;",
		"value  <= 2b",
		IF_STATEMENT_TAKEN),
	("variable_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"value  <= 2b",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value = 3b;",
		"value  <= 2b",
		IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value = 1b;",
		"value  == 2b",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"value  == 2b",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value = 3b;",
		"value  == 2b",
		IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value = 1b;",
		"value  != 2b",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"value  != 2b",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value = 3b;",
		"value  != 2b",
		IF_STATEMENT_TAKEN),


	("variable_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value = 1b;",
		"value  >= 2b",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"value  >= 2b",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value = 3b;",
		"value  >= 2b",
		IF_STATEMENT_TAKEN),


	("variable_const_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value = 1b;",
		"value  > 2b",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"value  > 2b",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value = 3b;",
		"value  > 2b",
		IF_STATEMENT_TAKEN),









	# constant CMP variable
	("const_variable_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 2b;",
		"1b < value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"2b < value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value = 2b;",
		"3b < value",
		IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value = 2b;",
		"1b  <= value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"2b  <= value", IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value = 2b;",
		 "3b  <= value", IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value = 2b;",
		"1b  == value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"2b  == value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value = 2b;",
		"3b  == value",
		IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value = 2b;",
		"1b  != value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"2b  != value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value = 2b;",
		"3b  != value",
		IF_STATEMENT_TAKEN),


	("const_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value = 2b;",
		"1b  >= value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"2b  >= value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value = 2b;",
		"3b >= value",
		IF_STATEMENT_TAKEN),

	("const_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value = 2b;",
		 "1b  > value",
		 IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value = 2b;",
		"2b  > value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value = 2b;",
		"3b  > value",
		IF_STATEMENT_TAKEN),










	# variable CMP variable
	("variable_variable_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 1b; let rhs = 2b;",
		"value  < rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value = 2b; let rhs = 2b;",
		"value  < rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value < rhs",
		 IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value = 1b; let rhs = 2b;",
		"value  <= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value = 2b; let rhs = 2b;",
		"value  <= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value  <= rhs",
		IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value = 1b; let rhs = 2b;",
		"value  == rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value = 2b; let rhs = 2b;",
		"value  == rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value  == rhs",
		IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value = 1b; let rhs = 2b;",
		"value  != rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value = 2b; let rhs = 2b;",
		"value  != rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value  != rhs",
		IF_STATEMENT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value = 1b; let rhs = 2b;",
		"value  >= rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value = 2b; let rhs = 2b;",
		"value  >= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value  >= rhs",
		IF_STATEMENT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value = 1b; let rhs = 2b;",
		"value  > rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value = 2b; let rhs = 2b;",
		"value  > rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value  > rhs",
		IF_STATEMENT_TAKEN),

	# expressions
	("constant_constant_expressions_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"",
		"2b + 3b > 2b*3b+1b ",
		IF_STATEMENT_NOT_TAKEN),
	("variable_constant_expressions_should_take_if_statement_when_both_side_are_equal",
		"let value = 2b; let rhs = 6b;",
		"value + rhs == 4b*2b ",
		IF_STATEMENT_TAKEN),
	("constant_variable_expressions_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 3b; let rhs = 2b;",
		"10b/3b < value*rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_expressions_should_not_take_if_statement_with_less_than_or_equal__comparison_when_lhs_is_greater",
		"let value = 3b; let rhs = 2b;",
		"value*5b  <= rhs/2b",
		IF_STATEMENT_NOT_TAKEN),

]

for pair in condition_answer_pairs:

	out = f'''program: |
  fn test_function() : int {{
      {pair[1]}

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




