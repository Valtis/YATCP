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
		"let condition: bool = 9S > 27S;",
		"condition",
		IF_STATEMENT_NOT_TAKEN),
	("if_statement_with_variable_expression_true_is_taken",
		"let condition: bool = 96S <= 120S;",
		"condition",
		IF_STATEMENT_TAKEN),


	# constant CMP constant
	("const_const_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"",
		"1S < 2S",
		 IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"",
		"2S < 2S",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"",
		"3S < 2S",
		IF_STATEMENT_NOT_TAKEN),


	("const_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"",
		"1S <= 2S",
		IF_STATEMENT_TAKEN),
	("const_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal", "",
		"2S <= 2S",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"",
		"3S <= 2S",
		IF_STATEMENT_NOT_TAKEN),


	("const_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"",
		"1S == 2S",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"",
		"2S == 2S",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"",
		"3S == 2S",
		IF_STATEMENT_NOT_TAKEN),



	("const_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"",
		"3S != 2S",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"",
		"2S != 2S",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"",
		"3S != 2S",
		IF_STATEMENT_TAKEN),


	("const_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"",
		"1S >= 2S",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"",
		"2S >= 2S",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"",
		"3S >= 2S",
		IF_STATEMENT_TAKEN),


	("const_const_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"",
		"1S > 2S",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"",
		"2S > 2S",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"",
		"3S > 2S",
		IF_STATEMENT_TAKEN),










	# variable CMP constant
	("variable_const_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 1S;",
		"value < 2S",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"value  < 2S",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value = 3S; ",
		"value < 2S",
		 IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value = 1S;",
		"value  <= 2S",
		IF_STATEMENT_TAKEN),
	("variable_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"value  <= 2S",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value = 3S;",
		"value  <= 2S",
		IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value = 1S;",
		"value  == 2S",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"value  == 2S",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value = 3S;",
		"value  == 2S",
		IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value = 1S;",
		"value  != 2S",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"value  != 2S",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value = 3S;",
		"value  != 2S",
		IF_STATEMENT_TAKEN),


	("variable_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value = 1S;",
		"value  >= 2S",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"value  >= 2S",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value = 3S;",
		"value  >= 2S",
		IF_STATEMENT_TAKEN),


	("variable_const_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value = 1S;",
		"value  > 2S",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"value  > 2S",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value = 3S;",
		"value  > 2S",
		IF_STATEMENT_TAKEN),









	# constant CMP variable
	("const_variable_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 2S;",
		"1S < value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"2S < value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value = 2S;",
		"3S < value",
		IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value = 2S;",
		"1S  <= value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"2S  <= value", IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value = 2S;",
		 "3S  <= value", IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value = 2S;",
		"1S  == value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"2S  == value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value = 2S;",
		"3S  == value",
		IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value = 2S;",
		"1S  != value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"2S  != value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value = 2S;",
		"3S  != value",
		IF_STATEMENT_TAKEN),


	("const_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value = 2S;",
		"1S  >= value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"2S  >= value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value = 2S;",
		"3S >= value",
		IF_STATEMENT_TAKEN),

	("const_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value = 2S;",
		 "1S  > value",
		 IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value = 2S;",
		"2S  > value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value = 2S;",
		"3S  > value",
		IF_STATEMENT_TAKEN),










	# variable CMP variable
	("variable_variable_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 1S; let rhs = 2S;",
		"value  < rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value = 2S; let rhs = 2S;",
		"value  < rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value < rhs",
		 IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value = 1S; let rhs = 2S;",
		"value  <= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value = 2S; let rhs = 2S;",
		"value  <= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value  <= rhs",
		IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value = 1S; let rhs = 2S;",
		"value  == rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value = 2S; let rhs = 2S;",
		"value  == rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value  == rhs",
		IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value = 1S; let rhs = 2S;",
		"value  != rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value = 2S; let rhs = 2S;",
		"value  != rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value  != rhs",
		IF_STATEMENT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value = 1S; let rhs = 2S;",
		"value  >= rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value = 2S; let rhs = 2S;",
		"value  >= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value  >= rhs",
		IF_STATEMENT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value = 1S; let rhs = 2S;",
		"value  > rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value = 2S; let rhs = 2S;",
		"value  > rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value  > rhs",
		IF_STATEMENT_TAKEN),

	# expressions
	("constant_constant_expressions_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"",
		"2S + 3S > 2S*3S+1S ",
		IF_STATEMENT_NOT_TAKEN),
	("variable_constant_expressions_should_take_if_statement_when_both_side_are_equal",
		"let value = 2S; let rhs = 6S;",
		"value + rhs == 4S*2S ",
		IF_STATEMENT_TAKEN),
	("constant_variable_expressions_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value = 3S; let rhs = 2S;",
		"10S/3S < value*rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_expressions_should_not_take_if_statement_with_less_than_or_equal__comparison_when_lhs_is_greater",
		"let value = 3S; let rhs = 2S;",
		"value*5S  <= rhs/2S",
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




