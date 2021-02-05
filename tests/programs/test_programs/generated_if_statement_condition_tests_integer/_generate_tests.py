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


	# boolean true/false constants
	("if_statement_with_constant_false_is_not_taken",
		"",
		"false",
		IF_STATEMENT_NOT_TAKEN),
	("if_statement_with_constant_true_is_taken",
		"",
		"true",
		IF_STATEMENT_TAKEN),







	# boolean true/false through variable
	("if_statement_with_variable_false_is_not_taken",
		"let condition: bool = false;",
		"condition",
		IF_STATEMENT_NOT_TAKEN),
	("if_statement_with_variable_true_is_taken",
		"let condition: bool = true;",
		"condition",
		IF_STATEMENT_TAKEN),


	# boolean boolean expression assigned to variable
	("if_statement_with_variable_expression_false_is_not_taken",
		"let condition: bool = 9 > 27;",
		"condition",
		IF_STATEMENT_NOT_TAKEN),
	("if_statement_with_variable_expression_true_is_taken",
		"let condition: bool = 96 <= 623;",
		"condition",
		IF_STATEMENT_TAKEN),



	# constant CMP constant
	("const_const_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"",
		"1 < 2",
		 IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"",
		"2 < 2",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"",
		"3 < 2",
		IF_STATEMENT_NOT_TAKEN),


	("const_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"",
		"1 <= 2",
		IF_STATEMENT_TAKEN),
	("const_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal", "",
		"2 <= 2",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"",
		"3 <= 2",
		IF_STATEMENT_NOT_TAKEN),


	("const_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"",
		"1 == 2",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"",
		"2 == 2",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"",
		"3 == 2",
		IF_STATEMENT_NOT_TAKEN),



	("const_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"",
		"3 != 2",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"",
		"2 != 2",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"",
		"3 != 2",
		IF_STATEMENT_TAKEN),


	("const_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"",
		"1 >= 2",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"",
		"2 >= 2",
		IF_STATEMENT_TAKEN),
	("const_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"",
		"3 >= 2",
		IF_STATEMENT_TAKEN),


	("const_const_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"",
		"1 > 2",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"",
		"2 > 2",
		IF_STATEMENT_NOT_TAKEN),
	("const_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"",
		"3 > 2",
		IF_STATEMENT_TAKEN),










	# variable CMP constant
	("variable_const_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value: int = 1;",
		"value  < 2",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"value  < 2",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value: int = 3; ",
		"value < 2",
		 IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value: int = 1;",
		"value  <= 2",
		IF_STATEMENT_TAKEN),
	("variable_const_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"value  <= 2",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value: int = 3;",
		"value  <= 2",
		IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value: int = 1;",
		"value  == 2",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"value  == 2",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value: int = 3;",
		"value  == 2",
		IF_STATEMENT_NOT_TAKEN),


	("variable_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value: int = 1;",
		"value  != 2",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"value  != 2",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value: int = 3;",
		"value  != 2",
		IF_STATEMENT_TAKEN),


	("variable_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value: int = 1;",
		"value  >= 2",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"value  >= 2",
		IF_STATEMENT_TAKEN),
	("variable_const_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value: int = 3;",
		"value  >= 2",
		IF_STATEMENT_TAKEN),


	("variable_const_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value: int = 1;",
		"value  > 2",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"value  > 2",
		IF_STATEMENT_NOT_TAKEN),
	("variable_const_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value: int = 3;",
		"value  > 2",
		IF_STATEMENT_TAKEN),









	# constant CMP variable
	("const_variable_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value: int = 2;",
		"1 < value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"2 < value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value: int = 2;",
		"3 < value",
		IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value: int = 2;",
		"1  <= value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"2  <= value", IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value: int = 2;",
		 "3  <= value", IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value: int = 2;",
		"1  == value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"2  == value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value: int = 2;",
		"3  == value",
		IF_STATEMENT_NOT_TAKEN),


	("const_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value: int = 2;",
		"1  != value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"2  != value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value: int = 2;",
		"3  != value",
		IF_STATEMENT_TAKEN),


	("const_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value: int = 2;",
		"1  >= value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"2  >= value",
		IF_STATEMENT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value: int = 2;",
		"3  >= value",
		IF_STATEMENT_TAKEN),

	("const_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value: int = 2;",
		 "1  > value",
		 IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value: int = 2;",
		"2  > value",
		IF_STATEMENT_NOT_TAKEN),
	("const_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value: int = 2;",
		"3  > value",
		IF_STATEMENT_TAKEN),










	# variable CMP variable
	("variable_variable_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value: int = 1; let rhs: int = 2;",
		"value  < rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_equal",
		"let value: int = 2; let rhs: int = 2;",
		"value  < rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value < rhs",
		 IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_less",
		"let value: int = 1; let rhs: int = 2;",
		"value  <= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_equal",
		"let value: int = 2; let rhs: int = 2;",
		"value  <= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_less_than_or_eq_comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value  <= rhs",
		IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_less",
		"let value: int = 1; let rhs: int = 2;",
		"value  == rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_eq_comparison_when_lhs_is_equal",
		"let value: int = 2; let rhs: int = 2;",
		"value  == rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_eq_comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value  == rhs",
		IF_STATEMENT_NOT_TAKEN),


	("variable_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_less",
		"let value: int = 1; let rhs: int = 2;",
		"value  != rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_not_eq_comparison_when_lhs_is_equal",
		"let value: int = 2; let rhs: int = 2;",
		"value  != rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_not_eq_comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value  != rhs",
		IF_STATEMENT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_less",
		"let value: int = 1; let rhs: int = 2;",
		"value  >= rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_equal",
		"let value: int = 2; let rhs: int = 2;",
		"value  >= rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_should_not_take_if_statement_with_greater_or_eq_comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value  >= rhs",
		IF_STATEMENT_TAKEN),


	("variable_variable_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"let value: int = 1; let rhs: int = 2;",
		"value  > rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_equal",
		"let value: int = 2; let rhs: int = 2;",
		"value  > rhs",
		IF_STATEMENT_NOT_TAKEN),
	("variable_variable_should_take_if_statement_with_greater_than_comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value  > rhs",
		IF_STATEMENT_TAKEN),

	# expressions
	("constant_constant_expressions_should_not_take_if_statement_with_greater_than_comparison_when_lhs_is_less",
		"",
		"2 + 3 > 2*3+1 ",
		IF_STATEMENT_NOT_TAKEN),
	("variable_constant_expressions_should_take_if_statement_when_both_side_are_equal",
		"let value: int = 2; let rhs: int = 6;",
		"value + rhs == 4*2 ",
		IF_STATEMENT_TAKEN),
	("constant_variable_expressions_should_take_if_statement_with_less_than_comparison_when_lhs_is_less",
		"let value: int = 3; let rhs: int = 2;",
		"10/3 < value*rhs",
		IF_STATEMENT_TAKEN),
	("variable_variable_expressions_should_not_take_if_statement_with_less_than_or_equal__comparison_when_lhs_is_greater",
		"let value: int = 3; let rhs: int = 2;",
		"value*5  <= rhs/2",
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




