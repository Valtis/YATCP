LOOP_TAKEN = 101 
LOOP_NOT_TAKEN = 20

"""
	Tuples of:
		* Test file name without extension
		* Pre-loop code (if any)
		* Loop condition
		* Expected answer (20 loop not taken, 101 loop taken)
"""
condition_answer_pairs = [
	

	# boolean true/false constants
	("while_loop_with_constant_false_is_not_taken", 
		"", 
		"false", 
		LOOP_NOT_TAKEN),
	("while_loop_with_constant_true_is_taken",
		"", 
		"true", 
		101),







	# boolean true/false through variable
	("while_loop_with_variable_false_is_not_taken", 
		"let condition: bool = false;", 
		"condition", 
		LOOP_NOT_TAKEN),
	("while_loop_with_variable_true_is_taken", 
		"let condition: bool = true;", 
		"condition", 
		101),








	# constant CMP constant
	("const_const_should_take_while_loop_with_less_than_comparison_when_lhs_is_less", 
		"", 
		"1 < 2",
		 LOOP_TAKEN),
	("const_const_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_equal", 
		"", 
		"2 < 2", 
		LOOP_NOT_TAKEN),
	("const_const_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_greater", 
		"",
		"3 < 2", 
		LOOP_NOT_TAKEN),


	("const_const_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_less", 
		"", 
		"1 <= 2", 
		LOOP_TAKEN),
	("const_const_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_equal", "", 
		"2 <= 2", 
		LOOP_TAKEN),
	("const_const_should_not_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_greater", 
		"", 
		"3 <= 2", 
		LOOP_NOT_TAKEN),


	("const_const_should_not_take_while_loop_with_eq_comparison_when_lhs_is_less", 
		"", 
		"1 == 2", 
		LOOP_NOT_TAKEN),
	("const_const_should_take_while_loop_with_eq_comparison_when_lhs_is_equal", 
		"", 
		"2 == 2", 
		LOOP_TAKEN),
	("const_const_should_not_take_while_loop_with_eq_comparison_when_lhs_is_greater", 
		"", 
		"3 == 2", 
		LOOP_NOT_TAKEN),



	("const_const_should_take_while_loop_with_not_eq_comparison_when_lhs_is_less", 
		"", 
		"3 != 2", 
		LOOP_TAKEN),
	("const_const_should_not_take_while_loop_with_not_eq_comparison_when_lhs_is_equal", 
		"", 
		"2 != 2", 
		LOOP_NOT_TAKEN),
	("const_const_should_take_while_loop_with_not_eq_comparison_when_lhs_is_greater", 
		"", 
		"3 != 2", 
		LOOP_TAKEN),


	("const_const_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_less", 
		"", 
		"1 >= 2", 
		LOOP_NOT_TAKEN),
	("const_const_should_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_equal", 
		"", 
		"2 >= 2", 
		LOOP_TAKEN),
	("const_const_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_greater", 
		"", 
		"3 >= 2", 
		LOOP_TAKEN),


	("const_const_should_not_take_while_loop_with_greater_than_comparison_when_lhs_is_less", 
		"", 
		"1 > 2", 
		LOOP_NOT_TAKEN),
	("const_const_should_take_while_loop_with_greater_than_comparison_when_lhs_is_equal", 
		"", 
		"2 > 2", 
		LOOP_NOT_TAKEN),
	("const_const_should_take_while_loop_with_greater_than_comparison_when_lhs_is_greater", 
		"", 
		"3 > 2", 
		LOOP_TAKEN),










	# variable CMP constant
	("variable_const_should_take_while_loop_with_less_than_comparison_when_lhs_is_less", 
		"let value: int = 1;", 
		"value  < 2", 
		LOOP_TAKEN),
	("variable_const_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"value  < 2", 
		LOOP_NOT_TAKEN),
	("variable_const_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_greater", 
		"let value: int = 3; ",
		"value < 2",
		 LOOP_NOT_TAKEN),


	("variable_const_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_less", 
		"let value: int = 1;", 
		"value  <= 2", 
		LOOP_TAKEN),
	("variable_const_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"value  <= 2", 
		LOOP_TAKEN),
	("variable_const_should_not_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3;", 
		"value  <= 2", 
		LOOP_NOT_TAKEN),


	("variable_const_should_not_take_while_loop_with_eq_comparison_when_lhs_is_less", 
		"let value: int = 1;", 
		"value  == 2", 
		LOOP_NOT_TAKEN),
	("variable_const_should_take_while_loop_with_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"value  == 2", 
		LOOP_TAKEN),
	("variable_const_should_not_take_while_loop_with_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3;", 
		"value  == 2", 
		LOOP_NOT_TAKEN),


	("variable_const_should_take_while_loop_with_not_eq_comparison_when_lhs_is_less", 
		"let value: int = 1;", 
		"value  != 2", 
		LOOP_TAKEN),
	("variable_const_should_not_take_while_loop_with_not_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"value  != 2", 
		LOOP_NOT_TAKEN),
	("variable_const_should_take_while_loop_with_not_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3;", 
		"value  != 2", 
		LOOP_TAKEN),


	("variable_const_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_less", 
		"let value: int = 1;", 
		"value  >= 2", 
		LOOP_NOT_TAKEN),
	("variable_const_should_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"value  >= 2", 
		LOOP_TAKEN),
	("variable_const_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3;", 
		"value  >= 2", 
		LOOP_TAKEN),


	("variable_const_should_not_take_while_loop_with_greater_than_comparison_when_lhs_is_less", 
		"let value: int = 1;", 
		"value  > 2", 
		LOOP_NOT_TAKEN),
	("variable_const_should_take_while_loop_with_greater_than_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"value  > 2", 
		LOOP_NOT_TAKEN),
	("variable_const_should_take_while_loop_with_greater_than_comparison_when_lhs_is_greater", 
		"let value: int = 3;", 
		"value  > 2", 
		LOOP_TAKEN),
	








	# constant CMP variable
	("const_variable_should_take_while_loop_with_less_than_comparison_when_lhs_is_less", 
		"let value: int = 2;", 
		"1 < value", 
		LOOP_TAKEN),
	("const_variable_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"2 < value", 
		LOOP_NOT_TAKEN),
	("const_variable_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_greater", 
		"let value: int = 2;", 
		"3 < value", 
		LOOP_NOT_TAKEN),


	("const_variable_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_less", 
		"let value: int = 2;", 
		"1  <= value", 
		LOOP_TAKEN),
	("const_variable_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"2  <= value", LOOP_TAKEN),
	("const_variable_should_not_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_greater", 
		"let value: int = 2;",
		 "3  <= value", LOOP_NOT_TAKEN),


	("const_variable_should_not_take_while_loop_with_eq_comparison_when_lhs_is_less", 
		"let value: int = 2;", 
		"1  == value", 
		LOOP_NOT_TAKEN),
	("const_variable_should_take_while_loop_with_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"2  == value", 
		LOOP_TAKEN),
	("const_variable_should_not_take_while_loop_with_eq_comparison_when_lhs_is_greater", 
		"let value: int = 2;", 
		"3  == value", 
		LOOP_NOT_TAKEN),


	("const_variable_should_take_while_loop_with_not_eq_comparison_when_lhs_is_less", 
		"let value: int = 2;", 
		"1  != value", 
		LOOP_TAKEN),
	("const_variable_should_not_take_while_loop_with_not_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"2  != value", 
		LOOP_NOT_TAKEN),
	("const_variable_should_take_while_loop_with_not_eq_comparison_when_lhs_is_greater", 
		"let value: int = 2;", 
		"3  != value", 
		LOOP_TAKEN),


	("const_variable_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_less", 
		"let value: int = 2;", 
		"1  >= value", 
		LOOP_NOT_TAKEN),
	("const_variable_should_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2;",
		"2  >= value", 
		LOOP_TAKEN),
	("const_variable_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_greater", 
		"let value: int = 2;", 
		"3  >= value", 
		LOOP_TAKEN),

	("const_variable_should_not_take_while_loop_with_greater_than_comparison_when_lhs_is_less", 
		"let value: int = 2;",
		 "1  > value", 
		 LOOP_NOT_TAKEN),
	("const_variable_should_take_while_loop_with_greater_than_comparison_when_lhs_is_equal", 
		"let value: int = 2;", 
		"2  > value", 
		LOOP_NOT_TAKEN),
	("const_variable_should_take_while_loop_with_greater_than_comparison_when_lhs_is_greater", 
		"let value: int = 2;", 
		"3  > value", 
		LOOP_TAKEN),










	# variable CMP variable
	("variable_variable_should_take_while_loop_with_less_than_comparison_when_lhs_is_less", 
		"let value: int = 1; let rhs: int = 2;", 
		"value  < rhs", 
		LOOP_TAKEN),
	("variable_variable_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_equal", 
		"let value: int = 2; let rhs: int = 2;", 
		"value  < rhs", 
		LOOP_NOT_TAKEN),
	("variable_variable_should_not_take_while_loop_with_less_than_comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;",
		"value < rhs",
		 LOOP_NOT_TAKEN),


	("variable_variable_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_less", 
		"let value: int = 1; let rhs: int = 2;", 
		"value  <= rhs", 
		LOOP_TAKEN),
	("variable_variable_should_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2; let rhs: int = 2;", 
		"value  <= rhs", 
		LOOP_TAKEN),
	("variable_variable_should_not_take_while_loop_with_less_than_or_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;", 
		"value  <= rhs", 
		LOOP_NOT_TAKEN),


	("variable_variable_should_not_take_while_loop_with_eq_comparison_when_lhs_is_less", 
		"let value: int = 1; let rhs: int = 2;", 
		"value  == rhs", 
		LOOP_NOT_TAKEN),
	("variable_variable_should_take_while_loop_with_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2; let rhs: int = 2;", 
		"value  == rhs", 
		LOOP_TAKEN),
	("variable_variable_should_not_take_while_loop_with_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;", 
		"value  == rhs", 
		LOOP_NOT_TAKEN),


	("variable_variable_should_take_while_loop_with_not_eq_comparison_when_lhs_is_less", 
		"let value: int = 1; let rhs: int = 2;", 
		"value  != rhs", 
		LOOP_TAKEN),
	("variable_variable_should_not_take_while_loop_with_not_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2; let rhs: int = 2;", 
		"value  != rhs", 
		LOOP_NOT_TAKEN),
	("variable_variable_should_take_while_loop_with_not_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;", 
		"value  != rhs", 
		LOOP_TAKEN),


	("variable_variable_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_less", 
		"let value: int = 1; let rhs: int = 2;", 
		"value  >= rhs", 
		LOOP_NOT_TAKEN),
	("variable_variable_should_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_equal", 
		"let value: int = 2; let rhs: int = 2;", 
		"value  >= rhs", 
		LOOP_TAKEN),
	("variable_variable_should_not_take_while_loop_with_greater_or_eq_comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;", 
		"value  >= rhs", 
		LOOP_TAKEN),


	("variable_variable_should_not_take_while_loop_with_greater_than_comparison_when_lhs_is_less", 
		"let value: int = 1; let rhs: int = 2;", 
		"value  > rhs", 
		LOOP_NOT_TAKEN),
	("variable_variable_should_take_while_loop_with_greater_than_comparison_when_lhs_is_equal", 
		"let value: int = 2; let rhs: int = 2;", 
		"value  > rhs", 
		LOOP_NOT_TAKEN),
	("variable_variable_should_take_while_loop_with_greater_than_comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;", 
		"value  > rhs", 
		LOOP_TAKEN),
	
	# expressions	
	("constant_constant_expressions_should_not_take_while_loop_with_greater_than_comparison_when_lhs_is_less", 
		"",
		"2 + 3 > 2*3+1 ", 
		LOOP_NOT_TAKEN),
	("variable_constant_expressions_should_take_while_loop_when_both_side_are_equal", 
		"let value: int = 2; let rhs: int = 6;", 
		"value + rhs == 4*2 ", 
		LOOP_TAKEN),
	("constant_variable_expressions_should_take_while_loop_with_less_than_comparison_when_lhs_is_less", 
		"let value: int = 3; let rhs: int = 2;", 
		"10/3 < value*rhs", 
		LOOP_TAKEN),
	("variable_variable_expressions_should_not_take_while_loop_with_less_than_or_equal__comparison_when_lhs_is_greater", 
		"let value: int = 3; let rhs: int = 2;", 
		"value*5  <= rhs/2", 
		LOOP_NOT_TAKEN),


]

for pair in condition_answer_pairs:

	out = f'''program: |
  fn test_function() : int {{
      let a: int = {LOOP_NOT_TAKEN};
      {pair[1]}

      while {pair[2]} {{
        a = a+1;
        if a > 100 {{
            return a;
        }}  
      }}
      return a;
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
	



