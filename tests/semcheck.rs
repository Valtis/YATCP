extern crate compiler;

#[macro_use]
mod test_reporter;

use compiler::ast::AstNode;
use compiler::ast::ArithmeticInfo;
use compiler::ast::NodeInfo;
use compiler::ast::FunctionInfo;
use compiler::ast::DeclarationInfo;

use compiler::error_reporter::ErrorReporter;
use compiler::error_reporter::Error;

use compiler::semcheck::SemanticsCheck;
use compiler::semcheck::Type;

use self::test_reporter::TestReporter;
use self::test_reporter::ReportedError;

use std::rc::Rc;
use std::cell::RefCell;

fn create_sem_checker() -> (Rc<RefCell<TestReporter>>, SemanticsCheck) {
    let reporter = Rc::new(RefCell::new(TestReporter::new()));

    (reporter.clone(), SemanticsCheck::new(reporter))
}


#[test]
fn assigning_boolean_is_allowed() {
   let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : bool = true;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(                    
                        Box::new(AstNode::Boolean(
                            true,
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Boolean,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);    
}

#[test]
fn concatenation_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() {
            let a : string = "hello " + "world";
        }
    */
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Plus(
                            Box::new(AstNode::Text(
                                "hello ".to_string(),
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Text(
                                "world".to_string(),
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn artihmetic_operation_with_integers_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : int = 3.2 + 14;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Integer(
                                3,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Integer(
                                14,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn artihmetic_operation_with_floats_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : float = 3.2f + 14f;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Float(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Float(
                                14f32,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Float,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn artihmetic_operation_with_doubles_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : double = 3.2 + 14d;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Double(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Double(
                                14f64,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Double,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn negation_of_number_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : double = -3.2;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Double(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Double,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);    
}

#[test]
fn expression_using_variables_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();
     /*
        fn foo() {
            let a : double = -3.2;
            let b : double = a * 4.2;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Double(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Double,
                            1, 1, 1),
                        ),
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Identifier(
                                "a".to_string(),
                                NodeInfo::new(9, 7, 4)
                            )),
                            Box::new(AstNode::Double(
                                4.2,
                                NodeInfo::new(9, 7, 4)
                            )),
                            ArithmeticInfo::new_alt(4, 5, 6)
                        )),
                        DeclarationInfo::new_alt(
                            "b".to_string(),
                            Type::Double,
                            2, 2, 2),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);    
}

#[test]
fn undeclared_variable_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
     /*
        fn foo() {
            let a : double = c
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Identifier(
                                "c".to_string(),
                                NodeInfo::new(9, 7, 4)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Double,
                            1, 1, 1),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);  

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::NameError,
        9,
        7,
        4); 
}

#[test]
fn redeclaration_of_variable_is_allowed_if_scopes_do_not_overlap() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() {
        {
        let a : int = 4;
        }        
        let a : float = 6.2f;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    4,
                                    NodeInfo::new(8, 6, 3)
                                )),
                                DeclarationInfo::new_alt(
                                    "a".to_string(),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Float(
                                6.2,
                                NodeInfo::new(12, 65, 4)
                            )),
                            DeclarationInfo::new_alt(
                                "a".to_string(),
                                Type::Float,
                                2, 3, 4),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn return_without_expression_in_void_function_is_allowed() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : void {
        return;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::Return(None, ArithmeticInfo::new_alt(5, 6, 7)),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn return_with_correct_expression_type_is_allowed_in_function() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        return 5;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::Return(
                            Some(Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(7, 23, 212)
                                ))), 
                            ArithmeticInfo::new_alt(5, 6, 7)),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Integer, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn redeclaration_of_variable_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() {
        let a : int = 4;
        let a : float = 6.2f;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                "a".to_string(),
                                Type::Integer,
                                1, 2, 3),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Float(
                                6.2,
                                NodeInfo::new(12, 65, 4)
                            )),
                            DeclarationInfo::new_alt(
                                "a".to_string(),
                                Type::Float,
                                2, 3, 4),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::NameError,
        2,
        3,
        4);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        1,
        2,
        3);
}


#[test]
fn declaration_of_variable_which_shares_name_with_function_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn a() : void {
    
    }

    fn foo() : void {
        let a : int = 4;
    }
    */
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0)
                    )
                ),
                FunctionInfo::new_alt("a".to_string(), Type::Void, 4, 5, 6)
            ),
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                "a".to_string(),
                                Type::Integer,
                                1, 2, 3),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::NameError,
        1,
        2,
        3);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        4,
        5,
        6);
}

#[test]
fn redefinition_of_a_function_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() : void {
    
        }

        fn foo : void() {
    
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 1, 2, 3)   
                ),
            AstNode::Function(
                Box::new(AstNode::Block(vec![], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 2, 3, 4)   
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::NameError,
        2,
        3,
        4);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        1,
        2,
        3);
}

#[test]
fn using_function_as_variable_in_expression_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() : void {
    
        }

        fn bar : void() {
            let a : int = foo;  
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 1, 2, 3)   
                ),
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Identifier(
                                "foo".to_string(),
                                NodeInfo::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                "a".to_string(),
                                Type::Integer,
                                1, 2, 3),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("bar".to_string(), Type::Void, 2, 3, 4)   
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        8,
        6,
        3);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        1,
        2,
        3);
}

#[test]
fn assigning_into_function_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() : void {
    
        }

        fn bar : void() {
            foo = 4;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 1, 2, 3)   
                ),
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                        AstNode::VariableAssignment(
                            Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(8, 6, 3)
                            )),
                            "foo".to_string(),
                            NodeInfo::new(9, 10, 11) 
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("bar".to_string(), Type::Void, 2, 3, 4)   
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        9,
        10,
        11);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        1,
        2,
        3);   
}

#[test]
fn type_error_when_variable_is_declared_is_reported() {
    let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() : void {
            let a : int = 3.2f;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Float(
                            3.2,
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Integer,
                            1, 1, 1),
                    )], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        8,
        6,
        3);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        1,
        1,
        1);
}

#[test]
fn type_error_in_plus_expression_is_reported() {
       let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : int = 3.2f + 14;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Plus(
                            Box::new(AstNode::Float(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Integer(
                                14,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn type_error_in_minus_expression_is_reported() {
       let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : int = 3.2f - 14;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Minus(
                            Box::new(AstNode::Float(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Integer(
                                14,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn type_error_in_multiplication_expression_is_reported() {
    let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : int = 3.2f * 14;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Float(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Integer(
                                14,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn type_error_in_division_expression_is_reported() {
       let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : int = 3.2f / 14;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Divide(
                            Box::new(AstNode::Float(
                                3.2,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Integer(
                                14,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn arithmetic_operation_on_booleans_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() {
            let a : bool = true * false;
        }
    */
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Boolean(
                                true,
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Boolean(
                                false,
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Boolean,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn non_concatenation_arithmetic_operation_on_text_is_reported() {
        let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() {
            let a : string = "hello " - "world";
        }
    */
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Minus(
                            Box::new(AstNode::Text(
                                "hello ".to_string(),
                                NodeInfo::new(8, 6, 3)
                            )),                       
                            Box::new(AstNode::Text(
                                "world".to_string(),
                                NodeInfo::new(9, 7, 2)
                            )),   
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}


#[test]
fn negation_of_string_is_reported() {
   let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : string = -"hello";
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Text(
                                "hello".to_string(),
                                NodeInfo::new(8, 6, 3)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);    
    
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn negation_of_string_variable_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    
    /*
        fn foo() {
            let a : string = "hello";
            a = -a;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Text(
                            "hello".to_string(),
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 1, 1),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Identifier(
                                "a".to_string(),
                                NodeInfo::new(11, 112, 1112),
                                )),
                            ArithmeticInfo::new_alt(21, 22, 23)
                        )),
                        "a".to_string(),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);    
    
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        21,
        22,
        23);
}

#[test]
fn negation_of_boolean_is_reported() {
   let (reporter, mut checker) = create_sem_checker();

    /*
        fn foo() {
            let a : bool = -false;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Boolean(
                                false,
                                NodeInfo::new(8, 6, 3)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Boolean,
                            1, 1, 1),
                        )
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);    
    
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        3,
        4,
        1);
}

#[test]
fn negation_of_boolean_variable_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    
    /*
        fn foo() {
            let a : bool = true;
            a = -a;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Boolean(
                            true,
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::Boolean,
                            1, 1, 1),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Identifier(
                                "a".to_string(),
                                NodeInfo::new(11, 112, 1112),
                                )),
                            ArithmeticInfo::new_alt(21, 22, 23)
                        )),
                        "a".to_string(),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);    
    
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        21,
        22,
        23);
}

#[test]
fn type_error_involving_variables_in_expression_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    
    /*
        fn foo() {
            let a : string = "hello";
            a = a / 4;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Text(
                            "hello".to_string(),
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 4, 8),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Divide(
                            Box::new(AstNode::Identifier(
                                "a".to_string(),
                                NodeInfo::new(33, 34, 35),
                            )),
                            Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(11, 112, 1112),
                            )),
                            ArithmeticInfo::new_alt(14, 41, 342)
                        )),
                        "a".to_string(),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 1);    
    
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        14,
        41,
        342);
} 

#[test]
fn type_error_when_assigning_into_variable_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    
    /*
        fn foo() {
            let a : string = "hello";
            a = 23;
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Text(
                            "hello".to_string(),
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 4, 8),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Integer(
                            23,
                            NodeInfo::new(11, 112, 1112),
                        )),
                        "a".to_string(),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);    
    
    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        11,
        112,
        1112);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        1,
        4,
        8);
}

#[test]
fn syntax_error_node_is_handled_correctly() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() {
            let a : string = <syntax error node>;
            <syntax error node>
        }
    */

    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::ErrorNode),
                        DeclarationInfo::new_alt(
                            "a".to_string(),
                            Type::String,
                            1, 4, 8),
                        ),
                    AstNode::ErrorNode,
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 0, 0, 0)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 0);    
}

#[test]
fn missing_expression_in_return_is_reported_if_function_has_non_void_return_type() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        return;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::Return(None, ArithmeticInfo::new_alt(5, 6, 7)),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Integer, 6, 5, 4)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        5,
        6,
        7);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        6,
        5,
        4);
}

#[test]
fn incorrect_type_of_return_expression_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : string {
        return;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::Return(None, ArithmeticInfo::new_alt(5, 6, 7)),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::String, 6, 5, 4)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        5,
        6,
        7);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        6,
        5,
        4);
}

#[test]
fn returning_value_from_void_function_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : void {
        return 5;
    }
    */
    
    let mut node = 
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::Return(
                            Some(Box::new(AstNode::Integer(
                                4,
                                NodeInfo::new(7, 23, 212)
                                ))), 
                            ArithmeticInfo::new_alt(5, 6, 7)),
                    ], 
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt("foo".to_string(), Type::Void, 6, 5, 4)   
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node); 

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0], 
        Error::TypeError,
        5,
        6,
        7);

    assert_eq_error!(reporter.borrow().errors()[1], 
        Error::Note,
        6,
        5,
        4);
}
