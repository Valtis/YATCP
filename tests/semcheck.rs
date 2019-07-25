#[macro_use]
mod test_reporter;

use compiler::ast::{AstNode, AstInteger, ArithmeticInfo, NodeInfo, FunctionInfo, DeclarationInfo };
use compiler::error_reporter::ReportKind;

use compiler::semcheck::SemanticsCheck;
use compiler::semcheck::Type;

use self::test_reporter::TestReporter;

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
                            Rc::new("a".to_string()),
                            Type::Boolean,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                                Rc::new("hello ".to_string()),
                                NodeInfo::new(8, 6, 3)
                            )),
                            Box::new(AstNode::Text(
                                Rc::new("world".to_string()),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                                AstInteger::from(3),
                                NodeInfo::new(8, 6, 3)
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(14),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                            Rc::new("a".to_string()),
                            Type::Float,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                            Rc::new("a".to_string()),
                            Type::Double,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                            Rc::new("a".to_string()),
                            Type::Double,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                            Rc::new("a".to_string()),
                            Type::Double,
                            1, 1, 1),
                        ),
                    AstNode::VariableDeclaration(
                        Box::new(AstNode::Multiply(
                            Box::new(AstNode::Identifier(
                                Rc::new("a".to_string()),
                                NodeInfo::new(9, 7, 4)
                            )),
                            Box::new(AstNode::Double(
                                4.2,
                                NodeInfo::new(9, 7, 4)
                            )),
                            ArithmeticInfo::new_alt(4, 5, 6)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("b".to_string()),
                            Type::Double,
                            2, 2, 2),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                                Rc::new("c".to_string()),
                                NodeInfo::new(9, 7, 4)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Double,
                            1, 1, 1),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
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
                                    AstInteger::from(4),
                                    NodeInfo::new(8, 6, 3)
                                )),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
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
                                Rc::new("a".to_string()),
                                Type::Float,
                                2, 3, 4),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                                AstInteger::from(4),
                                NodeInfo::new(7, 23, 212)
                                ))),
                            ArithmeticInfo::new_alt(5, 6, 7)),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn correct_while_loop_is_accepted() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        while 1 >= 23 {
            let a : int = 1;
        } else {
            let b : double = 1.23;
        }
    }
    */
    let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::While(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn if_statement_without_else_is_accepted() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        if 1 >= 23 {
            let a : int = 1;
        }
    }
    */
    let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    None,
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn if_statement_with_else_is_accepted() {
    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        if 1 >= 23 {
            let a : int = 1;
        } else {
            let b : double = 1.23;
        }
    }
    */
    let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Double(
                                1.23,
                                NodeInfo::new(7, 6, 5),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("b".to_string()),
                            Type::Double,
                            41, 51, 61)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn valid_function_call_with_arguments_is_accepted() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            foo(4, "hello");
        }

    }
    */
    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::FunctionCall(
                                vec![
                                    AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                    AstNode::Text(
                                        Rc::new("hello".to_string()),
                                        NodeInfo::new(0,0,0)),
                                ],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(0,0,0)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn using_function_parameters_in_function_is_accepted() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : int) : int {
            return a + b;
        }

    }
    */
    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::Integer,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::Plus(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    NodeInfo::new(9, 7, 4)
                                )),
                                Box::new(AstNode::Identifier(
                                    Rc::new("b".to_string()),
                                    NodeInfo::new(9, 7, 4)
                                )),
                                ArithmeticInfo::new_alt(4, 5, 6)
                            )
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn using_function_in_expression_is_accepted() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : int {

        }

        fn bar() : void {
            let a : int = foo(4, "hello");
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            NodeInfo::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    NodeInfo::new(0,0,0))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn calling_extern_function_is_accepted() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        extern fn foo(a : int, b : string);
        fn bar() : void {
            foo(4, "hello");
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
                AstNode::ExternFunction(foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            NodeInfo::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    NodeInfo::new(0,0,0))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);
}

#[test]
fn function_parameters_are_added_to_the_symbol_table_level_of_function_block() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : int {

        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 0);

    if let AstNode::Block(child_nodes, _, _) = node  {
        if let AstNode::Function(boxed_val, _) = child_nodes[0].clone() {
            if let AstNode::Block(_, Some(sym_tab), _) = *boxed_val {
                if sym_tab.find_symbol(&"a".to_string()) == None {
                    panic!("Failed to find symbol 'a' in symbol table");
                }

                if sym_tab.find_symbol(&"b".to_string()) == None {
                    panic!("Failed to find symbol 'b' in symbol table");
                }

                return;
            }
        }
    }
    panic!("Invalid node present");

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
                                AstInteger::from(4),
                                NodeInfo::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 2, 3),
                        ),
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Float(
                                6.2,
                                NodeInfo::new(12, 65, 4)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Float,
                                2, 3, 4),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        2,
        3,
        4);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                FunctionInfo::new_alt(Rc::new("a".to_string()), Type::Void, 4, 5, 6)
            ),
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                AstInteger::from(4),
                                NodeInfo::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 2, 3),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        1,
        2,
        3);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                ),
            AstNode::Function(
                Box::new(AstNode::Block(vec![],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 2, 3, 4)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        2,
        3,
        4);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                ),
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Identifier(
                                Rc::new("foo".to_string()),
                                NodeInfo::new(8, 6, 3)
                            )),
                            DeclarationInfo::new_alt(
                                Rc::new("a".to_string()),
                                Type::Integer,
                                1, 2, 3),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("bar".to_string()), Type::Void, 2, 3, 4)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        8,
        6,
        3);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 1, 2, 3)
                ),
            AstNode::Function(
                Box::new(AstNode::Block(vec![
                        AstNode::VariableAssignment(
                            Box::new(AstNode::Integer(
                                AstInteger::from(4),
                                NodeInfo::new(8, 6, 3)
                            )),
                            Rc::new("foo".to_string()),
                            NodeInfo::new(9, 10, 11)
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("bar".to_string()), Type::Void, 2, 3, 4)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        9,
        10,
        11);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                            Rc::new("a".to_string()),
                            Type::Integer,
                            1, 1, 1),
                    )],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        8,
        6,
        3);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                                AstInteger::from(14),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        3,
        4,
        5);
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
                                AstInteger::from(14),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        3,
        4,
        5);
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
                                AstInteger::from(14),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        3,
        4,
        5);
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
                                AstInteger::from(14),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        3,
        4,
        5);
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
                            Rc::new("a".to_string()),
                            Type::Boolean,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);
    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
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
                                Rc::new("hello ".to_string()),
                                NodeInfo::new(8, 6, 3)
                            )),
                            Box::new(AstNode::Text(
                                Rc::new("world".to_string()),
                                NodeInfo::new(9, 7, 2)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);
    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
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
                                Rc::new("hello".to_string()),
                                NodeInfo::new(8, 6, 3)
                            )),
                            ArithmeticInfo::new_alt(3, 4, 1)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
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
                            Rc::new("hello".to_string()),
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 1, 1),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Identifier(
                                Rc::new("a".to_string()),
                                NodeInfo::new(11, 112, 1112),
                                )),
                            ArithmeticInfo::new_alt(21, 22, 23)
                        )),
                        Rc::new("a".to_string()),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
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
                            Rc::new("a".to_string()),
                            Type::Boolean,
                            1, 1, 1),
                        )
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
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
                            Rc::new("a".to_string()),
                            Type::Boolean,
                            1, 1, 1),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Negate(
                            Box::new(AstNode::Identifier(
                                Rc::new("a".to_string()),
                                NodeInfo::new(11, 112, 1112),
                                )),
                            ArithmeticInfo::new_alt(21, 22, 23)
                        )),
                        Rc::new("a".to_string()),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
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
                            Rc::new("hello".to_string()),
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 4, 8),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Divide(
                            Box::new(AstNode::Identifier(
                                Rc::new("a".to_string()),
                                NodeInfo::new(33, 34, 35),
                            )),
                            Box::new(AstNode::Integer(
                                AstInteger::from(4),
                                NodeInfo::new(11, 112, 1112),
                            )),
                            ArithmeticInfo::new_alt(14, 41, 342)
                        )),
                        Rc::new("a".to_string()),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        14,
        34,
        1190);
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
                            Rc::new("hello".to_string()),
                            NodeInfo::new(8, 6, 3)
                        )),
                        DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 4, 8),
                        ),
                    AstNode::VariableAssignment(
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(11, 112, 1112),
                        )),
                        Rc::new("a".to_string()),
                        NodeInfo::new(2, 6, 34),
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        11,
        112,
        1112);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                            Rc::new("a".to_string()),
                            Type::String,
                            1, 4, 8),
                        ),
                    AstNode::ErrorNode,
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 0, 0, 0)
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
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 6, 5, 4)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        5,
        6,
        7);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::String, 6, 5, 4)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        5,
        6,
        7);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
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
                                AstInteger::from(4),
                                NodeInfo::new(7, 23, 212)
                                ))),
                            ArithmeticInfo::new_alt(5, 6, 7)),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 6, 5, 4)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        5,
        6,
        7);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        6,
        5,
        4);
}


#[test]
fn while_loop_with_non_boolean_expression_is_reported() {
   let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : void {
        while 4 {

        }
    }
    */

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::While(
                            Box::new(AstNode::Integer(
                                AstInteger::from(4),
                                NodeInfo::new(7, 23, 212)
                                )),
                            Box::new(AstNode::Block(vec![
                                ],
                                None,
                                NodeInfo::new(0, 0, 0))),
                            NodeInfo::new(89, 54, 12)
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 6, 5, 4)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        7,
        23,
        212);
}

#[test]
fn error_in_while_loop_body_is_handled() {
   let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : void {
        while 4 {

        }
    }
    */

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(vec![
                        AstNode::While(
                            Box::new(AstNode::Boolean(
                                true,
                                NodeInfo::new(7, 23, 212)
                                )),
                            Box::new(AstNode::Block(vec![
                                AstNode::Plus(
                                    Box::new(AstNode::Integer(
                                        AstInteger::from(4),
                                        NodeInfo::new(1, 2, 3))
                                    ),
                                    Box::new(AstNode::Text(
                                        Rc::new("hello".to_string()),
                                        NodeInfo::new(4, 3, 1))
                                    ),
                                    ArithmeticInfo::new_alt(9, 7, 6))
                                ],
                                None,
                                NodeInfo::new(0, 0, 0))),
                            NodeInfo::new(89, 54, 12)
                        ),
                    ],
                    None,
                    NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Void, 6, 5, 4)
            )],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        9,
        2,
        11);
}


#[test]
fn if_statement_with_non_boolean_expression_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
        /*
    fn foo() : int {
        if 1 {
            let a : string = "foo";
        } else {
            let b : double = 3.14159;
        }
    }*/

   let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::Integer(
                        AstInteger::from(1),
                        NodeInfo::new(11, 22, 33),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Text(
                                Rc::new("foo".to_string()),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Double(
                                3.14159,
                                NodeInfo::new(7, 6, 5),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("b".to_string()),
                            Type::Double,
                            41, 51, 61)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

   checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        11,
        22,
        33);
}

#[test]
fn if_statement_with_non_boolean_expression_in_else_if_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        if 1 > 23 {
            let a : string = "foo";
        } else if 4 {
            let b : double = 3.14159;
        }
    }*/

   let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Text(
                                Rc::new("foo".to_string()),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::String,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::If(
                        Box::new(AstNode::Integer(
                            AstInteger::from(4),
                            NodeInfo::new(99, 88, 77),
                        )),
                        Box::new(AstNode::Block(vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Double(
                                    3.14159,
                                    NodeInfo::new(7, 6, 5),
                                )),
                                DeclarationInfo::new_alt(
                                Rc::new("b".to_string()),
                                Type::Double,
                                41, 51, 61)),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                        None,
                        NodeInfo::new(0, 0, 0),
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

   checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        99,
        88,
        77);
}

#[test]
fn error_in_if_statement_true_block_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        if 1 > 23 {
            let a : int = "foo";
        } else {
            let b : double = 3.14159;
        }
    }*/

   let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Text(
                                Rc::new("foo".to_string()),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Double(
                                3.14159,
                                NodeInfo::new(7, 6, 5),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("b".to_string()),
                            Type::Double,
                            41, 51, 61)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

   checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        1,
        2,
        3);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        4,
        5,
        6);
}

#[test]
fn error_in_else_block_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
    fn foo() : int {
        if 1 > 23 {
            let a : int = 1;
        } else {
            let b : double = 3;
        }
    }*/

   let mut node = AstNode::Block(vec![
        AstNode::Function(
            Box::new(AstNode::Block(vec![
                AstNode::If(
                    Box::new(AstNode::GreaterOrEq(
                        Box::new(AstNode::Integer(
                            AstInteger::from(1),
                            NodeInfo::new(11, 22, 33),
                        )),
                        Box::new(AstNode::Integer(
                            AstInteger::from(23),
                            NodeInfo::new(13, 14, 15))),
                        NodeInfo::new(12, 23, 34),
                    )),
                    Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                AstInteger::from(1),
                                NodeInfo::new(1, 2, 3),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("a".to_string()),
                            Type::Integer,
                            4, 5, 6)),
                            ],
                        None,
                        NodeInfo::new(0, 0, 0),
                    )),
                    Some(Box::new(AstNode::Block(vec![
                        AstNode::VariableDeclaration(
                            Box::new(AstNode::Integer(
                                AstInteger::from(3),
                                NodeInfo::new(7, 6, 5),
                            )),
                            DeclarationInfo::new_alt(
                            Rc::new("b".to_string()),
                            Type::Double,
                            41, 51, 61)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0)
                    ))),
                    NodeInfo::new(5, 6, 7),
                ),
            ],
            None,
            NodeInfo::new(0, 0, 0),
            )),
            FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0))
        ],
        None,
        NodeInfo::new(0, 0, 0));

   checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        7,
        6,
        5);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        41,
        51,
        61);
}

#[test]
fn calling_nonexistent_function_is_reported() {
   let (reporter, mut checker) = create_sem_checker();
    /*
        fn bar() : void {
            foo();
        }

    }
    */

    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::FunctionCall(
                                vec![],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(7,8,9)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        7,
        8,
        9);
}

#[test]
fn using_variable_as_function_is_reported() {
   let (reporter, mut checker) = create_sem_checker();
    /*
        fn bar() : void {
            let foo : int = 2;
            foo();
        }

    }
    */

    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(4),
                                    NodeInfo::new(0,0,0))),
                                DeclarationInfo::new_alt(
                                    Rc::new("foo".to_string()),
                                    Type::Integer,
                                    1, 2, 3)),
                            AstNode::FunctionCall(
                                vec![],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(7,8,9)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        7,
        8,
        9);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn wrong_number_of_function_arguments_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            foo(4);
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        4, 7, 9);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::FunctionCall(
                                vec![
                                    AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                ],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(23,24,25)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        23,
        24,
        25);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        4,
        7,
        9);
}

#[test]
fn wrong_number_of_extern_function_arguments_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        extern fn foo(a : int, b : string) : void;

        fn bar() : void {
            foo(4);
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        4, 7, 9);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::ExternFunction(foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::FunctionCall(
                                vec![
                                    AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                ],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(23,24,25)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        23,
        24,
        25);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        4,
        7,
        9);
}

#[test]
fn wrong_type_in_function_argument_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            foo(4, 6);
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        4, 7, 9);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            4, 5, 6));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::FunctionCall(
                                vec![
                                    AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                    AstNode::Integer(AstInteger::from(6), NodeInfo::new(9,8,7)),
                                ],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(23,24,25)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        9,
        8,
        7);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        4,
        5,
        6);
}

#[test]
fn redefinition_of_function_parameter_in_function_body_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {
            let a : int = 0;
        }

        fn bar() : void {
            foo(4, 6);
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        4, 7, 9);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            5, 4, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            4, 5, 6));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Integer(
                                    AstInteger::from(0),
                                    NodeInfo::new(0,0,0))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        1,
        2,
        3);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        5,
        4,
        3);
}

#[test]
fn type_mismatch_with_function_parameter_usage_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {
            let a : int = 0;
        }

        fn bar() : void {
            foo(4, 6);
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        4, 7, 9);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            5, 4, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            4, 5, 6));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::Identifier(
                                    Rc::new("a".to_string()),
                                    NodeInfo::new(9,8,7))),
                                DeclarationInfo::new_alt(
                                    Rc::new("c".to_string()),
                                    Type::Float,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        9,
        8,
        7);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn using_void_funtion_in_expression_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            let a : int = foo(4, "hello");
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            NodeInfo::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    NodeInfo::new(23,24,25))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        23,
        24,
        25);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn using_non_void_function_with_wrong_type_in_expression_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            let a : int = foo(4, "hello");
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::String,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            NodeInfo::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    NodeInfo::new(23,24,25))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Integer,
                                    1, 2, 3),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        23,
        24,
        25);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn error_in_function_argument_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            foo(4-"abc", "hello");
        }

    }
    */
    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::FunctionCall(
                                vec![
                                    AstNode::Minus(
                                        Box::new(
                                            AstNode::Integer(
                                                AstInteger::from(4),
                                                NodeInfo::new(0,0,0))),
                                        Box::new(AstNode::Text(
                                            Rc::new("abc".to_string()),
                                            NodeInfo::new(0,0,0))),
                                        ArithmeticInfo::new_alt(8, 4, 2),
                                    ),
                                    AstNode::Text(
                                        Rc::new("hello".to_string()),
                                        NodeInfo::new(0,0,0)),
                                ],
                                Rc::new("foo".to_string()),
                                NodeInfo::new(0,0,0)),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);

    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        8, 0, 6);
}

#[test]
fn function_parameter_shadowing_function_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() {

        }

        extern fn bar(foo : int) {

        }
    */
    let mut func_info = FunctionInfo::new_alt(
            Rc::new("bar".to_string()),
            Type::String,
            7, 8 ,9);

    func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        8,12,14));

    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            NodeInfo::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("foo".to_string()),
                        Type::Void,
                        1, 2 ,3)),
                AstNode::Function(
                    Box::new(AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                    func_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        8,
        12,
        14);


    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn function_parameter_name_collision_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*

        fn bar(a : int, foo : int, a : int) {

        }
    */
    let mut func_info = FunctionInfo::new_alt(
            Rc::new("bar".to_string()),
            Type::String,
            7, 8 ,9);

     func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("a".to_string()),
        Type::Integer,
        1,2,3));

    func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        8,12,14));

     func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("a".to_string()),
        Type::Integer,
        9,99,34));

    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                    Box::new(AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                    func_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        9,
        99,
        34);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn extern_function_parameter_shadowing_function_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() {

        }

        extern fn bar(foo : int) {

        }
    */
    let mut func_info = FunctionInfo::new_alt(
            Rc::new("bar".to_string()),
            Type::String,
            7, 8 ,9);

    func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        8,12,14));

    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            NodeInfo::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("foo".to_string()),
                        Type::Void,
                        1, 2 ,3)),
                AstNode::ExternFunction(
                    func_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        8,
        12,
        14);


    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn extern_function_parameter_name_collision_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*

        extern fn bar(a : int, foo : int, a : int) {

        }
    */
    let mut func_info = FunctionInfo::new_alt(
            Rc::new("bar".to_string()),
            Type::String,
            7, 8 ,9);

     func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("a".to_string()),
        Type::Integer,
        1,2,3));

    func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        8,12,14));

     func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("a".to_string()),
        Type::Integer,
        9,99,34));

    let mut node =
        AstNode::Block(vec![
                AstNode::ExternFunction(
                    func_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        9,
        99,
        34);

    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn extern_function_redefinition_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo() : void {

        }

        extern fn foo() : string;
    */
    let mut node =
        AstNode::Block(vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            None,
                            NodeInfo::new(0, 0, 0))),
                    FunctionInfo::new_alt(
                        Rc::new("foo".to_string()),
                        Type::Void,
                        1, 2 ,3)),
                AstNode::ExternFunction(
                    FunctionInfo::new_alt(
                        Rc::new("foo".to_string()),
                        Type::String,
                        7, 8 ,9)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 2);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::NameError,
        7,
        8,
        9);


    assert_eq_error!(reporter.borrow().errors()[1],
        ReportKind::Note,
        1,
        2,
        3);
}

#[test]
fn declaring_void_variable_is_reported() {

    let (reporter, mut checker) = create_sem_checker();
    /*
        fn foo(a : int, b : string) : void {

        }

        fn bar() : void {
            let a : void = foo(4, "hello");
        }
    }
    */

    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Integer,
        0, 0 ,0);

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("a".to_string()),
            Type::Integer,
            1, 2, 3));

    foo_info.parameters.push(
        DeclarationInfo::new_alt(
            Rc::new("b".to_string()),
            Type::String,
            1, 2, 3));

    let mut node =
        AstNode::Block(vec![
            AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![],
                        None,
                        NodeInfo::new(0, 0, 0))),
                foo_info),
                AstNode::Function(
                Box::new(
                    AstNode::Block(
                        vec![
                            AstNode::VariableDeclaration(
                                Box::new(AstNode::FunctionCall(
                                    vec![
                                        AstNode::Integer(AstInteger::from(4), NodeInfo::new(0,0,0)),
                                        AstNode::Text(
                                            Rc::new("hello".to_string()),
                                            NodeInfo::new(0,0,0)),
                                    ],
                                    Rc::new("foo".to_string()),
                                    NodeInfo::new(0,0,0))),
                                DeclarationInfo::new_alt(
                                    Rc::new("a".to_string()),
                                    Type::Void,
                                    99, 88, 77),
                            ),
                        ],
                        None,
                        NodeInfo::new(0, 0, 0))),
                FunctionInfo::new_alt(
                    Rc::new("bar".to_string()),
                    Type::Void,
                    0, 0 ,0)),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        99,
        88,
        77);
}

#[test]
fn void_function_parameter_is_reported() {
    let (reporter, mut checker) = create_sem_checker();
    /*

        extern fn bar(a : int, foo : void) {

        }
    */
    let mut func_info = FunctionInfo::new_alt(
            Rc::new("bar".to_string()),
            Type::String,
            7, 8 ,9);

     func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("a".to_string()),
        Type::Integer,
        1,2,3));

    func_info.parameters.push(DeclarationInfo::new_alt(
        Rc::new("foo".to_string()),
        Type::Void,
        8,12,14));

    let mut node =
        AstNode::Block(vec![
                AstNode::ExternFunction(
                    func_info),
            ],
            None,
            NodeInfo::new(0, 0,0)
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TypeError,
        8,
        12,
        14);
}

#[test]
fn int_max_plus_one_is_reported() {

    let (reporter, mut checker) = create_sem_checker();

     let mut node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::IntMaxPlusOne,
                                            NodeInfo::new(9, 10, 11)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );

    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TokenError,
        9,
        10,
        11);
}

#[test]
fn integer_larger_than_i32_max_plus_one_generates_correct_ast() {

    let (reporter, mut checker) = create_sem_checker();

    let mut node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            AstInteger::Invalid(2147483649),
                                            NodeInfo::new(2, 3, 4)
                                        )
                                    ),
                                    DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0),
                                ),
                            ],
                            None,
                            NodeInfo::new(0, 0, 0)
                        )),
                    FunctionInfo::new_alt(Rc::new("foo".to_string()), Type::Integer, 0, 0, 0)
                )
            ],
            None,
            NodeInfo::new(0, 0, 0),
        );
    checker.check_semantics(&mut node);
    assert_eq!(reporter.borrow().error_count(), 1);

    assert_eq_error!(reporter.borrow().errors()[0],
        ReportKind::TokenError,
        2,
        3,
        4);
}