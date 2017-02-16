extern crate compiler;

use compiler::ast::AstNode;
use compiler::ast::ArithmeticInfo;
use compiler::ast::DeclarationInfo;
use compiler::ast::FunctionInfo;
use compiler::ast::NodeInfo;
use compiler::semcheck::Type;
use compiler::symbol_table::TableEntry;
use compiler::symbol_table::Symbol;
use compiler::tac_generator::TACGenerator;
use compiler::tac_generator::Statement;
use compiler::tac_generator::Operand;
use compiler::tac_generator::Operator;

use std::rc::Rc;

#[test]
fn program_with_variable_declarations_produces_correct_tac() {

    /*
    fn a() : int  {
        let a : int = 4;
        let b : int = 9 * 4;
        let c : int = 6;
    }*/
    let func_info = FunctionInfo::new_alt(Rc::new("a".to_string()), Type::Integer, 0, 0, 0);
    let mut block_symtab_entry = TableEntry::new();

    block_symtab_entry.add_symbol(Symbol::Function(func_info.clone()));

    let decl_info_a = DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0);

    let decl_info_b = DeclarationInfo::new_alt(
                                            Rc::new("b".to_string()),
                                            Type::Integer,
                                            0, 0, 0);

    let decl_info_c =  DeclarationInfo::new_alt(
                                        Rc::new("c".to_string()),
                                        Type::Integer,
                                        0, 0, 0);

    let mut func_symtab_entry = TableEntry::new();
    func_symtab_entry.add_symbol(Symbol::Variable(decl_info_a.clone(), 0));
    func_symtab_entry.add_symbol(Symbol::Variable(decl_info_b.clone(), 1));
    func_symtab_entry.add_symbol(Symbol::Variable(decl_info_c.clone(), 2));


    let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            4,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    decl_info_a.clone(),
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::Multiply(
                                        Box::new(AstNode::Integer(
                                                9,
                                                NodeInfo::new(0, 0, 0)
                                            )
                                        ),
                                        Box::new(AstNode::Integer(
                                                4,
                                                NodeInfo::new(0, 0, 0)
                                            )
                                        ),
                                        ArithmeticInfo::new_alt(0, 0, 0),
                                    )),
                                    decl_info_b.clone(),
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            6,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    decl_info_c.clone(),
                                ),
                            ],
                            Some(func_symtab_entry),
                            NodeInfo::new(0, 0, 0)
                        )),
                    func_info,
                )
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

    let mut generator = TACGenerator::new(3);
    let functions = generator.generate_tac_functions(&node);

    assert_eq!(1, functions.len());
    assert_eq!(3, functions[0].statements.len());

    assert_eq!(
        Statement::Assignment(
            None,
            Some(Operand::Variable(decl_info_a.clone(), 0)),
            None,
            Some(Operand::Integer(4))),
        functions[0].statements[0]);

    assert_eq!(
        Statement::Assignment(
            Some(Operator::Multiply),
            Some(Operand::Variable(decl_info_b.clone(), 1)),
            Some(Operand::Integer(9)),
            Some(Operand::Integer(4))),
        functions[0].statements[1]);


    assert_eq!(
        Statement::Assignment(
            None,
            Some(Operand::Variable(decl_info_c.clone(), 2)),
            None,
            Some(Operand::Integer(6))),
        functions[0].statements[2]);
}


#[test]
fn function_call_generates_correct_tac() {
    /*
    -- foo omitted, not actually required --

    fn a() : int  {
        let a : int = 4;
        let b : int = foo(a, 9);
    }*/

    let mut block_symtab_entry = TableEntry::new();
    let foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()), Type::Integer, 0, 0, 0);

    let func_info = FunctionInfo::new_alt(Rc::new("a".to_string()), Type::Integer, 0, 0, 0);

    block_symtab_entry.add_symbol(Symbol::Function(func_info.clone()));
    block_symtab_entry.add_symbol(Symbol::Function(foo_info.clone()));

    let decl_info_a = DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0);

    let decl_info_b = DeclarationInfo::new_alt(
                                            Rc::new("b".to_string()),
                                            Type::Integer,
                                            0, 0, 0);

    let mut func_symtab_entry = TableEntry::new();
    func_symtab_entry.add_symbol(Symbol::Variable(decl_info_a.clone(), 0));
    func_symtab_entry.add_symbol(Symbol::Variable(decl_info_b.clone(), 1));


    let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![
                                AstNode::VariableDeclaration(
                                    Box::new(
                                        AstNode::Integer(
                                            4,
                                            NodeInfo::new(0, 0, 0)
                                        )
                                    ),
                                    decl_info_a.clone(),
                                ),
                                AstNode::VariableDeclaration(
                                    Box::new(AstNode::FunctionCall(
                                        vec![
                                            AstNode::Identifier(
                                                Rc::new("a".to_string()),
                                                NodeInfo::new(0, 0, 0)),
                                            AstNode::Integer(
                                                9,
                                                NodeInfo::new(0, 0, 0))
                                        ],
                                       Rc::new("foo".to_string()),
                                       NodeInfo::new(0, 0, 0),
                                    )),
                                    decl_info_b.clone(),
                                ),
                            ],
                            Some(func_symtab_entry),
                            NodeInfo::new(0, 0, 0)
                        )),
                    func_info,
                )
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

    let mut generator = TACGenerator::new(2);
    let functions = generator.generate_tac_functions(&node);

    assert_eq!(1, functions.len());
    assert_eq!(3, functions[0].statements.len());

    assert_eq!(
        Statement::Assignment(
            None,
            Some(Operand::Variable(decl_info_a.clone(), 0)),
            None,
            Some(Operand::Integer(4))),
        functions[0].statements[0]);

    assert_eq!(
        Statement::Call(
            Rc::new("foo".to_string()),
            vec![
                Operand::Variable(decl_info_a, 0),
                Operand::Integer(9)
            ],
            Some(Operand::Variable(
                DeclarationInfo::new_alt(
                    Rc::new("%tmp".to_string()),
                    Type::Integer,
                    0, 0, 0),
                2))),
        functions[0].statements[1]);


    assert_eq!(
        Statement::Assignment(
            None,
            Some(Operand::Variable(decl_info_b.clone(), 1)),
            None,
            Some(
                Operand::Variable(
                    DeclarationInfo::new_alt(
                        Rc::new("%tmp".to_string()),
                        Type::Integer,
                        0, 0, 0),
                    2))),
        functions[0].statements[2]);
}


#[test]
fn function_with_parameters_generates_correct_tac() {


    /*
    fn foo(a: int) : int {

    }
    */

    let mut block_symtab_entry = TableEntry::new();
    let mut foo_info = FunctionInfo::new_alt(
        Rc::new("foo".to_string()), Type::Integer, 0, 0, 0);

    let decl_info_a = DeclarationInfo::new_alt(
                                        Rc::new("a".to_string()),
                                        Type::Integer,
                                        0, 0, 0);

    foo_info.parameters.push(decl_info_a.clone());

    block_symtab_entry.add_symbol(Symbol::Function(foo_info.clone()));

    let mut func_symtab_entry = TableEntry::new();
    func_symtab_entry.add_symbol(Symbol::Variable(decl_info_a.clone(), 0));


    let node = AstNode::Block(
            vec![
                AstNode::Function(
                    Box::new(
                        AstNode::Block(
                            vec![],
                            Some(func_symtab_entry),
                            NodeInfo::new(0, 0, 0)
                        )),
                    foo_info,
                )
            ],
            Some(block_symtab_entry),
            NodeInfo::new(0, 0, 0),
        );

    let mut generator = TACGenerator::new(1);
    let functions = generator.generate_tac_functions(&node);

    assert_eq!(1, functions.len());
    assert_eq!(1, functions[0].statements.len());

    assert_eq!(
        Statement::Assignment(
            None,
            Some(Operand::Variable(decl_info_a.clone(), 0)),
            None,
            Some(Operand::Initialized(Type::Integer))),
        functions[0].statements[0]);
    }