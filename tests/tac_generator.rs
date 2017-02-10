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