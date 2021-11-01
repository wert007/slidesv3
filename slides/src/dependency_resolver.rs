use crate::{DebugFlags, diagnostics::DiagnosticBag, parser::{self, syntax_nodes::{CompilationUnitNodeKind, ImportStatementNodeKind, SyntaxNode, SyntaxNodeKind}}, text::SourceText};

pub fn resolve<'a>(source_text: &'a SourceText<'a>, diagnostic_bag: &mut DiagnosticBag<'a>, debug_flags: DebugFlags) -> SyntaxNode<'a> {
    let compilation_unit = parser::parse(source_text, diagnostic_bag, debug_flags);
    if let SyntaxNodeKind::CompilationUnit(compilation_unit) = compilation_unit.kind {
        resolve_compilation_unit(compilation_unit, diagnostic_bag)
    } else {
        unreachable!("Root Syntax Node must be a Compilation Unit!");
    }
}

fn resolve_compilation_unit<'a>(compilation_unit: CompilationUnitNodeKind<'a>, diagnostic_bag: &mut DiagnosticBag) -> SyntaxNode<'a> {
    let mut statements = Vec::with_capacity(compilation_unit.statements.len());
    for statement in compilation_unit.statements {
        let statement = resolve_node(statement, diagnostic_bag);
        statements.push(statement);
    }
    SyntaxNode::compilation_unit(statements, compilation_unit.eoi)
}

fn resolve_node<'a>(statement: SyntaxNode<'a>, diagnostic_bag: &mut DiagnosticBag) -> SyntaxNode<'a> {
    match statement.kind {
        SyntaxNodeKind::ImportStatement(import_statement) => resolve_import_statement(import_statement, diagnostic_bag),
        _ => statement,
    }
}

fn resolve_import_statement<'a>(_import_statement: ImportStatementNodeKind<'a>, _diagnostic_bag: &mut DiagnosticBag) -> SyntaxNode<'a> {
    // Collect library name and also resolve that library.
    todo!()
}


// fn bind_import_function<'a>(import_function: SyntaxNode<'a>, binder: &mut BindingState<'a, '_>) -> Option<()> {
//     if let SyntaxNodeKind::FunctionCall(function_call) = import_function.kind {
//         if let SyntaxNodeKind::Variable(variable) = &function_call.base.kind {
//             match variable.token.lexeme {
//                 "lib" => {
//                     if function_call.arguments.len() != 1 {
//                         binder.diagnostic_bag.report_unexpected_argument_count(function_call.argument_span(), function_call.arguments.len(), 1);
//                         return None;
//                     }
//                     let file_name = bind_node(function_call.arguments[0].clone(), binder);
//                     let file_name = bind_conversion(file_name, &Type::String, binder);
//                     if file_name.constant_value.is_none() {
//                         binder.diagnostic_bag.report_expected_constant(file_name.span);
//                         return None;
//                     }
//                     let file_name = file_name.constant_value.unwrap().value;
//                     let file_name = file_name.as_string().unwrap();
//                     println!("Loading lib {}", file_name);
//                     Some(todo!())
//                 }
//                 unknown => {
//                     binder.diagnostic_bag.report_unknown_import_function(import_function.span, unknown);
//                     None
//                 }
//             }
//         } else {
//             binder.diagnostic_bag.report_only_function_call_in_import_statement(import_function.span);
//             None
//         }
//     } else {
//         binder.diagnostic_bag.report_only_function_call_in_import_statement(import_function.span);
//         None
//     }
// }
