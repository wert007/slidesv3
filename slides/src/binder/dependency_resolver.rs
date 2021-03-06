use std::path::{Path, PathBuf};

use crate::{
    binder::{symbols::Library, typing::Type, BindingState},
    parser::syntax_nodes::{
        CompilationUnitNodeKind, ImportStatementNodeKind, SyntaxNode, SyntaxNodeKind,
    },
    text::TextSpan,
    value::Value,
};

#[derive(Debug, Clone)]
pub struct BoundImportStatement<'a> {
    pub span: TextSpan,
    pub name: &'a str,
    pub function: ImportFunction,
}

#[derive(Debug, Clone)]
pub enum ImportFunction {
    Library(ImportLibraryFunction),
}

#[derive(Debug, Clone)]
pub struct ImportLibraryFunction {
    pub path: String,
}

pub(super) fn bind_import_statements<'a>(
    node: SyntaxNode<'a>,
    unmoveable_label_range: usize,
    binder: &mut BindingState<'a, '_>,
) -> Option<SyntaxNode<'a>> {
    match node.kind {
        SyntaxNodeKind::CompilationUnit(compilation_unit) => {
            Some(bind_import_statements_compilation_unit(
                compilation_unit,
                unmoveable_label_range,
                binder,
            ))
        }
        SyntaxNodeKind::ImportStatement(import_statement) => {
            bind_import_statements_import_statement(
                node.span,
                import_statement,
                unmoveable_label_range,
                binder,
            );
            None
        }
        SyntaxNodeKind::_ConstDeclaration(_)
        | SyntaxNodeKind::FunctionDeclaration(_)
        | SyntaxNodeKind::StructDeclaration(_)
        | SyntaxNodeKind::Literal(_)
        | SyntaxNodeKind::ArrayLiteral(_)
        | SyntaxNodeKind::RepetitionNode(_)
        | SyntaxNodeKind::CastExpression(_)
        | SyntaxNodeKind::ConstructorCall(_)
        | SyntaxNodeKind::Variable(_)
        | SyntaxNodeKind::Binary(_)
        | SyntaxNodeKind::Unary(_)
        | SyntaxNodeKind::Parenthesized(_)
        | SyntaxNodeKind::FunctionCall(_)
        | SyntaxNodeKind::ArrayIndex(_)
        | SyntaxNodeKind::FieldAccess(_)
        | SyntaxNodeKind::BlockStatement(_)
        | SyntaxNodeKind::ForStatement(_)
        | SyntaxNodeKind::IfStatement(_)
        | SyntaxNodeKind::VariableDeclaration(_)
        | SyntaxNodeKind::ReturnStatement(_)
        | SyntaxNodeKind::WhileStatement(_)
        | SyntaxNodeKind::Assignment(_)
        | SyntaxNodeKind::ExpressionStatement(_)
        | SyntaxNodeKind::StructField(_) => Some(node),
    }
}

fn bind_import_statements_compilation_unit<'a>(
    compilation_unit: CompilationUnitNodeKind<'a>,
    unmoveable_label_range: usize,
    binder: &mut BindingState<'a, '_>,
) -> SyntaxNode<'a> {
    let mut statements = Vec::with_capacity(compilation_unit.statements.len());
    for statement in compilation_unit.statements {
        if let Some(statement) = bind_import_statements(statement, unmoveable_label_range, binder) {
            statements.push(statement);
        }
    }
    SyntaxNode::compilation_unit(statements, compilation_unit.eoi)
}

fn bind_import_statements_import_statement<'a>(
    span: TextSpan,
    import_statement: ImportStatementNodeKind<'a>,
    unmoveable_label_range: usize,
    binder: &mut BindingState<'a, '_>,
) {
    bind_import_statement(span, import_statement, unmoveable_label_range, binder);
}

fn bind_import_statement<'a>(
    span: TextSpan,
    import_statement: ImportStatementNodeKind<'a>,
    unmoveable_label_range: usize,
    binder: &mut BindingState<'a, '_>,
) {
    let import_function = if let Some(it) = bind_import_function(*import_statement.function, binder)
    {
        it
    } else {
        return;
    };
    let name = import_statement.identifier.lexeme;
    let import_statement = BoundImportStatement {
        function: import_function,
        name,
        span,
    };
    execute_import_function(import_statement, unmoveable_label_range, binder);
}

fn bind_import_function<'a>(
    function: SyntaxNode<'a>,
    binder: &mut BindingState<'a, '_>,
) -> Option<ImportFunction> {
    let function_span = function.span;
    if let SyntaxNodeKind::FunctionCall(mut function) = function.kind {
        if let SyntaxNodeKind::Variable(base) = function.base.kind {
            match base.token.lexeme {
                "lib" => {
                    let path = function.arguments.pop();
                    if path.is_none() {
                        binder
                            .diagnostic_bag
                            .report_unexpected_argument_count(function_span, 0, 1);
                        return None;
                    }
                    let path = path.unwrap();
                    let argument_span = path.span;
                    let path = super::bind_node(path, binder);
                    if path.constant_value.is_none() {
                        binder.diagnostic_bag.report_expected_constant(path.span);
                        return None;
                    }
                    let path = path.constant_value.unwrap().value;
                    if path.as_string().is_none() {
                        let string_type = super::string_type(binder);
                        binder.diagnostic_bag.report_cannot_convert(
                            argument_span,
                            &path.infer_type(string_type.clone()),
                            &string_type,
                        );
                        return None;
                    }
                    let path = path.as_string()?.into();
                    Some(ImportFunction::Library(ImportLibraryFunction { path }))
                }
                function_name => {
                    binder
                        .diagnostic_bag
                        .report_unknown_import_function(base.token.span(), function_name);
                    None
                }
            }
        } else {
            binder
                .diagnostic_bag
                .report_only_function_call_in_import_statement(function.base.span);
            None
        }
    } else {
        binder
            .diagnostic_bag
            .report_only_function_call_in_import_statement(function.span);
        None
    }
}

fn execute_import_function<'a>(
    import: BoundImportStatement<'a>,
    unmoveable_label_range: usize,
    binder: &mut BindingState<'a, '_>,
) {
    match import.function {
        ImportFunction::Library(library) => {
            let directory = PathBuf::from(binder.directory);
            let path = directory.join(library.path).with_extension("sld");
            load_library_from_path(
                binder,
                &path,
                import.span,
                import.name,
                true,
                unmoveable_label_range,
            );
        }
    }
}

pub(super) fn load_library_from_path<'a>(
    binder: &mut BindingState<'a, '_>,
    path: &Path,
    span: TextSpan,
    library_name: &'a str,
    import_std_lib: bool,
    unmoveable_label_range: usize,
) {
    let (path, lib) = binder
        .libraries
        .iter()
        .find_map(|l| l.find_imported_library_by_path(path))
        .map(|(s, l)| (Some(s), l))
        .unwrap_or_else(|| {
            let lib = crate::load_library_from_path(path, binder.debug_flags, import_std_lib);
            let lib = match lib {
                Ok(lib) => lib,
                Err(_) => {
                    binder.diagnostic_bag.report_could_not_access(span, path);
                    Library::error()
                }
            };
            (None, lib)
        });
    // If it is a std library, there is no path to it, but it might still be already loaded.
    let path = if library_name.is_empty() {
        Some(String::new())
    } else {
        path
    };
    load_library_into_binder(
        span,
        library_name,
        lib,
        path,
        unmoveable_label_range,
        binder,
    );
}

fn load_library_into_binder<'a>(
    span: TextSpan,
    name: &'a str,
    mut lib: Library,
    path: Option<String>,
    unmoveable_label_range: usize,
    binder: &mut BindingState<'a, '_>,
) {
    binder.max_used_variables = binder
        .max_used_variables
        .max(lib.program.max_used_variables);
    if lib.has_errors {
        binder
            .diagnostic_bag
            .report_errors_in_referenced_library(span, name);
    }
    if !name.is_empty() {
        let index = binder.libraries.len();
        let mut should_load_library = true;
        let variable = if lib.has_errors {
            should_load_library = false;
            binder.register_constant(name, Value::Error)
        } else {
            binder.register_constant(name, Value::Library(index))
        };
        if variable.is_none() {
            binder
                .diagnostic_bag
                .report_cannot_declare_variable(span, name);
            should_load_library = false;
        }
        if !should_load_library {
            return;
        }
    }
    lib.name = name.into();
    let need_to_load_std_libs = lib.name.is_empty() && binder.libraries.is_empty();
    if need_to_load_std_libs || path.is_none() {
        lib.relocate_labels(binder.label_offset, unmoveable_label_range);
        lib.relocate_structs(binder.structs.len() + binder.struct_table.len());
        binder.label_offset += lib.program.label_count;
    }
    for strct in &lib.structs {
        match &path {
            Some(empty) if empty.is_empty() => {
                assert!(lib.name.is_empty());
                binder.register_maybe_generic_struct_as(strct.name(), strct);
            }
            Some(path) => {
                let old_name = format!("{}.{}", path, strct.name());
                let new_name = format!("{}.{}", name, strct.name());
                let struct_id = binder.look_up_struct_id_by_name(&old_name).unwrap();
                binder.rename_struct_by_id(struct_id, new_name);
            }
            None => {
                let struct_name = if lib.name.is_empty() {
                    strct.name().to_owned()
                } else {
                    format!("{}.{}", name, strct.name())
                };
                binder.register_maybe_generic_struct_as(&struct_name, strct);
            }
        }
    }
    if name.is_empty() {
        for function in &lib.functions {
            binder.register_generated_constant(
                function.name.clone(),
                Value::LabelPointer(
                    function.function_label as usize,
                    Type::function(function.function_type.clone()),
                ),
            );
        }
    } else {
        for function in &lib.functions {
            if function.is_member_function {
                binder.register_generated_constant(
                    format!("{}.{}", name, function.name),
                    Value::LabelPointer(
                        function.function_label as usize,
                        Type::function(function.function_type.clone()),
                    ),
                );
            }
        }
    }
    binder.libraries.push(lib);
}
