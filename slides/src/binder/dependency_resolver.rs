use std::path::{Path, PathBuf};

use crate::{
    binder::{
        symbols::Library,
        typing::{Type, TypeId},
        BindingState,
    },
    parser::syntax_nodes::{
        CompilationUnitNodeKind, ImportStatementNodeKind, SyntaxNode, SyntaxNodeKind,
    },
    text::{SourceText, TextLocation},
    value::Value,
};

#[derive(Debug, Clone)]
pub struct BoundImportStatement {
    pub location: TextLocation,
    pub name: String,
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
    node: SyntaxNode,
    binder: &mut BindingState<'_>,
) -> Option<SyntaxNode> {
    match node.kind {
        SyntaxNodeKind::CompilationUnit(compilation_unit) => Some(
            bind_import_statements_compilation_unit(compilation_unit, binder),
        ),
        SyntaxNodeKind::ImportStatement(import_statement) => {
            bind_import_statements_import_statement(node.location, import_statement, binder);
            None
        }
        _ => Some(node),
    }
}

fn bind_import_statements_compilation_unit<'a>(
    compilation_unit: CompilationUnitNodeKind,
    binder: &mut BindingState<'_>,
) -> SyntaxNode {
    let mut statements = Vec::with_capacity(compilation_unit.statements.len());
    for statement in compilation_unit.statements {
        if let Some(statement) = bind_import_statements(statement, binder) {
            statements.push(statement);
        }
    }
    SyntaxNode::compilation_unit(statements, compilation_unit.eoi)
}

fn bind_import_statements_import_statement<'a>(
    location: TextLocation,
    import_statement: ImportStatementNodeKind,
    binder: &mut BindingState<'_>,
) {
    bind_import_statement(location, import_statement, binder);
}

fn bind_import_statement<'a>(
    location: TextLocation,
    import_statement: ImportStatementNodeKind,
    binder: &mut BindingState<'a>,
) {
    let import_function = if let Some(it) = bind_import_function(*import_statement.function, binder)
    {
        it
    } else {
        return;
    };
    let name = binder.lexeme(import_statement.identifier).into();
    let import_statement = BoundImportStatement {
        function: import_function,
        name,
        location,
    };
    execute_import_function(import_statement, binder);
}

fn bind_import_function<'a>(
    function: SyntaxNode,
    binder: &mut BindingState<'_>,
) -> Option<ImportFunction> {
    let function_span = function.location;
    if let SyntaxNodeKind::FunctionCall(mut function) = function.kind {
        if let SyntaxNodeKind::Variable(base) = function.base.kind {
            match binder.lexeme(base.token) {
                "lib" => {
                    let path = function.arguments.pop();
                    if path.is_none() {
                        binder
                            .diagnostic_bag
                            .report_unexpected_argument_count(function_span, 0, 1);
                        return None;
                    }
                    let path = path.unwrap();
                    let argument_span = path.location;
                    let path = super::bind_node(path, binder);
                    if path.constant_value.is_none() {
                        binder
                            .diagnostic_bag
                            .report_expected_constant(path.location);
                        return None;
                    }
                    let path = path.constant_value.unwrap().value;
                    if path.as_string().is_none() {
                        binder.diagnostic_bag.report_cannot_convert(
                            argument_span,
                            path.infer_type(),
                            typeid!(Type::String),
                        );
                        return None;
                    }
                    let path = path.as_string()?.into();
                    Some(ImportFunction::Library(ImportLibraryFunction { path }))
                }
                _function_name => {
                    binder
                        .diagnostic_bag
                        .report_unknown_import_function(base.token.location, base.token.location);
                    None
                }
            }
        } else {
            binder
                .diagnostic_bag
                .report_only_function_call_in_import_statement(function.base.location);
            None
        }
    } else {
        binder
            .diagnostic_bag
            .report_only_function_call_in_import_statement(function.location);
        None
    }
}

fn execute_import_function<'a>(import: BoundImportStatement, binder: &mut BindingState<'_>) {
    match import.function {
        ImportFunction::Library(library) => {
            let directory = PathBuf::from(binder.directory());
            let path = directory.join(library.path).with_extension("sld");
            load_library_from_path(binder, &path, import.location, import.name, true);
        }
    }
}

pub(super) fn load_library_from_source<'a>(
    binder: &mut BindingState<'_>,
    path: &Path,
    source: &'static str,
    location: TextLocation,
    library_name: String,
    import_std_lib: bool,
) {
    let (path, lib) = binder
        .libraries
        .iter()
        .find_map(|l| l.find_imported_library_by_path(path))
        .map(|(s, l)| (Some(s), l))
        .unwrap_or_else(|| {
            let source_text = binder
                .project
                .source_text_collection
                .add(SourceText::new(source, "std.sld"));
            (
                None,
                binder.project.load_library(source_text, import_std_lib),
            )
        });
    // If it is a std library, there is no path to it, but it might still be already loaded.
    let path = if library_name.is_empty() {
        Some(String::new())
    } else {
        path
    };
    load_library_into_binder(location, library_name, lib, path, binder);
}

pub(super) fn load_library_from_path<'a>(
    binder: &mut BindingState<'_>,
    path: &Path,
    location: TextLocation,
    library_name: String,
    import_std_lib: bool,
) {
    let (path, lib) = binder
        .libraries
        .iter()
        .find_map(|l| l.find_imported_library_by_path(path))
        .map(|(s, l)| (Some(s), l))
        .unwrap_or_else(|| {
            (
                None,
                binder.project.load_library_from_path(path, import_std_lib),
            )
        });
    // If it is a std library, there is no path to it, but it might still be already loaded.
    let path = if library_name.is_empty() {
        Some(String::new())
    } else {
        path
    };
    load_library_into_binder(location, library_name, lib, path, binder);
}

fn load_library_into_binder<'a>(
    location: TextLocation,
    name: String,
    mut lib: Library,
    path: Option<String>,
    binder: &mut BindingState<'_>,
) {
    binder.max_used_variables = binder
        .max_used_variables
        .max(lib.program.max_used_variables);
    if lib.has_errors {
        binder
            .diagnostic_bag
            .report_errors_in_referenced_library(location, &name);
    }
    if !name.is_empty() {
        let index = binder.libraries.len();
        let mut should_load_library = true;
        let variable = if lib.has_errors {
            should_load_library = false;
            binder.register_generated_variable(name.clone(), typeid!(Type::Error), true)
        } else {
            let type_ = binder
                .project
                .types
                .look_up_or_add_type(Type::Library(index));
            binder.register_generated_variable(name.clone(), type_, true)
        };
        if variable.is_none() {
            binder
                .diagnostic_bag
                .report_cannot_declare_variable(location, name.as_str());
            should_load_library = false;
        }
        if !should_load_library {
            return;
        }
    }
    lib.name = name.clone();
    let need_to_load_std_libs = lib.name.is_empty() && binder.libraries.is_empty();
    if need_to_load_std_libs || path.is_none() {
        lib.relocate_labels(binder.label_offset);
        binder.label_offset += lib.program.label_count;
    }
    for &strct in &lib.structs {
        match &path {
            Some(empty) if empty.is_empty() => {
                assert!(lib.name.is_empty());
            }
            Some(_path) => {
                // let _old_name = format!("{}.{}", path, strct.name);
                // let _new_name = format!("{}.{}", name, strct.name);
                todo!("Libraries are not working yet.")
                // let struct_id = binder.look_up_struct_id_by_name(&old_name).unwrap();
                // binder.rename_struct_by_id(struct_id, new_name);
            }
            None => {
                let struct_name = if lib.name.is_empty() {
                    binder.project.types.name_of_type_id(strct)
                } else {
                    format!("{}.{}", name, binder.project.types.name_of_type_id(strct)).into()
                };
                binder.project.types[strct]
                    .as_struct_type_mut()
                    .unwrap()
                    .name = struct_name.into();
            }
        }
    }
    if name.is_empty() {
        for function in &lib.functions {
            let function_type = binder.project.types[function.function_type].clone();
            let function_type = binder.project.types.look_up_or_add_type(function_type);

            binder.register_constant(
                function.name.clone(),
                Value::LabelPointer(function.function_label as usize, function_type),
            );
        }
    } else {
        for function in &lib.functions {
            if function.is_member_function {
                let function_type = binder.project.types[function.function_type].clone();
                let function_type = binder.project.types.look_up_or_add_type(function_type);

                binder.register_constant(
                    format!("{}.{}", name, function.name),
                    Value::LabelPointer(function.function_label as usize, function_type),
                );
            }
        }
    }
    binder.libraries.push(lib);
}
