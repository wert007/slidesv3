pub mod bound_nodes;
pub mod operators;
#[cfg(test)]
mod tests;
pub mod typing;

pub mod control_flow_analyzer;
mod lowerer;

use crate::{
    binder::{
        bound_nodes::BoundNodeKind,
        operators::BoundUnaryOperator,
        typing::{ClosureType, Type},
    },
    diagnostics::DiagnosticBag,
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    parser::{
        self,
        syntax_nodes::{
            ArrayIndexNodeKind, ArrayLiteralNodeKind, AssignmentNodeKind, BinaryNodeKind,
            BlockStatementNodeKind, ConstructorCallNodeKind, ExpressionStatementNodeKind,
            FieldAccessNodeKind, ForStatementNodeKind, FunctionCallNodeKind,
            FunctionDeclarationNodeKind, FunctionTypeNode, IfStatementNodeKind, LiteralNodeKind,
            ParameterNode, ParenthesizedNodeKind, ReturnStatementNodeKind, StructBodyNode,
            StructDeclarationNodeKind, SyntaxNode, SyntaxNodeKind, TypeNode, UnaryNodeKind,
            VariableDeclarationNodeKind, VariableNodeKind, WhileStatementNodeKind,
        },
    },
    text::{SourceText, TextSpan},
    value::Value,
    DebugFlags,
};

use self::{
    bound_nodes::BoundNode,
    operators::BoundBinaryOperator,
    typing::{FunctionType, StructType, SystemCallKind},
};

enum SmartString<'a> {
    Heap(String),
    Reference(&'a str),
}

impl From<String> for SmartString<'_> {
    fn from(value: String) -> Self {
        Self::Heap(value)
    }
}

impl<'a> From<&'a str> for SmartString<'a> {
    fn from(value: &'a str) -> Self {
        Self::Reference(value)
    }
}

impl AsRef<str> for SmartString<'_> {
    fn as_ref(&self) -> &str {
        match self {
            SmartString::Heap(str) => &str,
            SmartString::Reference(str) => str,
        }
    }
}

#[derive(Clone, Debug)]
struct FunctionDeclarationBody<'a> {
    function_name: &'a str,
    body: SyntaxNode<'a>,
    parameters: Vec<(&'a str, Type)>,
    is_main: bool,
    function_id: u64,
    function_type: Type,
    return_type: Type,
}

#[derive(Clone, Debug)]
struct StructDeclarationBody<'a> {
    name: &'a str,
    id: u64,
    body: StructBodyNode<'a>,
}

struct VariableEntry {
    id: u64,
    type_: Type,
    is_read_only: bool,
}

struct StructField<'a> {
    name: &'a str,
    type_: Type,
    is_read_only: bool,
}

struct BoundVariableName<'a> {
    pub identifier: SmartString<'a>,
    pub type_: Type,
    pub is_read_only: bool,
}

#[derive(Debug, Clone)]
pub struct BoundField<'a> {
    pub name: &'a str,
    pub offset: u64,
    pub type_: Type,
    pub is_read_only: bool,
}

#[derive(Debug, Clone)]
pub struct BoundStructType<'a> {
    pub name: &'a str,
    pub fields: Vec<BoundField<'a>>,
}

impl<'a> BoundStructType<'a> {
    pub fn field(&self, name: &'a str) -> Option<&BoundField> {
        self.fields.iter().find(|f| f.name == name)
    }
}

struct BindingState<'a, 'b> {
    diagnostic_bag: &'b mut DiagnosticBag<'a>,
    variable_table: Vec<BoundVariableName<'a>>,
    type_table: Vec<BoundVariableName<'a>>,
    struct_table: Vec<BoundStructType<'a>>,
    functions: Vec<FunctionDeclarationBody<'a>>,
    structs: Vec<StructDeclarationBody<'a>>,
    print_variable_table: bool,
    max_used_variables: usize,
    function_return_type: Type,
}

impl<'a> BindingState<'a, '_> {
    fn register_variable(&mut self, name: &'a str, type_: Type, is_read_only: bool) -> Option<u64> {
        let index = self.variable_table.len() as u64;
        self.max_used_variables = self.max_used_variables.max(index as usize + 1);
        let variable_already_registered = self
            .variable_table
            .iter()
            .any(|variable| variable.identifier.as_ref() == name);
        if variable_already_registered {
            None
        } else {
            self.variable_table.push(BoundVariableName {
                identifier: name.into(),
                type_,
                is_read_only,
            });
            Some(index)
        }
    }

    fn register_generated_variable(
        &mut self,
        name: String,
        type_: Type,
        is_read_only: bool,
    ) -> Option<u64> {
        let index = self.variable_table.len() as u64;
        self.max_used_variables = self.max_used_variables.max(index as usize + 1);
        let variable_already_registered = self
            .variable_table
            .iter()
            .any(|variable| variable.identifier.as_ref() == name);
        if variable_already_registered {
            None
        } else {
            self.variable_table.push(BoundVariableName {
                identifier: name.into(),
                type_,
                is_read_only,
            });
            Some(index)
        }
    }

    fn register_struct_name(&mut self, name: &'a str) -> Option<u64> {
        let id = self.type_table.len() as u64;
        self.register_type(name, Type::StructReference(id))
    }

    fn register_struct(&mut self, id: u64, fields: Vec<StructField<'a>>) -> u64 {
        let struct_type = StructType {
            id,
            fields: fields
                .iter()
                .filter(|f| !matches!(&f.type_, &Type::Function(_)))
                .map(|f| f.type_.clone())
                .collect(),
            functions: fields
                .iter()
                .filter(|f| matches!(&f.type_, &Type::Function(_)))
                .map(|f| f.type_.clone())
                .collect(),
        };
        self.type_table[id as usize].type_ = Type::Struct(Box::new(struct_type));
        let name = if let SmartString::Reference(it) = self.type_table[id as usize].identifier {
            it
        } else {
            unreachable!("There should be no generated Struct Names!");
        };
        let mut offset = 0;
        let bound_struct_type = BoundStructType {
            name,
            fields: fields
                .into_iter()
                .map(|field| {
                    offset += field.type_.size_in_bytes();
                    BoundField {
                        name: field.name,
                        offset: offset - field.type_.size_in_bytes(),
                        type_: field.type_,
                        is_read_only: field.is_read_only,
                    }
                })
                .collect(),
        };
        self.struct_table.push(bound_struct_type);
        id
    }

    fn register_type(&mut self, name: &'a str, type_: Type) -> Option<u64> {
        let index = self.type_table.len() as u64;
        let type_name_already_registered = self
            .type_table
            .iter()
            .any(|type_| type_.identifier.as_ref() == name);
        if type_name_already_registered {
            None
        } else {
            self.type_table.push(BoundVariableName {
                identifier: name.into(),
                type_,
                is_read_only: true,
            });
            Some(index)
        }
    }

    fn delete_variables_until(&mut self, index: usize) {
        if self.variable_table.is_empty() {
            return;
        }
        let current = self.variable_table.len() - 1;
        for _ in index..=current {
            self.variable_table.pop();
        }
    }

    fn look_up_variable_by_name(&self, name: &str) -> Option<VariableEntry> {
        self.variable_table
            .iter()
            .enumerate()
            .find(|(_, v)| v.identifier.as_ref() == name)
            .map(|(i, v)| VariableEntry {
                id: i as u64,
                type_: v.type_.clone(),
                is_read_only: v.is_read_only,
            })
    }

    fn look_up_type_by_name(&self, name: &str) -> Option<Type> {
        self.type_table
            .iter()
            .find(|v| v.identifier.as_ref() == name)
            .map(|v| v.type_.clone())
            .map(|t| {
                if let Type::Struct(struct_type) = t {
                    Type::StructReference(struct_type.id)
                } else {
                    t
                }
            })
    }

    fn get_struct_type_by_id(&self, id: u64) -> Option<&BoundStructType<'a>> {
        // FIXME: The 3 are the three primary types in the type table (int,
        // bool, and string), this could be moved to a better place i think.
        self.struct_table.get(id as usize - 3)
    }

    fn get_struct_by_id(&self, id: u64) -> Option<StructType> {
        let mut iterator =
            self.type_table
                .iter()
                .filter_map(|BoundVariableName { type_, .. }| {
                    if let Type::Struct(it) = type_ {
                        if it.id == id {
                            Some(*it.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                });
        let result = iterator.next();
        assert!(iterator.next().is_none());
        result
    }
}

fn print_variable_table(variable_table: &Vec<BoundVariableName>) {
    let mut variable_table: Vec<_> = variable_table.iter().enumerate().collect();
    variable_table.sort_unstable_by_key(|f| f.0);
    for (index, variable) in variable_table {
        println!(
            "  {:00}: {} : {}",
            index,
            variable.identifier.as_ref(),
            variable.type_
        );
    }
    println!();
}

pub struct BoundProgram<'a> {
    pub program: BoundNode<'a>,
    pub fixed_variable_count: usize,
    pub max_used_variables: usize,
}

impl BoundProgram<'_> {
    pub fn error() -> Self {
        Self {
            program: BoundNode::error(TextSpan::zero()),
            fixed_variable_count: 0,
            max_used_variables: 0,
        }
    }
}

fn type_table() -> Vec<BoundVariableName<'static>> {
    vec![
        BoundVariableName {
            identifier: "int".into(),
            type_: Type::Integer,
            is_read_only: true,
        },
        BoundVariableName {
            identifier: "bool".into(),
            type_: Type::Boolean,
            is_read_only: true,
        },
        BoundVariableName {
            identifier: "string".into(),
            type_: Type::String,
            is_read_only: true,
        },
    ]
}

pub fn bind<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> BoundProgram<'a> {
    let node = parser::parse(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return BoundProgram::error();
    }
    let mut binder = BindingState {
        diagnostic_bag,
        variable_table: vec![],
        type_table: type_table(),
        struct_table: vec![],
        functions: vec![],
        structs: vec![],
        print_variable_table: debug_flags.print_variable_table(),
        max_used_variables: 0,
        function_return_type: Type::Error,
    };
    let span = node.span();
    let mut statements = default_statements(&mut binder);
    bind_top_level_statements(node, &mut binder);
    statements.push(call_main(&mut binder));

    for node in binder.structs.clone() {
        let fields = bind_struct_body(node.name, node.body, &mut binder);
        binder.register_struct(node.id, fields);
    }

    let fixed_variable_count = binder.variable_table.len();
    let mut label_count = binder.functions.len();
    for (index, node) in binder.functions.clone().into_iter().enumerate() {
        let mut parameters = Vec::with_capacity(node.parameters.len());
        for (variable_name, variable_type) in node.parameters {
            if let Some(it) = binder.register_variable(variable_name, variable_type, false) {
                parameters.push(it);
            } else {
                // binder.diagnostic_bag.report_cannot_declare_variable(span, variable_name)
                panic!("Could not declare a parameter! This sounds like an error in the binder!");
            }
        }
        binder.function_return_type = node.return_type;
        let mut body = bind_node(node.body, &mut binder);
        if matches!(&binder.function_return_type, Type::Void) {
            body = BoundNode::block_statement(
                body.span,
                vec![
                    body,
                    BoundNode::return_statement(TextSpan::zero(), None, !node.is_main),
                ],
            );
        }
        let body_statements = lowerer::flatten(body, &mut label_count);
        if !matches!(&binder.function_return_type, Type::Void)
            && !control_flow_analyzer::check_if_all_paths_return(
                node.function_name,
                &body_statements,
                debug_flags,
            )
        {
            binder
                .diagnostic_bag
                .report_missing_return_statement(span, &binder.function_return_type);
        }
        let body = BoundNode::block_statement(span, body_statements);
        statements.insert(
            0,
            BoundNode::assignment(
                TextSpan::zero(),
                BoundNode::variable(TextSpan::zero(), node.function_id, node.function_type),
                BoundNode::label_reference(index),
            ),
        );
        statements.push(BoundNode::function_declaration(
            index,
            node.is_main,
            body,
            parameters,
        ));
        binder.delete_variables_until(fixed_variable_count);
    }

    let program = BoundNode::block_statement(span, statements);
    if debug_flags.print_bound_program {
        crate::debug::print_bound_node_as_code(&program);
    }
    if debug_flags.print_struct_table {
        for (id, entry) in binder.struct_table.iter().enumerate() {
            println!("  {}: {:#?}", id, entry);
        }
    }
    BoundProgram {
        program,
        fixed_variable_count,
        max_used_variables: binder.max_used_variables,
    }
}

fn default_statements<'a, 'b>(binder: &mut BindingState<'a, 'b>) -> Vec<BoundNode<'a>> {
    let span = TextSpan::zero();
    let variable_index = binder
        .register_variable("print", Type::SystemCall(SystemCallKind::Print), true)
        .unwrap();
    let print_statement = BoundNode::variable_declaration(
        span,
        variable_index,
        BoundNode::literal_from_value(Value::SystemCall(SystemCallKind::Print)),
    );
    vec![print_statement]
}

fn call_main<'a, 'b>(binder: &mut BindingState<'a, 'b>) -> BoundNode<'a> {
    let span = TextSpan::zero();
    let base = binder
        .look_up_variable_by_name("main")
        .expect("No main function found..");
    let base = BoundNode::variable(span, base.id, base.type_);
    let call_main = BoundNode::function_call(span, base, vec![], false, Type::Void);
    BoundNode::expression_statement(span, call_main)
}

fn bind_top_level_statements<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) {
    match node.kind {
        SyntaxNodeKind::CompilationUnit(compilation_unit) => {
            for statement in compilation_unit.statements {
                bind_top_level_statement(statement, binder);
            }
        }
        SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
            bind_function_declaration(*function_declaration, binder)
        }
        SyntaxNodeKind::StructDeclaration(struct_declaration) => {
            bind_struct_declaration(struct_declaration, binder)
        }
        _ => binder
            .diagnostic_bag
            .report_invalid_top_level_statement(node.span(), node.kind),
    }
}

fn bind_top_level_statement<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) {
    match node.kind {
        SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
            bind_function_declaration(*function_declaration, binder)
        }
        SyntaxNodeKind::StructDeclaration(struct_declaration) => {
            bind_struct_declaration(struct_declaration, binder)
        }
        _ => {
            binder
                .diagnostic_bag
                .report_invalid_top_level_statement(node.span(), node.kind);
        }
    }
}

fn bind_function_declaration<'a, 'b>(
    function_declaration: FunctionDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    let (function_type, variables) =
        bind_function_type(None, function_declaration.function_type, binder);
    let type_ = Type::Function(Box::new(function_type.clone()));
    let is_main = function_declaration.identifier.lexeme == "main";
    let function_id = if let Some(it) =
        binder.register_variable(function_declaration.identifier.lexeme, type_.clone(), false)
    {
        it
    } else {
        binder.diagnostic_bag.report_cannot_declare_variable(
            function_declaration.identifier.span(),
            function_declaration.identifier.lexeme,
        );
        0
    };
    binder.functions.push(FunctionDeclarationBody {
        function_name: function_declaration.identifier.lexeme,
        body: *function_declaration.body,
        parameters: variables,
        is_main,
        function_id,
        function_type: type_,
        return_type: function_type.return_type,
    });
}

fn bind_function_declaration_for_struct<'a, 'b>(
    struct_name: &'a str,
    function_declaration: FunctionDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> StructField<'a> {
    let struct_type = binder.look_up_type_by_name(struct_name).unwrap();
    let (function_type, mut variables) = bind_function_type(
        Some(struct_type.clone()),
        function_declaration.function_type,
        binder,
    );
    variables.push(("this", struct_type));
    let function_name = format!(
        "{}::{}",
        struct_name, function_declaration.identifier.lexeme
    );
    let type_ = Type::Function(Box::new(function_type.clone()));
    let function_id = if let Some(it) =
        binder.register_generated_variable(function_name.clone(), type_.clone(), true)
    {
        it
    } else {
        binder.diagnostic_bag.report_cannot_declare_variable(
            function_declaration.identifier.span(),
            function_declaration.identifier.lexeme,
        );
        0
    };
    binder.functions.push(FunctionDeclarationBody {
        function_name: function_declaration.identifier.lexeme,
        body: *function_declaration.body,
        parameters: variables,
        is_main: false,
        function_id,
        function_type: type_.clone(),
        return_type: function_type.return_type,
    });
    StructField {
        name: function_declaration.identifier.lexeme,
        type_,
        is_read_only: true,
    }
}

fn bind_struct_declaration<'a, 'b>(
    struct_declaration: StructDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    if let Some(id) = binder.register_struct_name(struct_declaration.identifier.lexeme) {
        binder.structs.push(StructDeclarationBody {
            name: struct_declaration.identifier.lexeme,
            id,
            body: *struct_declaration.body,
        });
    } else {
        binder.diagnostic_bag.report_cannot_declare_struct(
            struct_declaration.identifier.span(),
            struct_declaration.identifier.lexeme,
        );
    }
}

fn bind_function_type<'a>(
    this_type: Option<Type>,
    function_type: FunctionTypeNode<'a>,
    binder: &mut BindingState<'a, '_>,
) -> (FunctionType, Vec<(&'a str, Type)>) {
    let variable_count = binder.variable_table.len();
    let mut parameters = vec![];
    for parameter in function_type.parameters {
        let parameter = bind_parameter(parameter, binder);
        binder.register_generated_variable(parameter.0.into(), parameter.1.clone(), false);
        parameters.push(parameter);
    }
    let return_type = if let Some(it) = function_type.return_type {
        bind_type(it.return_type, binder)
    } else {
        Type::Void
    };
    binder.delete_variables_until(variable_count);
    (
        FunctionType {
            parameter_types: parameters.clone().into_iter().map(|(_, t)| t).collect(),
            this_type,
            return_type,
            system_call_kind: None,
        },
        parameters,
    )
}

fn bind_struct_body<'a>(
    struct_name: &'a str,
    struct_body: StructBodyNode<'a>,
    binder: &mut BindingState<'a, '_>,
) -> Vec<StructField<'a>> {
    let mut result = vec![];
    for statement in struct_body.statements {
        let span = statement.span;
        let field = match statement.kind {
            SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
                bind_function_declaration_for_struct(struct_name, *function_declaration, binder)
            }
            SyntaxNodeKind::StructField(struct_field) => {
                let (name, type_) = bind_parameter(struct_field.field, binder);
                StructField {
                    name,
                    type_,
                    is_read_only: false,
                }
            }
            unexpected => unreachable!("Unexpected Struct Member {:#?} found!", unexpected),
        };
        if result
            .iter()
            .find(|f: &&StructField| f.name == field.name)
            .is_some()
        {
            binder
                .diagnostic_bag
                .report_parameter_already_declared(span, field.name);
        }
        result.push(field);
    }
    result
}

fn bind_parameter<'a>(parameter: ParameterNode<'a>, binder: &mut BindingState) -> (&'a str, Type) {
    let name = parameter.identifier.lexeme;
    let type_ = bind_type(parameter.type_, binder);
    (name, type_)
}

fn bind_type(type_: TypeNode, binder: &mut BindingState) -> Type {
    let type_name = type_.identifier.lexeme;
    let mut result = binder.look_up_type_by_name(type_name).unwrap_or_else(|| {
        binder
            .diagnostic_bag
            .report_unknown_type(type_.span(), type_name);
        Type::Error
    });
    for _ in &type_.brackets {
        result = Type::array(result);
    }
    result
}

fn bind_node<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) -> BoundNode<'a> {
    match node.kind {
        SyntaxNodeKind::Literal(literal) => bind_literal(node.span, literal, binder),
        SyntaxNodeKind::ArrayLiteral(array_literal) => {
            bind_array_literal(node.span, array_literal, binder)
        }
        SyntaxNodeKind::ConstructorCall(constructor_call) => {
            bind_constructor_call(node.span, constructor_call, binder)
        }
        SyntaxNodeKind::Variable(variable) => bind_variable(node.span, variable, binder),
        SyntaxNodeKind::Binary(binary) => bind_binary(node.span, binary, binder),
        SyntaxNodeKind::Unary(unary) => bind_unary(node.span, unary, binder),
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            bind_parenthesized(node.span, parenthesized, binder)
        }
        SyntaxNodeKind::FunctionCall(function_call) => {
            bind_function_call(node.span, function_call, binder)
        }
        SyntaxNodeKind::ArrayIndex(array_index) => bind_array_index(node.span, array_index, binder),
        SyntaxNodeKind::FieldAccess(field_access) => {
            bind_field_access(node.span, field_access, binder)
        }
        SyntaxNodeKind::BlockStatement(block_statement) => {
            bind_block_statement(node.span, block_statement, binder)
        }
        SyntaxNodeKind::ForStatement(for_statement) => {
            bind_for_statement(node.span, for_statement, binder)
        }
        SyntaxNodeKind::IfStatement(if_statement) => {
            bind_if_statement(node.span, if_statement, binder)
        }
        SyntaxNodeKind::VariableDeclaration(variable_declaration) => {
            bind_variable_declaration(node.span, variable_declaration, binder)
        }
        SyntaxNodeKind::ReturnStatement(return_statement) => {
            bind_return_statement(node.span, return_statement, binder)
        }
        SyntaxNodeKind::WhileStatement(while_statement) => {
            bind_while_statement(node.span, while_statement, binder)
        }
        SyntaxNodeKind::Assignment(assignment) => bind_assignment(node.span, assignment, binder),
        SyntaxNodeKind::ExpressionStatement(expression_statement) => {
            bind_expression_statement(node.span, expression_statement, binder)
        }
        _ => unreachable!(),
    }
}

fn bind_node_for_assignment<'a, 'b>(
    node: SyntaxNode<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    match node.kind {
        SyntaxNodeKind::Variable(variable) => {
            bind_variable_for_assignment(node.span, variable, binder)
        }
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            bind_node_for_assignment(*parenthesized.expression, binder)
        }
        SyntaxNodeKind::FunctionCall(_) => todo!(),
        SyntaxNodeKind::ArrayIndex(array_index) => {
            bind_array_index_for_assignment(node.span, array_index, binder)
        }
        SyntaxNodeKind::FieldAccess(field_access) => {
            bind_field_access_for_assignment(node.span, field_access, binder)
        }
        _ => unreachable!("Unexpected left hand side of assignment {:#?}", node),
    }
}

fn bind_literal<'a>(
    span: TextSpan,
    literal: LiteralNodeKind<'a>,
    _: &mut BindingState,
) -> BoundNode<'a> {
    BoundNode::literal(span, literal)
}

fn bind_array_literal<'a, 'b>(
    span: TextSpan,
    mut array_literal: ArrayLiteralNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let first_element = array_literal.children.remove(0);
    let first_element = bind_node(first_element, binder);
    let type_ = first_element.type_.clone();
    let mut children = vec![first_element];
    for child in array_literal.children {
        let child_span = child.span;
        let child = bind_node(child, binder);
        if child.type_.can_be_converted_to(&type_) {
            children.push(child);
        } else {
            binder
                .diagnostic_bag
                .report_cannot_convert(child_span, &child.type_, &type_);
            children.push(BoundNode::error(child_span));
        }
    }
    BoundNode::array_literal(span, children, Type::array(type_))
}

fn bind_constructor_call<'a>(
    span: TextSpan,
    constructor_call: ConstructorCallNodeKind<'a>,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode<'a> {
    let type_name = constructor_call.type_name.lexeme;
    let type_name_span = constructor_call.type_name.span();
    let type_ = bind_type(
        TypeNode {
            identifier: constructor_call.type_name,
            brackets: vec![],
        },
        binder,
    );
    let struct_type = match type_ {
        Type::Struct(it) => *it,
        Type::StructReference(id) => binder.get_struct_by_id(id).unwrap(),
        _ => {
            binder
                .diagnostic_bag
                .report_unknown_type(type_name_span, type_name);
            return BoundNode::error(span);
        }
    };
    if constructor_call.arguments.len() != struct_type.fields.len() {
        binder.diagnostic_bag.report_unexpected_argument_count(
            span,
            constructor_call.arguments.len(),
            struct_type.fields.len(),
        );
    }
    let mut arguments = vec![];
    for (argument, parameter_type) in constructor_call
        .arguments
        .into_iter()
        .zip(struct_type.fields.iter())
    {
        let argument = bind_node(argument, binder);
        if !argument.type_.can_be_converted_to(parameter_type) {
            binder.diagnostic_bag.report_cannot_convert(
                argument.span,
                &argument.type_,
                parameter_type,
            );
        }
        arguments.push(argument);
    }

    BoundNode::constructor_call(span, arguments, struct_type)
}

fn bind_variable<'a, 'b>(
    span: TextSpan,
    variable: VariableNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    match binder.look_up_variable_by_name(variable.token.lexeme) {
        Some(VariableEntry { id, type_, .. }) => BoundNode::variable(span, id, type_),
        None => {
            binder
                .diagnostic_bag
                .report_variable_not_found(span, variable.token.lexeme);
            BoundNode::error(span)
        }
    }
}

fn bind_variable_for_assignment<'a, 'b>(
    span: TextSpan,
    variable: VariableNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let variable_is_read_only = binder
        .look_up_variable_by_name(variable.token.lexeme)
        .map(|ve| ve.is_read_only);
    if variable_is_read_only == Some(true) {
        binder
            .diagnostic_bag
            .report_variable_is_read_only(variable.token.span());
    }
    bind_variable(span, variable, binder)
}

fn bind_binary<'a, 'b>(
    span: TextSpan,
    binary: BinaryNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let lhs = bind_node(*binary.lhs, binder);
    let rhs = bind_node(*binary.rhs, binder);
    match bind_binary_operator(span, &lhs, binary.operator_token, &rhs, binder) {
        Some((operator_token, type_)) => BoundNode::binary(span, lhs, operator_token, rhs, type_),
        None => BoundNode::error(span),
    }
}

fn bind_binary_operator<'a, 'b>(
    span: TextSpan,
    lhs: &BoundNode,
    operator_token: SyntaxToken,
    rhs: &BoundNode,
    binder: &mut BindingState<'a, 'b>,
) -> Option<(BoundBinaryOperator, Type)> {
    let result = match operator_token.kind {
        SyntaxTokenKind::Plus => BoundBinaryOperator::ArithmeticAddition,
        SyntaxTokenKind::Minus => BoundBinaryOperator::ArithmeticSubtraction,
        SyntaxTokenKind::Star => BoundBinaryOperator::ArithmeticMultiplication,
        SyntaxTokenKind::Slash => BoundBinaryOperator::ArithmeticDivision,
        SyntaxTokenKind::EqualsEquals => BoundBinaryOperator::Equals,
        SyntaxTokenKind::BangEquals => BoundBinaryOperator::NotEquals,
        SyntaxTokenKind::LessThan => BoundBinaryOperator::LessThan,
        SyntaxTokenKind::GreaterThan => BoundBinaryOperator::GreaterThan,
        SyntaxTokenKind::LessThanEquals => BoundBinaryOperator::LessThanEquals,
        SyntaxTokenKind::GreaterThanEquals => BoundBinaryOperator::GreaterThanEquals,
        _ => unreachable!(),
    };
    match (&lhs.type_, result, &rhs.type_) {
        (lhs, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, rhs)
            if lhs == rhs && lhs != &Type::Void =>
        {
            Some((result, Type::Boolean))
        }
        (
            Type::Integer,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer,
        ) => Some((result, Type::Integer)),
        (
            Type::Integer,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer,
        ) => Some((result, Type::Boolean)),
        (Type::String, BoundBinaryOperator::ArithmeticAddition, Type::String) => {
            Some((BoundBinaryOperator::StringConcat, Type::String))
        }
        (Type::String, BoundBinaryOperator::ArithmeticAddition, _)
        | (_, BoundBinaryOperator::ArithmeticAddition, Type::String) => {
            Some((BoundBinaryOperator::StringConcat, Type::String))
        }
        (Type::Error, _, _) | (_, _, Type::Error) => None,
        _ => {
            binder.diagnostic_bag.report_no_binary_operator(
                span,
                &lhs.type_,
                operator_token.lexeme,
                &rhs.type_,
            );
            None
        }
    }
}

fn bind_unary<'a, 'b>(
    span: TextSpan,
    unary: UnaryNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let operand = bind_node(*unary.operand, binder);
    match bind_unary_operator(span, &operand, unary.operator_token, binder) {
        Some((operator_token, type_)) => BoundNode::unary(span, operator_token, operand, type_),
        None => BoundNode::error(span),
    }
}

fn bind_unary_operator<'a, 'b>(
    span: TextSpan,
    operand: &BoundNode,
    operator_token: SyntaxToken,
    binder: &mut BindingState<'a, 'b>,
) -> Option<(BoundUnaryOperator, Type)> {
    let result = match &operator_token.kind {
        SyntaxTokenKind::Plus => BoundUnaryOperator::ArithmeticIdentity,
        SyntaxTokenKind::Minus => BoundUnaryOperator::ArithmeticNegate,
        SyntaxTokenKind::Bang => todo!(),
        _ => unreachable!(),
    };
    match operand.type_ {
        Type::Integer => Some((result, Type::Integer)),
        Type::Error
        | Type::Void
        | Type::Any
        | Type::SystemCall(_)
        | Type::Function(_)
        | Type::Closure(_)
        | Type::Struct(_)
        | Type::StructReference(_)
        | Type::Boolean
        | Type::String
        | Type::Array(_) => {
            binder.diagnostic_bag.report_no_unary_operator(
                span,
                operator_token.lexeme,
                &operand.type_,
            );
            None
        }
    }
}

fn bind_parenthesized<'a, 'b>(
    _: TextSpan,
    parenthesized: ParenthesizedNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    bind_node(*parenthesized.expression, binder)
}

fn function_type(type_: &Type) -> FunctionType {
    match type_ {
        Type::SystemCall(SystemCallKind::Print) => FunctionType {
            parameter_types: vec![Type::Any],
            this_type: None,
            return_type: Type::Void,
            system_call_kind: type_.as_system_call(),
        },
        Type::SystemCall(SystemCallKind::ArrayLength) => FunctionType {
            parameter_types: vec![],
            // Actually Array or String, but there is no way to call this system
            // call directly, so that it is already checked somewhere else.
            this_type: Some(Type::Any),
            return_type: Type::Integer,
            system_call_kind: type_.as_system_call(),
        },
        Type::Function(result) => *result.clone(),
        Type::Closure(closure) => closure.base_function_type.clone(),
        Type::Error => FunctionType::error(),
        _ => unimplemented!("Not implemented for {}", type_),
    }
}

fn bind_function_call<'a, 'b>(
    span: TextSpan,
    function_call: FunctionCallNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let argument_span = function_call.argument_span();
    let function = bind_node(*function_call.base, binder);
    let mut arguments = vec![];
    let function = if let BoundNodeKind::Closure(closure) = function.kind {
        arguments.append(&mut closure.arguments());
        match closure.function {
            typing::FunctionKind::FunctionId(id) => {
                BoundNode::variable(function.span, id, function.type_)
            }
            typing::FunctionKind::SystemCall(kind) => {
                BoundNode::literal_from_value(Value::SystemCall(kind))
            }
        }
    } else {
        function
    };
    let function_type = function_type(&function.type_);
    arguments.append(&mut bind_arguments_for_function(
        argument_span,
        function_call.arguments,
        &function_type,
        binder,
    ));
    match &function.type_ {
        Type::Error => BoundNode::error(span),
        &Type::SystemCall(system_call) => {
            BoundNode::system_call(span, system_call, arguments, function_type.return_type)
        }
        Type::Function(_) => {
            BoundNode::function_call(
                span,
                function,
                arguments,
                function_type.this_type.is_some(),
                function_type.return_type,
            )
        }
        Type::Closure(closure) => {
            if let Some(system_call) = closure.base_function_type.system_call_kind {
                if system_call == SystemCallKind::ArrayLength {
                    arguments.push(function);
                }
                BoundNode::system_call(span, system_call, arguments, function_type.return_type)
            } else {
                BoundNode::function_call(
                    span,
                    function,
                    arguments,
                    function_type.this_type.is_some(),
                    function_type.return_type,
                )
            }
        },
        _ => unreachable!(),
    }
}

fn bind_array_index<'a, 'b>(
    span: TextSpan,
    array_index: ArrayIndexNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let index_span = array_index.index.span;
    let index = bind_node(*array_index.index, binder);
    if !index.type_.can_be_converted_to(&Type::Integer) {
        binder
            .diagnostic_bag
            .report_cannot_convert(index_span, &index.type_, &Type::Integer);
    }
    let base_span = array_index.base.span;
    let base = bind_node(*array_index.base, binder);
    let type_ = match base.type_.clone() {
        Type::Array(base_type) => *base_type,
        error => {
            binder.diagnostic_bag.report_cannot_convert(
                base_span,
                &base.type_,
                &Type::array(error),
            );
            Type::Error
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_array_index_for_assignment<'a, 'b>(
    span: TextSpan,
    array_index: ArrayIndexNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let index_span = array_index.index.span;
    let index = bind_node(*array_index.index, binder);
    if !index.type_.can_be_converted_to(&Type::Integer) {
        binder
            .diagnostic_bag
            .report_cannot_convert(index_span, &index.type_, &Type::Integer);
    }
    let base_span = array_index.base.span;
    let base = bind_node_for_assignment(*array_index.base, binder);
    let type_ = match base.type_.clone() {
        Type::Array(base_type) => *base_type,
        error => {
            binder.diagnostic_bag.report_cannot_convert(
                base_span,
                &base.type_,
                &Type::array(error),
            );
            Type::Error
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_field_access<'a, 'b>(
    span: TextSpan,
    field_access: FieldAccessNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let base_span = field_access.base.span();
    let base = bind_node(*field_access.base, binder);
    match &base.type_ {
        Type::Error => base,
        Type::Any => todo!(),
        Type::StructReference(id) => {
            let field_name = field_access.field.lexeme;
            let bound_struct_type = binder
                .get_struct_type_by_id(*id)
                .unwrap_or_else(|| {
                    panic!("Referenced a struct, which doesn't exist, somehow.");
                })
                .clone();
            if let Some(field) = bound_struct_type.field(field_name) {
                if let Type::Function(function_type) = &field.type_ {
                    let function_id = binder
                        .look_up_variable_by_name(&format!(
                            "{}::{}",
                            bound_struct_type.name, field_name
                        ))
                        .unwrap();
                    BoundNode::closure(
                        span,
                        base,
                        function_id.id,
                        Type::Closure(Box::new(ClosureType {
                            base_function_type: *function_type.clone(),
                        })),
                    )
                } else {
                    BoundNode::field_access(span, base, field.offset, field.type_.clone())
                }
            } else {
                binder.diagnostic_bag.report_no_field_named_on_struct(
                    field_access.field.span(),
                    field_name,
                    bound_struct_type,
                );
                BoundNode::error(span)
            }
        }
        Type::Struct(struct_type) => {
            let field_name = field_access.field.lexeme;
            let bound_struct_type = binder
                .get_struct_type_by_id(struct_type.id)
                .unwrap_or_else(|| {
                    panic!("Referenced a struct, which doesn't exist, somehow.");
                })
                .clone();
            if let Some(field) = bound_struct_type.field(field_name) {
                if let Type::Function(function_type) = &field.type_ {
                    let function_id = binder
                        .look_up_variable_by_name(&format!(
                            "{}::{}",
                            bound_struct_type.name, field_name
                        ))
                        .unwrap();
                    BoundNode::closure(
                        span,
                        base,
                        function_id.id,
                        Type::Closure(Box::new(ClosureType {
                            base_function_type: *function_type.clone(),
                        })),
                    )
                } else {
                    BoundNode::field_access(span, base, field.offset, field.type_.clone())
                }
            } else {
                binder.diagnostic_bag.report_no_field_named_on_struct(
                    field_access.field.span(),
                    field_name,
                    bound_struct_type,
                );
                BoundNode::error(span)
            }
        }
        Type::Void
        | Type::Integer
        | Type::Boolean
        | Type::Function(_)
        | Type::Closure(_)
        | Type::SystemCall(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base_span, &base.type_);
            BoundNode::error(span)
        }
        Type::Array(_) | Type::String => {
            if field_access.field.lexeme == "length" {
                let function_type = FunctionType::system_call(SystemCallKind::ArrayLength);
                BoundNode::system_call_closure(
                    span,
                    base,
                    SystemCallKind::ArrayLength,
                    Type::Closure(Box::new(ClosureType {
                        base_function_type: function_type,
                    })),
                )
            } else {
                binder.diagnostic_bag.report_no_field_named_on_type(
                    span,
                    field_access.field.lexeme,
                    &base.type_,
                );
                BoundNode::error(span)
            }
        }
    }
}

fn bind_field_access_for_assignment<'a>(
    span: TextSpan,
    field_access: FieldAccessNodeKind<'a>,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode<'a> {
    let base = bind_node_for_assignment(*field_access.base, binder);
    match &base.type_ {
        Type::Error => base,
        Type::Any => todo!(),
        Type::Void
        | Type::Integer
        | Type::Boolean
        | Type::SystemCall(_)
        | Type::Function(_)
        | Type::Closure(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base.span, &base.type_);
            BoundNode::error(span)
        }
        Type::Array(_) | Type::String => {
            binder.diagnostic_bag.report_cannot_assign_to(span);
            BoundNode::error(span)
        }
        Type::StructReference(id) => {
            let field_name = field_access.field.lexeme;
            let bound_struct_type = binder
                .get_struct_type_by_id(*id)
                .unwrap_or_else(|| {
                    panic!("Referenced a struct, which doesn't exist, somehow.");
                })
                .clone();
            if let Some(field) = bound_struct_type.field(field_name) {
                if field.is_read_only {
                    binder.diagnostic_bag.report_cannot_assign_to(span);
                    BoundNode::error(span)
                } else {
                    BoundNode::field_access(span, base, field.offset, field.type_.clone())
                }
            } else {
                binder.diagnostic_bag.report_no_field_named_on_struct(
                    field_access.field.span(),
                    field_name,
                    bound_struct_type,
                );
                BoundNode::error(span)
            }
        }
        Type::Struct(struct_type) => {
            let field_name = field_access.field.lexeme;
            let bound_struct_type = binder
                .get_struct_type_by_id(struct_type.id)
                .unwrap_or_else(|| {
                    panic!("Referenced a struct, which doesn't exist, somehow.");
                })
                .clone();
            if let Some(field) = bound_struct_type.field(field_name) {
                if field.is_read_only {
                    binder.diagnostic_bag.report_cannot_assign_to(span);
                    BoundNode::error(span)
                } else {
                    BoundNode::field_access(span, base, field.offset, field.type_.clone())
                }
            } else {
                binder.diagnostic_bag.report_no_field_named_on_struct(
                    field_access.field.span(),
                    field_name,
                    bound_struct_type,
                );
                BoundNode::error(span)
            }
        }
    }
}

fn bind_arguments_for_function<'a, 'b>(
    span: TextSpan,
    arguments: Vec<SyntaxNode<'a>>,
    function_type: &FunctionType,
    binder: &mut BindingState<'a, 'b>,
) -> Vec<BoundNode<'a>> {
    let mut result = vec![];
    if function_type.parameter_types.len() != arguments.len() {
        binder.diagnostic_bag.report_unexpected_argument_count(
            span,
            arguments.len(),
            function_type.parameter_types.len(),
        );
    }
    for (argument, parameter_type) in arguments
        .into_iter()
        .zip(function_type.parameter_types.iter())
    {
        let argument = bind_node(argument, binder);
        if matches!(argument.type_, Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.span);
        }
        if !argument.type_.can_be_converted_to(parameter_type) {
            binder.diagnostic_bag.report_cannot_convert(
                argument.span,
                &argument.type_,
                parameter_type,
            );
        }
        if let Type::Function(_) = argument.type_ {
            if let Some(SystemCallKind::Print) = function_type.system_call_kind {
                binder
                    .diagnostic_bag
                    .report_cannot_print_type(argument.span, &argument.type_);
            }
        }
        result.push(argument);
    }
    result
}

fn bind_for_statement<'a, 'b>(
    span: TextSpan,
    for_statement: ForStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let variable_count = binder.variable_table.len();
    let collection = bind_node(*for_statement.collection, binder);
    if !matches!(collection.type_, Type::Array(_)) {
        binder.diagnostic_bag.report_cannot_convert(
            collection.span,
            &collection.type_,
            &Type::array(collection.type_.clone()),
        );
        return BoundNode::error(span);
    }
    let variable_type = collection.type_.array_base_type().unwrap();
    let variable =
        binder.register_variable(for_statement.variable.lexeme, variable_type.clone(), true);
    if variable.is_none() {
        binder.diagnostic_bag.report_cannot_declare_variable(
            for_statement.variable.span(),
            for_statement.variable.lexeme,
        );
        return BoundNode::error(span);
    }
    let variable = variable.unwrap();
    let index_variable = match for_statement.optional_index_variable {
        Some(index_variable) => {
            binder.register_variable(index_variable.lexeme, Type::Integer, true)
        }
        None => binder.register_generated_variable(
            format!("{}$index", for_statement.variable.lexeme),
            Type::Integer,
            true,
        ),
    };
    if index_variable.is_none() {
        binder.diagnostic_bag.report_cannot_declare_variable(
            for_statement.variable.span(),
            for_statement.variable.lexeme,
        );
        return BoundNode::error(span);
    }
    let index_variable = index_variable.unwrap();
    let collection_variable = binder
        .register_generated_variable(
            format!("{}$collection", for_statement.variable.lexeme),
            collection.type_.clone(),
            true,
        )
        .unwrap();
    let body = bind_node(*for_statement.body, binder);
    let variable = BoundNode::variable(span, variable, variable_type.clone());
    let result = BoundNode::for_statement(
        span,
        index_variable,
        collection_variable,
        variable,
        collection,
        body,
    );
    if binder.print_variable_table {
        print_variable_table(&binder.variable_table);
    }
    binder.delete_variables_until(variable_count);

    result
}

fn bind_if_statement<'a, 'b>(
    span: TextSpan,
    if_statement: IfStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let condition = bind_node(*if_statement.condition, binder);
    if !matches!(condition.type_, Type::Boolean) {
        binder.diagnostic_bag.report_cannot_convert(
            condition.span,
            &condition.type_,
            &Type::Boolean,
        );
    }
    let body = bind_node(*if_statement.body, binder);
    let else_body = if_statement.else_clause.map(|e| bind_node(*e.body, binder));
    BoundNode::if_statement(span, condition, body, else_body)
}

fn bind_variable_declaration<'a, 'b>(
    span: TextSpan,
    variable_declaration: VariableDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let initializer = bind_node(*variable_declaration.initializer, binder);
    if matches!(initializer.type_, Type::Void) {
        binder
            .diagnostic_bag
            .report_invalid_void_expression(initializer.span);
    }
    let variable_index = binder.register_variable(
        variable_declaration.identifier.lexeme,
        initializer.type_.clone(),
        false,
    );
    if let Some(variable_index) = variable_index {
        BoundNode::variable_declaration(span, variable_index, initializer)
    } else {
        let span = TextSpan::bounds(
            variable_declaration.let_keyword.span(),
            variable_declaration.identifier.span(),
        );
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(span, variable_declaration.identifier.lexeme);
        BoundNode::error(span)
    }
}

fn bind_return_statement<'a, 'b>(
    span: TextSpan,
    return_statement: ReturnStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let expression = return_statement
        .optional_expression
        .map(|e| bind_node(*e, binder));
    let function_return_type = &binder.function_return_type;
    if expression.is_none() && !function_return_type.can_be_converted_to(&Type::Void) {
        binder
            .diagnostic_bag
            .report_missing_return_value(span, function_return_type);
    } else if let Some(expression) = &expression {
        if !expression.type_.can_be_converted_to(&function_return_type) {
            binder.diagnostic_bag.report_cannot_convert(
                expression.span,
                &expression.type_,
                function_return_type,
            );
        }
    }
    // TODO: This works with implicit returns in main only. Because for main
    // restores_variables must be false. There needs to be a flag in the binder
    // not only knowing the return type of the current function but also if it
    // is the main function or not.
    BoundNode::return_statement(span, expression, true)
}

fn bind_while_statement<'a, 'b>(
    span: TextSpan,
    while_statement: WhileStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let condition = bind_node(*while_statement.condition, binder);
    if !matches!(condition.type_, Type::Boolean) {
        binder.diagnostic_bag.report_cannot_convert(
            condition.span,
            &condition.type_,
            &Type::Boolean,
        );
    }
    let body = bind_node(*while_statement.body, binder);
    BoundNode::while_statement(span, condition, body)
}

fn bind_assignment<'a, 'b>(
    span: TextSpan,
    assignment: AssignmentNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let lhs = bind_node_for_assignment(*assignment.lhs, binder);
    let expression = bind_node(*assignment.expression, binder);
    if !expression.type_.can_be_converted_to(&lhs.type_) {
        binder
            .diagnostic_bag
            .report_cannot_convert(span, &expression.type_, &lhs.type_);
    }
    BoundNode::assignment(span, lhs, expression)
}

fn bind_block_statement<'a, 'b>(
    span: TextSpan,
    block_statement: BlockStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let variable_count = binder.variable_table.len();
    let mut statements = vec![];
    for node in block_statement.statements {
        statements.push(bind_node(node, binder));
    }
    if binder.print_variable_table {
        print_variable_table(&binder.variable_table);
    }
    binder.delete_variables_until(variable_count);
    BoundNode::block_statement(span, statements)
}

fn bind_expression_statement<'a, 'b>(
    span: TextSpan,
    expression_statement: ExpressionStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let expression = bind_node(*expression_statement.expression, binder);
    BoundNode::expression_statement(span, expression)
}
