pub mod bound_nodes;
pub mod operators;
#[cfg(test)]
mod tests;
pub mod typing;

mod lowerer;
mod control_flow_analyzer;

use std::collections::HashMap;

use crate::{DebugFlags, binder::{operators::BoundUnaryOperator, typing::Type}, diagnostics::DiagnosticBag, lexer::syntax_token::{SyntaxToken, SyntaxTokenKind}, parser::{self, syntax_nodes::{ArrayIndexNodeKind, ArrayLiteralNodeKind, AssignmentNodeKind, BinaryNodeKind, BlockStatementNodeKind, ExpressionStatementNodeKind, FieldAccessNodeKind, ForStatementNodeKind, FunctionCallNodeKind, FunctionDeclarationNodeKind, FunctionTypeNode, IfStatementNodeKind, LiteralNodeKind, ParenthesizedNodeKind, ReturnStatementNodeKind, SyntaxNode, SyntaxNodeKind, TypeNode, UnaryNodeKind, VariableDeclarationNodeKind, VariableNodeKind, WhileStatementNodeKind}}, text::{SourceText, TextSpan}, value::Value};

use self::{
    bound_nodes::{BoundNode, BoundNodeKind},
    operators::BoundBinaryOperator,
    typing::{FunctionType, SystemCallKind},
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

struct VariableEntry {
    id: u64,
    type_: Type,
    is_read_only: bool,
}

struct BoundVariableName<'a> {
    pub identifier: SmartString<'a>,
    pub type_: Type,
    pub is_read_only: bool,
}

struct BindingState<'a, 'b> {
    diagnostic_bag: &'b mut DiagnosticBag<'a>,
    variable_table: HashMap<u64, BoundVariableName<'a>>,
    functions: Vec<FunctionDeclarationBody<'a>>,
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
            .values()
            .any(|variable| variable.identifier.as_ref() == name);
        if variable_already_registered {
            None
        } else {
            self.variable_table.insert(
                index,
                BoundVariableName {
                    identifier: name.into(),
                    type_,
                    is_read_only,
                },
            );
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
        self.max_used_variables = self.max_used_variables.max(index as _);
        let variable_already_registered = self
            .variable_table
            .values()
            .any(|variable| variable.identifier.as_ref() == name);
        if variable_already_registered {
            None
        } else {
            self.variable_table.insert(
                index,
                BoundVariableName {
                    identifier: name.into(),
                    type_,
                    is_read_only,
                },
            );
            Some(index)
        }
    }

    fn delete_variables_until(&mut self, index: usize) {
        if self.variable_table.is_empty() {
            return;
        }
        let current = self.variable_table.len() - 1;
        for i in index..=current {
            self.variable_table.remove(&(i as _));
        }
    }

    fn look_up_variable_by_name(&self, name: &str) -> Option<VariableEntry> {
        self.variable_table
            .iter()
            .find(|(_, v)| v.identifier.as_ref() == name)
            .map(|(&i, v)| VariableEntry {
                id: i,
                type_: v.type_.clone(),
                is_read_only: v.is_read_only,
            })
    }
}

fn print_variable_table(variable_table: &HashMap<u64, BoundVariableName>) {
    let mut variable_table: Vec<_> = variable_table.iter().collect();
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
        variable_table: HashMap::new(),
        functions: vec![],
        print_variable_table: debug_flags.print_variable_table(),
        max_used_variables: 0,
        function_return_type: Type::Error,
    };
    let span = node.span();
    let mut statements = default_statements(&mut binder);
    bind_top_level_statements(node, &mut binder);
    statements.push(call_main(&mut binder));

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
        let body = bind_node(node.body, &mut binder);
        let body = BoundNode::block_statement(body.span, vec![body, BoundNode::return_statement(TextSpan::zero(), None)]);
        let body = BoundNode::block_statement(span, lowerer::flatten(body, &mut label_count));
        if !matches!(&binder.function_return_type, Type::Void) {
            if !control_flow_analyzer::check_if_all_paths_return(&body) {
                binder.diagnostic_bag.report_missing_return_statement(span, &binder.function_return_type);
            }
        }
        statements.insert(0, BoundNode::assignment(
            TextSpan::zero(),
            BoundNode::variable(TextSpan::zero(), node.function_id, node.function_type),
            BoundNode::label_reference(index),
        ));
        statements.push(BoundNode::function_declaration(index, node.is_main, body, parameters));
        binder.delete_variables_until(fixed_variable_count);
    }

    let program = BoundNode::block_statement(span, statements);
    if debug_flags.print_bound_program {
        crate::debug::print_bound_node_as_code(&program);
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
    let call_main = BoundNode::function_call(span, base, vec![], Type::Void);
    let call_main = BoundNode::expression_statement(span, call_main);
    call_main
}

fn bind_top_level_statements<'a, 'b>(
    node: SyntaxNode<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    match node.kind {
        SyntaxNodeKind::CompilationUnit(compilation_unit) => {
            for statement in compilation_unit.statements {
                bind_top_level_statement(statement, binder);
            }
        }
        SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
            bind_function_declaration(function_declaration, binder)
        }
        _ => binder
            .diagnostic_bag
            .report_invalid_top_level_statement(node.span(), node.kind),
    }
}

fn bind_top_level_statement<'a, 'b>(
    node: SyntaxNode<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    match node.kind {
        SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
            bind_function_declaration(function_declaration, binder)
        }
        _ => {
            binder
                .diagnostic_bag
                .report_invalid_top_level_statement(node.span(), node.kind);
        }
    }
}

#[derive(Clone, Debug)]
struct FunctionDeclarationBody<'a> {
    body: SyntaxNode<'a>,
    parameters: Vec<(&'a str, Type)>,
    is_main: bool,
    function_id: u64,
    function_type: Type,
    return_type: Type,
}

fn bind_function_declaration<'a, 'b>(
    function_declaration: FunctionDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    let (function_type, variables) = bind_function_type(function_declaration.function_type, binder);
    let is_main = function_declaration.identifier.lexeme == "main";
    let function_id = if let Some(it) =
        binder.register_variable(function_declaration.identifier.lexeme, function_type.clone(), false)
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
        body: *function_declaration.body,
        parameters: variables,
        is_main,
        function_id,
        function_type,
        return_type: Type::Void,
    });
}

fn bind_function_type<'a>(
    function_type: FunctionTypeNode<'a>,
    binder: &mut BindingState,
) -> (Type, Vec<(&'a str, Type)>) {
    let variable_count = binder.variable_table.len();
    let mut parameter_types = Vec::with_capacity(function_type.parameters.len());
    let mut parameter_names = Vec::with_capacity(function_type.parameters.len());
    for parameter in function_type.parameters {
        let parameter_name = parameter.identifier.lexeme;
        let parameter_type = bind_type(parameter.type_, binder);
        parameter_types.push(parameter_type.clone());
        parameter_names.push(parameter_name);
        binder.register_generated_variable(parameter_name.into(), parameter_type, false);
    }
    binder.delete_variables_until(variable_count);
    let variables: Vec<(&'a str, Type)> = parameter_names
        .into_iter()
        .zip(parameter_types.iter().map(|t| t.clone()))
        .collect();
    (Type::function(parameter_types), variables)
}

fn bind_type(type_: TypeNode, binder: &mut BindingState) -> Type {
    let mut result = match type_.identifier.lexeme {
        "int" => Type::Integer,
        "string" => Type::String,
        "bool" => Type::Boolean,
        type_name => {
            binder
                .diagnostic_bag
                .report_unknown_type(type_.span(), type_name);
            return Type::Error;
        }
    };
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
    let this = match &function.kind {
        BoundNodeKind::FieldAccess(field_access) => Some(*field_access.base.clone()),
        _ => None,
    };
    let function_type = function_type(&function.type_);
    // match (&this, &function_type.this_type) {
    //     (Some(this), Some(type_)) => {
    //         if !this.type_.can_be_converted_to(type_) {
    //             binder.diagnostic_bag.report_cannot_convert(this.span, &this.type_, type_);
    //             return BoundNode::error(span);
    //         }
    //     }
    //     (None, None) => {},
    //     (Some(this), None) => {
    //         binder.diagnostic_bag.report_expected_no_this(this.span, &function_type);
    //         return BoundNode::error(span)
    //     }
    //     (None, Some(_)) => {
    //         binder.diagnostic_bag.report_expected_this_parameter(function.span, &function_type);
    //         return BoundNode::error(span)
    //     }
    // }
    let mut arguments = bind_arguments_for_function(
        argument_span,
        function_call.arguments,
        &function_type,
        binder,
    );
    if function_type.this_type.is_some() {
        arguments.insert(
            0,
            this.unwrap_or_else(|| {
                function.clone() // Wrong Type, wrong idea!
            }),
        );
    }
    if let Type::SystemCall(system_call) = function.type_ {
        return BoundNode::system_call(span, system_call, arguments, function_type.return_type);
    }
    BoundNode::function_call(span, function, arguments, function_type.return_type)
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
        Type::Error => todo!(),
        Type::Any => todo!(),
        Type::Void | Type::Integer | Type::Boolean | Type::Function(_) | Type::SystemCall(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base_span, &base.type_);
            BoundNode::error(span)
        }
        Type::Array(_) | Type::String => {
            if field_access.field.lexeme == "length" {
                BoundNode::field_access(span, base, Type::SystemCall(SystemCallKind::ArrayLength))
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

fn bind_arguments_for_function<'a, 'b>(
    span: TextSpan,
    arguments: Vec<SyntaxNode<'a>>,
    function_type: &FunctionType,
    binder: &mut BindingState<'a, 'b>,
) -> Vec<BoundNode<'a>> {
    let mut result = vec![];
    if matches!(function_type.system_call_kind, Some(SystemCallKind::Print)) && arguments.len() != 1
    {
        binder
            .diagnostic_bag
            .report_unexpected_argument_count(span, arguments.len(), 1);
    }
    for argument in arguments {
        let argument = bind_node(argument, binder);
        if matches!(argument.type_, Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.span);
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
    let result = BoundNode::for_statement(span, index_variable, collection_variable, variable, collection, body);
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
    let expression = return_statement.optional_expression.map(|e| bind_node(*e, binder));
    let function_return_type = &binder.function_return_type;
    if expression.is_none() && !function_return_type.can_be_converted_to(&Type::Void) {
        binder.diagnostic_bag.report_missing_return_value(span, function_return_type);
    } else if let Some(expression) = &expression {
        if !expression.type_.can_be_converted_to(&function_return_type) {
            binder.diagnostic_bag.report_cannot_convert(expression.span, &expression.type_, function_return_type);
        }
    }
    BoundNode::return_statement(span, expression)
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
