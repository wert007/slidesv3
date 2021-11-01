pub mod bound_nodes;
pub mod operators;
#[cfg(test)]
mod tests;
pub mod typing;

pub mod control_flow_analyzer;
mod lowerer;

use crate::{DebugFlags, binder::{
        bound_nodes::{is_same_expression::IsSameExpression, BoundNodeKind},
        operators::{BoundBinary, BoundUnaryOperator},
        typing::Type,
    }, dependency_resolver, diagnostics::DiagnosticBag, lexer::syntax_token::{SyntaxToken, SyntaxTokenKind}, parser::{syntax_nodes::{ArrayIndexNodeKind, ArrayLiteralNodeKind, AssignmentNodeKind, BinaryNodeKind, BlockStatementNodeKind, CastExpressionNodeKind, ConstDeclarationNodeKind, ConstructorCallNodeKind, ExpressionStatementNodeKind, FieldAccessNodeKind, ForStatementNodeKind, FunctionCallNodeKind, FunctionDeclarationNodeKind, FunctionTypeNode, IfStatementNodeKind, LiteralNodeKind, ParameterNode, ParenthesizedNodeKind, ReturnStatementNodeKind, StructBodyNode, StructDeclarationNodeKind, SyntaxNode, SyntaxNodeKind, TypeNode, UnaryNodeKind, VariableDeclarationNodeKind, VariableNodeKind, WhileStatementNodeKind}}, text::{SourceText, TextSpan}, value::Value};

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

#[derive(Debug, Clone)]
enum SafeNodeInCondition<'a> {
    None,
    IfBody(BoundNode<'a>, Type),
    ElseBody(BoundNode<'a>, Type),
}

impl SafeNodeInCondition<'_> {
    pub fn negate(self) -> Self {
        match self {
            SafeNodeInCondition::None => self,
            SafeNodeInCondition::IfBody(node, safe_type) => Self::ElseBody(node, safe_type),
            SafeNodeInCondition::ElseBody(node, safe_type) => Self::IfBody(node, safe_type),
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
    is_struct_function: bool,
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
    /// The diagnostic bag collects all errors the user supplied code has. They
    /// will be outputted after the stage they first occurred. So if there are
    /// type conversion errors, the compiler will not try to turn the bound
    /// program into instructions.
    diagnostic_bag: &'b mut DiagnosticBag<'a>,
    /// Every variable is turned into an variable index by the binder, since
    /// variables are turned into registers and those are accessed by indices.
    /// The table stores the index as index of the vector, the name of the
    /// variable, the type it has and if it is read only, which means it cannot
    /// be on the left side of an assignment.
    variable_table: Vec<BoundVariableName<'a>>,
    /// Every (struct) type is also turned into a type index by the binder. This
    /// allows for binding recursive structs, since they only held references to
    /// other structs, the type of the field should also be only a reference to
    /// the other type of the struct. Also contains primitive types, since the
    /// type table is used to look up a type by its name. The type table is also
    /// used by the diagnostic bag to pretty print the type names, since the Type
    /// enum can/should not contain strings. Is read only is always set to true,
    /// but is actually not really read from, since trying to assign to a type
    /// would already be a whole lot of other errors in the user supplied
    /// program.
    type_table: Vec<BoundVariableName<'a>>,
    /// When registering a type for the type table, it is also registered in the
    /// struct table if it is a struct. The struct table is used to access field
    /// information on a struct. Since the type of a struct only has the types of
    /// the fields and their offset into the struct it cannot be used to check if
    /// a struct has a field with a certain name. This is what this is used for.
    /// So this is used by constructors or field accesses. Fields can also be
    /// read only, even though this is currently (12.10.2021) only used for
    /// functions on structs since they are also fields in some way. But
    /// functions on structs are still registered in the functions table.
    struct_table: Vec<BoundStructType<'a>>,
    /// This contains all function declarations the binder found while walking
    /// the top level statements and after that the struct fields. The function
    /// declaration body contains the necessary information for the function
    /// signature, like the name, the paremeters (but the name of a paremeter is
    /// not part of the signature) and the return type (but also not really part
    /// of the signature, it is only needed to bind a function call, before the
    /// binder knows, what the function does and returns.). The body is needed to
    /// later bind the function, if all types and functions are known. The
    /// function id is equal to the variable index of the function, since all
    /// functions are just stored in variables, and the function type is the type
    /// of the variable. Is struct and is main is used in some places to
    /// differentiate them.
    functions: Vec<FunctionDeclarationBody<'a>>,
    /// This collects all the structs, which fields still need to be bound by the
    /// binder. This should be empty, before starting to bind the functions.
    structs: Vec<StructDeclarationBody<'a>>,
    /// This is a simple hack to bring constants into the binder.
    constants: Vec<Value>,
    /// These safe nodes are the nodes which are normally of a noneable type, but
    /// are currently be known to have an actual value and not be none. Next to
    /// the actual safe node is its safe type stored, which it should be
    /// converted to, when found in the program. This is a vector of option,
    /// since they are accessed by an index, but also can be unsafe, in a
    /// different order, then they went safe. So if you would remove a unsafe
    /// node in the middle of the vector, the indices of all following nodes
    /// would be wrong now. So this is why this is used in a vector. This may
    /// also be done in a HashMap, but finding the next index would be harder
    /// then, I think.
    safe_nodes: Vec<Option<(BoundNode<'a>, Type)>>,
    /// This is the debug flag. This will print all variable names and their
    /// indices, when ever some go out of scope. (Self::delete_variables_until())
    print_variable_table: bool,
    /// This is used to preallocate the registers.
    max_used_variables: usize,
    /// The return type of the current function. Is Type::Error if not in a
    /// function. This is needed by return statements to ensure they return the
    /// correct value.
    function_return_type: Type,
    /// This is used to reserve a this variable inside the function.
    is_struct_function: bool,
    /// The type the current expression is expected to have. This is only used in
    /// bind_array_literal, to convert each expression inside the array literal
    /// to the right type. Everywhere else the expression is converted
    /// afterwards.
    expected_type: Option<Type>,
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

    fn register_constant(&mut self, variable_index: u64, value: Value) {
        while self.constants.len() <= variable_index as _ {
            self.constants.push(Value::None);
        }
        self.constants[variable_index as usize] = value;
    }

    fn register_safe_node(&mut self, node: BoundNode<'a>, safe_type: Type) -> usize {
        if let Some(index) = self.safe_nodes.iter().position(|n| n.is_none()) {
            self.safe_nodes[index] = Some((node, safe_type));
            index
        } else {
            let index = self.safe_nodes.len();
            self.safe_nodes.push(Some((node, safe_type)));
            index
        }
    }

    fn delete_safe_node(&mut self, index: usize) {
        // NOTE: Maybe shrink the vector if there are many unused nodes? Maybe
        // this would make allocating a new node faster. Or maybe actually don't
        // refill "tombstones" (None values) with values but remove them, once
        // they are many (all?) at the end.
        self.safe_nodes[index] = None;
    }

    fn get_safe_node(&self, node: &BoundNode<'a>) -> Option<BoundNode<'a>> {
        self.safe_nodes
            .iter()
            .filter_map(|n| n.as_ref())
            .find(|(n, _)| n.is_same_expression(node))
            .map(|(n, t)| BoundNode::conversion(n.span, n.clone(), t.clone()))
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

    fn get_variable_name_by_id(&self, id: u64) -> Option<&str> {
        self.variable_table
            .get(id as usize)
            .map(|v| v.identifier.as_ref())
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
        // FIXME: The 4 represents the four primary types in the type table
        // (int, bool, string, and any), this could be moved to a better place I
        // think.
        self.struct_table.get(id as usize - 4)
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

fn print_variable_table(variable_table: &[BoundVariableName]) {
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
    pub label_count: usize,
}

impl BoundProgram<'_> {
    pub fn error() -> Self {
        Self {
            program: BoundNode::error(TextSpan::zero()),
            fixed_variable_count: 0,
            max_used_variables: 0,
            label_count: 0,
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
        BoundVariableName {
            identifier: "any".into(),
            type_: Type::Any,
            is_read_only: true,
        },
    ]
}

pub fn bind<'a>(
    source_text: &'a SourceText<'a>,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> BoundProgram<'a> {
    let node = dependency_resolver::resolve(source_text, diagnostic_bag, debug_flags);
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
        constants: vec![],
        safe_nodes: vec![],
        print_variable_table: debug_flags.print_variable_table(),
        max_used_variables: 0,
        function_return_type: Type::Error,
        is_struct_function: false,
        expected_type: None,
    };
    let span = node.span();
    let mut statements = default_statements(&mut binder);
    bind_top_level_statements(node, &mut binder);
    for (index, constant) in binder.constants.iter().enumerate() {
        statements.push(BoundNode::assignment(TextSpan::zero(), BoundNode::variable(TextSpan::zero(), index as _, constant.infer_type()), BoundNode::literal_from_value(constant.clone())));
    }
    statements.push(call_main(&mut binder));

    for node in binder.structs.clone() {
        let fields = bind_struct_body(node.name, node.body, &mut binder);
        binder.register_struct(node.id, fields);
    }

    let mut type_names = binder
        .type_table
        .iter()
        .map(|b| b.identifier.as_ref().to_owned())
        .collect();
    binder
        .diagnostic_bag
        .registered_types
        .append(&mut type_names);

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
        binder.is_struct_function = node.is_struct_function;
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
        label_count,
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
        None,
    );
    let variable_index = binder
        .register_variable(
            "heapdump",
            Type::SystemCall(SystemCallKind::DebugHeapDump),
            true,
        )
        .unwrap();
    let heapdump_statement = BoundNode::variable_declaration(
        span,
        variable_index,
        BoundNode::literal_from_value(Value::SystemCall(SystemCallKind::DebugHeapDump)),
        None,
    );
    vec![print_statement, heapdump_statement]
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
        _ => unreachable!(),
    }
}

fn bind_top_level_statement<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) {
    match node.kind {
        SyntaxNodeKind::ConstDeclaration(const_declaration) => {
            bind_const_declaration(const_declaration, binder)
        }
        SyntaxNodeKind::ImportStatement(_) => {
            unreachable!("Imports should be resolved by dependency_resolver!");
        }
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

fn bind_const_declaration<'a>(
    const_declaration: ConstDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, '_>,
) {
    let initializer = bind_node(*const_declaration.initializer, binder);
    let index = binder.register_variable(const_declaration.identifier.lexeme, initializer.type_.clone(), true);
    let index = if let Some(it) = index {
        it
    } else {
        binder.diagnostic_bag.report_cannot_declare_variable(const_declaration.identifier.span(), const_declaration.identifier.lexeme);
        return;
    };
    if let Some(constant_value) = initializer.constant_value {
        binder.register_constant(index, constant_value.value);
    } else {
        binder.diagnostic_bag.report_expected_constant(initializer.span);
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
        is_struct_function: false,
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
    let function_id =
        if let Some(it) = binder.register_generated_variable(function_name, type_.clone(), true) {
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
        is_struct_function: true,
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
    let mut result: Vec<StructField<'a>> = vec![];
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
        if result.iter().any(|f| f.name == field.name) {
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
    let type_ = bind_type(parameter.type_declaration.type_, binder);
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
    if type_.optional_question_mark.is_some() {
        result = Type::noneable(result);
    }
    for _ in &type_.brackets {
        result = Type::array(result);
    }
    result
}

fn bind_node<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) -> BoundNode<'a> {
    let result = match node.kind {
        SyntaxNodeKind::Literal(literal) => bind_literal(node.span, literal, binder),
        SyntaxNodeKind::ArrayLiteral(array_literal) => {
            bind_array_literal(node.span, array_literal, binder)
        }
        SyntaxNodeKind::CastExpression(cast_expression) => {
            bind_cast_expression(node.span, cast_expression, binder)
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
    };
    if let Some(result) = binder.get_safe_node(&result) {
        result
    } else {
        result
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
    let expected_type = if let Some(Type::Array(base_type)) = &binder.expected_type {
        Some(*base_type.clone())
    } else {
        None
    };
    let first_child = array_literal.children.remove(0);
    let (type_, mut children) = bind_array_literal_first_child(first_child, expected_type, binder);
    for child in array_literal.children {
        let (child, repetition) = if let SyntaxNodeKind::RepetitionNode(repetition_node) = child.kind {
            let base_expression = bind_node(*repetition_node.base_expression, binder);
            let repetition = bind_node(*repetition_node.repetition, binder);
            let repetition = bind_conversion(repetition, &Type::Integer, binder);
            let repetition = match repetition.constant_value {
                Some(value) => value.value.as_integer().unwrap(),
                None => {
                    binder.diagnostic_bag.report_expected_constant(repetition.span);
                    1
                },
            } as usize;
            (base_expression, repetition)
        } else {
            (bind_node(child, binder), 1)
        };
        let child = bind_conversion(child, &type_, binder);
        for _ in 0..repetition {
            children.push(child.clone());
        }
    }
    BoundNode::array_literal(span, children, Type::array(type_.clone()))
}

fn bind_array_literal_first_child<'a>(first_child: SyntaxNode<'a>, expected_type: Option<Type>, binder: &mut BindingState<'a, '_>) -> (Type, Vec<BoundNode<'a>>) {
    let children = if let SyntaxNodeKind::RepetitionNode(repetition_node) = first_child.kind {
        let base_expression = bind_node(*repetition_node.base_expression, binder);
        let repetition = bind_node(*repetition_node.repetition, binder);
        let repetition = bind_conversion(repetition, &Type::Integer, binder);
        let repetition = match repetition.constant_value {
            Some(value) => value.value.as_integer().unwrap(),
            None => {
                binder.diagnostic_bag.report_expected_constant(repetition.span);
                1
            },
        } as usize;
        vec![base_expression;repetition]
    } else {
        vec![bind_node(first_child, binder)]
    };
    let type_ = expected_type.unwrap_or(children[0].type_.clone());
    let children = children.into_iter().map(|c| bind_conversion(c, &type_, binder)).collect();
    (type_, children)
}

fn bind_cast_expression<'a>(
    span: TextSpan,
    cast_expression: CastExpressionNodeKind<'a>,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode<'a> {
    let expression = bind_node(*cast_expression.expression, binder);
    let type_ = bind_type(cast_expression.type_, binder);
    // Cast unnecessary and will always return a valid value.
    if expression.type_.can_be_converted_to(&type_) {
        binder
            .diagnostic_bag
            .report_unnecessary_cast(span, &expression.type_, &type_);
        return bind_conversion(expression, &type_, binder);
    }
    if !expression.type_.can_be_casted_to(&type_) {
        binder
            .diagnostic_bag
            .report_impossible_cast(span, &expression.type_, &type_);
        return BoundNode::error(span);
    }
    let type_ = Type::noneable(type_);
    if expression.type_.can_be_converted_to(&type_) {
        binder.diagnostic_bag.report_unnecessary_cast(
            span,
            &expression.type_,
            type_.noneable_base_type().unwrap(),
        );
        return bind_conversion(expression, &type_, binder);
    }
    BoundNode::conversion(span, expression, type_)
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
            optional_question_mark: None,
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
        let argument = bind_conversion(argument, parameter_type, binder);
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
        Some(bound_binary) => {
            let lhs = bind_conversion(lhs, &bound_binary.lhs, binder);
            let rhs = bind_conversion(rhs, &bound_binary.rhs, binder);
            BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
        }
        None => BoundNode::error(span),
    }
}

fn bind_binary_operator<'a, 'b>(
    span: TextSpan,
    lhs: &BoundNode,
    operator_token: SyntaxToken,
    rhs: &BoundNode,
    binder: &mut BindingState<'a, 'b>,
) -> Option<BoundBinary> {
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
        SyntaxTokenKind::QuestionMarkQuestionMark => BoundBinaryOperator::NoneableOrValue,
        _ => unreachable!(),
    };
    match (&lhs.type_, result, &rhs.type_) {
        (lhs, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, rhs)
            if lhs == rhs && lhs != &Type::Void =>
        {
            Some(BoundBinary::same_input(lhs, result, Type::Boolean))
        }
        (
            Type::Noneable(inner),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            outer,
        )
        | (
            outer,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Noneable(inner),
        ) if inner.as_ref() == outer && outer != &Type::Void => Some(BoundBinary::same_input(
            &Type::Noneable(inner.clone()),
            result,
            Type::Boolean,
        )),
        (
            Type::Noneable(inner),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::None,
        )
        | (
            Type::None,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Noneable(inner),
        ) if inner.as_ref() != &Type::Void => Some(BoundBinary::same_input(
            &Type::Noneable(inner.clone()),
            result,
            Type::Boolean,
        )),
        (
            Type::Integer,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer,
        ) => Some(BoundBinary::same_output(result, Type::Integer)),
        (
            Type::Integer,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer,
        ) => Some(BoundBinary::same_input(
            &Type::Integer,
            result,
            Type::Boolean,
        )),
        (Type::String, BoundBinaryOperator::ArithmeticAddition, Type::String) => Some(
            // Currently a call to to$string will be emitted. And for that call
            // both sides need to be of type any. There is an optimization here,
            // where there might be two versions of StringConcat and only one of
            // those works with to$string.
            //
            // BoundBinary::same_output(BoundBinaryOperator::StringConcat, Type::String),
            BoundBinary::same_input(&Type::Any, BoundBinaryOperator::StringConcat, Type::String),
        ),
        (Type::String, BoundBinaryOperator::ArithmeticAddition, _) => Some(BoundBinary::new(
            &Type::Any,
            BoundBinaryOperator::StringConcat,
            &Type::Any,
            Type::String,
        )),
        (_, BoundBinaryOperator::ArithmeticAddition, Type::String) => Some(BoundBinary::new(
            &Type::Any,
            BoundBinaryOperator::StringConcat,
            &Type::Any,
            Type::String,
        )),
        (Type::Noneable(lhs_type), BoundBinaryOperator::NoneableOrValue, rhs_type) => {
            if rhs_type.can_be_converted_to(lhs_type) {
                Some(BoundBinary::new(
                    &Type::Noneable(lhs_type.clone()),
                    result,
                    rhs_type,
                    *lhs_type.clone(),
                ))
            } else {
                binder
                    .diagnostic_bag
                    .report_cannot_convert(span, rhs_type, lhs_type);
                None
            }
        }
        // Special case, where none ?? value is used. This could be optimized
        // away later.
        (Type::None, BoundBinaryOperator::NoneableOrValue, rhs_type) => Some(BoundBinary::new(
            &Type::None,
            result,
            rhs_type,
            rhs_type.clone(),
        )),
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
        SyntaxTokenKind::Bang => BoundUnaryOperator::LogicalNegation,
        _ => unreachable!(),
    };
    match operand.type_ {
        Type::Integer if result != BoundUnaryOperator::LogicalNegation => {
            Some((result, Type::Integer))
        }
        Type::Boolean | Type::Noneable(_) if result == BoundUnaryOperator::LogicalNegation => {
            Some((result, Type::Boolean))
        }
        Type::Error => None,
        _ => {
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
        Type::SystemCall(kind) => FunctionType::system_call(*kind),
        // Type::SystemCall(SystemCallKind::Print) => FunctionType {
        //     parameter_types: vec![Type::Any],
        //     this_type: None,
        //     return_type: Type::Void,
        //     system_call_kind: type_.as_system_call(),
        // },
        // Type::SystemCall(SystemCallKind::ArrayLength) => FunctionType {
        //     parameter_types: vec![],
        //     // Actually Array or String, but there is no way to call this system
        //     // call directly, so that it is already checked somewhere else.
        //     this_type: Some(Type::Any),
        //     return_type: Type::Integer,
        //     system_call_kind: type_.as_system_call(),
        // },
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
    let mut more_arguments = vec![];
    let function = if let BoundNodeKind::Closure(closure) = function.kind {
        more_arguments.append(&mut closure.arguments());
        match closure.function {
            typing::FunctionKind::FunctionId(id) => {
                let type_ = if let Type::Closure(closure_type) = function.type_ {
                    Type::Function(Box::new(closure_type.base_function_type))
                } else {
                    unreachable!("There is a closure, which is not of type closure???");
                };
                BoundNode::variable(function.span, id, type_)
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
    arguments.append(&mut more_arguments);
    match &function.type_ {
        Type::Error => BoundNode::error(span),
        &Type::SystemCall(system_call) => {
            BoundNode::system_call(span, system_call, arguments, function_type.return_type)
        }
        Type::Function(_) => BoundNode::function_call(
            span,
            function,
            arguments,
            function_type.this_type.is_some(),
            function_type.return_type,
        ),
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
        }
        _ => unreachable!(),
    }
}

fn bind_array_index<'a, 'b>(
    span: TextSpan,
    array_index: ArrayIndexNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let index = bind_node(*array_index.index, binder);
    let index = bind_conversion(index, &Type::Integer, binder);
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
    let index = bind_node(*array_index.index, binder);
    let index = bind_conversion(index, &Type::Integer, binder);
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
                        Type::closure(*function_type.clone()),
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
                        Type::closure(*function_type.clone()),
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
        | Type::None
        | Type::Function(_)
        | Type::Closure(_)
        | Type::SystemCall(_)
        | Type::Noneable(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base_span, &base.type_);
            BoundNode::error(span)
        }
        Type::Array(_) | Type::String => {
            if field_access.field.lexeme == "length" {
                let base = bind_conversion(base, &Type::Any, binder);
                let function_type = FunctionType::system_call(SystemCallKind::ArrayLength);
                BoundNode::system_call_closure(
                    span,
                    base,
                    SystemCallKind::ArrayLength,
                    Type::closure(function_type),
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
        | Type::None
        | Type::SystemCall(_)
        | Type::Noneable(_)
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
        let mut argument = bind_node(argument, binder);
        if matches!(argument.type_, Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.span);
        }
        argument = bind_conversion(argument, parameter_type, binder);
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

fn bind_conversion<'a>(
    base: BoundNode<'a>,
    type_: &Type,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode<'a> {
    if &base.type_ == type_ {
        base
    } else if base.type_.can_be_converted_to(type_) {
        BoundNode::conversion(base.span, base, type_.clone())
    } else if type_ != &Type::Error && base.type_ != Type::Error {
        binder
            .diagnostic_bag
            .report_cannot_convert(base.span, &base.type_, type_);
        BoundNode::error(base.span)
    } else {
        BoundNode::error(base.span)
    }
}

fn bind_condition_conversion<'a>(
    base: BoundNode<'a>,
    binder: &mut BindingState<'a, '_>,
) -> (BoundNode<'a>, SafeNodeInCondition<'a>) {
    match &base.type_ {
        Type::Error => (base, SafeNodeInCondition::None),
        Type::None => (base, SafeNodeInCondition::None),
        Type::Noneable(base_type) => {
            let safe_type = *base_type.clone();
            let node = base.clone();
            (base, SafeNodeInCondition::IfBody(node, safe_type))
        }
        Type::Boolean => {
            let safe_node = register_contained_safe_nodes(&base);
            (base, safe_node)
        }
        Type::Void
        | Type::Any
        | Type::Integer
        | Type::SystemCall(_)
        | Type::Array(_)
        | Type::String
        | Type::Function(_)
        | Type::Closure(_)
        | Type::Struct(_)
        | Type::StructReference(_) => (
            bind_conversion(base, &Type::Boolean, binder),
            SafeNodeInCondition::None,
        ),
    }
}

fn register_contained_safe_nodes<'a>(base: &BoundNode<'a>) -> SafeNodeInCondition<'a> {
    match &base.kind {
        BoundNodeKind::UnaryExpression(unary)
            if unary.operator_token == BoundUnaryOperator::LogicalNegation =>
        {
            match &unary.operand.type_ {
                Type::Boolean => register_contained_safe_nodes(&unary.operand).negate(),
                Type::Noneable(base_type) => {
                    SafeNodeInCondition::ElseBody(*unary.operand.clone(), *base_type.clone())
                }
                _ => SafeNodeInCondition::None,
            }
        }
        BoundNodeKind::BinaryExpression(binary)
            if binary.operator_token == BoundBinaryOperator::Equals
                && matches!(
                    (&binary.lhs.type_, &binary.rhs.type_),
                    (Type::Noneable(_), Type::Noneable(_))
                ) =>
        {
            match (&binary.lhs.constant_value, &binary.rhs.constant_value) {
                (None, Some(constant)) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.lhs).negate()
                    } else {
                        register_contained_safe_nodes(&binary.lhs)
                    }
                }
                (Some(constant), None) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.rhs).negate()
                    } else {
                        register_contained_safe_nodes(&binary.rhs)
                    }
                }
                _ => SafeNodeInCondition::None,
            }
        }
        BoundNodeKind::BinaryExpression(binary)
            if binary.operator_token == BoundBinaryOperator::NotEquals
                && matches!(
                    (&binary.lhs.type_, &binary.rhs.type_),
                    (Type::Noneable(_), Type::Noneable(_))
                ) =>
        {
            match (&binary.lhs.constant_value, &binary.rhs.constant_value) {
                (None, Some(constant)) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.lhs)
                    } else {
                        SafeNodeInCondition::None
                    }
                }
                (Some(constant), None) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.rhs)
                    } else {
                        SafeNodeInCondition::None
                    }
                }
                _ => SafeNodeInCondition::None,
            }
        }
        _ => {
            if let Type::Noneable(base_type) = &base.type_ {
                SafeNodeInCondition::IfBody(base.clone(), *base_type.clone())
            } else {
                SafeNodeInCondition::None
            }
        }
    }
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
    let (condition, safe_node) = bind_condition_conversion(condition, binder);
    let optional_index = if let SafeNodeInCondition::IfBody(node, safe_type) = safe_node.clone() {
        Some(binder.register_safe_node(node, safe_type))
    } else {
        None
    };
    let body = bind_node(*if_statement.body, binder);
    if let Some(index) = optional_index {
        binder.delete_safe_node(index);
    }
    let optional_index = if let SafeNodeInCondition::ElseBody(node, safe_type) = safe_node {
        Some(binder.register_safe_node(node, safe_type))
    } else {
        None
    };
    let else_body = if_statement.else_clause.map(|e| bind_node(*e.body, binder));
    if let Some(index) = optional_index {
        binder.delete_safe_node(index);
    }
    BoundNode::if_statement(span, condition, body, else_body)
}

fn bind_variable_declaration<'a, 'b>(
    span: TextSpan,
    variable_declaration: VariableDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    binder.expected_type = variable_declaration
        .optional_type_declaration
        .map(|td| bind_type(td.type_, binder));
    let initializer = bind_node(*variable_declaration.initializer, binder);
    if matches!(initializer.type_, Type::Void) {
        binder
            .diagnostic_bag
            .report_invalid_void_expression(initializer.span);
    }
    let type_ = binder
        .expected_type
        .take()
        .unwrap_or_else(|| initializer.type_.clone());
    if type_ == Type::None {
        binder
            .diagnostic_bag
            .report_invalid_variable_type_none(span);
    }
    let initializer = bind_conversion(initializer, &type_, binder);
    let variable_index =
        binder.register_variable(variable_declaration.identifier.lexeme, type_.clone(), false);
    if let Some(variable_index) = variable_index {
        BoundNode::variable_declaration(span, variable_index, initializer, Some(type_))
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
    let function_return_type = binder.function_return_type.clone();
    if expression.is_none() && !function_return_type.can_be_converted_to(&Type::Void) {
        binder
            .diagnostic_bag
            .report_missing_return_value(span, &function_return_type);
    }

    let expression =
        expression.map(|expression| bind_conversion(expression, &function_return_type, binder));
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
    let condition = bind_conversion(condition, &Type::Boolean, binder);
    let body = bind_node(*while_statement.body, binder);
    BoundNode::while_statement(span, condition, body)
}

fn bind_assignment<'a, 'b>(
    span: TextSpan,
    assignment: AssignmentNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode<'a> {
    let lhs = bind_node_for_assignment(*assignment.lhs, binder);
    if let BoundNodeKind::VariableExpression(variable) = &lhs.kind {
        let name = binder.get_variable_name_by_id(variable.variable_index);
        if name == Some("this") && binder.is_struct_function {
            binder.diagnostic_bag.report_cannot_assign_to(lhs.span);
        }
    }
    let expression = bind_node(*assignment.expression, binder);
    let expression = bind_conversion(expression, &lhs.type_, binder);
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
