pub mod bound_nodes;
pub mod control_flow_analyzer;
mod dependency_resolver;
mod lowerer;
pub mod operators;
pub mod symbols;
#[cfg(test)]
mod tests;
mod type_replacer;
pub mod typing;

use std::{borrow::Cow, convert::TryFrom, path::Path, collections::HashMap};

use crate::{
    binder::{
        bound_nodes::{is_same_expression::IsSameExpression, BoundNodeKind},
        operators::{BoundBinary, BoundUnaryOperator},
        symbols::StructFunctionKind,
        typing::{Type, IntegerType},
    },
    diagnostics::DiagnosticBag,
    instruction_converter::{self, instruction::Instruction, InstructionOrLabelReference},
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    parser::{
        self,
        syntax_nodes::{
            ArrayIndexNodeKind, ArrayLiteralNodeKind, AssignmentNodeKind, BinaryNodeKind,
            BlockStatementNodeKind, CastExpressionNodeKind, ConstDeclarationNodeKind,
            ConstructorCallNodeKind, ExpressionStatementNodeKind, FieldAccessNodeKind,
            ForStatementNodeKind, FunctionCallNodeKind, FunctionDeclarationNodeKind,
            FunctionTypeNode, IfStatementNodeKind, LiteralNodeKind, ParameterNode,
            ParenthesizedNodeKind, ReturnStatementNodeKind, StructBodyNode,
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
    symbols::{
        FunctionSymbol, GenericFunction, GenericStructSymbol, Library, MaybeGenericStructSymbol,
        StructFieldSymbol, StructFunctionTable, StructSymbol,
    },
    typing::{FunctionType, StructReferenceType, StructType, SystemCallKind, TypedGenericStructType},
};

#[derive(Debug, Clone)]
enum SafeNodeInCondition {
    None,
    IfBody(BoundNode, Type),
    ElseBody(BoundNode, Type),
}

impl SafeNodeInCondition {
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
    header_span: TextSpan,
    function_name: Cow<'a, str>,
    body: SyntaxNode<'a>,
    parameters: Vec<(&'a str, Type)>,
    is_main: bool,
    is_generic: bool,
    function_label: u64,
    function_type: FunctionType,
    base_struct: Option<u64>,
    struct_function_kind: Option<StructFunctionKind>,
}

// FIXME: Move somewhere sensible!
#[derive(Default, Clone, Debug, Copy, PartialEq, Eq)]
pub struct SimpleStructFunctionTable {
    pub constructor_function: Option<usize>,
    pub to_string_function: Option<usize>,
    pub get_function: Option<usize>,
    pub set_function: Option<usize>,
    pub element_count_function: Option<usize>,
    pub equals_function: Option<usize>,
}

impl SimpleStructFunctionTable {
    pub fn set(&mut self, kind: StructFunctionKind, label: usize) {
        match kind {
            StructFunctionKind::Constructor => self.constructor_function = Some(label),
            StructFunctionKind::ToString => self.to_string_function = Some(label),
            StructFunctionKind::Get => self.get_function = Some(label),
            StructFunctionKind::Set => self.set_function = Some(label),
            StructFunctionKind::ElementCount => self.element_count_function = Some(label),
            StructFunctionKind::Equals => self.equals_function = Some(label),
        }
    }

    pub fn get(&self, kind: StructFunctionKind) -> Option<usize> {
        match kind {
            StructFunctionKind::Constructor => self.constructor_function,
            StructFunctionKind::ToString => self.to_string_function,
            StructFunctionKind::Get => self.get_function,
            StructFunctionKind::Set => self.set_function,
            StructFunctionKind::ElementCount => self.element_count_function,
            StructFunctionKind::Equals => self.equals_function,
        }
    }
}

impl From<&StructFunctionTable> for SimpleStructFunctionTable {
    fn from(it: &StructFunctionTable) -> Self {
        Self {
            constructor_function: it
                .constructor_function
                .as_ref()
                .map(|f| f.function_label as _),
            to_string_function: it
                .to_string_function
                .as_ref()
                .map(|f| f.function_label as _),
            get_function: it.get_function.as_ref().map(|f| f.function_label as _),
            set_function: it.set_function.as_ref().map(|f| f.function_label as _),
            element_count_function: it
                .element_count_function
                .as_ref()
                .map(|f| f.function_label as _),
            equals_function: it.equals_function.as_ref().map(|f| f.function_label as _),
        }
    }
}

#[derive(Clone, Debug)]
struct StructDeclarationBody<'a> {
    name: Cow<'a, str>,
    id: u64,
    body: StructBodyNode<'a>,
    _is_generic: bool,
    struct_function_table: SimpleStructFunctionTable,
}

struct VariableEntry {
    id: u64,
    type_: Type,
    is_read_only: bool,
}

#[derive(Debug)]
struct BoundVariableName<'a> {
    pub identifier: Cow<'a, str>,
    pub type_: Type,
    pub is_read_only: bool,
}

#[derive(Debug)]
struct BoundConstant<'a> {
    pub identifier: Cow<'a, str>,
    pub value: Value,
}

#[derive(Debug, PartialEq)]
struct VariableOrConstant {
    type_: Type,
    is_read_only: bool,
    kind: VariableOrConstantKind,
}

impl VariableOrConstant {
    pub fn none() -> Self {
        Self {
            type_: Type::Error,
            is_read_only: true,
            kind: VariableOrConstantKind::None,
        }
    }
}

impl From<VariableEntry> for VariableOrConstant {
    fn from(it: VariableEntry) -> Self {
        Self {
            type_: it.type_,
            is_read_only: it.is_read_only,
            kind: VariableOrConstantKind::Variable(it.id),
        }
    }
}

impl From<Value> for VariableOrConstant {
    fn from(it: Value) -> Self {
        Self {
            type_: it.infer_type(),
            is_read_only: true,
            kind: VariableOrConstantKind::Constant(it),
        }
    }
}

#[derive(Debug, PartialEq)]
enum VariableOrConstantKind {
    None,
    Variable(u64),
    Constant(Value),
}

#[derive(Debug)]
pub enum BoundMaybeGenericStructSymbol<'a> {
    Struct(BoundStructSymbol<'a>),
    GenericStruct(BoundGenericStructSymbol<'a>),
    Empty,
}

impl<'a> BoundMaybeGenericStructSymbol<'a> {
    /// Returns `true` if the bound maybe generic struct symbol is [`Empty`].
    ///
    /// [`Empty`]: BoundMaybeGenericStructSymbol::Empty
    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    pub fn as_struct(&self) -> Option<&BoundStructSymbol<'a>> {
        if let Self::Struct(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_generic_struct(&self) -> Option<&BoundGenericStructSymbol<'a>> {
        if let Self::GenericStruct(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_struct_mut(&mut self) -> Option<&mut BoundStructSymbol<'a>> {
        if let Self::Struct(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_generic_struct_mut(&mut self) -> Option<&mut BoundGenericStructSymbol<'a>> {
        if let Self::GenericStruct(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn name(&self) -> &str {
        match self {
            BoundMaybeGenericStructSymbol::Struct(it) => &it.name,
            BoundMaybeGenericStructSymbol::GenericStruct(it) => &it.struct_type.name,
            BoundMaybeGenericStructSymbol::Empty => "",
        }
    }

    fn set_name(&mut self, name: impl Into<Cow<'a, str>>) {
        let name = name.into();
        match self {
            BoundMaybeGenericStructSymbol::Struct(it) => it.name = name,
            BoundMaybeGenericStructSymbol::GenericStruct(it) => it.struct_type.name = name,
            BoundMaybeGenericStructSymbol::Empty => {
                unreachable!("Tried setting name of empty slot to {}", name)
            }
        }
    }
}

impl<'a> From<BoundStructSymbol<'a>> for BoundMaybeGenericStructSymbol<'a> {
    fn from(value: BoundStructSymbol<'a>) -> Self {
        Self::Struct(value)
    }
}

impl<'a> From<BoundGenericStructSymbol<'a>> for BoundMaybeGenericStructSymbol<'a> {
    fn from(value: BoundGenericStructSymbol<'a>) -> Self {
        Self::GenericStruct(value)
    }
}

impl From<MaybeGenericStructSymbol> for BoundMaybeGenericStructSymbol<'_> {
    fn from(value: MaybeGenericStructSymbol) -> Self {
        match value {
            MaybeGenericStructSymbol::Struct(it) => BoundStructSymbol::from(it).into(),
            MaybeGenericStructSymbol::GenericStruct(it) => {
                BoundGenericStructSymbol::from(it).into()
            }
        }
    }
}

/// This is quite similiar to symbols::StructSymbol, the only difference being,
/// that this version does not need an allocation. They can be easily converted
/// into each other. This version is only used during binding, while the other
/// version is exported in the symbols::Library type.
#[derive(Debug, Clone)]
pub struct BoundStructSymbol<'a> {
    pub name: Cow<'a, str>,
    pub fields: Vec<BoundStructFieldSymbol<'a>>,
    pub function_table: StructFunctionTable,
    pub is_generic: bool,
}

impl BoundStructSymbol<'_> {
    pub fn field_functions_first(&self, name: &str) -> Option<&BoundStructFieldSymbol> {
        let mut iter = self.fields.iter().filter(|f| f.name == name);
        let mut result = iter.next()?;
        for next in iter {
            if matches!(result.type_, Type::Function(_)) {
                break;
            }
            result = next;
        }
        Some(result)
    }

    pub fn field_fields_first(&self, name: &str) -> Option<&BoundStructFieldSymbol> {
        let mut iter = self.fields.iter().filter(|f| f.name == name);
        let mut result = iter.next()?;
        for next in iter {
            if !matches!(result.type_, Type::Function(_)) {
                break;
            }
            result = next;
        }
        Some(result)
    }

    pub fn empty() -> Self {
        Self {
            name: String::new().into(),
            fields: vec![],
            function_table: StructFunctionTable::default(),
            is_generic: false,
        }
    }
}

impl From<StructSymbol> for BoundStructSymbol<'_> {
    fn from(it: StructSymbol) -> Self {
        Self {
            name: it.name.into(),
            fields: it.fields.into_iter().map(Into::into).collect(),
            function_table: it.function_table,
            is_generic: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundGenericStructSymbol<'a> {
    pub struct_type: BoundStructSymbol<'a>,
    pub functions: Vec<GenericFunction>,
}

impl BoundGenericStructSymbol<'_> {
    pub fn empty() -> Self {
        Self {
            struct_type: BoundStructSymbol::empty(),
            functions: vec![],
        }
    }
}

impl From<GenericStructSymbol> for BoundGenericStructSymbol<'_> {
    fn from(it: GenericStructSymbol) -> Self {
        Self {
            struct_type: BoundStructSymbol {
                name: it.name.into(),
                fields: it.fields.into_iter().map(Into::into).collect(),
                function_table: it.function_table,
                is_generic: true,
            },
            functions: it.functions,
        }
    }
}

/// This is quite similiar to symbols::StructFieldSymbol, the only difference
/// being, that this version does not need an allocation. They can be easily
/// converted into each other. This version is only used during binding, while
/// the other version is exported in the symbols::Library type.
#[derive(Debug, Clone)]
pub struct BoundStructFieldSymbol<'a> {
    pub name: Cow<'a, str>,
    pub type_: Type,
    pub offset: u64,
    pub is_read_only: bool,
}

impl From<StructFieldSymbol> for BoundStructFieldSymbol<'_> {
    fn from(it: StructFieldSymbol) -> Self {
        Self {
            name: it.name.into(),
            type_: it.type_,
            offset: it.offset,
            is_read_only: it.is_read_only,
        }
    }
}

enum StdTypeKind {
    Range,
    UnsignedRange,
    Array,
}

impl StdTypeKind {
    pub fn name(&self) -> &str {
        match self {
            StdTypeKind::Range => "Range",
            StdTypeKind::UnsignedRange => "UnsignedRange",
            StdTypeKind::Array => "Array",
        }
    }
}

struct BindingState<'a, 'b> {
    debug_flags: DebugFlags,
    /// The directory, in which the current file relative or absolute to the
    /// current executing program was.
    directory: &'a str,
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
    struct_table: Vec<BoundMaybeGenericStructSymbol<'a>>,
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
    generic_functions: Vec<FunctionDeclarationBody<'a>>,
    bound_generic_functions: Vec<GenericFunction>,
    bound_functions: Vec<BoundNode>,
    /// This is equal to the amount of labels used by the referenced libraries
    /// and will ensure, that all labels will be unique per bound program.
    label_offset: usize,
    /// This collects all the structs, which fields still need to be bound by the
    /// binder. This should be empty, before starting to bind the functions.
    structs: Vec<StructDeclarationBody<'a>>,
    generic_structs: Vec<StructDeclarationBody<'a>>,
    generic_type: Option<Type>,
    /// This has all libraries which where loaded by imports. They can be
    /// referenced for name lookups.
    libraries: Vec<Library>,
    /// This is a simple hack to bring constants into the binder.
    constants: Vec<BoundConstant<'a>>,
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
    safe_nodes: Vec<Option<(BoundNode, Type)>>,
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
    /// This is used to check if all fields have been set to a value in a
    /// constructor.
    assigned_fields: Vec<&'a str>,
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

    fn register_struct_name(
        &mut self,
        name: &'a str,
        struct_function_table: SimpleStructFunctionTable,
    ) -> Option<u64> {
        let id = self.type_table.len() as u64;
        self.register_type(
            name,
            Type::StructReference(StructReferenceType {
                id,
                simple_function_table: struct_function_table,
            }),
        )
    }

    fn register_generated_struct_name(
        &mut self,
        name: String,
        struct_function_table: SimpleStructFunctionTable,
    ) -> Option<u64> {
        let id = self.type_table.len() as u64;
        self.register_generated_type(
            name,
            Type::StructReference(StructReferenceType {
                id,
                simple_function_table: struct_function_table,
            }),
        )
    }

    fn register_generic_struct(
        &mut self,
        id: u64,
        fields: Vec<BoundStructFieldSymbol<'a>>,
        function_table: StructFunctionTable,
    ) -> u64 {
        let name = self.type_table[id as usize].identifier.clone();
        let bound_struct_type = BoundGenericStructSymbol {
            struct_type: BoundStructSymbol {
                name,
                fields,
                function_table,
                is_generic: true,
            },
            functions: vec![],
        };
        let struct_id = id as usize - type_table().len();
        while struct_id >= self.struct_table.len() {
            self.struct_table.push(BoundMaybeGenericStructSymbol::Empty);
        }
        assert!(self.struct_table[struct_id].is_empty());
        self.struct_table[struct_id] = bound_struct_type.into();
        id
    }

    fn register_struct(
        &mut self,
        id: u64,
        fields: Vec<BoundStructFieldSymbol<'a>>,
        function_table: StructFunctionTable,
    ) -> u64 {
        let struct_type = StructType {
            id,
            fields: fields
                .iter()
                .filter(|f| !matches!(f.type_, Type::Function(_)))
                .map(|f| f.type_.clone())
                .collect(),
            functions: fields
                .iter()
                .filter(|f| matches!(f.type_, Type::Function(_)))
                .map(|f| f.type_.clone())
                .collect(),
            function_table: function_table.clone(),
            is_generic: false,
        };
        self.type_table[id as usize].type_ = Type::Struct(Box::new(struct_type));

        self.insert_into_struct_table(
            id,
            fields.into_iter().map(Into::into).collect(),
            function_table,
        );
        id
    }

    pub fn register_maybe_generic_struct_as(
        &mut self,
        name: &str,
        maybe_generic_struct: &MaybeGenericStructSymbol,
    ) -> Option<u64> {
        let id = self.register_generated_struct_name(
            name.to_owned(),
            maybe_generic_struct.function_table().into(),
        )?;
        let fields = maybe_generic_struct.fields().to_vec();
        let function_table = maybe_generic_struct.function_table().clone();
        let struct_type = StructType {
            id,
            fields: fields
                .iter()
                .filter(|f| !matches!(f.type_, Type::Function(_)))
                .map(|f| f.type_.clone())
                .collect(),
            functions: fields
                .iter()
                .filter(|f| matches!(f.type_, Type::Function(_)))
                .map(|f| f.type_.clone())
                .collect(),
            function_table,
            is_generic: false,
        };
        self.type_table[id as usize].type_ = Type::Struct(Box::new(struct_type));

        let struct_id = id as usize - type_table().len();
        while struct_id >= self.struct_table.len() {
            self.struct_table.push(BoundMaybeGenericStructSymbol::Empty);
        }

        assert!(self.struct_table[struct_id].is_empty());
        self.struct_table[struct_id] = maybe_generic_struct.to_owned().into();
        self.struct_table[struct_id].set_name(name.to_owned());

        Some(id)
    }

    pub fn insert_into_struct_table(
        &mut self,
        id: u64,
        fields: Vec<BoundStructFieldSymbol<'a>>,
        function_table: StructFunctionTable,
    ) {
        let name = self.type_table[id as usize].identifier.clone();
        let bound_struct_type = BoundStructSymbol {
            name,
            fields,
            function_table,
            is_generic: false,
        };
        let struct_id = id as usize - type_table().len();
        while struct_id >= self.struct_table.len() {
            self.struct_table.push(BoundMaybeGenericStructSymbol::Empty);
        }

        assert!(self.struct_table[struct_id].is_empty());
        self.struct_table[struct_id] = bound_struct_type.into();
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

    fn register_generated_type(&mut self, name: String, type_: Type) -> Option<u64> {
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

    fn register_constant(&mut self, name: &'a str, value: Value) -> Option<u64> {
        let index = self.constants.len() as u64;
        let constant_name_already_registered = self.constants.iter().any(|c| c.identifier == name);
        if constant_name_already_registered {
            None
        } else {
            self.constants.push(BoundConstant {
                identifier: name.into(),
                value,
            });
            Some(index)
        }
    }

    fn register_generated_constant(&mut self, name: String, value: Value) -> Option<u64> {
        let index = self.constants.len() as u64;
        let constant_name_already_registered = self.constants.iter().any(|c| c.identifier == name);
        if constant_name_already_registered {
            None
        } else {
            self.constants.push(BoundConstant {
                identifier: name.into(),
                value,
            });
            Some(index)
        }
    }

    fn look_up_constant_by_name(&self, name: &str) -> Option<Value> {
        self.constants
            .iter()
            .find(|c| c.identifier == name)
            .map(|c| c.value.clone())
    }

    fn register_safe_node(&mut self, node: BoundNode, safe_type: Type) -> usize {
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

    fn get_safe_node(&self, node: &BoundNode) -> Option<BoundNode> {
        self.safe_nodes
            .iter()
            .filter_map(|n| n.as_ref())
            .find(|(n, _)| n.is_same_expression(node))
            .map(|(n, t)| BoundNode::conversion(n.span, n.clone(), t.clone()))
    }

    fn delete_variables_until(&mut self, index: usize) {
        if self.variable_table.is_empty() || index == self.variable_table.len() {
            return;
        }
        if self.print_variable_table {
            print_variable_table(&self.variable_table);
        }
        let current = self.variable_table.len() - 1;
        for _ in index..=current {
            self.variable_table.pop();
        }
    }

    fn look_up_variable_or_constant_by_name(&self, name: &str) -> VariableOrConstant {
        match self.look_up_variable_by_name(name) {
            Some(it) => it.into(),
            None => match self.look_up_constant_by_name(name) {
                Some(it) => it.into(),
                None => VariableOrConstant::none(),
            },
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

    fn look_up_std_struct_id(&self, kind: StdTypeKind) -> u64 {
        self.look_up_struct_id_by_name(kind.name())
            .unwrap_or_else(|| panic!("Could not load std type {}.", kind.name()))
    }

    fn look_up_type_by_name(&self, name: &str) -> Option<Type> {
        if name == "$Type" && self.generic_type.is_some() {
            return Some(self.generic_type.clone().unwrap());
        }
        self.type_table
            .iter()
            .find(|v| v.identifier.as_ref() == name)
            .map(|v| v.type_.clone())
            .map(|t| {
                if let Type::Struct(struct_type) = t {
                    Type::StructReference(StructReferenceType {
                        id: struct_type.id,
                        simple_function_table: (&struct_type.function_table).into(),
                    })
                } else {
                    t
                }
            })
    }

    fn look_up_type_by_generated_name(&self, name: String) -> Option<Type> {
        self.type_table
            .iter()
            .find(|v| v.identifier.as_ref() == name)
            .map(|v| v.type_.clone())
            .map(|t| {
                if let Type::Struct(struct_type) = t {
                    Type::StructReference(StructReferenceType {
                        id: struct_type.id,
                        simple_function_table: (&struct_type.function_table).into(),
                    })
                } else {
                    t
                }
            })
    }

    fn get_struct_type_by_id(&self, id: u64) -> Option<&BoundStructSymbol<'a>> {
        let index = id as usize - type_table().len();

        self.struct_table
            .get(index)
            .filter(|e| !e.is_empty())
            .map(|e| {
                e.as_struct()
                    .or_else(|| e.as_generic_struct().map(|e| &e.struct_type))
            })
            .flatten()
    }

    fn get_generic_struct_type_by_id(&self, id: u64) -> Option<&BoundGenericStructSymbol<'a>> {
        self.struct_table
            .get(id as usize - type_table().len())
            .map(|e| e.as_generic_struct())
            .flatten()
    }

    fn get_generic_struct_type_by_id_mut(
        &mut self,
        id: u64,
    ) -> Option<&mut BoundGenericStructSymbol<'a>> {
        self.struct_table
            .get_mut(id as usize - type_table().len())
            .map(|e| e.as_generic_struct_mut())
            .flatten()
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

    pub fn look_up_struct_id_by_name(&self, name: &str) -> Option<u64> {
        self.struct_table
            .iter()
            .enumerate()
            .find(|(_, s)| s.name() == name)
            .map(|(i, _)| (i + type_table().len()) as u64)
    }

    pub fn rename_struct_by_id(&mut self, struct_id: u64, new_name: String) {
        self.struct_table[struct_id as usize - type_table().len()].set_name(new_name.clone());
        self.type_table
            .iter_mut()
            .find(|t| match &t.type_ {
                Type::Struct(struct_type) => struct_type.id == struct_id,
                // NOTE: Is there a reason to format this like it currently is?
                Type::StructReference(id) if id.id == struct_id => true,
                _ => false,
            })
            .unwrap()
            .identifier = new_name.into();
    }

    fn convert_struct_reference_to_struct(&self, type_: Type) -> Type {
        match type_ {
            Type::Error
            | Type::Void
            | Type::Any
            | Type::Integer(_)
            | Type::IntegerLiteral
            | Type::Boolean
            | Type::None
            | Type::String
            | Type::Function(_)
            | Type::Closure(_)
            | Type::Library(_)
            | Type::GenericType
            | Type::Struct(_)
            | Type::TypedGenericStruct(_)
            | Type::Pointer
            | Type::SystemCall(_) => type_,
            Type::Noneable(base_type) => {
                Type::noneable(self.convert_struct_reference_to_struct(*base_type))
            }
            Type::StructReference(id) => {
                debug_assert!(self.generic_structs.is_empty());
                debug_assert!(self.structs.is_empty());
                Type::Struct(Box::new(self.get_struct_by_id(id.id).unwrap()))
            }
            Type::PointerOf(base_type) => {
                Type::pointer_of(self.convert_struct_reference_to_struct(*base_type))
            }
        }
    }

    pub fn generate_label(&mut self) -> usize {
        let result = self.label_offset;
        self.label_offset += 1;
        result
    }

    #[allow(dead_code)]
    fn find_generic_function_by_label(&self, label: usize) -> Option<&GenericFunction> {
        self.bound_generic_functions
            .iter()
            .find(|f| f.function_label == label as u64)
    }

    pub fn add_function_declaration(&mut self, function_declaration: FunctionDeclarationBody<'a>) {
        if function_declaration.is_generic {
            self.generic_functions.push(function_declaration);
        } else {
            self.functions.push(function_declaration);
        }
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

fn print_constant_table(constant_table: &[BoundConstant]) {
    let mut constant_table: Vec<_> = constant_table.iter().enumerate().collect();
    constant_table.sort_unstable_by_key(|f| f.0);
    for (index, constant) in constant_table {
        println!(
            "  {:00}: {} = {}",
            index,
            constant.identifier.as_ref(),
            constant.value
        );
    }
    println!();
}

pub struct BoundProgram {
    pub startup: Vec<InstructionOrLabelReference>,
    pub functions: BoundNode,
    pub fixed_variable_count: usize,
    pub max_used_variables: usize,
    pub label_count: usize,
    pub referenced_libraries: Vec<Library>,
}

impl BoundProgram {
    pub fn error() -> Self {
        Self {
            startup: vec![],
            functions: BoundNode::error(TextSpan::zero()),
            fixed_variable_count: 0,
            max_used_variables: 0,
            label_count: 0,
            referenced_libraries: vec![],
        }
    }
}

pub struct BoundLibrary {
    pub program: BoundProgram,
    pub exported_functions: Vec<FunctionSymbol>,
    pub exported_structs: Vec<MaybeGenericStructSymbol>,
}

impl BoundLibrary {
    pub fn error() -> Self {
        Self {
            program: BoundProgram::error(),
            exported_functions: vec![],
            exported_structs: vec![],
        }
    }
}

fn type_table() -> Vec<BoundVariableName<'static>> {
    vec![
        BoundVariableName {
            identifier: "int".into(),
            type_: Type::Integer(IntegerType::Signed64),
            is_read_only: true,
        },
        BoundVariableName {
            identifier: "byte".into(),
            type_: Type::Integer(IntegerType::Unsigned8),
            is_read_only: true,
        },
        BoundVariableName {
            identifier: "uint".into(),
            type_: Type::Integer(IntegerType::Unsigned64),
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
        BoundVariableName {
            identifier: "$Type".into(),
            type_: Type::GenericType,
            is_read_only: true,
        },
    ]
}

pub fn bind_program<'a>(
    source_text: &'a SourceText,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
) -> BoundProgram {
    bind(source_text, diagnostic_bag, false, true, debug_flags).program
}

pub fn bind_library<'a>(
    source_text: &'a SourceText,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    debug_flags: DebugFlags,
    import_std_lib: bool,
) -> BoundLibrary {
    bind(
        source_text,
        diagnostic_bag,
        true,
        import_std_lib,
        debug_flags,
    )
}

fn bind<'a>(
    source_text: &'a SourceText,
    diagnostic_bag: &mut DiagnosticBag<'a>,
    is_library: bool,
    import_std_lib: bool,
    debug_flags: DebugFlags,
) -> BoundLibrary {
    let node = parser::parse(source_text, diagnostic_bag, debug_flags);
    if diagnostic_bag.has_errors() {
        return BoundLibrary::error();
    }
    let mut binder = BindingState {
        debug_flags,
        directory: source_text.directory(),
        diagnostic_bag,
        variable_table: vec![],
        type_table: type_table(),
        generic_type: None,
        struct_table: vec![],
        functions: vec![],
        generic_functions: vec![],
        bound_generic_functions: vec![],
        bound_functions: vec![],
        label_offset: 0,
        structs: vec![],
        generic_structs: vec![],
        libraries: vec![],
        constants: vec![],
        safe_nodes: vec![],
        print_variable_table: debug_flags.print_variable_table(),
        max_used_variables: 0,
        function_return_type: Type::Error,
        is_struct_function: false,
        assigned_fields: vec![],
        expected_type: None,
    };
    let span = node.span();
    let mut startup = vec![];
    default_statements(&mut binder);
    let node = dependency_resolver::bind_import_statements(node, &mut binder).unwrap();
    if import_std_lib {
        std_imports(&mut binder);
    }
    bind_top_level_statements(node, &mut binder);

    for lib in binder.libraries.iter_mut() {
        startup.append(
            &mut lib
                .program
                .startup_instructions
                .iter()
                .map(|&i| i.into())
                .collect(),
        );
    }

    while let Some(node) = binder.generic_structs.pop() {
        let (fields, function_table) = bind_struct_body(
            &node.name,
            node.id,
            node.body,
            node.struct_function_table,
            &mut binder,
        );
        binder.register_generic_struct(node.id, fields, function_table);
    }

    while let Some(node) = binder.structs.pop() {
        let (fields, function_table) = bind_struct_body(
            &node.name,
            node.id,
            node.body,
            node.struct_function_table,
            &mut binder,
        );
        binder.register_struct(node.id, fields, function_table);
    }

    let fixed_variable_count = binder.variable_table.len();
    while let Some(node) = binder.generic_functions.pop() {
        let function = bind_function_declaration_body(node.clone(), &mut binder);

        let body = if let BoundNodeKind::FunctionDeclaration(function_declaration) = function.kind {
            *function_declaration.body
        } else {
            unreachable!("The function was not a function declaration!");
        };
        let generic_function = GenericFunction {
            function_label: node.function_label,
            function_name: node.function_name.into_owned(),
            function_type: node.function_type,
            body,
            labels: vec![],
        };
        if let Some(base_struct) = node.base_struct {
            binder
                .get_generic_struct_type_by_id_mut(base_struct)
                .unwrap()
                .functions
                .push(generic_function);
        } else {
            binder.bound_generic_functions.push(generic_function);
        }
    }

    let mut exported_functions = Vec::with_capacity(binder.functions.len());
    while let Some(node) = binder.functions.pop() {
        exported_functions.push(node.clone());
        let function = bind_function_declaration_body(node, &mut binder);
        binder.bound_functions.push(function);
    }

    if !is_library {
        startup.append(&mut call_main(&mut binder));
    }

    let functions = BoundNode::block_statement(span, binder.bound_functions);
    if debug_flags.print_bound_program {
        crate::debug::print_bound_node_as_code(&functions);
    }
    if debug_flags.print_struct_table {
        for (id, entry) in binder.struct_table.iter().enumerate() {
            println!("  {}: {}", id, entry.name());
        }
    }
    if debug_flags.print_constant_table {
        print_constant_table(&binder.constants);
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

    let program = BoundProgram {
        startup,
        functions,
        fixed_variable_count,
        max_used_variables: binder.max_used_variables,
        label_count: binder.label_offset,
        referenced_libraries: binder.libraries,
    };
    BoundLibrary {
        program,
        exported_functions: exported_functions.into_iter().map(Into::into).collect(),
        exported_structs: binder.struct_table.into_iter().map(Into::into).collect(),
    }
}

fn bind_function_declaration_body<'a, 'b>(
    node: FunctionDeclarationBody<'a>,
    binder: &'b mut BindingState<'a, '_>,
) -> BoundNode {
    let fixed_variable_count = binder.variable_table.len();
    let label = node.function_label as usize;
    let mut parameters = Vec::with_capacity(node.parameters.len());
    for (variable_name, variable_type) in node.parameters {
        if let Some(it) = binder.register_variable(variable_name, variable_type, false) {
            parameters.push(it);
        } else {
            // binder.diagnostic_bag.report_cannot_declare_variable(span, variable_name)
            panic!("Could not declare a parameter! This sounds like an error in the binder!");
        }
    }
    binder.function_return_type = node.function_type.return_type.clone();
    binder.is_struct_function = node.base_struct.is_some();
    let span = node.body.span;
    let mut body = bind_node(node.body, binder);
    match node.struct_function_kind {
        Some(StructFunctionKind::Constructor) => {
            let strct = node.base_struct.unwrap();
            let strct = binder.get_struct_type_by_id(strct).unwrap().to_owned();
            let unassigned_fields: Vec<_> = strct
                .fields
                .iter()
                .filter_map(|f| {
                    if matches!(f.type_, Type::Function(_)) {
                        None
                    } else {
                        Some(f.name.as_ref())
                    }
                })
                .filter(|s| !binder.assigned_fields.contains(s))
                .collect();
            if !unassigned_fields.is_empty() {
                binder
                    .diagnostic_bag
                    .report_not_all_fields_have_been_assigned(
                        node.header_span,
                        &strct.name,
                        &unassigned_fields,
                    );
            }
        }
        Some(
            StructFunctionKind::ToString
            | StructFunctionKind::Get
            | StructFunctionKind::Set
            | StructFunctionKind::ElementCount
            | StructFunctionKind::Equals,
        )
        | None => {}
    }
    binder.assigned_fields.clear();
    if matches!(&binder.function_return_type, Type::Void) {
        body = BoundNode::block_statement(
            body.span,
            vec![
                body,
                BoundNode::return_statement(TextSpan::zero(), None, !node.is_main),
            ],
        );
    }
    let body_statements = lowerer::flatten(body, &mut binder.label_offset);
    if !matches!(&binder.function_return_type, Type::Void)
        && !binder.diagnostic_bag.has_errors()
        && !control_flow_analyzer::check_if_all_paths_return(
            &node.function_name,
            &body_statements,
            binder.debug_flags,
        )
    {
        binder
            .diagnostic_bag
            .report_missing_return_statement(span, &binder.function_return_type);
    }
    let body = BoundNode::block_statement(span, body_statements);
    let result = BoundNode::function_declaration(label, node.is_main, body, parameters);
    binder.function_return_type = Type::Void;
    binder.delete_variables_until(fixed_variable_count);
    result
}

fn default_statements(binder: &mut BindingState) {
    binder.register_constant("print", Value::SystemCall(SystemCallKind::Print));
    binder.register_constant("heapdump", Value::SystemCall(SystemCallKind::HeapDump));
    binder.register_constant("break", Value::SystemCall(SystemCallKind::Break));
    binder.register_constant("reallocate", Value::SystemCall(SystemCallKind::Reallocate));
    binder.register_constant(
        "runtimeError",
        Value::SystemCall(SystemCallKind::RuntimeError),
    );
    binder.register_constant("addressOf", Value::SystemCall(SystemCallKind::AddressOf));
    binder.register_constant("garbageCollect", Value::SystemCall(SystemCallKind::GarbageCollect));
}

fn std_imports(binder: &mut BindingState) {
    dependency_resolver::load_library_from_path(
        binder,
        Path::new("../slides/builtin/std.sld"),
        TextSpan::zero(),
        "",
        false,
    );
}

fn call_main(binder: &mut BindingState) -> Vec<InstructionOrLabelReference> {
    let base = binder.look_up_variable_or_constant_by_name("main");
    match base.kind {
        VariableOrConstantKind::None => {
            binder.diagnostic_bag.report_no_main_function_found();
            vec![]
        }
        VariableOrConstantKind::Variable(variable_id) => {
            vec![
                Instruction::load_register(variable_id).into(),
                Instruction::load_immediate(0).into(),
                Instruction::function_call().into(),
            ]
        }
        VariableOrConstantKind::Constant(value) => {
            let mut result = instruction_converter::convert_value(
                TextSpan::zero(),
                value,
                &mut instruction_converter::InstructionConverter::default(),
            );
            result.push(Instruction::load_immediate(0).into());
            result.push(Instruction::function_call().into());
            result
        }
    }
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
        SyntaxNodeKind::_ConstDeclaration(const_declaration) => {
            bind_const_declaration(const_declaration, binder)
        }
        SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
            bind_function_declaration(*function_declaration, binder)
        }
        SyntaxNodeKind::ImportStatement(_) => {
            //bind_import_statement(node.span, import_statement, binder)
            unreachable!("ImportStatements should already be resolved by dependency_resolver!");
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
    if let Some(constant_value) = initializer.constant_value {
        binder.register_constant(const_declaration.identifier.lexeme, constant_value.value);
    } else {
        binder
            .diagnostic_bag
            .report_expected_constant(initializer.span);
    }
}

fn bind_function_declaration<'a, 'b>(
    function_declaration: FunctionDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    let header_span = TextSpan::bounds(
        function_declaration.func_keyword.span(),
        function_declaration
            .function_type
            .return_type
            .as_ref()
            .map(|r| r.return_type.span())
            .unwrap_or_else(|| function_declaration.function_type.rparen.span()),
    );
    let (function_type, variables) =
        bind_function_type(None, function_declaration.function_type, binder);
    let type_ = Type::Function(Box::new(function_type.clone()));
    let is_main = function_declaration.identifier.lexeme == "main";
    let is_generic = function_declaration.optional_generic_keyword.is_some();
    // assert!(is_generic == false);
    // FIXME: Turn into usize
    let function_label = binder.generate_label() as u64;
    binder.register_constant(
        function_declaration.identifier.lexeme,
        Value::LabelPointer(function_label as _, type_),
    );
    binder.add_function_declaration(FunctionDeclarationBody {
        header_span,
        function_name: function_declaration.identifier.lexeme.into(),
        body: *function_declaration.body,
        parameters: variables,
        is_main,
        is_generic,
        function_label,
        function_type,
        base_struct: None,
        struct_function_kind: None,
    });
}

fn bind_function_declaration_for_struct<'a, 'b>(
    struct_name: &str,
    struct_id: u64,
    struct_is_generic: bool,
    function_declaration: FunctionDeclarationNodeKind<'a>,
    struct_function_table: &mut StructFunctionTable,
    simple_struct_function_table: SimpleStructFunctionTable,
    binder: &mut BindingState<'a, 'b>,
) -> Option<BoundStructFieldSymbol<'a>> {
    let header_span = TextSpan::bounds(
        function_declaration.func_keyword.span(),
        function_declaration
            .function_type
            .return_type
            .as_ref()
            .map(|r| r.return_type.span())
            .unwrap_or_else(|| function_declaration.function_type.rparen.span()),
    );
    let struct_type = binder.look_up_type_by_name(struct_name).unwrap();
    let (function_type, mut variables) = bind_function_type(
        Some(struct_type.clone()),
        function_declaration.function_type,
        binder,
    );
    variables.push(("this", struct_type.clone()));
    let type_ = Type::Function(Box::new(function_type.clone()));
    // FIXME: Turn into usize
    let function_name = format!(
        "{}::{}",
        struct_name, function_declaration.identifier.lexeme
    );
    // FIXME: implement getter for generic structs.
    match StructFunctionKind::try_from(function_declaration.identifier.lexeme) {
        Ok(struct_function_kind) => {
            let function_label = simple_struct_function_table
                .get(struct_function_kind)
                .unwrap() as u64;
            type_check_struct_function_kind(
                header_span,
                struct_function_kind,
                &function_type,
                &struct_type,
                binder,
            );
            binder.add_function_declaration(FunctionDeclarationBody {
                header_span,
                function_name: function_name.clone().into(),
                body: *function_declaration.body,
                parameters: variables,
                is_main: false,
                is_generic: struct_is_generic,
                function_label,
                function_type: function_type.clone(),
                base_struct: Some(struct_id),
                struct_function_kind: Some(struct_function_kind),
            });
            struct_function_table.set(
                struct_function_kind,
                FunctionSymbol {
                    name: function_name,
                    function_type,
                    function_label,
                    // FIXME: Actually it is, but, we do not want other libraries
                    // register this as a constant, since it is not actually
                    // callable.
                    is_member_function: false,
                },
            );
            None
        }
        Err(identifier) => {
            if identifier.starts_with('$') {
                binder.diagnostic_bag.report_unrecognized_operator_function(
                    function_declaration.identifier.span(),
                    identifier,
                );
                None
            } else {
                let function_label = binder.generate_label() as u64;
                binder.register_generated_constant(
                    function_name.clone(),
                    Value::LabelPointer(function_label as usize, type_.clone()),
                );
                binder.add_function_declaration(FunctionDeclarationBody {
                    header_span,
                    function_name: function_name.into(),
                    body: *function_declaration.body,
                    parameters: variables,
                    is_main: false,
                    is_generic: struct_is_generic,
                    function_label,
                    function_type,
                    base_struct: Some(struct_id),
                    struct_function_kind: None,
                });
                Some(BoundStructFieldSymbol {
                    name: function_declaration.identifier.lexeme.into(),
                    type_,
                    is_read_only: true,
                    offset: 0,
                })
            }
        }
    }
}

fn type_check_struct_function_kind(
    span: TextSpan,
    struct_function_kind: StructFunctionKind,
    function_type: &FunctionType,
    struct_type: &Type,
    binder: &mut BindingState,
) {
    match struct_function_kind {
        StructFunctionKind::Constructor => {
            if !function_type.return_type.can_be_converted_to(&Type::Void) {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.return_type,
                    &Type::Void,
                );
            }
        }
        StructFunctionKind::ToString => {
            if !function_type.return_type.can_be_converted_to(&Type::String) {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.return_type,
                    &Type::String,
                );
            }
            if !function_type.parameter_types.is_empty() {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    span,
                    function_type.parameter_types.len(),
                    0,
                );
            }
        }
        StructFunctionKind::Get => {
            if function_type.parameter_types.len() != 1 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    span,
                    function_type.parameter_types.len(),
                    1,
                );
            }
            if !function_type.parameter_types[0].can_be_converted_to(&Type::Integer(IntegerType::Unsigned64)) {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.parameter_types[0],
                    &Type::Integer(IntegerType::Unsigned64),
                );
            }
        }
        StructFunctionKind::Set => {
            if function_type.parameter_types.len() != 2 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    span,
                    function_type.parameter_types.len(),
                    2,
                );
            }
            if !function_type.parameter_types[0].can_be_converted_to(&Type::Integer(IntegerType::Unsigned64)) {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.parameter_types[0],
                    &Type::Integer(IntegerType::Unsigned64),
                );
            }
        }
        StructFunctionKind::ElementCount => {
            if !function_type.parameter_types.is_empty() {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    span,
                    function_type.parameter_types.len(),
                    0,
                );
            }
            if !function_type
                .return_type
                .can_be_converted_to(&Type::Integer(IntegerType::Unsigned64))
            {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.return_type,
                    &Type::Integer(IntegerType::Unsigned64),
                );
            }
        }
        StructFunctionKind::Equals => {
            if function_type.parameter_types.len() != 1 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    span,
                    function_type.parameter_types.len(),
                    1,
                );
            }
            if !function_type.parameter_types[0].can_be_converted_to(struct_type) {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.parameter_types[0],
                    struct_type,
                );
            }
            if !function_type
                .return_type
                .can_be_converted_to(&Type::Boolean)
            {
                binder.diagnostic_bag.report_cannot_convert(
                    span,
                    &function_type.return_type,
                    &Type::Boolean,
                );
            }
        }
    }
}

fn bind_struct_declaration<'a, 'b>(
    struct_declaration: StructDeclarationNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) {
    let mut struct_function_table = SimpleStructFunctionTable::default();
    for statement in &struct_declaration.body.statements {
        match &statement.kind {
            SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
                let function_name = function_declaration.identifier.lexeme;
                if !function_name.starts_with('$') {
                    continue;
                }
                let struct_function_kind = match StructFunctionKind::try_from(function_name) {
                    Ok(it) => it,
                    Err(_) => {
                        // Note: This error will be reported later, when all
                        // functions inside the struct are type checked,
                        // right now we are only interested in the correctly
                        // named functions for our function table.
                        continue;
                    }
                };
                struct_function_table.set(struct_function_kind, binder.generate_label());
            }
            SyntaxNodeKind::StructField(_) => {}
            _ => {}
        }
    }
    if let Some(id) =
        binder.register_struct_name(struct_declaration.identifier.lexeme, struct_function_table)
    {
        if struct_declaration.optional_generic_keyword.is_some() {
            binder.generic_structs.push(StructDeclarationBody {
                name: struct_declaration.identifier.lexeme.into(),
                id,
                body: *struct_declaration.body,
                _is_generic: struct_declaration.optional_generic_keyword.is_some(),
                struct_function_table,
            });
        } else {
            binder.structs.push(StructDeclarationBody {
                name: struct_declaration.identifier.lexeme.into(),
                id,
                body: *struct_declaration.body,
                _is_generic: struct_declaration.optional_generic_keyword.is_some(),
                struct_function_table,
            });
        }
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
        bind_type(it.return_type, binder).convert_typed_generic_struct_to_struct()
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
            is_generic: function_type.is_generic,
        },
        parameters,
    )
}

// FIXME: We already have struct which contains all the struct_ fields, why are
// not using it directly??
fn bind_struct_body<'a>(
    struct_name: &str,
    struct_id: u64,
    struct_body: StructBodyNode<'a>,
    struct_function_table: SimpleStructFunctionTable,
    binder: &mut BindingState<'a, '_>,
) -> (Vec<BoundStructFieldSymbol<'a>>, StructFunctionTable) {
    let mut fields: Vec<BoundStructFieldSymbol> = vec![];
    let mut function_table = StructFunctionTable::default();
    let mut offset = 0;
    let struct_is_generic = struct_body.is_generic;
    for statement in struct_body.statements {
        let span = statement.span;
        let is_function;
        let field = match statement.kind {
            SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
                is_function = true;
                bind_function_declaration_for_struct(
                    struct_name,
                    struct_id,
                    struct_is_generic,
                    *function_declaration,
                    &mut function_table,
                    struct_function_table,
                    binder,
                )
            }
            SyntaxNodeKind::StructField(struct_field) => {
                is_function = false;
                let (name, type_) = bind_parameter(struct_field.field, binder);
                let field_offset = {
                    offset += type_.size_in_bytes();
                    offset - type_.size_in_bytes()
                };
                Some(BoundStructFieldSymbol {
                    name: name.into(),
                    offset: field_offset,
                    type_,
                    is_read_only: false,
                })
            }
            unexpected => unreachable!("Unexpected Struct Member {:#?} found!", unexpected),
        };
        if field.is_none() {
            continue;
        }
        let field = field.unwrap();
        if !is_function && fields.iter().any(|f| f.name == field.name) {
            binder
                .diagnostic_bag
                .report_field_already_declared(span, &field.name);
        }
        fields.push(field);
    }
    (fields, function_table)
}

fn bind_parameter<'a>(parameter: ParameterNode<'a>, binder: &mut BindingState) -> (&'a str, Type) {
    let name = parameter.identifier.lexeme;
    let type_ = bind_type(parameter.type_declaration.type_, binder).convert_typed_generic_struct_to_struct();
    (name, type_)
}

fn bind_type(type_: TypeNode, binder: &mut BindingState) -> Type {
    let type_name = type_.type_name.lexeme;
    let type_span = type_.span();
    let mut result = if let Some(library_token) = &type_.library_name {
        let library_name = library_token.lexeme;
        let library_variable = binder.look_up_variable_or_constant_by_name(library_name);
        let library_variable = match library_variable.kind {
            VariableOrConstantKind::None => {
                binder
                    .diagnostic_bag
                    .report_unknown_library(library_token.span(), library_name);
                return Type::Error;
            }
            _ => library_variable,
        };
        match &library_variable.type_ {
            Type::Error => return Type::Error,
            Type::Library(_) => {}
            wrong_type => {
                binder.diagnostic_bag.report_cannot_convert(
                    library_token.span(),
                    wrong_type,
                    &Type::Library(0),
                );
                return Type::Error;
            }
        }
        binder.look_up_type_by_generated_name(format!("{}.{}", library_name, type_name))
    } else {
        binder.look_up_type_by_name(type_name)
    }
    .unwrap_or_else(|| {
        binder
            .diagnostic_bag
            .report_unknown_type(type_span, &type_.full_type_name());
        Type::Error
    });
    if type_.optional_question_mark.is_some() {
        result = Type::noneable(result);
    }
    if type_.optional_ampersand_token.is_some() {
        result = Type::pointer_of(result);
    }
    if !type_.brackets.is_empty() {
        let array_base_type = binder.look_up_std_struct_id(StdTypeKind::Array);
        for _ in &type_.brackets {
            let (new_type, _) =
                bind_generic_struct_type_for_type(array_base_type, &result, None, binder);
            result = Type::TypedGenericStruct(Box::new(TypedGenericStructType {
                id: new_type.id,
                type_: result.clone(),
                function_table: new_type.function_table.clone(),
                struct_type: new_type,
            }));
        }
    }
    result
}

fn bind_node<'a, 'b>(node: SyntaxNode<'a>, binder: &mut BindingState<'a, 'b>) -> BoundNode {
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
) -> BoundNode {
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

fn bind_literal(span: TextSpan, literal: LiteralNodeKind, _: &mut BindingState) -> BoundNode {
    BoundNode::literal(span, literal.value)
}

fn bind_array_literal<'a, 'b>(
    span: TextSpan,
    mut array_literal: ArrayLiteralNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let array_type = binder.look_up_std_struct_id(StdTypeKind::Array);
    let expected_type = if let Some(Type::TypedGenericStruct(typed_generic_struct_type)) = &binder.expected_type {
        Some(typed_generic_struct_type.type_.clone())
    } else {
        None
    };
    let first_child = array_literal.children.remove(0);
    let (mut type_, mut children) = bind_array_literal_first_child(first_child, expected_type, binder);
    for child in array_literal.children {
        let (child, repetition) =
            if let SyntaxNodeKind::RepetitionNode(repetition_node) = child.kind {
                let base_expression = bind_node(*repetition_node.base_expression, binder);
                let repetition = bind_node(*repetition_node.repetition, binder);
                let repetition = bind_conversion(repetition, &Type::Integer(IntegerType::Unsigned64), binder);
                let repetition = match repetition.constant_value {
                    Some(value) => value.value.as_integer().unwrap(),
                    None => {
                        binder
                            .diagnostic_bag
                            .report_expected_constant(repetition.span);
                        1
                    }
                } as usize;
                (base_expression, repetition)
            } else {
                (bind_node(child, binder), 1)
            };
        // If we only read integer literals until now, and the current entry is
        // not an integer literal, try using it as the element type of the
        // array.
        if type_ == Type::IntegerLiteral && child.type_ != Type::IntegerLiteral {
            if type_.can_be_converted_to(&child.type_) {
                // FIXME: Do we need to update all children until now??
                type_ = child.type_.clone();
            }
        }
        let child = bind_conversion(child, &type_, binder);
        for _ in 0..repetition {
            children.push(child.clone());
        }
    }
    if type_ == Type::IntegerLiteral {
        type_ = Type::Integer(IntegerType::Signed64);
    }
    let type_ = bind_generic_struct_type_for_type(array_type, &type_, None, binder).0;
    let type_ = Type::Struct(Box::new(type_));
    BoundNode::array_literal(span, children, type_)
}

fn bind_array_literal_first_child<'a>(
    first_child: SyntaxNode<'a>,
    expected_type: Option<Type>,
    binder: &mut BindingState<'a, '_>,
) -> (Type, Vec<BoundNode>) {
    let children = if let SyntaxNodeKind::RepetitionNode(repetition_node) = first_child.kind {
        let base_expression = bind_node(*repetition_node.base_expression, binder);
        let repetition = bind_node(*repetition_node.repetition, binder);
        let repetition = bind_conversion(repetition, &Type::Integer(IntegerType::Unsigned64), binder);
        let repetition = match repetition.constant_value {
            Some(value) => value.value.as_integer().unwrap(),
            None => {
                binder
                    .diagnostic_bag
                    .report_expected_constant(repetition.span);
                1
            }
        } as usize;
        vec![base_expression; repetition]
    } else {
        vec![bind_node(first_child, binder)]
    };
    let type_ = expected_type.unwrap_or_else(|| children[0].type_.clone());
    // If expected_type was None, this would be a noop.
    let children = children
        .into_iter()
        .map(|c| bind_conversion(c, &type_, binder))
        .collect();
    (type_, children)
}

fn bind_cast_expression<'a>(
    span: TextSpan,
    cast_expression: CastExpressionNodeKind<'a>,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode {
    let expression = bind_node(*cast_expression.expression, binder);
    let type_ = bind_type(cast_expression.type_, binder).convert_typed_generic_struct_to_struct();
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
) -> BoundNode {
    let type_name = constructor_call.type_name.lexeme;
    let type_name_span = constructor_call.type_name.span();
    let type_ = bind_type(
        TypeNode {
            optional_ampersand_token: None,
            library_name: constructor_call.library_name,
            type_name: constructor_call.type_name,
            optional_question_mark: None,
            brackets: vec![],
        },
        binder,
    ).convert_typed_generic_struct_to_struct();
    let (struct_type, struct_id) = match type_ {
        Type::Struct(it) => (binder.get_struct_type_by_id(it.id).unwrap(), it.id),
        Type::StructReference(id) => (binder.get_struct_type_by_id(id.id).unwrap(), id.id),
        Type::Error => return BoundNode::error(span),
        _ => {
            binder
                .diagnostic_bag
                .report_unknown_type(type_name_span, type_name);
            return BoundNode::error(span);
        }
    };
    let struct_type = struct_type.clone();

    let (arguments, function, struct_type) = match &struct_type.function_table.constructor_function
    {
        Some(function) => {
            if struct_type.is_generic {
                let (arguments, label, struct_type) =
                    bind_arguments_for_generic_constructor_on_struct(
                        span,
                        constructor_call.arguments,
                        function.function_label as usize,
                        &function.function_type,
                        struct_id,
                        binder,
                    );
                (arguments, Some(label as u64), struct_type)
            } else {
                (
                    bind_arguments_for_function(
                        span,
                        constructor_call.arguments,
                        &function.function_type,
                        binder,
                    ),
                    Some(function.function_label),
                    binder.get_struct_by_id(struct_id).unwrap(),
                )
            }
        }
        None => {
            // HACK: We somehow get functions in our fields, if we use generic
            // structs, so we filter them out, since user can not declare
            // functions as fields of the time being.
            let fields: Vec<_> = struct_type
                .fields
                .iter()
                .filter(|f| !matches!(f.type_, Type::Function(_)))
                .collect();
            if constructor_call.arguments.len() != fields.len() {
                binder.diagnostic_bag.report_unexpected_argument_count(
                    span,
                    constructor_call.arguments.len(),
                    fields.len(),
                );
            }
            let mut arguments = Vec::with_capacity(constructor_call.arguments.len());
            let mut resolved_struct_type = if struct_type.is_generic {
                None
            } else {
                Some(binder.get_struct_by_id(struct_id).unwrap())
            };
            for (argument, parameter_type) in constructor_call
                .arguments
                .into_iter()
                .zip(fields.iter().map(|f| &f.type_))
            {
                let argument = bind_node(argument, binder);
                let parameter_type = if struct_type.is_generic {
                    if parameter_type == &Type::GenericType {
                        if binder.generic_type.is_none() {
                            binder.generic_type = Some(argument.type_.clone());
                            resolved_struct_type = Some(
                                bind_generic_struct_type_for_type(
                                    struct_id,
                                    &argument.type_,
                                    None,
                                    binder,
                                )
                                .0,
                            );
                        }
                        argument.type_.clone()
                    } else if parameter_type == &Type::PointerOf(Box::new(Type::GenericType)) {
                        if binder.generic_type.is_none() {
                            if let Type::PointerOf(type_) = argument.type_.clone() {
                                binder.generic_type = Some(*type_.clone());
                                resolved_struct_type = Some(
                                    bind_generic_struct_type_for_type(
                                        struct_id, &type_, None, binder,
                                    )
                                    .0,
                                );
                            } else {
                                // We know this is wrong, but hey, im sure there
                                // will be a compile time error somewhere.
                                binder.generic_type = Some(argument.type_.clone());
                            }
                        }
                        argument.type_.clone()
                    } else {
                        parameter_type.clone()
                    }
                } else {
                    parameter_type.clone()
                };
                let argument = bind_conversion(argument, &parameter_type, binder);
                arguments.push(argument);
            }
            binder.generic_type = None;
            (arguments, None, resolved_struct_type.unwrap())
        }
    };

    BoundNode::constructor_call(span, arguments, struct_type, function)
}

fn bind_generic_struct_type_for_type(
    struct_id: u64,
    type_: &Type,
    constructor_label: Option<usize>,
    binder: &mut BindingState,
) -> (StructType, Option<usize>) {
    let mut generic_struct = binder
        .get_generic_struct_type_by_id(struct_id)
        .unwrap()
        .clone();
    let struct_name = if let Some(struct_id) = type_.as_struct_id() {
        format!("{}<struct#{}>", generic_struct.struct_type.name, struct_id)
    } else {
        format!("{}<{}>", generic_struct.struct_type.name, type_)
    };
    let mut changed_constructor_label = None;

    let id = if let Some(id) = binder.look_up_struct_id_by_name(&struct_name) {
        if let Some(constructor_label) = constructor_label {
            changed_constructor_label = binder
                .get_struct_type_by_id(id)
                .unwrap()
                .function_table
                .label_relocation
                .iter()
                .find(|(old, _)| *old == constructor_label as u64)
                .map(|(_, new)| *new as usize);
        }
        id
    } else {
        let function_labels_map : HashMap<_, _> = generic_struct.functions.iter().map(|f| (f.function_label as usize, binder.generate_label())).collect();
        let mut simple_function_table = SimpleStructFunctionTable::default();
        for (kind, function) in generic_struct
            .struct_type
            .function_table
            .available_struct_function_kinds().into_iter().zip(generic_struct.struct_type.function_table.function_symbols_iter_mut()) // TODO: Use a function_symbols_iter instead of iter_mut
        {
            simple_function_table.set(kind, *function_labels_map.get(&(function.function_label as usize)).unwrap());
        }
        let id = binder
            .register_generated_struct_name(struct_name.clone(), simple_function_table)
            .unwrap();
        let mut fields = generic_struct.struct_type.fields.clone();
        let mut function_labels = Vec::with_capacity(generic_struct.functions.len());
        for function in generic_struct.functions.iter_mut() {
            let old_label = function.function_label;
            let target_label = StructFunctionKind::try_from(function.function_name.as_str())
                .ok()
                .map(|k| simple_function_table.get(k))
                .flatten().or(Some(*function_labels_map.get(&(old_label as usize)).unwrap()));
            let new_label =
                bind_generic_function_for_type(function, true, type_, target_label, function_labels_map.clone(), binder) as u64;
            if Some(old_label as usize) == constructor_label {
                changed_constructor_label = Some(new_label as usize);
            }
            let function_name = format!(
                "{}::{}",
                struct_name,
                function.function_name.split_once("::").unwrap().1
            );
            let mut function_type = function.function_type.clone();
            type_replacer::replace_type_with_other_type_in_function_type(
                &mut function_type,
                &Type::GenericType,
                type_,
            );
            binder.register_generated_constant(
                function_name,
                Value::LabelPointer(new_label as usize, Type::function(function_type)),
            );
            function_labels.push((old_label, new_label));
        }
        fields.iter_mut().for_each(|f| {
            type_replacer::replace_type_with_other_type(&mut f.type_, &Type::GenericType, type_);
        });
        let mut function_table = generic_struct
            .struct_type
            .function_table
            .clone()
            .replace_labels(function_labels);
        function_table.function_symbols_iter_mut().for_each(|s| {
            type_replacer::replace_type_with_other_type_in_function_type(
                &mut s.function_type,
                &Type::GenericType,
                type_,
            )
        });
        binder.register_struct(id, fields, function_table);
        id
    };
    (
        binder.get_struct_by_id(id).unwrap(),
        changed_constructor_label,
    )
}

fn bind_variable<'a, 'b>(
    span: TextSpan,
    variable: VariableNodeKind,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let variable_name = variable.token.lexeme;
    let variable = binder.look_up_variable_or_constant_by_name(variable_name);
    match variable.kind {
        VariableOrConstantKind::None => {
            binder
                .diagnostic_bag
                .report_variable_not_found(span, variable_name);
            BoundNode::error(span)
        }
        VariableOrConstantKind::Variable(variable_index) => {
            BoundNode::variable(span, variable_index, variable.type_)
        }
        VariableOrConstantKind::Constant(value) => {
            let mut result = BoundNode::literal_from_value(value);
            result.span = span;
            result
        }
    }
}

fn bind_variable_for_assignment<'a, 'b>(
    span: TextSpan,
    variable: VariableNodeKind,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let variable_is_read_only = binder
        .look_up_variable_or_constant_by_name(variable.token.lexeme)
        .is_read_only;
    if variable_is_read_only {
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
) -> BoundNode {
    let lhs = bind_node(*binary.lhs, binder);
    let rhs = bind_node(*binary.rhs, binder);
    bind_binary_insertion(span, lhs, binary.operator_token, rhs, binder)
}

fn bind_binary_insertion<'a>(
    span: TextSpan,
    lhs: BoundNode,
    operator_token: SyntaxToken,
    rhs: BoundNode,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode {
    match bind_binary_operator(span, &lhs, operator_token, &rhs, binder) {
        Some(bound_binary) => {
            let (lhs, rhs) = if bound_binary.op == BoundBinaryOperator::StringConcat {
                (call_to_string(lhs, binder), call_to_string(rhs, binder))
            } else {
                (
                    bind_conversion(lhs, &bound_binary.lhs, binder),
                    bind_conversion(rhs, &bound_binary.rhs, binder),
                )
            };
            match bound_binary.op {
                BoundBinaryOperator::Range => {
                    let base_type = if matches!(lhs.type_, Type::Integer(integer_type) if integer_type.is_signed()) {
                    binder
                        .get_struct_by_id(binder.look_up_std_struct_id(StdTypeKind::Range))
                        .unwrap()
                    } else {
                        binder.get_struct_by_id(binder.look_up_std_struct_id(StdTypeKind::UnsignedRange))
                        .unwrap()
                    };
                    let function = base_type
                        .function_table
                        .constructor_function
                        .as_ref()
                        .map(|c| c.function_label);
                    BoundNode::constructor_call(span, vec![lhs, rhs], base_type, function)
                }
                BoundBinaryOperator::LogicalAnd => BoundNode::logical_and(span, lhs, rhs),
                BoundBinaryOperator::LogicalOr => BoundNode::logical_or(span, lhs, rhs),
                BoundBinaryOperator::Equals => {
                    let type_ = binder.convert_struct_reference_to_struct(lhs.type_.clone());
                    if let Type::Struct(struct_type) = type_ {
                        if let Some(equals_function) = struct_type.function_table.equals_function {
                            let base = BoundNode::label_reference(
                                equals_function.function_label as usize,
                                Type::function(equals_function.function_type),
                            );
                            BoundNode::function_call(
                                span,
                                base,
                                vec![lhs, rhs],
                                false,
                                Type::Boolean,
                            )
                        } else {
                            BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                        }
                    } else {
                        BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                    }
                }
                BoundBinaryOperator::NotEquals => {
                    let type_ = binder.convert_struct_reference_to_struct(lhs.type_.clone());
                    if let Type::Struct(struct_type) = type_ {
                        if let Some(equals_function) = struct_type.function_table.equals_function {
                            let base = BoundNode::label_reference(
                                equals_function.function_label as usize,
                                Type::function(equals_function.function_type),
                            );
                            BoundNode::unary(
                                span,
                                BoundUnaryOperator::LogicalNegation,
                                BoundNode::function_call(
                                    span,
                                    base,
                                    vec![lhs, rhs],
                                    false,
                                    Type::Boolean,
                                ),
                                Type::Boolean,
                            )
                        } else {
                            BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                        }
                    } else {
                        BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                    }
                }
                _ => BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result),
            }
        }
        None => BoundNode::error(span),
    }
}

fn call_to_string(lhs: BoundNode, binder: &mut BindingState) -> BoundNode {
    if lhs.type_ == Type::String {
        lhs
    } else {
        BoundNode::system_call(
            lhs.span,
            SystemCallKind::ToString,
            vec![bind_conversion(lhs, &Type::Any, binder)],
            Type::String,
        )
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
        SyntaxTokenKind::PeriodPeriod => BoundBinaryOperator::Range,
        SyntaxTokenKind::AmpersandAmpersand => BoundBinaryOperator::LogicalAnd,
        SyntaxTokenKind::PipePipe => BoundBinaryOperator::LogicalOr,
        _ => unreachable!(),
    };
    match (&lhs.type_, result, &rhs.type_) {
        (lhs, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, rhs)
            if lhs == rhs && lhs != &Type::Void =>
        {
            Some(BoundBinary::same_input(lhs, result, Type::Boolean))
        }
        (Type::Integer(lhs), BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, Type::IntegerLiteral) =>
        {
            Some(BoundBinary::same_input(&Type::Integer(*lhs), result, Type::Boolean))
        }
        (Type::IntegerLiteral, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, Type::Integer(rhs)) =>
        {
            Some(BoundBinary::same_input(&Type::Integer(*rhs), result, Type::Boolean))
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
        ) if outer.can_be_converted_to(inner) && outer != &Type::Void => Some(BoundBinary::same_input(
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
            Type::Pointer | Type::PointerOf(_),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::None,
        )
        | (
            Type::None,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Pointer | Type::PointerOf(_),
        ) => Some(BoundBinary::same_input(&Type::Pointer, result, Type::Boolean)),
        (
            Type::Integer(lhs_integer_type),
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer(rhs_integer_type),
        ) if lhs_integer_type == rhs_integer_type => Some(BoundBinary::same_output(result, Type::Integer(*lhs_integer_type))),
        (
            Type::Integer(lhs_integer_type),
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer(rhs_integer_type),
        ) if lhs_integer_type.equals_ignoring_sign(rhs_integer_type) => Some(BoundBinary::same_output(result, Type::Integer(lhs_integer_type.to_signed()))),
        (
            Type::Integer(lhs_integer_type),
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_output(result, Type::Integer(*lhs_integer_type))),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer(rhs_integer_type),
        ) => Some(BoundBinary::same_output(result, Type::Integer(*rhs_integer_type))),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_output(result, Type::IntegerLiteral)),
        (
            Type::Integer(lhs_integer_type),
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer(_) | Type::IntegerLiteral,
        ) => Some(BoundBinary::same_input(
            &Type::Integer(*lhs_integer_type),
            result,
            Type::Boolean,
        )),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer(rhs_integer_type),
        ) => Some(BoundBinary::same_input(
            &Type::Integer(*rhs_integer_type),
            result,
            Type::Boolean,
        )),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_input(
            &Type::IntegerLiteral,
            result,
            Type::Boolean,
        )),
        (
            Type::Boolean,
            BoundBinaryOperator::LogicalAnd | BoundBinaryOperator::LogicalOr,
            Type::Boolean,
        ) => Some(BoundBinary::same_output(result, Type::Boolean)),
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
        (Type::Integer(IntegerType::Signed64) | Type::IntegerLiteral, BoundBinaryOperator::Range, Type::Integer(IntegerType::Signed64) | Type::IntegerLiteral) => {
            let range_type = binder.look_up_std_struct_id(StdTypeKind::Range);
            let range_type = binder.get_struct_by_id(range_type).unwrap();
            let range_type = Type::Struct(Box::new(range_type));
            Some(BoundBinary::same_input(&Type::Integer(IntegerType::Signed64), result, range_type))
        }
        (Type::Integer(IntegerType::Unsigned64) | Type::IntegerLiteral, BoundBinaryOperator::Range, Type::Integer(IntegerType::Unsigned64) | Type::IntegerLiteral) => {
            let range_type = binder.look_up_std_struct_id(StdTypeKind::Range);
            let range_type = binder.get_struct_by_id(range_type).unwrap();
            let range_type = Type::Struct(Box::new(range_type));
            Some(BoundBinary::same_input(&Type::Integer(IntegerType::Unsigned64), result, range_type))
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
) -> BoundNode {
    let operand = bind_node(*unary.operand, binder);
    match bind_unary_operator(span, &operand, unary.operator_token, binder) {
        Some((operator_token, type_)) => BoundNode::unary(span, operator_token, operand, type_),
        None => BoundNode::error(span),
    }
}

fn bind_unary_operator<'a, 'b>(
    span: TextSpan,
    operand: &BoundNode,
    operator_token: SyntaxToken<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> Option<(BoundUnaryOperator, Type)> {
    let result = match &operator_token.kind {
        SyntaxTokenKind::Plus => BoundUnaryOperator::ArithmeticIdentity,
        SyntaxTokenKind::Minus => BoundUnaryOperator::ArithmeticNegate,
        SyntaxTokenKind::Bang => BoundUnaryOperator::LogicalNegation,
        _ => unreachable!(),
    };
    match operand.type_ {
        Type::Integer(integer_type) if result != BoundUnaryOperator::LogicalNegation => {
            if result == BoundUnaryOperator::ArithmeticNegate {
                Some((result, Type::Integer(integer_type.to_signed())))
            } else {
                Some((result, Type::Integer(integer_type)))
            }
        }
        // FIXME: Is there a conversion of the operand needed?
        Type::IntegerLiteral if result != BoundUnaryOperator::LogicalNegation => {
            if result == BoundUnaryOperator::ArithmeticNegate {
                Some((result, Type::Integer(IntegerType::Signed64)))
            } else {
                Some((result, Type::IntegerLiteral))
            }
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
) -> BoundNode {
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
) -> BoundNode {
    let argument_span = function_call.argument_span();
    let function = bind_node(*function_call.base, binder);
    let mut arguments = vec![];
    let mut more_arguments = vec![];
    let function = if let BoundNodeKind::Closure(mut closure) = function.kind {
        more_arguments.append(&mut closure.arguments);
        let type_ = if let Type::Closure(closure_type) = function.type_ {
            Type::Function(Box::new(closure_type.base_function_type))
        } else {
            unreachable!("There is a closure, which is not of type closure???");
        };
        match closure.function {
            typing::FunctionKind::FunctionId(id) => BoundNode::variable(function.span, id, type_),
            typing::FunctionKind::SystemCall(kind) => {
                BoundNode::literal_from_value(Value::SystemCall(kind))
            }
            typing::FunctionKind::LabelReference(label_reference) => {
                BoundNode::label_reference(label_reference, type_)
            }
        }
    } else {
        function
    };
    if matches!(function.type_, Type::Error) {
        return BoundNode::error(span);
    }
    let function_type = function_type(&function.type_);
    if function_type.is_generic {
        todo!("Generic functions are not yet supported!");
        // let old_label = function
        //     .constant_value
        //     .unwrap()
        //     .value
        //     .as_label_pointer()
        //     .unwrap()
        //     .0;
        // let (mut tmp_arguments, label) = bind_arguments_for_generic_function(
        //     argument_span,
        //     function_call.arguments,
        //     old_label,
        //     &function_type,
        //     HashMap::default(),
        //     binder,
        // );
        // function = BoundNode::label_reference(label, function.type_);
        // arguments.append(&mut tmp_arguments);
    } else {
        arguments.append(&mut bind_arguments_for_function(
            argument_span,
            function_call.arguments,
            &function_type,
            binder,
        ));
    }
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
) -> BoundNode {
    let index = bind_node(*array_index.index, binder);
    let index = bind_conversion(index, &Type::Integer(IntegerType::Unsigned64), binder);
    let base_span = array_index.base.span;
    let base = bind_node(*array_index.base, binder);
    let type_ = match binder.convert_struct_reference_to_struct(base.type_.clone()) {
        Type::PointerOf(base_type) => *base_type,
        Type::Struct(struct_type) => match struct_type.function_table.get_function {
            Some(get_function) => {
                let function_base = BoundNode::label_reference(
                    get_function.function_label as _,
                    Type::function(get_function.function_type.clone()),
                );
                return BoundNode::function_call(
                    span,
                    function_base,
                    vec![index, base],
                    false,
                    get_function.function_type.return_type,
                );
            }
            None => {
                binder
                    .diagnostic_bag
                    .report_cannot_index_get(base_span, &base.type_);
                Type::Error
            }
        },
        error => {
            binder
                .diagnostic_bag
                .report_cannot_index_get(base_span, &error);
            Type::Error
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_array_index_for_assignment<'a, 'b>(
    span: TextSpan,
    array_index: ArrayIndexNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let index = bind_node(*array_index.index, binder);
    let index = bind_conversion(index, &Type::Integer(IntegerType::Unsigned64), binder);
    let base_span = array_index.base.span;
    let base = bind_node(*array_index.base, binder);
    let type_ = match base.type_.clone() {
        Type::PointerOf(base_type) => *base_type,
        Type::Struct(struct_type) => match struct_type.function_table.set_function {
            Some(set_function) => {
                let type_ = Type::closure(set_function.function_type.clone());
                return BoundNode::closure_label(
                    span,
                    vec![base, index],
                    set_function.function_label as _,
                    type_,
                );
            }
            None => {
                binder
                    .diagnostic_bag
                    .report_cannot_index_set(base_span, &base.type_);
                Type::Error
            }
        },
        error => {
            binder
                .diagnostic_bag
                .report_cannot_index_get(base_span, &error);
            Type::Error
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_field_access<'a, 'b>(
    span: TextSpan,
    field_access: FieldAccessNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let base_span = field_access.base.span();
    let field = field_access.field;
    let base = bind_node(*field_access.base, binder);
    let mut struct_handler = |id: u64, base| {
        let field_name = field.lexeme;
        let bound_struct_type = binder
            .get_struct_type_by_id(id)
            .unwrap_or_else(|| {
                panic!("Referenced a struct, which doesn't exist, somehow.");
            })
            .clone();
        let field_symbol = if binder.is_struct_function {
            bound_struct_type.field_fields_first(field_name)
        } else {
            bound_struct_type.field_functions_first(field_name)
        };
        if let Some(field) = field_symbol {
            let function_name = format!("{}::{}", bound_struct_type.name, field_name);
            if let Type::Function(function_type) = &field.type_ {
                let type_ = Type::closure(*function_type.clone());
                let variable = binder.look_up_variable_or_constant_by_name(&function_name);
                match variable.kind {
                    VariableOrConstantKind::None => {
                        unreachable!(
                            "The binder missed a function on a struct somehow ({})",
                            function_name
                        )
                    }
                    VariableOrConstantKind::Variable(variable_id) => {
                        BoundNode::closure(span, vec![base], variable_id, type_)
                    }
                    VariableOrConstantKind::Constant(value) => {
                        let label_index = value.as_label_pointer().unwrap().0;
                        BoundNode::closure_label(span, vec![base], label_index, type_)
                    }
                }
            } else {
                BoundNode::field_access(span, base, field.offset, field.type_.clone())
            }
        } else {
            binder.diagnostic_bag.report_no_field_named_on_struct(
                field.span(),
                field_name,
                bound_struct_type,
            );
            BoundNode::error(span)
        }
    };
    match &base.type_ {
        Type::Error => base,
        Type::StructReference(id) => struct_handler(id.id, base),
        Type::Struct(struct_type) => struct_handler(struct_type.id, base),
        Type::TypedGenericStruct(typed_generic_struct) => {
            struct_handler(typed_generic_struct.id, base)
        }
        Type::Library(index) => {
            let library = &binder.libraries[*index];
            let function_name = field.lexeme;
            if let Some(function) = library.look_up_function_by_name(function_name) {
                BoundNode::label_reference(
                    function.function_label as _,
                    Type::function(function.function_type.clone()),
                )
            } else {
                binder.diagnostic_bag.report_no_field_named_on_type(
                    span,
                    function_name,
                    &Type::Library(*index),
                );
                BoundNode::error(span)
            }
        }
        Type::Any
        | Type::Void
        | Type::Integer(_)
        | Type::IntegerLiteral
        | Type::Boolean
        | Type::None
        | Type::Function(_)
        | Type::Closure(_)
        | Type::SystemCall(_)
        | Type::Pointer
        | Type::GenericType
        | Type::PointerOf(_)
        | Type::Noneable(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base_span, &base.type_);
            BoundNode::error(span)
        }
        Type::String => {
            if field.lexeme == "length" {
                let base = bind_conversion(base, &Type::Any, binder);
                let function_type = FunctionType::system_call(SystemCallKind::ArrayLength);
                BoundNode::system_call_closure(
                    span,
                    vec![base],
                    SystemCallKind::ArrayLength,
                    Type::closure(function_type),
                )
            } else {
                binder.diagnostic_bag.report_no_field_named_on_type(
                    span,
                    field.lexeme,
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
) -> BoundNode {
    let base_is_this = if let SyntaxNodeKind::Variable(identifier) = &field_access.base.kind {
        identifier.token.lexeme == "this"
    } else {
        false
    };
    let base = bind_node(*field_access.base, binder);
    let field = field_access.field;
    let mut struct_handler = |id: u64, base| {
        let field_name = field.lexeme;
        let bound_struct_type = binder
            .get_struct_type_by_id(id)
            .unwrap_or_else(|| {
                panic!("Referenced a struct, which doesn't exist, somehow.");
            })
            .clone();
        let field_symbol = if binder.is_struct_function {
            bound_struct_type.field_fields_first(field_name)
        } else {
            bound_struct_type.field_functions_first(field_name)
        };
        if let Some(field) = field_symbol {
            if field.is_read_only {
                binder.diagnostic_bag.report_cannot_assign_to(span);
                BoundNode::error(span)
            } else {
                if base_is_this && !binder.assigned_fields.contains(&field_name) {
                    binder.assigned_fields.push(field_name);
                }
                BoundNode::field_access(span, base, field.offset, field.type_.clone())
            }
        } else {
            binder.diagnostic_bag.report_no_field_named_on_struct(
                field.span(),
                field_name,
                bound_struct_type,
            );
            BoundNode::error(span)
        }
    };
    match &base.type_ {
        Type::Error => base,
        Type::Any
        | Type::Void
        | Type::Integer(_)
        | Type::IntegerLiteral
        | Type::Boolean
        | Type::None
        | Type::SystemCall(_)
        | Type::Noneable(_)
        | Type::Function(_)
        | Type::Pointer
        | Type::PointerOf(_)
        | Type::GenericType
        | Type::Closure(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base.span, &base.type_);
            BoundNode::error(span)
        }
        Type::String | Type::Library(_) => {
            binder.diagnostic_bag.report_cannot_assign_to(span);
            BoundNode::error(span)
        }
        Type::StructReference(id) => struct_handler(id.id, base),
        Type::Struct(struct_type) => struct_handler(struct_type.id, base),
        Type::TypedGenericStruct(typed_generic_struct) => {
            struct_handler(typed_generic_struct.id, base)
        }
    }
}

fn bind_arguments_for_function<'a, 'b>(
    span: TextSpan,
    arguments: Vec<SyntaxNode<'a>>,
    function_type: &FunctionType,
    binder: &mut BindingState<'a, 'b>,
) -> Vec<BoundNode> {
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
            result.push(BoundNode::error(argument.span));
            continue;
        }
        argument = bind_conversion(argument, parameter_type, binder);
        result.push(argument);
    }
    result
}

#[allow(dead_code)]
fn bind_arguments_for_generic_function<'a, 'b>(
    span: TextSpan,
    arguments: Vec<SyntaxNode<'a>>,
    label: usize,
    function_type: &FunctionType,
    function_labels_map: HashMap<usize, usize>,
    binder: &mut BindingState<'a, 'b>,
) -> (Vec<BoundNode>, usize) {
    let mut result = vec![];
    let mut changed_label = 0;
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
            result.push(BoundNode::error(argument.span));
            continue;
        }
        let parameter_type = if matches!(parameter_type, Type::GenericType) {
            if binder.generic_type.is_none() {
                binder.generic_type = Some(argument.type_.clone());
                let mut generic_function = binder
                    .find_generic_function_by_label(label)
                    .unwrap()
                    .clone();
                changed_label = bind_generic_function_for_type(
                    &mut generic_function,
                    function_type.this_type.is_some(),
                    &argument.type_,
                    // Note: this may be wrong if a $ function is compiled. Then
                    // again, this should not happen, since this gets called by
                    // function calls and you cannot call al $ function.
                    None,
                    function_labels_map.clone(),
                    binder,
                );
            }
            binder.generic_type.clone().unwrap()
        } else {
            parameter_type.clone()
        };
        argument = bind_conversion(argument, &parameter_type, binder);
        result.push(argument);
    }
    binder.generic_type = None;
    (result, changed_label)
}

fn bind_arguments_for_generic_constructor_on_struct<'a, 'b>(
    span: TextSpan,
    arguments: Vec<SyntaxNode<'a>>,
    label: usize,
    function_type: &FunctionType,
    struct_id: u64,
    binder: &mut BindingState<'a, 'b>,
) -> (Vec<BoundNode>, usize, StructType) {
    let mut result = vec![];
    let mut changed_label = 0;
    if function_type.parameter_types.len() != arguments.len() {
        binder.diagnostic_bag.report_unexpected_argument_count(
            span,
            arguments.len(),
            function_type.parameter_types.len(),
        );
    }

    let mut struct_type = None;

    for (argument, parameter_type) in arguments
        .into_iter()
        .zip(function_type.parameter_types.iter())
    {
        let mut argument = bind_node(argument, binder);
        if matches!(argument.type_, Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.span);
            result.push(BoundNode::error(argument.span));
            continue;
        }
        let parameter_type = if matches!(parameter_type, Type::GenericType) {
            struct_type = if binder.generic_type.is_none() {
                let (struct_type, new_label) = bind_generic_struct_type_for_type(
                    struct_id,
                    &argument.type_,
                    Some(label),
                    binder,
                );
                binder.generic_type = Some(argument.type_.clone());
                changed_label = new_label.unwrap();
                Some(struct_type)
            } else {
                struct_type
            };
            binder.generic_type.clone().unwrap()
        } else {
            parameter_type.clone()
        };
        argument = bind_conversion(argument, &parameter_type, binder);
        result.push(argument);
    }
    binder.generic_type = None;
    (result, changed_label, struct_type.unwrap())
}

// TODO: Move this to after the binding.
fn bind_generic_function_for_type(
    generic_function: &mut GenericFunction,
    has_this_parameter: bool,
    type_: &Type,
    target_label: Option<usize>,
    function_labels_map: HashMap<usize, usize>,
    binder: &mut BindingState,
) -> usize {
    // TODO: Add new stage to compiler
    // let body = bind_node(generic_function.body, binder);
    let mut body = type_replacer::replace_generic_type_with(generic_function.body.clone(), type_);
    let mut labels = function_labels_map;
    body.for_each_child_mut(&mut |child: &mut BoundNode| {
        // child.span.set_is_foreign(true);
        match &mut child.kind {
            BoundNodeKind::Label(old_label) => {
                let new_label = binder.generate_label();
                labels.insert(*old_label, new_label);
                *old_label = new_label;
            }
            BoundNodeKind::LabelReference(old_label) if labels.contains_key(old_label) => {
                *old_label = labels[old_label];
            }
            _ => {}
        }
    });
    // Needs to be executed a second time, since we might have missed some
    // labels before.
    body.for_each_child_mut(&mut |child: &mut BoundNode| {
        // child.span.set_is_foreign(true);
        match &mut child.kind {
            BoundNodeKind::LabelReference(old_label) if labels.contains_key(old_label) => {
                *old_label = labels[old_label];
            }
            _ => {}
        }
    });
    assert!(!matches!(body.kind, BoundNodeKind::FunctionDeclaration(_)));
    let label = target_label.unwrap_or_else(|| binder.generate_label());
    generic_function.labels.push(label);
    // FIXME: Do NOT use just a stupid range here, if parameters
    // will ever not be the first register (like with local
    // functions or lambdas), then this will fail tragically!
    let parameter_count = generic_function.function_type.parameter_types.len() as u64
        + if has_this_parameter { 1 } else { 0 };
    let function =
        BoundNode::function_declaration(label, false, body, (0..parameter_count).collect());
    binder.bound_functions.push(function);
    label
}

fn bind_conversion<'a>(
    mut base: BoundNode,
    type_: &Type,
    binder: &mut BindingState<'a, '_>,
) -> BoundNode {
    if &base.type_ == type_ {
        base
    } else if base.type_.can_be_converted_to(type_) {
        base.type_ = binder.convert_struct_reference_to_struct(base.type_);
        let type_ = binder.convert_struct_reference_to_struct(type_.clone());
        BoundNode::conversion(base.span, base, type_)
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
    base: BoundNode,
    binder: &mut BindingState<'a, '_>,
) -> (BoundNode, SafeNodeInCondition) {
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
        | Type::Integer(_)
        | Type::IntegerLiteral
        | Type::SystemCall(_)
        | Type::String
        | Type::Function(_)
        | Type::Closure(_)
        | Type::Struct(_)
        | Type::StructReference(_)
        | Type::TypedGenericStruct(_)
        | Type::Pointer
        | Type::PointerOf(_) => (
            bind_conversion(base, &Type::Boolean, binder),
            SafeNodeInCondition::None,
        ),
        Type::Library(_) | Type::GenericType => unimplemented!(),
    }
}

fn register_contained_safe_nodes(base: &BoundNode) -> SafeNodeInCondition {
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
) -> BoundNode {
    let variable_count = binder.variable_table.len();
    let collection = bind_node(*for_statement.collection, binder);
    let struct_type = if let Type::Struct(struct_type) =
        binder.convert_struct_reference_to_struct(collection.type_.clone())
    {
        let is_iterable = struct_type.function_table.get_function.is_some()
            && struct_type.function_table.element_count_function.is_some();
        if !is_iterable {
            binder
                .diagnostic_bag
                .report_cannot_iterate(collection.span, &collection.type_);
            return BoundNode::error(span);
        }
        Some(*struct_type)
    } else {
        if collection.type_ != Type::Error {
            binder
                .diagnostic_bag
                .report_cannot_iterate(collection.span, &collection.type_);
        }
        return BoundNode::error(span);
    };
    let variable_type = struct_type
        .as_ref()
        .unwrap()
        .function_table
        .get_function
        .as_ref()
        .unwrap()
        .function_type
        .return_type
        .clone();
    let variable_name = for_statement.variable.lexeme;
    let variable_span = for_statement.variable.span();
    let variable = binder.register_variable(variable_name, variable_type.clone(), true);
    if variable.is_none() {
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(variable_span, variable_name);
        return BoundNode::error(span);
    }
    let variable = variable.unwrap();
    let variable = BoundNode::variable(span, variable, variable_type);
    let index_variable = match for_statement.optional_index_variable.map(|i| i.lexeme) {
        Some(index_variable) => binder.register_variable(index_variable, Type::Integer(IntegerType::Unsigned64), true),
        None => binder.register_generated_variable(
            format!("{}$index", variable_name),
            Type::Integer(IntegerType::Unsigned64),
            true,
        ),
    };
    if index_variable.is_none() {
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(variable_span, variable_name);
        return BoundNode::error(span);
    }
    let index_variable = index_variable.unwrap();

    let collection_variable = binder
        .register_generated_variable(
            format!("{}$collection", variable_name),
            collection.type_.clone(),
            true,
        )
        .unwrap();

    let body = bind_node(*for_statement.body, binder);
    let result = BoundNode::for_statement(
        span,
        index_variable,
        collection_variable,
        variable,
        collection,
        body,
        struct_type.unwrap().function_table,
    );
    binder.delete_variables_until(variable_count);

    result
}

fn bind_if_statement<'a, 'b>(
    span: TextSpan,
    if_statement: IfStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let condition = bind_node(*if_statement.condition, binder);
    // NOTE: In a very far future, you could do part of this before the binder,
    // by analyzing the syntax tree and marking all if conditions and their
    // occurrences later in the then statements and else statements. Then you
    // would not have to compare every bound node in a if statement with the
    // condition. But this is a very far future, and will only be a performance
    // improvement, I think.
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
) -> BoundNode {
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
    let type_ = match type_ {
        Type::IntegerLiteral => Type::Integer(IntegerType::Signed64),
        Type::TypedGenericStruct(typed_generic_struct_type) => Type::Struct(Box::new(binder.get_struct_by_id(typed_generic_struct_type.id).unwrap())),
        _ => type_,
    };
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
) -> BoundNode {
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
) -> BoundNode {
    let condition = bind_node(*while_statement.condition, binder);
    let condition = bind_conversion(condition, &Type::Boolean, binder);
    let body = bind_node(*while_statement.body, binder);
    BoundNode::while_statement(span, condition, body)
}

fn bind_assignment<'a, 'b>(
    span: TextSpan,
    assignment: AssignmentNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let lhs = bind_node_for_assignment(*assignment.lhs, binder);
    if let BoundNodeKind::VariableExpression(variable) = &lhs.kind {
        let name = binder.get_variable_name_by_id(variable.variable_index);
        if name == Some("this") && binder.is_struct_function {
            binder.diagnostic_bag.report_cannot_assign_to(lhs.span);
        }
    }
    let expression = bind_node(*assignment.expression, binder);
    // NOTE: Maybe this could be moved to another stage, since this is not
    // directly checking types and variables, but deconstructing a closure and
    // changing its argument order. On the other hand, we still need to check,
    // that the closure takes the correct type of our expression, otherwise we
    // could call functions with incorrect types.
    if let Type::Closure(closure) = &lhs.type_ {
        let function_type = &closure.base_function_type;
        if let BoundNodeKind::Closure(mut closure) = lhs.kind {
            let expression = bind_conversion(expression, &function_type.parameter_types[1], binder);
            let base = match closure.function {
                typing::FunctionKind::FunctionId(_) => {
                    todo!("Assignment as FunctionCall not implemented for FunctionId")
                }
                typing::FunctionKind::SystemCall(_) => {
                    todo!("Assignment as FunctionCall not implemented for SystemCalls")
                }
                typing::FunctionKind::LabelReference(label_reference) => {
                    BoundNode::label_reference(
                        label_reference,
                        Type::function(function_type.clone()),
                    )
                }
            };
            let mut arguments = Vec::with_capacity(closure.arguments.len() + 1);
            // index
            arguments.push(closure.arguments.pop().unwrap());
            // value
            arguments.push(expression);
            // this
            arguments.push(closure.arguments.pop().unwrap());
            assert!(closure.arguments.is_empty());
            BoundNode::function_call(span, base, arguments, false, Type::Void)
        } else {
            // TODO: This is untested and it is actually expected, that this
            // will fail. The order of the arguments will probably be wrong..
            unimplemented!("Found a closure in assignment, which could not be called directly!");
            // let expression = bind_conversion(expression, &lhs.type_, binder);
            // BoundNode::function_call(span, lhs, vec![expression], false, Type::Void)
        }
    } else {
        let expression = bind_conversion(expression, &lhs.type_, binder);
        BoundNode::assignment(span, lhs, expression)
    }
}

fn bind_block_statement<'a, 'b>(
    span: TextSpan,
    block_statement: BlockStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let variable_count = binder.variable_table.len();
    let mut statements = vec![];
    for node in block_statement.statements {
        statements.push(bind_node(node, binder));
    }
    binder.delete_variables_until(variable_count);
    BoundNode::block_statement(span, statements)
}

fn bind_expression_statement<'a, 'b>(
    span: TextSpan,
    expression_statement: ExpressionStatementNodeKind<'a>,
    binder: &mut BindingState<'a, 'b>,
) -> BoundNode {
    let expression = bind_node(*expression_statement.expression, binder);
    BoundNode::expression_statement(span, expression)
}
