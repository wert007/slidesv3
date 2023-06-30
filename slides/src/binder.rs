#[macro_use]
pub mod typing;
pub mod bound_nodes;
pub mod control_flow_analyzer;
mod dependency_resolver;
mod function_call_replacer;
mod lowerer;
pub mod operators;
pub mod symbols;
// #[cfg(test)]
// mod tests;

use std::collections::hash_map::DefaultHasher;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};
use std::{
    collections::{HashMap, VecDeque},
    convert::TryFrom,
    path::Path,
};

use either::Either;

use crate::{
    binder::{
        bound_nodes::{is_same_expression::IsSameExpression, BoundNodeKind},
        operators::{BoundBinary, BoundUnaryOperator},
        symbols::StructFunctionKind,
        typing::{GenericStruct, IntegerType, Type},
    },
    diagnostics::DiagnosticBag,
    instruction_converter::{
        instruction::Instruction, InstructionOrLabelReference, LabelReference,
    },
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    parser::{self, syntax_nodes::*},
    text::{SourceTextCollection, SourceTextId, TextLocation},
    value::Value,
    Project,
};

use self::bound_nodes::BoundMatchCaseExpression;
use self::{
    bound_nodes::{BoundMatchCase, BoundNode},
    operators::BoundBinaryOperator,
    symbols::{
        FunctionSymbol, GenericFunction, Library, StructFieldSymbol, StructFunctionTable,
        StructSymbol,
    },
    typing::{
        FunctionType, GenericStructPlaceholder, GenericType, GenericTypeId, Member,
        MemberOffsetOrAddress, StructPlaceholderType, StructType, SystemCallKind, TypeCollection,
        TypeCollectionIndexOutput, TypeId, TypeOrGenericType, TypeOrGenericTypeId,
    },
};

#[derive(Debug, Clone)]
pub enum SourceCow {
    Source(TextLocation),
    Owned(String),
}

impl From<String> for SourceCow {
    fn from(value: String) -> Self {
        Self::Owned(value)
    }
}

impl From<TextLocation> for SourceCow {
    fn from(value: TextLocation) -> Self {
        Self::Source(value)
    }
}

impl SourceCow {
    pub fn as_str<'a>(&'a self, source_text_collection: &'a SourceTextCollection) -> &'a str {
        match self {
            SourceCow::Source(it) => &source_text_collection[*it],
            SourceCow::Owned(it) => &it,
        }
    }

    pub fn to_owned(self, source_text_collection: &SourceTextCollection) -> String {
        match self {
            SourceCow::Source(it) => source_text_collection[it].to_owned(),
            SourceCow::Owned(it) => it,
        }
    }
}

#[derive(Debug, Clone)]
enum SafeNodeInCondition {
    None,
    IfBody(BoundNode, TypeId),
    ElseBody(BoundNode, TypeId),
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

#[derive(Clone)]
struct FunctionDeclarationBody<T> {
    header_location: TextLocation,
    function_name: SourceCow,
    body: SyntaxNode,
    parameters: Vec<(SourceCow, T)>,
    is_main: bool,
    available_generic_types: Vec<SourceCow>,
    function_label: u64,
    function_type: T,
    base_struct: Option<T>,
    struct_function_kind: Option<StructFunctionKind>,
}

impl<T: Debug> Debug for FunctionDeclarationBody<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FunctionDeclarationBody")
            .field("header_location", &self.header_location)
            .field("function_name", &self.function_name)
            .field("parameters", &self.parameters)
            .field("is_main", &self.is_main)
            .field("available_generic_types", &self.available_generic_types)
            .field("function_label", &self.function_label)
            .field("function_type", &self.function_type)
            .field("base_struct", &self.base_struct)
            .field("struct_function_kind", &self.struct_function_kind)
            .finish()
    }
}

impl FunctionDeclarationBody<TypeId> {
    fn into_function_symbol(self, source_text_collection: &SourceTextCollection) -> FunctionSymbol {
        FunctionSymbol {
            name: self.function_name.to_owned(source_text_collection),
            function_type: self.function_type,
            function_label: self.function_label,
            is_member_function: self.base_struct.is_some(),
        }
    }
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
    pub add_function: Option<usize>,
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
            StructFunctionKind::Add => self.add_function = Some(label),
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
            StructFunctionKind::Add => self.add_function,
        }
    }

    fn available_function_kinds(&self) -> impl Iterator<Item = StructFunctionKind> {
        self.constructor_function
            .map(|_| StructFunctionKind::Constructor)
            .into_iter()
            .chain(
                self.to_string_function
                    .map(|_| StructFunctionKind::ToString),
            )
            .chain(self.get_function.map(|_| StructFunctionKind::Get))
            .chain(self.set_function.map(|_| StructFunctionKind::Set))
            .chain(
                self.element_count_function
                    .map(|_| StructFunctionKind::ElementCount),
            )
            .chain(self.equals_function.map(|_| StructFunctionKind::Equals))
            .chain(self.add_function.map(|_| StructFunctionKind::Add))
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
            add_function: it.add_function.as_ref().map(|f| f.function_label as _),
        }
    }
}

#[derive(Clone, Debug)]
struct StructDeclarationBody {
    location: TextLocation,
    name: SourceCow,
    body: StructBodyNode,
    struct_function_table: SimpleStructFunctionTable,
    parent_id: Option<TypeId>,
    is_abstract: bool,
    generic_parameters: Vec<SourceCow>,
}

struct VariableEntry {
    id: u64,
    type_: TypeId,
    is_read_only: bool,
}

#[derive(Debug)]
struct BoundVariableName {
    pub identifier: SourceCow,
    pub type_: TypeId,
    pub is_read_only: bool,
}

#[derive(Debug)]
struct BoundConstant {
    pub identifier: SourceCow,
    pub value: Value,
}

#[derive(Debug, PartialEq)]
struct VariableOrConstant {
    type_: TypeId,
    is_read_only: bool,
    kind: VariableOrConstantKind,
}

impl VariableOrConstant {
    pub fn none() -> Self {
        Self {
            type_: typeid!(Type::Error),
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
pub enum BoundMaybeGenericStructSymbol {
    Struct(BoundStructSymbol),
    GenericStruct(BoundGenericStructSymbol),
}

impl From<BoundStructSymbol> for BoundMaybeGenericStructSymbol {
    fn from(value: BoundStructSymbol) -> Self {
        Self::Struct(value)
    }
}

impl From<BoundGenericStructSymbol> for BoundMaybeGenericStructSymbol {
    fn from(value: BoundGenericStructSymbol) -> Self {
        Self::GenericStruct(value)
    }
}

// impl From<MaybeGenericStructSymbol> for BoundMaybeGenericStructSymbol<'_> {
//     fn from(value: MaybeGenericStructSymbol) -> Self {
//         match value {
//             MaybeGenericStructSymbol::Struct(it) => BoundStructSymbol::from(it).into(),
//             MaybeGenericStructSymbol::GenericStruct(it) => {
//                 BoundGenericStructSymbol::from(it).into()
//             }
//         }
//     }
// }

/// This is quite similiar to symbols::StructSymbol, the only difference being,
/// that this version does not need an allocation. They can be easily converted
/// into each other. This version is only used during binding, while the other
/// version is exported in the symbols::Library type.
#[derive(Debug, Clone)]
pub struct BoundStructSymbol {
    pub name: SourceCow,
    pub fields: Vec<BoundStructFieldSymbol>,
    pub functions: Vec<BoundStructFieldSymbol>,
    pub function_table: StructFunctionTable,
    pub is_generic: bool,
    pub parent: Option<Box<BoundStructSymbol>>,
}

impl BoundStructSymbol {
    // pub fn field_functions_first(
    //     &self,
    //     name: &str,
    //     source_text_collection: &SourceTextCollection,
    // ) -> Option<&BoundStructFieldSymbol> {
    //     if let Some(result) = self
    //         .parent
    //         .as_ref()
    //         .map(|p| p.field_functions_first(name, source_text_collection))
    //         .flatten()
    //     {
    //         return Some(result);
    //     }
    //     self.functions
    //         .iter()
    //         .chain(self.fields.iter())
    //         .filter(|f| f.name.as_str(source_text_collection) == name)
    //         .next()
    // }

    // pub fn field_fields_first(
    //     &self,
    //     name: &str,
    //     source_text_collection: &SourceTextCollection,
    // ) -> Option<&BoundStructFieldSymbol> {
    //     if let Some(result) = self
    //         .parent
    //         .as_ref()
    //         .map(|p| p.field_fields_first(name, source_text_collection))
    //         .flatten()
    //     {
    //         return Some(result);
    //     }
    //     self.fields
    //         .iter()
    //         .chain(self.functions.iter())
    //         .filter(|f| f.name.as_str(source_text_collection) == name)
    //         .next()
    // }

    // pub fn empty() -> Self {
    //     Self {
    //         name: String::new().into(),
    //         fields: vec![],
    //         functions: vec![],
    //         function_table: StructFunctionTable::default(),
    //         is_generic: false,
    //         parent: None,
    //     }
    // }

    // fn _fields_for_constructor(&self) -> Vec<&BoundStructFieldSymbol> {
    //     let mut result = Vec::new();
    //     if let Some(parent) = &self.parent {
    //         result.append(&mut parent._fields_for_constructor());
    //     }
    //     result.extend(&self.fields);
    //     result
    // }

    // fn _size_in_bytes(&self, types: &TypeCollection) -> u64 {
    //     self._fields_for_constructor()
    //         .iter()
    //         .map(|f| types[f.type_].size_in_bytes())
    //         .sum()
    // }
}

impl From<StructSymbol> for BoundStructSymbol {
    fn from(it: StructSymbol) -> Self {
        Self {
            name: it.name.into(),
            fields: it.fields.into_iter().map(Into::into).collect(),
            functions: it.functions.into_iter().map(Into::into).collect(),
            function_table: it.function_table,
            is_generic: false,
            parent: None, // TODO
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundGenericStructSymbol {
    pub struct_type: BoundStructSymbol,
    pub functions: Vec<GenericFunction>,
}

/// This is quite similiar to symbols::StructFieldSymbol, the only difference
/// being, that this version does not need an allocation. They can be easily
/// converted into each other. This version is only used during binding, while
/// the other version is exported in the symbols::Library type.
#[derive(Debug, Clone)]
pub struct BoundStructFieldSymbol {
    pub name: SourceCow,
    pub type_: TypeId,
    pub offset: u64,
    pub is_read_only: bool,
}

impl From<StructFieldSymbol> for BoundStructFieldSymbol {
    fn from(it: StructFieldSymbol) -> Self {
        Self {
            name: it.name.into(),
            type_: it.type_,
            offset: it.offset,
            is_read_only: it.is_read_only,
        }
    }
}

/// This is a generic function, which body still needs to be bound. It is
/// already in the list of bound_functions but the body of the
/// BoundFunctionDeclaration is an error expression.
struct UnfixedBoundFunction {
    bound_function_index: usize,
    base_generic_struct: GenericTypeId,
    base_struct: TypeId,
    function_name: String,
    applied_types: Vec<TypeId>,
}

struct UnfixedGenericType {
    location: TextLocation,
    struct_id: GenericTypeId,
    type_id: TypeId,
    applied_types: Vec<TypeId>,
}

struct BindingState<'b> {
    project: &'b mut Project,
    // debug_flags: DebugFlags,
    /// The directory, in which the current file relative or absolute to the
    /// current executing program was.
    source_text: SourceTextId,
    /// The diagnostic bag collects all errors the user supplied code has. They
    /// will be outputted after the stage they first occurred. So if there are
    /// type conversion errors, the compiler will not try to turn the bound
    /// program into instructions.
    diagnostic_bag: &'b mut DiagnosticBag,
    /// Every variable is turned into an variable index by the binder, since
    /// variables are turned into registers and those are accessed by indices.
    /// The table stores the index as index of the vector, the name of the
    /// variable, the type it has and if it is read only, which means it cannot
    /// be on the left side of an assignment.
    variable_table: Vec<BoundVariableName>,
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
    functions: Vec<FunctionDeclarationBody<TypeId>>,
    generic_functions: Vec<FunctionDeclarationBody<TypeOrGenericTypeId>>,
    bound_generic_functions: Vec<GenericFunction>,
    bound_functions: Vec<BoundNode>,
    /// This is equal to the amount of labels used by the referenced libraries
    /// and will ensure, that all labels will be unique per bound program.
    label_offset: usize,
    /// This collects all the structs, which fields still need to be bound by the
    /// binder. This should be empty, before starting to bind the functions.
    structs: Vec<StructDeclarationBody>,
    exported_structs: Vec<TypeId>,
    exported_enums: Vec<TypeId>,
    exported_generic_structs: Vec<GenericTypeId>,
    generic_structs: Vec<StructDeclarationBody>,
    generic_types: Vec<TypeId>,
    /// This has all libraries which where loaded by imports. They can be
    /// referenced for name lookups.
    libraries: Vec<Library>,
    /// This is a simple hack to bring constants into the binder.
    constants: Vec<BoundConstant>,
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
    safe_nodes: Vec<Option<(BoundNode, TypeId)>>,
    /// This is used to preallocate the registers.
    max_used_variables: usize,
    /// The return type of the current function. Is Type::Error if not in a
    /// function. This is needed by return statements to ensure they return the
    /// correct value.
    function_return_type: TypeId,
    /// This is used to reserve a this variable inside the function.
    is_struct_function: bool,
    /// This is used to check if all fields have been set to a value in a
    /// constructor.
    assigned_fields: Vec<String>,
    available_generic_types: Vec<SourceCow>,
    /// The type the current expression is expected to have. This is only used in
    /// bind_array_literal, to convert each expression inside the array literal
    /// to the right type. Everywhere else the expression is converted
    /// afterwards.
    expected_type: Option<TypeId>,

    generic_placeholder_types_that_need_to_be_bound_for_a_type: VecDeque<UnfixedGenericType>,
    bound_functions_without_actual_body: Vec<UnfixedBoundFunction>,
}

impl BindingState<'_> {
    fn register_variable(
        &mut self,
        name: impl Into<SourceCow>,
        type_: TypeId,
        is_read_only: bool,
    ) -> Option<u64> {
        let name = name.into();
        let index = self.variable_table.len() as u64;
        self.max_used_variables = self.max_used_variables.max(index as usize + 1);
        let variable_already_registered = self.variable_table.iter().any(|variable| {
            variable
                .identifier
                .as_str(&self.project.source_text_collection)
                == name.as_str(&self.project.source_text_collection)
        });
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
        type_: TypeId,
        is_read_only: bool,
    ) -> Option<u64> {
        let index = self.variable_table.len() as u64;
        self.max_used_variables = self.max_used_variables.max(index as usize + 1);
        let variable_already_registered = self.variable_table.iter().any(|variable| {
            variable
                .identifier
                .as_str(&self.project.source_text_collection)
                == name
        });
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

    fn register_constant(&mut self, name: impl Into<SourceCow>, value: Value) -> Option<u64> {
        let index = self.constants.len() as u64;
        let name = name.into();
        let constant_name_already_registered = self.constants.iter().any(|c| {
            c.identifier.as_str(&self.project.source_text_collection)
                == name.as_str(&self.project.source_text_collection)
        });
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
            .find(|c| c.identifier.as_str(&self.project.source_text_collection) == name)
            .map(|c| c.value.clone())
    }

    fn register_safe_node(&mut self, node: BoundNode, safe_type: TypeId) -> usize {
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
            .map(|(n, t)| BoundNode::conversion(n.location, n.clone(), t.clone()))
    }

    fn delete_variables_until(&mut self, index: usize) {
        if self.variable_table.is_empty() || index == self.variable_table.len() {
            return;
        }
        if self.project.debug_flags.print_variable_table {
            print_variable_table(
                &self.variable_table,
                &self.project.types,
                &self.project.source_text_collection,
            );
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
            .find(|(_, v)| v.identifier.as_str(&self.project.source_text_collection) == name)
            .map(|(i, v)| VariableEntry {
                id: i as u64,
                type_: v.type_.clone(),
                is_read_only: v.is_read_only,
            })
    }

    fn get_variable_name_by_id(&self, id: u64) -> Option<&str> {
        self.variable_table
            .get(id as usize)
            .map(|v| v.identifier.as_str(&self.project.source_text_collection))
    }

    fn look_up_type_by_source_cow(&self, name: &SourceCow) -> Option<TypeOrGenericTypeId> {
        self.project
            .types
            .look_up_type_by_name(name.as_str(&self.project.source_text_collection))
    }

    fn look_up_type_by_name(&mut self, name: &str) -> Option<TypeOrGenericTypeId> {
        let generic_types: Vec<_> = (self.available_generic_types.iter().enumerate())
            .map(|(i, n)| {
                self.project.types.look_up_or_add_type(Type::GenericType(
                    i,
                    n.clone().to_owned(&self.project.source_text_collection),
                ))
            })
            .collect();
        self.available_generic_types
            .iter()
            .position(|t| {
                name.starts_with('$')
                    && t.as_str(&self.project.source_text_collection) == &name[1..]
            })
            .map(|i| generic_types[i].into())
            .or_else(|| {
                self.project
                    .types
                    .look_up_type_by_name(name)
                    .filter(|t| match t {
                        TypeOrGenericTypeId::Type(id)
                            if matches!(self.project.types[*id], Type::GenericType(..)) =>
                        {
                            false
                        }
                        _ => true,
                    })
            })
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

    pub fn add_function_declaration(
        &mut self,
        function_declaration: FunctionDeclarationBody<TypeId>,
    ) {
        self.functions.push(function_declaration);
    }

    pub fn add_generic_function_declaration(
        &mut self,
        function_declaration: FunctionDeclarationBody<TypeOrGenericTypeId>,
    ) {
        self.generic_functions.push(function_declaration);
    }

    fn register_struct_name(
        &mut self,
        name: impl Into<SourceCow>,
        struct_function_table: SimpleStructFunctionTable,
        applied_types: Vec<TypeId>,
    ) -> Option<TypeId> {
        let name = name.into();
        let result = self
            .project
            .types
            .add_type(Type::StructPlaceholder(StructPlaceholderType {
                name: name.to_owned(&self.project.source_text_collection),
                function_table: struct_function_table,
                applied_types,
            }))
            .ok();
        if let Some(it) = result {
            self.exported_structs.push(it)
        }
        result
    }

    fn register_generic_struct_name(
        &mut self,
        name: impl Into<SourceCow>,
        struct_function_table: SimpleStructFunctionTable,
        generic_parameters: Vec<String>,
    ) -> Option<GenericTypeId> {
        let name = name.into();
        let result = self
            .project
            .types
            .add_generic_type(GenericType::StructPlaceholder(GenericStructPlaceholder {
                name: name.to_owned(&self.project.source_text_collection),
                function_table: struct_function_table,
                generic_parameters,
            }))
            .ok();

        if let Some(it) = result {
            self.exported_generic_structs.push(it)
        }
        result
    }

    fn replace_in_type_for_types(
        &mut self,
        location: TextLocation,
        type_: TypeId,
        replace: &[TypeId],
        replace_with: &[TypeId],
    ) -> TypeId {
        match self
            .project
            .types
            .replace_in_type_for_types(type_, replace, replace_with)
        {
            Ok(it) => it,
            Err((struct_type, applied_types)) => {
                let _t = bind_generic_struct_type_for_types(
                    location,
                    struct_type.generic_base_type.unwrap(),
                    &applied_types,
                    self,
                );
                self.replace_in_type_for_types(location, type_, replace, replace_with)
            }
        }
    }

    fn lexeme(&self, token: SyntaxToken) -> &str {
        &self.project.source_text_collection[token.location]
    }

    fn directory(&self) -> &str {
        self.project.source_text_collection[self.source_text].directory()
    }

    fn generate_temporary_variable(&mut self, prefix: &str, location: TextLocation) -> u64 {
        let mut state = DefaultHasher::new();
        self.project.source_text_collection[location].hash(&mut state);
        let hash = state.finish();
        self.register_variable(
            format!("temporary$variable${prefix}${hash}"),
            typeid!(Type::Error),
            false,
        )
        .expect("Failed to register temporary variable!")
    }
}

fn print_variable_table(
    variable_table: &[BoundVariableName],
    type_collection: &TypeCollection,
    source_text_collection: &SourceTextCollection,
) {
    let mut variable_table: Vec<_> = variable_table.iter().enumerate().collect();
    variable_table.sort_unstable_by_key(|f| f.0);
    for (index, variable) in variable_table {
        println!(
            "  {:00}: {} : {}",
            index,
            variable.identifier.as_str(source_text_collection),
            type_collection.name_of_type_id(variable.type_),
        );
    }
    println!();
}

fn print_constant_table(
    constant_table: &[BoundConstant],
    source_text_collection: &SourceTextCollection,
) {
    let mut constant_table: Vec<_> = constant_table.iter().enumerate().collect();
    constant_table.sort_unstable_by_key(|f| f.0);
    for (index, constant) in constant_table {
        println!(
            "  {:00}: {} = {}",
            index,
            constant.identifier.as_str(source_text_collection),
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
    pub fn error(location: TextLocation) -> Self {
        Self {
            startup: vec![],
            functions: BoundNode::error(location),
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
    pub exported_structs: Vec<TypeId>,
    pub exported_enums: Vec<TypeId>,
    pub exported_generic_structs: Vec<GenericTypeId>,
}

impl BoundLibrary {
    pub fn error(location: TextLocation) -> Self {
        Self {
            program: BoundProgram::error(location),
            exported_functions: vec![],
            exported_structs: vec![],
            exported_enums: vec![],
            exported_generic_structs: vec![],
        }
    }
}

pub fn bind_program_with_project_parameter<'a>(
    source_text: SourceTextId,
    diagnostic_bag: &mut DiagnosticBag,
    project: &'a mut Project,
) -> BoundProgram {
    bind_with_project_parameter(source_text, diagnostic_bag, false, true, project).program
}

pub fn bind_library_with_project_parameter<'a>(
    source_text: SourceTextId,
    diagnostic_bag: &mut DiagnosticBag,
    project: &'a mut Project,
    import_std_lib: bool,
) -> BoundLibrary {
    bind_with_project_parameter(source_text, diagnostic_bag, true, import_std_lib, project)
}

fn bind_with_project_parameter<'a>(
    source_text: SourceTextId,
    diagnostic_bag: &mut DiagnosticBag,
    is_library: bool,
    import_std_lib: bool,
    project: &'a mut Project,
) -> BoundLibrary {
    let node = parser::parse(
        source_text,
        &project.source_text_collection,
        diagnostic_bag,
        project.debug_flags,
    );
    if project.debug_flags.print_syntax_program {
        crate::debug::print_syntax_node_as_code(&node, &project.source_text_collection);
    }
    if diagnostic_bag.has_errors() {
        return BoundLibrary::error(TextLocation::zero_in_file(source_text));
    }
    let debug_flags = project.debug_flags;
    let mut binder = BindingState {
        project,
        source_text,
        diagnostic_bag,
        variable_table: vec![],
        generic_types: vec![],
        functions: vec![],
        generic_functions: vec![],
        bound_generic_functions: vec![],
        bound_functions: vec![],
        label_offset: 0,
        structs: vec![],
        generic_structs: vec![],
        exported_structs: vec![],
        exported_enums: vec![],
        exported_generic_structs: vec![],
        libraries: vec![],
        constants: vec![],
        safe_nodes: vec![],
        max_used_variables: 0,
        function_return_type: typeid!(Type::Error),
        is_struct_function: false,
        available_generic_types: vec![],
        assigned_fields: vec![],
        expected_type: None,
        generic_placeholder_types_that_need_to_be_bound_for_a_type: VecDeque::new(),
        bound_functions_without_actual_body: Vec::new(),
    };
    let location = node.location;
    let mut startup = vec![];
    default_statements(&mut binder);
    let node = dependency_resolver::bind_import_statements(node, &mut binder).unwrap();
    if import_std_lib {
        std_imports(&mut binder, source_text);
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

    // binder
    //     .project
    //     .types
    //     .maybe_print_type_table(binder.project.debug_flags);

    let mut generic_structs = binder.generic_structs;
    binder.generic_structs = Vec::new();
    // dependency_resolver::sort_structs_by_dependencies(&mut generic_structs, &mut binder);

    while let Some(node) = generic_structs.pop() {
        let name = node.name.clone();
        let generic_parameter_count = node.generic_parameters.len();
        let generic_parameters = node
            .generic_parameters
            .clone()
            .into_iter()
            .map(|s| s.to_owned(&binder.project.source_text_collection))
            .collect();
        let struct_type = bind_struct_body(node, &mut binder);
        let placeholder = binder
            .look_up_type_by_source_cow(&name)
            .expect("Failed to preregister type!")
            .unwrap_generic_type_id();
        let type_id_placeholder = binder.project.types.to_fake_type_id(placeholder);
        binder.project.types.overwrite_generic_type(
            placeholder,
            GenericType::Struct(GenericStruct::new(struct_type.clone(), generic_parameters)),
        );
        assert_eq!(
            generic_parameter_count,
            binder.available_generic_types.len()
        );
        let applied_types: Vec<_> = (binder.available_generic_types.iter().enumerate())
            .map(|(i, n)| {
                binder.project.types.look_up_or_add_type(Type::GenericType(
                    i,
                    n.to_owned()
                        .to_owned(&binder.project.source_text_collection),
                ))
            })
            .collect();
        let name = binder
            .project
            .types
            .generate_generic_name(placeholder, &applied_types);
        binder.generic_types = Vec::new();
        binder.project.types.overwrite_type(
            type_id_placeholder,
            Type::Struct(StructType {
                name,
                applied_types,
                generic_base_type: Some(placeholder),
                is_generic: false,
                ..struct_type
            }),
        );
    }

    while let Some(unfixed) = binder
        .generic_placeholder_types_that_need_to_be_bound_for_a_type
        .pop_back()
    {
        let struct_name = binder
            .project
            .types
            .name_of_type_id(unfixed.type_id)
            .into_owned();
        bind_generic_struct_type_for_types_no_lookup(
            unfixed.location,
            &mut binder,
            unfixed.type_id,
            unfixed.struct_id,
            struct_name,
            &unfixed.applied_types,
        );
        let new_type = binder.project.types[unfixed.type_id].clone();
        if matches!(new_type, Type::StructPlaceholder(..)) {
            binder
                .generic_placeholder_types_that_need_to_be_bound_for_a_type
                .push_front(unfixed);
        } else {
            binder
                .project
                .types
                .overwrite_type(unfixed.type_id, new_type);
        }
    }

    // TODO: Remove me
    binder.structs.reverse();
    while let Some(node) = binder.structs.pop() {
        let name = node.name.clone();
        let struct_type = bind_struct_body(node, &mut binder);
        let placeholder = binder
            .look_up_type_by_source_cow(&name)
            .expect("Failed to preregister type!")
            .unwrap_type_id();
        binder
            .project
            .types
            .overwrite_type(placeholder, Type::Struct(struct_type));
    }

    let fixed_variable_count = binder.variable_table.len();
    // binder.project.types.maybe_print_type_table(binder.project.debug_flags);
    while let Some(node) = binder.generic_functions.pop() {
        let (body, labels) = bind_generic_function_declaration_body(node.clone(), &mut binder);

        let generic_function = GenericFunction {
            function_label: node.function_label,
            function_name: node
                .function_name
                .to_owned(&binder.project.source_text_collection),
            function_type: node.function_type.unwrap_generic_type_id(),
            body,
            labels,
        };

        if let Some(base_struct) = node.base_struct {
            let base_struct = base_struct.unwrap_generic_type_id();
            binder
                .project
                .types
                .add_generic_function_to_generic_struct(base_struct, generic_function)
                .expect("Failed to add generic function to generic type!");
            // panic!("Not implemented, but probably should.");
        } else {
            panic!("We do not have this right now!");
            // binder.bound_generic_functions.push(generic_function);
        }
    }

    let mut exported_functions = Vec::with_capacity(binder.functions.len());
    // binder
    //     .project
    //     .types
    //     .maybe_print_type_table(binder.project.debug_flags);
    while let Some(node) = binder.functions.pop() {
        exported_functions.push(node.clone());
        let function = bind_function_declaration_body(node, &mut binder);
        binder.bound_functions.push(function);
    }

    while let Some(unfixed) = binder.bound_functions_without_actual_body.pop() {
        let generic_parameters = binder.project.types[unfixed.base_generic_struct]
            .generic_parameters()
            .to_owned();
        let replace: Vec<_> = (generic_parameters.into_iter().enumerate())
            .map(|(i, n)| {
                binder
                    .project
                    .types
                    .look_up_or_add_type(Type::GenericType(i, n.clone()))
            })
            .collect();

        let mut generic_function = binder.project.types[unfixed.base_generic_struct]
            .as_generic_functions()
            .unwrap()
            .iter()
            .filter(|f| {
                let (_, name) = f.function_name.split_once("::").unwrap();
                name == unfixed.function_name || f.function_name == unfixed.function_name
            })
            .next()
            .unwrap_or_else(|| {
                panic!(
                    "Expected some function with name {}!",
                    unfixed.function_name
                )
            })
            .body
            .clone();
        let old_struct_type = binder.project.types[unfixed.base_generic_struct]
            .as_struct_type()
            .unwrap();
        let new_struct_type = binder.project.types[unfixed.base_struct]
            .as_struct_type()
            .unwrap();
        let label_relocations: HashMap<_, _> = old_struct_type
            .functions
            .iter()
            .zip(new_struct_type.functions.iter())
            .map(|(old, new)| {
                assert_eq!(new.name, old.name);
                (
                    old.offset_or_address.unwrap_address(),
                    new.offset_or_address.unwrap_address(),
                )
            })
            .chain(
                old_struct_type
                    .function_table
                    .function_symbols_iter()
                    .zip(new_struct_type.function_table.function_symbols_iter())
                    .map(|(old, new)| (old.function_label as usize, new.function_label as usize)),
            )
            .collect();
        generic_function.for_each_child_mut(&mut |n| {
            n.type_ = binder.replace_in_type_for_types(
                n.location,
                n.type_,
                &replace,
                &unfixed.applied_types,
            );
            match &mut n.kind {
                BoundNodeKind::Label(it) if label_relocations.contains_key(it) => {
                    *it = label_relocations[it]
                }
                BoundNodeKind::LabelReference(it) if label_relocations.contains_key(it) => {
                    *it = label_relocations[it]
                }
                BoundNodeKind::Conversion(it) => {
                    it.type_ = binder.replace_in_type_for_types(
                        n.location,
                        it.type_,
                        &replace,
                        &unfixed.applied_types,
                    );
                }
                BoundNodeKind::ConstructorCall(it) => {
                    it.base_type = binder.replace_in_type_for_types(
                        n.location,
                        it.base_type,
                        &replace,
                        &unfixed.applied_types,
                    );
                    let had_function = it.function.is_some();
                    it.function = binder.project.types[it.base_type]
                        .as_struct_type()
                        .map(|s| {
                            s.function_table
                                .constructor_function
                                .as_ref()
                                .map(|f| f.function_label)
                        })
                        .unwrap_or_else(|| {
                            if let Type::StructPlaceholder(it) = &binder.project.types[it.base_type]
                            {
                                it.function_table.constructor_function.map(|it| it as u64)
                            } else {
                                unreachable!("Constructor calls only work for structs!")
                            }
                        });
                    assert_eq!(had_function, it.function.is_some());
                }
                _ => {}
            }
        });

        let function = &mut binder.bound_functions[unfixed.bound_function_index];
        let BoundNodeKind::FunctionDeclaration(function_declaration) = &mut function.kind else {
            unreachable!("bound_functions can only contain Function declarations!");
        };
        function_declaration.body = Box::new(generic_function);
    }

    if !is_library {
        startup.append(&mut call_main(&mut binder, source_text));
    }

    let exported_functions = exported_functions
        .into_iter()
        .map(|f| f.into_function_symbol(&binder.project.source_text_collection))
        .collect();
    let functions = BoundNode::block_statement(location, binder.bound_functions);
    if debug_flags.print_bound_program {
        crate::debug::print_bound_node_as_code(&functions, &binder.project.types);
    }
    if debug_flags.print_struct_table {
        unimplemented!();
    }
    if debug_flags.print_constant_table {
        print_constant_table(&binder.constants, &binder.project.source_text_collection);
    }

    binder.project.maybe_print_type_table();

    binder.diagnostic_bag.registered_types = binder.project.types.clone();

    BoundLibrary {
        exported_functions,
        program: BoundProgram {
            startup,
            functions,
            fixed_variable_count,
            max_used_variables: binder.max_used_variables,
            label_count: binder.label_offset,
            referenced_libraries: binder.libraries,
        },
        exported_structs: binder.exported_structs,
        exported_enums: binder.exported_enums,
        exported_generic_structs: binder.exported_generic_structs,
    }
}

fn bind_function_declaration_body<'a>(
    node: FunctionDeclarationBody<TypeId>,
    binder: &mut BindingState<'_>,
) -> BoundNode {
    binder.available_generic_types = node.available_generic_types;
    let fixed_variable_count = binder.variable_table.len();
    let label = node.function_label as usize;
    let mut parameters = Vec::with_capacity(node.parameters.len());
    for (variable_name, variable_type) in node.parameters {
        if let Some(it) = binder.register_variable(variable_name.clone(), variable_type, false) {
            parameters.push(it);
        } else {
            print_variable_table(
                &binder.variable_table,
                &binder.project.types,
                &binder.project.source_text_collection,
            );
            dbg!(variable_name, node.function_name);
            panic!("Could not declare a parameter! This sounds like an error in the binder!");
        }
    }
    let base_register = if let Some(parent) = node
        .base_struct
        .map(|id| {
            binder.project.types[id]
                .as_struct_type()
                .expect("Only structs can have functions defined!")
                .parent
        })
        .flatten()
    {
        Some(
            binder
                .register_generated_variable("base".into(), parent, false)
                .expect("Failed to declare variable base..."),
        )
    } else {
        None
    };
    binder.function_return_type = binder.project.types[node.function_type]
        .as_function_type()
        .expect("FunctionType")
        .return_type;
    binder.is_struct_function = node.base_struct.is_some();
    let location = node.body.location;
    let mut body = bind_node(node.body, binder);
    match node.struct_function_kind {
        Some(StructFunctionKind::Constructor) => {
            let strct = node.base_struct.unwrap();
            let strct = binder.project.types[strct]
                .as_struct_type()
                .expect("StructType");
            let unassigned_fields: Vec<_> = strct
                .fields
                .iter()
                .map(|f| f.name.as_str())
                .filter(|s| !binder.assigned_fields.contains(&s.to_string()))
                .collect();
            if !unassigned_fields.is_empty() {
                binder
                    .diagnostic_bag
                    .report_not_all_fields_have_been_assigned(
                        node.header_location,
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
            | StructFunctionKind::Equals
            | StructFunctionKind::Add,
        )
        | None => {}
    }
    binder.assigned_fields.clear();
    if typeid!(Type::Void) == binder.function_return_type {
        let location = body.location;
        body = BoundNode::block_statement(
            location,
            vec![
                body,
                BoundNode::return_statement(location, None, !node.is_main),
            ],
        );
    }
    let body_statements = lowerer::flatten(body, &mut binder.label_offset, binder.project);
    if binder.function_return_type != typeid!(Type::Void)
        // FIXME: This means, that we only check this, if there are not other
        // errors. That is weird, lets make sure the control_flow_analyzer can
        // handle BoundNode::Error instead.
        && !binder.diagnostic_bag.has_errors()
        && !control_flow_analyzer::check_if_all_paths_return(
            &node
                .function_name
                .as_str(&binder.project.source_text_collection),
            &body_statements,
            &binder.project.types,
            binder.project.debug_flags,
        )
    {
        // FIXME: We actually know, where the return is missing. We should use
        // that here.
        binder
            .diagnostic_bag
            .report_missing_return_statement(location, binder.function_return_type);
    }
    let body = BoundNode::block_statement(location, body_statements);
    let result = BoundNode::function_declaration(
        TextLocation::bounds(node.header_location, body.location),
        label,
        node.is_main,
        body,
        parameters,
        base_register,
        node.function_type,
    );
    binder.function_return_type = typeid!(Type::Void);
    binder.delete_variables_until(fixed_variable_count);
    result
}

fn bind_generic_function_declaration_body<'a>(
    node: FunctionDeclarationBody<TypeOrGenericTypeId>,
    binder: &mut BindingState<'_>,
) -> (BoundNode, Vec<usize>) {
    binder.available_generic_types = node.available_generic_types;
    let fixed_variable_count = binder.variable_table.len();
    let mut parameters = Vec::with_capacity(node.parameters.len());
    for (variable_name, variable_type) in node.parameters {
        let variable_type = match variable_type {
            TypeOrGenericTypeId::Type(it) => it,
            TypeOrGenericTypeId::GenericType(it) => binder.project.types.to_fake_type_id(it),
        };
        if let Some(it) = binder.register_variable(variable_name.clone(), variable_type, false) {
            parameters.push(it);
        } else {
            print_variable_table(
                &binder.variable_table,
                &binder.project.types,
                &binder.project.source_text_collection,
            );
            dbg!(variable_name, node.function_name);
            panic!("Could not declare a parameter! This sounds like an error in the binder!");
        }
    }
    let _base_register = if let Some(parent) = node
        .base_struct
        .map(|id| binder.project.types[id].as_struct_type().unwrap().parent)
        .flatten()
    {
        Some(
            binder
                .register_generated_variable("base".into(), parent, false)
                .expect("Failed to declare base..."),
        )
    } else {
        None
    };
    binder.function_return_type = binder.project.types[node.function_type]
        .as_function_type()
        .expect("FunctionType")
        .return_type
        .clone();
    binder.is_struct_function = node.base_struct.is_some();
    let location = node.body.location;
    let label_start = binder.label_offset;
    let mut body = bind_node(node.body, binder);
    match node.struct_function_kind {
        Some(StructFunctionKind::Constructor) => {
            let strct = node.base_struct.unwrap();
            let strct = binder.project.types[strct]
                .as_struct_type()
                .expect("StructType");
            let unassigned_fields: Vec<_> = strct
                .fields
                .iter()
                .map(|f| f.name.as_str())
                .filter(|s| !binder.assigned_fields.contains(&s.to_string()))
                .collect();
            if !unassigned_fields.is_empty() {
                binder
                    .diagnostic_bag
                    .report_not_all_fields_have_been_assigned(
                        node.header_location,
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
            | StructFunctionKind::Equals
            | StructFunctionKind::Add,
        )
        | None => {}
    }
    binder.assigned_fields.clear();
    if typeid!(Type::Void) == binder.function_return_type {
        let location = body.location;
        body = BoundNode::block_statement(
            location,
            vec![
                body,
                BoundNode::return_statement(location, None, !node.is_main),
            ],
        );
    }
    let body_statements = lowerer::flatten(body, &mut binder.label_offset, binder.project);
    if binder.function_return_type != typeid!(Type::Void)
        && !binder.diagnostic_bag.has_errors()
        && !control_flow_analyzer::check_if_all_paths_return(
            &node
                .function_name
                .as_str(&binder.project.source_text_collection),
            &body_statements,
            &binder.project.types,
            binder.project.debug_flags,
        )
    {
        binder
            .diagnostic_bag
            .report_missing_return_statement(location, binder.function_return_type);
    }
    let body = BoundNode::block_statement(location, body_statements);
    let label_end = binder.label_offset;
    binder.function_return_type = typeid!(Type::Void);
    binder.delete_variables_until(fixed_variable_count);
    (body, (label_start..label_end).collect())
}

fn default_statements(binder: &mut BindingState) {
    binder.register_constant("print".to_owned(), Value::SystemCall(SystemCallKind::Print));
    binder.register_constant(
        "heapdump".to_owned(),
        Value::SystemCall(SystemCallKind::HeapDump),
    );
    binder.register_constant("break".to_owned(), Value::SystemCall(SystemCallKind::Break));
    binder.register_constant(
        "reallocate".to_owned(),
        Value::SystemCall(SystemCallKind::Reallocate),
    );
    binder.register_constant(
        "runtimeError".to_owned(),
        Value::SystemCall(SystemCallKind::RuntimeError),
    );
    binder.register_constant(
        "addressOf".to_owned(),
        Value::SystemCall(SystemCallKind::AddressOf),
    );
    binder.register_constant(
        "garbageCollect".to_owned(),
        Value::SystemCall(SystemCallKind::GarbageCollect),
    );
    binder.register_constant("hash".to_owned(), Value::SystemCall(SystemCallKind::Hash));
    binder.register_constant(
        "byteToChar".to_owned(),
        Value::SystemCall(SystemCallKind::ByteToChar),
    );
    binder.register_constant(
        "typeOfValue".to_owned(),
        Value::SystemCall(SystemCallKind::TypeOfValue),
    );
}

fn std_imports(binder: &mut BindingState, source_text: SourceTextId) {
    dependency_resolver::load_library_from_source(
        binder,
        Path::new("std.sld"),
        include_str!("../../builtin/std.sld"),
        TextLocation::zero_in_file(source_text),
        String::new(),
        false,
    );
}

fn call_main(
    binder: &mut BindingState,
    source_text: SourceTextId,
) -> Vec<InstructionOrLabelReference> {
    let base = binder.look_up_variable_or_constant_by_name("main");
    let location = TextLocation::zero_in_file(source_text);
    match base.kind {
        VariableOrConstantKind::None => {
            binder
                .diagnostic_bag
                .report_no_main_function_found(source_text);
            vec![]
        }
        VariableOrConstantKind::Variable(variable_id) => {
            vec![
                Instruction::load_register(variable_id, location).into(),
                Instruction::load_immediate(0, location).into(),
                Instruction::function_call(location, 0, false).into(),
            ]
        }
        VariableOrConstantKind::Constant(value) => {
            let mut result = Vec::with_capacity(3);
            result.push(
                LabelReference {
                    label_reference: value.as_label_pointer().unwrap().0,
                    location,
                }
                .into(),
            );
            result.push(Instruction::load_immediate(0, location).into());
            result.push(Instruction::function_call(location, 0, false).into());
            result
        }
    }
}

fn bind_top_level_statements<'a, 'b>(node: SyntaxNode, binder: &mut BindingState<'b>) {
    match node.kind {
        SyntaxNodeKind::CompilationUnit(compilation_unit) => {
            for statement in compilation_unit.statements {
                bind_top_level_statement(statement, binder);
            }
        }
        _ => unreachable!(),
    }
}

fn bind_top_level_statement<'a, 'b>(node: SyntaxNode, binder: &mut BindingState<'b>) {
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
        SyntaxNodeKind::EnumDeclaration(enum_declaration) => {
            bind_enum_declaration(enum_declaration, binder)
        }
        _ => {
            binder
                .diagnostic_bag
                .report_invalid_top_level_statement(node.location, node.kind);
        }
    }
}

fn bind_const_declaration<'a>(
    const_declaration: ConstDeclarationNodeKind,
    binder: &mut BindingState<'_>,
) {
    let initializer = bind_node(*const_declaration.initializer, binder);
    if let Some(constant_value) = initializer.constant_value {
        binder.register_constant(const_declaration.identifier.location, constant_value.value);
    } else {
        binder
            .diagnostic_bag
            .report_expected_constant(initializer.location);
    }
}

fn bind_function_declaration<'a, 'b>(
    function_declaration: FunctionDeclarationNodeKind,
    binder: &mut BindingState<'b>,
) {
    let header_span = TextLocation::bounds(
        function_declaration.func_keyword.location,
        function_declaration
            .function_type
            .return_type
            .as_ref()
            .map(|r| r.return_type.location())
            .unwrap_or_else(|| function_declaration.function_type.rparen.location),
    );
    let (function_type, variables) = bind_function_type(
        binder.lexeme(function_declaration.identifier).into(),
        None,
        function_declaration.function_type,
        binder,
    );
    let type_ = binder
        .project
        .types
        .look_up_or_add_type(Type::Function(function_type.clone()));
    let is_main = binder.lexeme(function_declaration.identifier) == "main";
    // assert!(is_generic == false);
    // FIXME: Turn into usize
    let function_label = binder.generate_label() as u64;
    binder.register_constant(
        function_declaration.identifier.location,
        Value::LabelPointer(function_label as _, type_),
    );
    binder.add_function_declaration(FunctionDeclarationBody {
        header_location: header_span,
        function_name: function_declaration.identifier.location.into(),
        body: *function_declaration.body,
        parameters: variables,
        is_main,
        available_generic_types: binder.available_generic_types.clone(),
        function_label,
        function_type: type_,
        base_struct: None,
        struct_function_kind: None,
    });
}

fn bind_function_declaration_for_struct<'a, 'b>(
    struct_name: &SourceCow,
    function_declaration: FunctionDeclarationNodeKind,
    struct_function_table: &mut StructFunctionTable,
    simple_struct_function_table: SimpleStructFunctionTable,
    binder: &mut BindingState<'b>,
) -> Option<Member> {
    let header_span = TextLocation::bounds(
        function_declaration.func_keyword.location,
        function_declaration
            .function_type
            .return_type
            .as_ref()
            .map(|r| r.return_type.location())
            .unwrap_or_else(|| function_declaration.function_type.rparen.location),
    );
    let struct_type = binder.look_up_type_by_source_cow(struct_name).unwrap();
    let function_name = format!(
        "{}::{}",
        struct_name.as_str(&binder.project.source_text_collection),
        binder.lexeme(function_declaration.identifier)
    );
    match struct_type {
        TypeOrGenericTypeId::Type(struct_type) => {
            let (function_type, mut variables) = bind_function_type(
                function_name.clone(),
                Some(struct_type),
                function_declaration.function_type,
                binder,
            );
            variables.push(("this".to_owned().into(), struct_type));
            let type_ = binder
                .project
                .types
                .look_up_or_add_type(Type::Function(function_type.clone()));

            // FIXME: implement getter for generic structs.
            match StructFunctionKind::try_from(binder.lexeme(function_declaration.identifier)) {
                Ok(struct_function_kind) => {
                    let function_label = simple_struct_function_table
                        .get(struct_function_kind)
                        .unwrap() as u64;
                    type_check_struct_function_kind(
                        header_span,
                        struct_function_kind,
                        &function_type,
                        struct_type,
                        binder,
                    );
                    binder.add_function_declaration(FunctionDeclarationBody {
                        header_location: header_span,
                        function_name: function_name.clone().into(),
                        body: *function_declaration.body,
                        parameters: variables,
                        is_main: false,
                        available_generic_types: binder.available_generic_types.clone(),
                        function_label,
                        function_type: type_,
                        base_struct: Some(struct_type),
                        struct_function_kind: Some(struct_function_kind),
                    });
                    struct_function_table.set(
                        struct_function_kind,
                        FunctionSymbol {
                            name: function_name,
                            function_type: type_,
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
                            function_declaration.identifier.location,
                            function_declaration.identifier.location,
                        );
                        None
                    } else {
                        let function_label = binder.generate_label() as u64;
                        binder.register_constant(
                            function_name.clone(),
                            Value::LabelPointer(function_label as usize, type_.clone()),
                        );
                        binder.add_function_declaration(FunctionDeclarationBody {
                            header_location: header_span,
                            function_name: function_name.into(),
                            body: *function_declaration.body,
                            parameters: variables,
                            is_main: false,
                            available_generic_types: binder.available_generic_types.clone(),
                            function_label,
                            function_type: type_,
                            base_struct: Some(struct_type),
                            struct_function_kind: None,
                        });
                        Some(Member {
                            name: binder.lexeme(function_declaration.identifier).into(),
                            type_,
                            is_read_only: true,
                            offset_or_address: typing::MemberOffsetOrAddress::Address(
                                function_label as _,
                            ),
                        })
                    }
                }
            }
        }
        TypeOrGenericTypeId::GenericType(struct_type) => {
            let (function_type, mut variables) = bind_generic_function_type(
                function_name.clone(),
                Some(struct_type),
                function_declaration.function_type,
                binder,
            );
            variables.push(("this".to_owned().into(), struct_type.into()));
            let generic_parameters = binder.project.types[struct_type]
                .generic_parameters()
                .to_owned();
            let type_ = binder
                .project
                .types
                .look_up_or_add_generic_type(GenericType::Function(
                    function_type.clone(),
                    generic_parameters,
                ));

            // FIXME: implement getter for generic structs.
            match StructFunctionKind::try_from(binder.lexeme(function_declaration.identifier)) {
                Ok(struct_function_kind) => {
                    // let function_label = struct_function_kind as u64;
                    let function_label = binder.generate_label() as u64;
                    {
                        let struct_type = binder.project.types.to_fake_type_id(struct_type);
                        let function_type =
                            function_type.to_fake_type_id(&mut binder.project.types);
                        type_check_struct_function_kind(
                            header_span,
                            struct_function_kind,
                            &function_type,
                            struct_type,
                            binder,
                        );
                    }
                    binder.add_generic_function_declaration(FunctionDeclarationBody {
                        header_location: header_span,
                        function_name: function_name.clone().into(),
                        body: *function_declaration.body,
                        parameters: variables,
                        is_main: false,
                        available_generic_types: binder.available_generic_types.clone(),
                        function_label,
                        function_type: type_.into(),
                        base_struct: Some(struct_type.into()),
                        struct_function_kind: None,
                    });

                    struct_function_table.set(
                        struct_function_kind,
                        FunctionSymbol {
                            name: function_name,
                            function_type: binder.project.types.to_fake_type_id(type_),
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
                            function_declaration.identifier.location,
                            function_declaration.identifier.location,
                        );
                        None
                    } else {
                        let function_label = binder.generate_label() as u64;
                        binder.add_generic_function_declaration(FunctionDeclarationBody {
                            header_location: header_span,
                            function_name: function_name.clone().into(),
                            body: *function_declaration.body,
                            parameters: variables,
                            is_main: false,
                            available_generic_types: binder.available_generic_types.clone(),
                            function_label,
                            function_type: type_.into(),
                            base_struct: Some(struct_type.into()),
                            struct_function_kind: None,
                        });
                        let type_ = binder.project.types.to_fake_type_id(type_);
                        let (struct_name, function_name) = function_name.split_once("::").unwrap();
                        let function_name = {
                            use std::fmt::Write;
                            let mut result = format!("{}<", struct_name);
                            for t in &binder.available_generic_types {
                                write!(
                                    result,
                                    "${}, ",
                                    t.as_str(&binder.project.source_text_collection)
                                )
                                .unwrap();
                            }
                            write!(result, ">::{}", function_name).unwrap();
                            result
                        };
                        binder.register_constant(
                            function_name.clone(),
                            Value::LabelPointer(function_label as usize, type_.clone()),
                        );
                        Some(Member {
                            name: binder.lexeme(function_declaration.identifier).into(),
                            type_,
                            is_read_only: true,
                            offset_or_address: typing::MemberOffsetOrAddress::Address(
                                function_label as _,
                            ),
                        })
                    }
                }
            }
        }
    }
}

fn type_check_struct_function_kind(
    location: TextLocation,
    struct_function_kind: StructFunctionKind,
    function_type: &FunctionType<TypeId>,
    struct_type: TypeId,
    binder: &mut BindingState,
) {
    match struct_function_kind {
        StructFunctionKind::Constructor => {
            if !binder
                .project
                .types
                .can_be_converted(function_type.return_type, typeid!(Type::Void))
            {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.return_type,
                    typeid!(Type::Void),
                );
            }
        }
        StructFunctionKind::ToString => {
            if !binder
                .project
                .types
                .can_be_converted(function_type.return_type, typeid!(Type::String))
            {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.return_type,
                    typeid!(Type::String),
                );
            }
            if !function_type.parameter_types.is_empty() {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    location,
                    function_type.parameter_types.len(),
                    0,
                );
            }
        }
        StructFunctionKind::Get => {
            if function_type.parameter_types.len() != 1 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    location,
                    function_type.parameter_types.len(),
                    1,
                );
            }
        }
        StructFunctionKind::Set => {
            if function_type.parameter_types.len() != 2 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    location,
                    function_type.parameter_types.len(),
                    2,
                );
            }
        }
        StructFunctionKind::ElementCount => {
            if !function_type.parameter_types.is_empty() {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    location,
                    function_type.parameter_types.len(),
                    0,
                );
            }
            if !binder.project.types.can_be_converted(
                function_type.return_type,
                typeid!(Type::Integer(IntegerType::Unsigned64)),
            ) {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.return_type,
                    typeid!(Type::Integer(IntegerType::Unsigned64)),
                );
            }
        }
        StructFunctionKind::Equals => {
            if function_type.parameter_types.len() != 1 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    location,
                    function_type.parameter_types.len(),
                    1,
                );
            }
            if !binder
                .project
                .types
                .can_be_converted(function_type.parameter_types[0], struct_type)
            {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.parameter_types[0],
                    struct_type,
                );
            }
            if !binder
                .project
                .types
                .can_be_converted(function_type.return_type, typeid!(Type::Boolean))
            {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.return_type,
                    typeid!(Type::Boolean),
                );
            }
        }
        StructFunctionKind::Add => {
            if function_type.parameter_types.len() != 1 {
                binder.diagnostic_bag.report_unexpected_parameter_count(
                    location,
                    function_type.parameter_types.len(),
                    2,
                );
            }
        }
    }
}

fn bind_struct_declaration<'a, 'b>(
    struct_declaration: StructDeclarationNodeKind,
    binder: &mut BindingState<'b>,
) {
    let mut struct_function_table = SimpleStructFunctionTable::default();
    for statement in &struct_declaration.body.statements {
        match &statement.kind {
            SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
                let function_name = binder.lexeme(function_declaration.identifier);
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
    let is_generic = !struct_declaration.generic_parameters.is_empty();
    let successfull_struct_declaration = if is_generic {
        binder
            .register_generic_struct_name(
                struct_declaration.identifier.location,
                struct_function_table,
                struct_declaration
                    .generic_parameters
                    .iter()
                    .map(|t| binder.project.source_text_collection[t.location].to_string())
                    .collect(),
            )
            .map(|_| ())
    } else {
        binder
            .register_struct_name(
                struct_declaration.identifier.location,
                struct_function_table,
                Vec::new(),
            )
            .map(|_| ())
    };
    let is_abstract = struct_declaration.optional_abstract_keyword.is_some();
    if successfull_struct_declaration == Some(()) {
        let parent_id = struct_declaration.optional_parent.map(|i| {
            let i = binder.lexeme(i).to_owned();
            binder
                .look_up_type_by_name(&i)
                .expect("Implement error message if parent does not exist!")
                .unwrap_type_id()
        });
        if !struct_declaration.generic_parameters.is_empty() {
            use std::fmt::Write;

            let mut name = format!("{}<", binder.lexeme(struct_declaration.identifier));
            let mut is_first = true;
            let generic_parameters: Vec<_> = struct_declaration
                .generic_parameters
                .into_iter()
                .map(|p| SourceCow::Source(p.location))
                .collect();
            for parameter in &generic_parameters {
                if is_first {
                    is_first = false;
                } else {
                    write!(name, ", ").unwrap();
                }
                write!(
                    name,
                    "{}",
                    parameter.as_str(&binder.project.source_text_collection)
                )
                .unwrap();
            }
            write!(name, ">").unwrap();
            let applied_types = (generic_parameters.iter().enumerate())
                .map(|(i, n)| {
                    binder.project.types.look_up_or_add_type(Type::GenericType(
                        i,
                        n.to_owned()
                            .to_owned(&binder.project.source_text_collection),
                    ))
                })
                .collect();
            binder
                .register_struct_name(name, struct_function_table, applied_types)
                .unwrap();
            binder.generic_structs.push(StructDeclarationBody {
                location: struct_declaration.identifier.location,
                name: struct_declaration.identifier.location.into(),
                body: *struct_declaration.body,
                struct_function_table,
                parent_id,
                is_abstract,
                generic_parameters,
            });
        } else {
            binder.structs.push(StructDeclarationBody {
                location: struct_declaration.identifier.location,
                name: struct_declaration.identifier.location.into(),
                body: *struct_declaration.body,
                struct_function_table,
                parent_id,
                is_abstract,
                generic_parameters: Vec::new(),
            });
        }
    } else {
        binder.diagnostic_bag.report_cannot_declare_struct(
            struct_declaration.identifier.location,
            struct_declaration.identifier.location,
        );
    }
}

fn bind_enum_declaration<'a>(
    enum_declaration: EnumDeclarationNodeKind,
    binder: &mut BindingState<'_>,
) {
    let mut values = Vec::new();
    for value in &enum_declaration.body.values {
        values.push(binder.lexeme(value.identifier).into());
    }
    let type_ = binder.project.types.look_up_or_add_type(Type::Enum(
        binder.lexeme(enum_declaration.identifier).into(),
        values.clone(),
    ));
    binder.exported_enums.push(type_);
    binder.register_constant(
        enum_declaration.identifier.location,
        Value::EnumType(values, type_),
    );
}

fn bind_function_type<'a>(
    name: String,
    this_type: Option<TypeId>,
    function_type: FunctionTypeNode,
    binder: &mut BindingState<'_>,
) -> (FunctionType<TypeId>, Vec<(SourceCow, TypeId)>) {
    let variable_count = binder.variable_table.len();
    let mut parameters = vec![];
    for parameter in function_type.parameters {
        let parameter = bind_parameter(parameter, binder);
        let parameter = (parameter.0, parameter.1.unwrap_type_id());
        binder.register_variable(parameter.0, parameter.1, false);
        let parameter = (parameter.0.into(), parameter.1);
        parameters.push(parameter);
    }
    let return_type = if let Some(it) = function_type.return_type {
        bind_type(it.return_type, binder).unwrap_type_id()
    } else {
        typeid!(Type::Void)
    };
    binder.delete_variables_until(variable_count);
    (
        FunctionType {
            parameter_types: parameters.clone().into_iter().map(|(_, t)| t).collect(),
            this_type,
            return_type,
            system_call_kind: None,
            name: Some(name.into()),
            is_generic: function_type.is_generic,
        },
        parameters,
    )
}

fn bind_generic_function_type<'a>(
    name: String,
    this_type: Option<GenericTypeId>,
    function_type: FunctionTypeNode,
    binder: &mut BindingState<'_>,
) -> (
    FunctionType<GenericTypeId>,
    Vec<(SourceCow, TypeOrGenericTypeId)>,
) {
    let variable_count = binder.variable_table.len();
    let mut parameters = vec![];
    for parameter in function_type.parameters {
        let (parameter_name, parameter_type) = bind_parameter(parameter, binder);
        let variable_type = match parameter_type {
            TypeOrGenericTypeId::Type(it) => it,
            TypeOrGenericTypeId::GenericType(it) => binder.project.types.to_fake_type_id(it),
        };
        binder.register_variable(parameter_name, variable_type, false);
        parameters.push((parameter_name.into(), parameter_type));
    }
    let return_type = if let Some(it) = function_type.return_type {
        bind_type(it.return_type, binder).unwrap_type_id()
    } else {
        typeid!(Type::Void)
    };
    binder.delete_variables_until(variable_count);
    (
        FunctionType {
            parameter_types: parameters
                .clone()
                .into_iter()
                .map(|(_, t)| t.convert_to_type_id(&mut binder.project.types))
                .collect(),
            this_type,
            return_type,
            system_call_kind: None,
            name: Some(name.into()),
            is_generic: function_type.is_generic,
        },
        parameters,
    )
}

fn bind_struct_body<'a>(
    struct_node: StructDeclarationBody,
    binder: &mut BindingState<'_>,
) -> StructType {
    let mut fields: Vec<Member> = vec![];
    let mut functions = vec![];
    let mut function_table = StructFunctionTable::default();
    let mut offset = 0;
    let struct_is_generic = struct_node.body.is_generic;
    let is_abstract = struct_node.is_abstract;
    let mut used_generic_type = vec![false; struct_node.generic_parameters.len()];

    binder.available_generic_types = struct_node.generic_parameters;

    let parent_name = struct_node
        .parent_id
        .map(|p| binder.project.types.name_of_type_id(p).into_owned());
    if let Some(is_abstract) =
        struct_node
            .parent_id
            .map(|t| match binder.project.types[t].as_struct_type() {
                Some(it) => it.is_abstract,
                None => {
                    binder.diagnostic_bag.report_expected_struct(
                        struct_node.location,
                        parent_name.as_ref().unwrap().as_str(),
                    );
                    true
                }
            })
    {
        if !is_abstract {
            binder
                .diagnostic_bag
                .report_struct_parent_not_abstract(struct_node.location, parent_name.unwrap());
        }
    }

    for statement in struct_node.body.statements {
        let span = statement.location;
        let is_function;
        let member = match statement.kind {
            SyntaxNodeKind::FunctionDeclaration(function_declaration) => {
                is_function = true;
                bind_function_declaration_for_struct(
                    &struct_node.name,
                    *function_declaration,
                    &mut function_table,
                    struct_node.struct_function_table,
                    binder,
                )
            }
            SyntaxNodeKind::StructField(struct_field) => {
                is_function = false;
                // let type_span = struct_field.field.type_declaration.type_.location();
                let (name, type_) = bind_parameter(struct_field.field, binder);
                let type_ = type_.unwrap_type_id();
                let field_offset = {
                    let type_ = &binder.project.types[type_];
                    offset += type_.size_in_bytes();
                    offset - type_.size_in_bytes()
                };
                for (i, n) in binder.available_generic_types.iter().enumerate() {
                    let generic_type = binder.project.types.look_up_or_add_type(Type::GenericType(
                        i,
                        n.to_owned()
                            .to_owned(&binder.project.source_text_collection),
                    ));
                    if binder.project.types.contains_type(type_, generic_type) {
                        used_generic_type[i] = true;
                    }
                }
                Some(Member {
                    name: binder.project.source_text_collection[name].into(),
                    offset_or_address: MemberOffsetOrAddress::Offset(field_offset as usize),
                    type_,
                    is_read_only: false,
                })
            }
            unexpected => unreachable!("Unexpected Struct Member {:#?} found!", unexpected),
        };
        if member.is_none() {
            continue;
        }
        let member = member.unwrap();
        if !is_function && fields.iter().any(|f| f.name == member.name) {
            binder
                .diagnostic_bag
                .report_field_already_declared(span, &member.name);
        }
        if is_function {
            functions.push(member);
        } else {
            fields.push(member);
        }
    }
    if struct_is_generic {
        for i in used_generic_type
            .into_iter()
            .enumerate()
            .filter(|(_, u)| !u)
            .map(|(i, _)| i)
        {
            binder
                .diagnostic_bag
                .report_generic_struct_without_generic_type(
                    struct_node.location,
                    struct_node.name.clone(),
                    binder.available_generic_types[i].clone(),
                );
        }
    } else {
        if used_generic_type.into_iter().any(|u| u) {
            binder
                .diagnostic_bag
                .report_generic_type_in_ungeneric_struct(
                    struct_node.location,
                    struct_node.name.clone(),
                );
        }
    }
    let mut size_in_bytes = fields
        .iter()
        .map(|f| binder.project.types[f.type_].size_in_bytes())
        .sum();
    let mut current_type_id = struct_node.parent_id;
    while let Some(type_id) = current_type_id {
        let struct_type = binder.project.types[type_id]
            .as_struct_type()
            .expect("Parent should have been a struct!");
        size_in_bytes += struct_type.size_in_bytes;
        current_type_id = struct_type.parent;
    }
    StructType {
        name: struct_node
            .name
            .clone()
            .to_owned(&binder.project.source_text_collection)
            .into(),
        fields,
        functions,
        function_table,
        is_generic: struct_is_generic,
        is_abstract,
        parent: struct_node.parent_id,
        size_in_bytes,
        applied_types: Vec::new(),
        generic_base_type: None,
    }
}

fn bind_parameter(
    parameter: ParameterNode,
    binder: &mut BindingState,
) -> (TextLocation, TypeOrGenericTypeId) {
    let name = parameter.identifier.location;
    let type_ = bind_type(parameter.type_declaration.type_, binder);
    (name, type_)
}

fn bind_type(type_: TypeNode, binder: &mut BindingState) -> TypeOrGenericTypeId {
    let type_name = binder.lexeme(type_.type_name);
    let type_location = type_.location();
    let mut result = if let Some(library_token) = &type_.library_name {
        let library_name = binder.lexeme(*library_token);
        let library_variable = binder.look_up_variable_or_constant_by_name(library_name);
        let library_variable = match library_variable.kind {
            VariableOrConstantKind::None => {
                binder
                    .diagnostic_bag
                    .report_unknown_library(library_token.location, library_token.location);
                return typeid!(Type::Error).into();
            }
            _ => library_variable,
        };
        match &binder.project.types[library_variable.type_] {
            Type::Error => return typeid!(Type::Error).into(),
            Type::Library(_) => {}
            _ => {
                binder.diagnostic_bag.report_cannot_convert(
                    library_token.location,
                    library_variable.type_,
                    binder.project.types.look_up_or_add_type(Type::Library(0)),
                );
                return typeid!(Type::Error).into();
            }
        }
        binder.look_up_type_by_name(&format!("{}.{}", library_name, type_name))
    } else {
        binder.look_up_type_by_name(&type_name.to_owned())
    }
    .unwrap_or_else(|| {
        binder.diagnostic_bag.report_unknown_type(
            type_location,
            &type_.full_type_name(&binder.project.source_text_collection),
        );
        typeid!(Type::Error).into()
    });
    let generic_type_qualifiers: Vec<TypeId> = type_
        .generic_type_qualifier
        .into_iter()
        .map(|t| bind_type(t, binder).unwrap_type_id())
        .collect();
    if !generic_type_qualifiers.is_empty() {
        if generic_type_qualifiers
            .iter()
            .all(|t| matches!(&binder.project.types[*t], Type::GenericType(_, _)))
        {
            result = binder
                .project
                .types
                .to_fake_type_id(result.unwrap_generic_type_id())
                .into();
        } else {
            result = bind_generic_struct_type_for_types(
                type_location,
                result.unwrap_generic_type_id(),
                &generic_type_qualifiers,
                binder,
            )
            .into();
        }
    }
    if type_.optional_question_mark.is_some() {
        let noneable_base_type = binder
            .project
            .types
            .look_up_type_by_name("Noneable")
            .expect("Failed to load type Nonable from stdlib.")
            .unwrap_generic_type_id();
        result = bind_generic_struct_type_for_types(
            type_location,
            noneable_base_type,
            &[result.unwrap_type_id()],
            binder,
        )
        .into();
    }
    if type_.optional_ampersand_token.is_some() {
        result = binder
            .project
            .types
            .create_pointer_of_version(result.unwrap_type_id())
            .into();
    }
    if !type_.brackets.is_empty() {
        let array_base_type = binder
            .project
            .types
            .look_up_type_by_name("Array")
            .expect("Failed to load type Array from stdlib.")
            .unwrap_generic_type_id();
        for _ in &type_.brackets {
            let new_type = bind_generic_struct_type_for_types(
                type_location,
                array_base_type,
                &[result.unwrap_type_id()],
                binder,
            );
            result = new_type.into();
        }
    }
    result
}

fn bind_node<'a, 'b>(node: SyntaxNode, binder: &mut BindingState<'b>) -> BoundNode {
    let result = match node.kind {
        SyntaxNodeKind::Literal(literal) => bind_literal(node.location, literal, binder),
        SyntaxNodeKind::ArrayLiteral(array_literal) => {
            bind_array_literal(node.location, array_literal, binder)
        }
        SyntaxNodeKind::DictionaryLiteral(dictionary_literal) => {
            bind_dictionary_literal(node.location, dictionary_literal, binder)
        }
        SyntaxNodeKind::CastExpression(cast_expression) => {
            bind_cast_expression(node.location, cast_expression, binder)
        }
        SyntaxNodeKind::ConstructorCall(constructor_call) => {
            bind_constructor_call(node.location, constructor_call, binder)
        }
        SyntaxNodeKind::Variable(variable) => bind_variable(node.location, variable, binder),
        SyntaxNodeKind::Binary(binary) => bind_binary(node.location, binary, binder),
        SyntaxNodeKind::Unary(unary) => bind_unary(node.location, unary, binder),
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            bind_parenthesized(node.location, parenthesized, binder)
        }
        SyntaxNodeKind::FunctionCall(function_call) => {
            bind_function_call(node.location, function_call, binder)
        }
        SyntaxNodeKind::ArrayIndex(array_index) => {
            bind_array_index(node.location, array_index, binder)
        }
        SyntaxNodeKind::FieldAccess(field_access) => {
            bind_field_access(node.location, field_access, binder)
        }
        SyntaxNodeKind::BlockStatement(block_statement) => {
            bind_block_statement(node.location, block_statement, binder)
        }
        SyntaxNodeKind::ForStatement(for_statement) => {
            bind_for_statement(node.location, for_statement, binder)
        }
        SyntaxNodeKind::IfStatement(if_statement) => {
            bind_if_statement(node.location, if_statement, binder)
        }
        SyntaxNodeKind::VariableDeclaration(variable_declaration) => {
            bind_variable_declaration(node.location, variable_declaration, binder)
        }
        SyntaxNodeKind::ReturnStatement(return_statement) => {
            bind_return_statement(node.location, return_statement, binder)
        }
        SyntaxNodeKind::WhileStatement(while_statement) => {
            bind_while_statement(node.location, while_statement, binder)
        }
        SyntaxNodeKind::MatchStatement(match_statement) => {
            bind_match_statement(node.location, match_statement, binder)
        }
        SyntaxNodeKind::Assignment(assignment) => {
            bind_assignment(node.location, assignment, binder)
        }
        SyntaxNodeKind::ExpressionStatement(expression_statement) => {
            bind_expression_statement(node.location, expression_statement, binder)
        }
        SyntaxNodeKind::RepetitionNode(repetition_node) => {
            bind_repetition_node(node.location, repetition_node, binder)
        }
        err => unreachable!("{err:#?}"),
    };
    if let Some(result) = binder.get_safe_node(&result) {
        result
    } else {
        result
    }
}

fn bind_node_for_assignment<'a, 'b>(node: SyntaxNode, binder: &mut BindingState<'b>) -> BoundNode {
    match node.kind {
        SyntaxNodeKind::Variable(variable) => {
            bind_variable_for_assignment(node.location, variable, binder)
        }
        SyntaxNodeKind::Parenthesized(parenthesized) => {
            bind_node_for_assignment(*parenthesized.expression, binder)
        }
        SyntaxNodeKind::FunctionCall(_) => todo!(),
        SyntaxNodeKind::ArrayIndex(array_index) => {
            bind_array_index_for_assignment(node.location, array_index, binder)
        }
        SyntaxNodeKind::FieldAccess(field_access) => {
            bind_field_access_for_assignment(node.location, field_access, binder)
        }
        _ => unreachable!("Unexpected left hand side of assignment {:#?}", node),
    }
}

fn bind_literal(span: TextLocation, literal: LiteralNodeKind, _: &mut BindingState) -> BoundNode {
    BoundNode::literal(span, literal.value)
}

fn bind_array_literal<'a, 'b>(
    location: TextLocation,
    mut array_literal: ArrayLiteralNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let array_type = if array_literal.optional_array_list_keyword.is_some() {
        binder
            .project
            .types
            .look_up_type_by_name("List")
            .expect("Failed to load type List from std lib.")
            .unwrap_generic_type_id()
    } else {
        binder
            .project
            .types
            .look_up_type_by_name("Array")
            .expect("Failed to load type Array from std lib.")
            .unwrap_generic_type_id()
    };
    let expected_type = if let Some(struct_type) = binder
        .expected_type
        .map(|t| binder.project.types[t].as_struct_type())
        .flatten()
    {
        Some(struct_type.applied_types.first().unwrap().clone())
    } else {
        None
    };
    if array_literal.children.is_empty() {
        return if let Some(expected_type) = expected_type {
            let type_ =
                bind_generic_struct_type_for_types(location, array_type, &[expected_type], binder);
            BoundNode::array_literal(location, Vec::new(), type_)
        } else {
            binder
                .diagnostic_bag
                .report_underspecified_generic_struct(location, "Array");
            BoundNode::error(location)
        };
    }
    let first_child = array_literal.children.remove(0);
    let (mut type_, mut children) =
        bind_array_literal_first_child(first_child, expected_type, binder);
    for child in array_literal.children {
        let child = bind_node(child, binder);
        // If we only read integer literals until now, and the current entry is
        // not an integer literal, try using it as the element type of the
        // array.
        if type_ == typeid!(Type::IntegerLiteral) && child.type_ != typeid!(Type::IntegerLiteral) {
            if binder.project.types.can_be_converted(type_, child.type_) {
                // FIXME: Do we need to update all children until now??
                type_ = child.type_.clone();
            }
        }
        let child = bind_conversion(child, type_, binder);
        children.push(child);
    }
    if type_ == typeid!(Type::IntegerLiteral) {
        type_ = typeid!(Type::Integer(IntegerType::Signed64));
    }
    let type_ = bind_generic_struct_type_for_types(location, array_type, &[type_], binder);
    BoundNode::array_literal(location, children, type_)
}

fn bind_array_literal_first_child<'a>(
    first_child: SyntaxNode,
    expected_type: Option<TypeId>,
    binder: &mut BindingState<'_>,
) -> (TypeId, Vec<BoundNode>) {
    let children = vec![bind_node(first_child, binder)];
    let type_ = expected_type.unwrap_or_else(|| children[0].type_.clone());
    // If expected_type was None, this would be a noop.
    let children = children
        .into_iter()
        .map(|c| bind_conversion(c, type_, binder))
        .collect();
    (type_, children)
}

fn bind_dictionary_literal<'a, 'b>(
    location: TextLocation,
    mut dictionary_literal: DictionaryLiteralNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let dict_type = binder
        .project
        .types
        .look_up_type_by_name("Dict")
        .expect("Failed to load type Dict from std lib.")
        .unwrap_generic_type_id();
    let expected_type = if let Some(struct_type) = binder
        .expected_type
        .map(|t| binder.project.types[t].as_struct_type())
        .flatten()
    {
        let key_type = struct_type.applied_types[0];
        let value_type = struct_type.applied_types[1];
        Some((key_type, value_type))
    } else {
        None
    };
    if dictionary_literal.values.is_empty() {
        return if let Some((key, value)) = expected_type {
            let type_ =
                bind_generic_struct_type_for_types(location, dict_type, &[key, value], binder);
            let function = binder.project.types[type_]
                .as_struct_type()
                .unwrap()
                .function_table
                .constructor_function
                .as_ref()
                .map(|f| f.function_label);
            BoundNode::constructor_call(location, Vec::new(), type_, function)
        } else {
            binder
                .diagnostic_bag
                .report_underspecified_generic_struct(location, "Dict");
            BoundNode::error(location)
        };
    }
    let first_child = dictionary_literal.values.remove(0);
    let (mut type_, mut values) =
        bind_dictionary_literal_first_child(first_child, expected_type, binder);
    for (key, value) in dictionary_literal.values {
        let key = bind_node(key, binder);
        // If we only read integer literals until now, and the current entry is
        // not an integer literal, try using it as the element type of the
        // array.
        if type_.0 == typeid!(Type::IntegerLiteral) && key.type_ != typeid!(Type::IntegerLiteral) {
            if binder.project.types.can_be_converted(type_.0, key.type_) {
                // FIXME: Do we need to update all children until now??
                type_.0 = key.type_.clone();
            }
        }
        let key = bind_conversion(key, type_.0, binder);

        let value = bind_node(value, binder);
        // If we only read integer literals until now, and the current entry is
        // not an integer literal, try using it as the element type of the
        // array.
        if type_.1 == typeid!(Type::IntegerLiteral) && value.type_ != typeid!(Type::IntegerLiteral)
        {
            if binder.project.types.can_be_converted(type_.1, value.type_) {
                // FIXME: Do we need to update all children until now??
                type_.1 = value.type_.clone();
            }
        }
        let value = bind_conversion(value, type_.1, binder);

        values.push((key, value));
    }
    if type_.0 == typeid!(Type::IntegerLiteral) {
        type_.0 = typeid!(Type::Integer(IntegerType::Signed64));
    }
    if type_.1 == typeid!(Type::IntegerLiteral) {
        type_.1 = typeid!(Type::Integer(IntegerType::Signed64));
    }
    let type_ =
        bind_generic_struct_type_for_types(location, dict_type, &[type_.0, type_.1], binder);
    let temporary_variable = binder
        .register_variable("$tmpDictionary".to_owned(), type_, false)
        .expect("Failed to declare temporary variable!");
    BoundNode::dictionary_literal(
        location,
        temporary_variable,
        values,
        type_,
        &binder.project.types,
    )
}

fn bind_dictionary_literal_first_child<'a>(
    first_child: (SyntaxNode, SyntaxNode),
    expected_type: Option<(TypeId, TypeId)>,
    binder: &mut BindingState<'_>,
) -> ((TypeId, TypeId), Vec<(BoundNode, BoundNode)>) {
    let children = vec![(
        bind_node(first_child.0, binder),
        bind_node(first_child.1, binder),
    )];
    let type_ = expected_type.unwrap_or_else(|| (children[0].0.type_, children[0].1.type_));
    // If expected_type was None, this would be a noop.
    let children = children
        .into_iter()
        .map(|c| {
            (
                bind_conversion(c.0, type_.0, binder),
                bind_conversion(c.1, type_.1, binder),
            )
        })
        .collect();
    (type_, children)
}

fn bind_cast_expression<'a>(
    location: TextLocation,
    cast_expression: CastExpressionNodeKind,
    binder: &mut BindingState<'_>,
) -> BoundNode {
    let expression = bind_node(*cast_expression.expression, binder);
    let type_ = match bind_type(cast_expression.type_, binder) {
        TypeOrGenericTypeId::Type(it) => it,
        TypeOrGenericTypeId::GenericType(generic) => {
            binder
                .diagnostic_bag
                .report_expected_type_found_generic(location, generic);
            return BoundNode::error(location);
        }
    };
    // Cast unnecessary and will always return a valid value.
    if binder
        .project
        .types
        .can_be_converted(expression.type_, type_)
    {
        binder
            .diagnostic_bag
            .report_unnecessary_cast(location, expression.type_, type_);
        return bind_conversion(expression, type_, binder);
    }
    if !binder
        .project
        .types
        .can_be_casted_to(expression.type_, type_)
    {
        binder
            .diagnostic_bag
            .report_impossible_cast(location, expression.type_, type_);
        return BoundNode::error(location);
    }
    let noneable_base_type = binder
        .project
        .types
        .look_up_type_by_name("Noneable")
        .expect("Failed to load type Noneable from stdlib.")
        .unwrap_generic_type_id();
    let type_noneable =
        bind_generic_struct_type_for_types(location, noneable_base_type, &[type_], binder);
    if binder
        .project
        .types
        .can_be_converted(expression.type_, type_noneable)
    {
        binder
            .diagnostic_bag
            .report_unnecessary_cast(location, expression.type_, type_);
        return bind_conversion(expression, type_noneable, binder);
    }
    BoundNode::conversion(location, expression, type_noneable)
}

fn bind_constructor_call<'a>(
    location: TextLocation,
    constructor_call: ConstructorCallNodeKind,
    binder: &mut BindingState<'_>,
) -> BoundNode {
    let type_name_location = constructor_call.type_name.location;
    let type_ = {
        let type_ = bind_type(
            TypeNode {
                optional_ampersand_token: None,
                library_name: constructor_call.library_name,
                type_name: constructor_call.type_name,
                generic_type_qualifier: Vec::new(), // TODO: we might wanna use this later.
                optional_question_mark: None,
                brackets: vec![],
            },
            binder,
        );
        match (type_, binder.expected_type) {
            (TypeOrGenericTypeId::GenericType(it), Some(expected))
                if binder.project.types[expected].as_struct_type().is_some()
                    && binder.project.types[it].as_struct_type().is_some() =>
            {
                let it = binder.project.types[it].as_struct_type().unwrap();
                let expected_struct = binder.project.types[expected].as_struct_type().unwrap();
                // NOTE: If we have something like List<int> the name of
                // expected_struct will be List<int> but the name of type_ will
                // be just List. So we do this little hack to make them
                // comparable.
                let expected_base_name = expected_struct
                    .name
                    .split_once('<')
                    .map(|(s, _)| s)
                    .unwrap_or(&expected_struct.name);
                if it.name == expected_base_name
                    && it.applied_types.is_empty()
                    && expected_struct.applied_types.len() == 1
                {
                    expected.into()
                } else {
                    type_
                }
            }
            _ => type_,
        }
    };
    let struct_id = type_;
    let type_ = &binder.project.types[type_];
    let struct_type = match type_ {
        TypeOrGenericType::Type(Type::Struct(it)) => it,
        TypeOrGenericType::Type(Type::Error) => return BoundNode::error(location),
        TypeOrGenericType::GenericType(GenericType::Struct(it)) => &it.struct_type,
        _ => {
            dbg!(&type_);
            binder.diagnostic_bag.report_unknown_type(
                type_name_location,
                &binder.project.source_text_collection[constructor_call.type_name.location],
            );
            return BoundNode::error(location);
        }
    };
    let struct_type = struct_type.clone();
    let (arguments, function, struct_type) = match &struct_type.function_table.constructor_function
    {
        Some(function) => {
            if struct_type.is_generic {
                let (arguments, label, struct_type) =
                    bind_arguments_for_generic_constructor_on_struct(
                        location,
                        constructor_call.arguments,
                        function.function_label as usize,
                        function.function_type,
                        struct_id.unwrap_generic_type_id(),
                        binder,
                    );
                (arguments, Some(label as u64), struct_type)
            } else {
                (
                    bind_arguments_for_function(
                        location,
                        constructor_call.arguments,
                        function.function_type,
                        binder,
                    ),
                    Some(function.function_label),
                    struct_id.unwrap_type_id(),
                )
            }
        }
        None => {
            binder.generic_types.clear();
            let replace = if let TypeOrGenericType::GenericType(g) =
                &binder.project.types[struct_id].clone()
            {
                g.generic_parameters()
                    .clone()
                    .into_iter()
                    .enumerate()
                    .map(|(i, n)| {
                        binder
                            .project
                            .types
                            .look_up_or_add_type(Type::GenericType(i, n.clone()))
                    })
                    .collect()
            } else {
                Vec::new()
            };
            binder.generic_types = std::iter::repeat(typeid!(Type::Void))
                .take(replace.len())
                .collect();
            let fields: Vec<_> = struct_type
                .fields_for_constructor(&binder.project.types)
                .into_iter()
                .cloned()
                .collect();
            if constructor_call.arguments.len() != fields.len() {
                binder.diagnostic_bag.report_unexpected_argument_count(
                    location,
                    constructor_call.arguments.len(),
                    fields.len(),
                );
            }
            let mut arguments = Vec::with_capacity(constructor_call.arguments.len());
            let mut resolved_struct_type = if struct_type.is_generic {
                None
            } else {
                Some(struct_id)
            };
            for (argument, parameter_type) in constructor_call
                .arguments
                .into_iter()
                .zip(fields.iter().map(|f| f.type_))
            {
                let old_expected_type = binder.expected_type.take();
                binder.expected_type = if binder.project.types.contains_generic_type(parameter_type)
                {
                    None
                } else {
                    Some(parameter_type)
                };
                let argument = bind_node(argument, binder);
                binder.expected_type = old_expected_type;
                let parameter_type = if struct_type.is_generic {
                    let generic_types = binder
                        .project
                        .types
                        .get_specified_generic_types(parameter_type, argument.type_);
                    for (parameter_type, argument_type) in generic_types {
                        if let Type::GenericType(generic_type_index, _name) =
                            &binder.project.types[parameter_type]
                        {
                            if binder.generic_types[*generic_type_index] == typeid!(Type::Void) {
                                binder.generic_types[*generic_type_index] =
                                    if argument_type == typeid!(Type::IntegerLiteral) {
                                        typeid!(Type::Integer(IntegerType::Signed64))
                                    } else {
                                        argument_type
                                    };
                            }
                        } else {
                            unreachable!()
                        }
                    }
                    if binder.project.types.contains_generic_type(parameter_type) {
                        let replace_with = binder.generic_types.clone();
                        binder.replace_in_type_for_types(
                            argument.location,
                            parameter_type,
                            &replace,
                            &replace_with,
                        )
                    } else {
                        parameter_type
                    }
                } else {
                    parameter_type.clone()
                };
                let argument = bind_conversion(argument, parameter_type, binder);
                arguments.push(argument);
            }
            if resolved_struct_type.is_none()
                && binder
                    .generic_types
                    .iter()
                    .copied()
                    .all(|t| t != typeid!(Type::Void))
            {
                resolved_struct_type = Some(
                    bind_generic_struct_type_for_types(
                        location,
                        struct_id.unwrap_generic_type_id(),
                        &binder.generic_types.clone(),
                        binder,
                    )
                    .into(),
                );
            }
            // binder.generic_types.clear();
            match resolved_struct_type {
                Some(it) => (arguments, None, it.unwrap_type_id()),
                // An error should already been reported.
                None => {
                    assert!(binder.diagnostic_bag.has_errors());
                    return BoundNode::error(location);
                }
            }
        }
    };

    BoundNode::constructor_call(location, arguments, struct_type, function)
}

fn bind_generic_struct_type_for_types(
    location: TextLocation,
    struct_id: GenericTypeId,
    types: &[TypeId],
    binder: &mut BindingState,
) -> TypeId {
    // TODO: Replace Type::IntegerLiteral in types.
    let struct_name = binder.project.types.generate_generic_name(struct_id, types);
    let id = if let Some(id) = binder.look_up_type_by_name(&struct_name) {
        id.unwrap_type_id()
    } else {
        let mut struct_function_table = SimpleStructFunctionTable::default();
        match &binder.project.types[struct_id] {
            GenericType::StructPlaceholder(it) => {
                it.function_table
                    .available_function_kinds()
                    .for_each(|k| struct_function_table.set(k, binder.generate_label()));
            }
            GenericType::Struct(it) => {
                it.struct_type
                    .function_table
                    .available_struct_function_kinds()
                    .into_iter()
                    .for_each(|k| struct_function_table.set(k, binder.generate_label()));
            }
            _ => {
                unreachable!()
            }
        }
        let id = binder
            .register_struct_name(struct_name.clone(), struct_function_table, types.to_vec())
            .expect("Add diagnostic message here?");
        bind_generic_struct_type_for_types_no_lookup(
            location,
            binder,
            id,
            struct_id,
            struct_name,
            types,
        );
        id
    };
    id
}

fn bind_generic_struct_type_for_types_no_lookup(
    location: TextLocation,
    binder: &mut BindingState,
    id: TypeId,
    struct_id: GenericTypeId,
    struct_name: String,
    types: &[TypeId],
) {
    let replace: Vec<_> = (binder.project.types[struct_id]
        .generic_parameters()
        .to_vec()
        .iter()
        .enumerate())
    .map(|(i, n)| {
        binder
            .project
            .types
            .look_up_or_add_type(Type::GenericType(i, n.to_owned()))
    })
    .collect();

    if types.len() != replace.len() {
        binder
            .diagnostic_bag
            .report_underspecified_generic_struct(location, struct_name);
        return;
    }

    let (generic_struct, _function_bodies): (_, HashMap<_, _>) =
        match &binder.project.types[struct_id] {
            GenericType::StructPlaceholder(_) => {
                binder
                    .generic_placeholder_types_that_need_to_be_bound_for_a_type
                    .push_back(UnfixedGenericType {
                        location,
                        struct_id,
                        type_id: id,
                        applied_types: types.to_vec(),
                    });
                return;
            }
            GenericType::Struct(it) => (
                it.struct_type.clone(),
                it.function_bodies()
                    .into_iter()
                    .map(|(a, b)| (a.to_owned(), b.clone()))
                    .collect(),
            ),
            GenericType::Function(..) => todo!(),
        };
    let mut function_labels_map: HashMap<usize, usize> = HashMap::default();
    function_labels_map.extend(
        generic_struct
            .function_table
            .function_symbols_iter()
            .map(|f| (f.function_label as usize, binder.generate_label())),
    );
    let mut simple_function_table = SimpleStructFunctionTable::default();
    // FIXME: StructFunctionTable should have a method that returns both the kind and the function at once!
    for (kind, function) in generic_struct
        .function_table
        .available_struct_function_kinds()
        .into_iter()
        .zip(generic_struct.function_table.function_symbols_iter())
    {
        // function.
        simple_function_table.set(
            kind,
            *function_labels_map
                .get(&(function.function_label as usize))
                .unwrap(),
        );
    }
    let fields: Vec<_> = generic_struct
        .fields
        .iter()
        .cloned()
        .map(|mut m| {
            m.type_ = binder.replace_in_type_for_types(location, m.type_, &replace, types);
            m
        })
        .collect();
    generic_struct
        .function_table
        .function_symbols_iter()
        .for_each(|f| {
            // FIXME: This assertions sadly does not hold true. It would very
            // nice if it would, but right now it is not necessary, it seems.
            // let new_type = binder
            //     .project
            //     .types
            //     .replace_in_type_for_types(f.function_type, &replace, types)
            //     .expect("Da fuck?");
            // assert_eq!(
            //     f.function_type,
            //     new_type,
            //     "{} vs {}
            //     {:#?}
            //     VERSUS
            //     {:#?}",
            //     binder.project.types.name_of_type_id_debug(f.function_type),
            //     binder.project.types.name_of_type_id_debug(new_type),
            //     binder.project.types[f.function_type],
            //     binder.project.types[new_type],
            // );
            // let label = binder.generate_label();
            let parameters = binder.project.types[f.function_type]
                .as_function_type()
                .unwrap()
                .parameter_types
                .iter()
                // This is the "this" argument of the function. It doesn't
                // really matter, what type we use, since we are only interested
                // in a numbered list!
                .chain(Some(&typeid!(Type::Error)))
                .enumerate()
                .map(|(i, _)| i as u64)
                .collect();
            let mut body = BoundNode::function_declaration(
                TextLocation::zero_in_file(binder.source_text),
                f.function_label as _,
                false,
                BoundNode::error(TextLocation::zero_in_file(binder.source_text)),
                parameters,
                None,
                f.function_type,
            );
            body.for_each_child_mut(&mut |n| {
                n.type_ = binder
                    .project
                    .types
                    .replace_in_type_for_types(n.type_, &replace, types)
                    .expect("Da fuck..");
            });
            let index_to_change = binder.bound_functions.len();
            binder
                .bound_functions_without_actual_body
                .push(UnfixedBoundFunction {
                    bound_function_index: index_to_change,
                    base_generic_struct: struct_id,
                    function_name: f.name.clone(),
                    applied_types: types.to_vec(),
                    base_struct: id,
                });
            binder.bound_functions.push(body);
            // new_f.function_label = label as _;
        });
    let mut functions: Vec<_> = generic_struct
        .functions
        .iter()
        .map(|f| {
            let mut new_f = f.clone();
            new_f.type_ = binder.replace_in_type_for_types(location, new_f.type_, &replace, types);
            let label = binder.generate_label();
            let parameters = binder.project.types[new_f.type_]
                .as_function_type()
                .unwrap()
                .parameter_types
                .iter()
                // This is the "this" argument of the function. It doesn't
                // really matter, what type we use, since we are only interested
                // in a numbered list!
                .chain(Some(&typeid!(Type::Error)))
                .enumerate()
                .map(|(i, _)| i as u64)
                .collect();
            let mut body = BoundNode::function_declaration(
                TextLocation::zero_in_file(binder.source_text),
                label,
                false,
                BoundNode::error(TextLocation::zero_in_file(binder.source_text)),
                parameters,
                None,
                new_f.type_,
            );
            body.for_each_child_mut(&mut |n| {
                n.type_ = binder
                    .project
                    .types
                    .replace_in_type_for_types(n.type_, &replace, types)
                    .expect("Da fuck..");
            });
            let index_to_change = binder.bound_functions.len();
            binder
                .bound_functions_without_actual_body
                .push(UnfixedBoundFunction {
                    bound_function_index: index_to_change,
                    base_generic_struct: struct_id,
                    function_name: new_f.name.clone(),
                    applied_types: types.to_vec(),
                    base_struct: id,
                });
            binder.bound_functions.push(body);
            new_f.offset_or_address = MemberOffsetOrAddress::Address(label);
            binder.register_constant(
                format!(
                    "{}::{}",
                    binder.project.types.name_of_type_id(id),
                    new_f.name
                ),
                Value::LabelPointer(label, new_f.type_),
            );
            new_f
        })
        .collect();
    let generic_functions = binder.project.types[struct_id]
        .as_generic_functions()
        .expect("This has to be a generic struct!")
        .to_vec();
    let extension: Vec<_> = generic_functions
        .iter()
        .filter(|f| !function_labels_map.contains_key(&(f.function_label as usize)))
        .map(|f| (f.function_label as usize, binder.generate_label()))
        .collect();
    function_labels_map.extend(extension);
    // let mut struct_function_table = SimpleStructFunctionTable::default();
    // for function in generic_struct
    //     .function_table
    //     .available_struct_function_kinds()
    // {
    //     let label = function_labels_map[&(generic_struct
    //         .function_table
    //         .get(function)
    //         .unwrap()
    //         .function_label as usize)];
    //     struct_function_table.set(function, label);
    // }
    let mut function_table = StructFunctionTable::default();
    for (kind, function) in generic_struct
        .function_table
        .available_struct_function_kinds()
        .into_iter()
        .zip(generic_struct.function_table.function_symbols_iter())
    {
        let mut function = function.clone();
        function.function_type = binder
            .project
            .types
            .replace_in_type_for_types(function.function_type, &replace, types)
            .expect("Da fuck?");
        function_table.set(kind, function.clone());
    }
    for generic_function in generic_functions.iter() {
        let label = function_labels_map[&(generic_function.function_label as usize)];
        let bare_function_name = generic_function.function_name.split_once("::").unwrap().1;
        let function_name = format!("{}::{}", struct_name, bare_function_name,);
        let function = bind_generic_function_for_type(
            location,
            generic_function,
            label,
            &function_labels_map,
            types,
            id,
            &function_name,
            binder,
        );
        match StructFunctionKind::try_from(bare_function_name) {
            Ok(it) => {
                function_table.set(
                    it,
                    FunctionSymbol {
                        name: function_name.clone(),
                        function_type: function,
                        function_label: label as u64,
                        is_member_function: false,
                    },
                );
            }
            Err(_) => {}
        }
        functions.push(Member {
            name: bare_function_name.into(),
            type_: function,
            offset_or_address: MemberOffsetOrAddress::Address(label),
            is_read_only: true,
        });
    }
    let size_in_bytes = fields
        .iter()
        .map(|f| binder.project.types[f.type_].size_in_bytes())
        .sum();

    binder.project.types.overwrite_type(
        id,
        Type::Struct(StructType {
            name: struct_name,
            fields,
            functions,
            function_table,
            is_generic: false,
            is_abstract: false, // Should there be abstract generic structs??
            parent: None,
            size_in_bytes,
            applied_types: types.to_vec(),
            generic_base_type: Some(struct_id),
        }),
    );
}

fn bind_variable<'a, 'b>(
    span: TextLocation,
    variable: VariableNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let variable_name = binder.lexeme(variable.token);
    let variable_loaction = variable.token.location;
    let variable = binder.look_up_variable_or_constant_by_name(variable_name);
    match variable.kind {
        VariableOrConstantKind::None => {
            binder
                .diagnostic_bag
                .report_variable_not_found(span, variable_loaction);
            BoundNode::error(span)
        }
        VariableOrConstantKind::Variable(variable_index) => {
            BoundNode::variable(span, variable_index, variable.type_)
        }
        VariableOrConstantKind::Constant(value) => BoundNode::literal(span, value),
    }
}

fn bind_variable_for_assignment<'a, 'b>(
    span: TextLocation,
    variable: VariableNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let variable_is_read_only = binder
        .look_up_variable_or_constant_by_name(binder.lexeme(variable.token))
        .is_read_only;
    if variable_is_read_only {
        binder
            .diagnostic_bag
            .report_variable_is_read_only(variable.token.location);
    }
    bind_variable(span, variable, binder)
}

fn bind_binary<'a, 'b>(
    span: TextLocation,
    binary: BinaryNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let lhs = bind_node(*binary.lhs, binder);
    let rhs = bind_node(*binary.rhs, binder);
    bind_binary_insertion(span, lhs, binary.operator_token, rhs, binder)
}

fn bind_binary_insertion<'a>(
    span: TextLocation,
    lhs: BoundNode,
    operator_token: SyntaxToken,
    rhs: BoundNode,
    binder: &mut BindingState<'_>,
) -> BoundNode {
    match bind_binary_operator(span, &lhs, operator_token, &rhs, binder) {
        Some(bound_binary) => {
            let (lhs, rhs) = if bound_binary.op == BoundBinaryOperator::StringConcat {
                (call_to_string(lhs, binder), call_to_string(rhs, binder))
            } else {
                (
                    bind_conversion(lhs, bound_binary.lhs, binder),
                    bind_conversion(rhs, bound_binary.rhs, binder),
                )
            };
            match bound_binary.op {
                BoundBinaryOperator::Range => {
                    let base_type = binder.project.types.look_up_type_by_name(if matches!(&binder.project.types[lhs.type_], Type::Integer(integer_type) if integer_type.is_signed())
                    {
                        "Range"
                    } else {
                        "UnsignedRange"
                    }).expect("Failed to load types from stdlib.");
                    let base_struct_type = binder.project.types[base_type.unwrap_type_id()]
                        .as_struct_type()
                        .expect("StructType");
                    let function = base_struct_type
                        .function_table
                        .constructor_function
                        .as_ref()
                        .map(|c| c.function_label);
                    BoundNode::constructor_call(
                        span,
                        vec![lhs, rhs],
                        base_type.unwrap_type_id(),
                        function,
                    )
                }
                BoundBinaryOperator::LogicalAnd => BoundNode::logical_and(span, lhs, rhs),
                BoundBinaryOperator::LogicalOr => BoundNode::logical_or(span, lhs, rhs),
                BoundBinaryOperator::Equals => {
                    let type_ = &binder.project.types[lhs.type_];
                    if let Type::Struct(struct_type) = type_ {
                        if let Some(equals_function) = &struct_type.function_table.equals_function {
                            let base = BoundNode::label_reference(
                                operator_token.location,
                                equals_function.function_label as usize,
                                equals_function.function_type.clone(),
                            );
                            BoundNode::equals_function_call(
                                span,
                                base,
                                vec![lhs, rhs],
                                false,
                                typeid!(Type::Boolean),
                            )
                        } else {
                            BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                        }
                    } else {
                        BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                    }
                }
                BoundBinaryOperator::NotEquals => {
                    let type_ = &binder.project.types[lhs.type_];
                    if let Type::Struct(struct_type) = type_ {
                        if let Some(equals_function) = &struct_type.function_table.equals_function {
                            let base = BoundNode::label_reference(
                                operator_token.location,
                                equals_function.function_label as usize,
                                equals_function.function_type.clone(),
                            );
                            BoundNode::unary(
                                span,
                                BoundUnaryOperator::LogicalNegation,
                                BoundNode::equals_function_call(
                                    span,
                                    base,
                                    vec![lhs, rhs],
                                    false,
                                    typeid!(Type::Boolean),
                                ),
                                typeid!(Type::Boolean),
                            )
                        } else {
                            BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                        }
                    } else {
                        BoundNode::binary(span, lhs, bound_binary.op, rhs, bound_binary.result)
                    }
                }
                BoundBinaryOperator::ArithmeticAddition => {
                    let type_ = &binder.project.types[lhs.type_];
                    if let Type::Struct(struct_type) = type_ {
                        if let Some(add_function) = &struct_type.function_table.add_function {
                            let base = BoundNode::label_reference(
                                operator_token.location,
                                add_function.function_label as usize,
                                add_function.function_type.clone(),
                            );
                            BoundNode::function_call(
                                span,
                                base,
                                vec![lhs, rhs],
                                false,
                                bound_binary.result,
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
    if lhs.type_ == typeid!(Type::String) {
        lhs
    } else {
        BoundNode::system_call(
            lhs.location,
            SystemCallKind::ToString,
            vec![bind_conversion(lhs, typeid!(Type::Any), binder)],
            typeid!(Type::String),
        )
    }
}

fn bind_binary_operator<'a, 'b>(
    location: TextLocation,
    lhs: &BoundNode,
    operator_token: SyntaxToken,
    rhs: &BoundNode,
    binder: &mut BindingState<'b>,
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
    let lhs_type = &binder.project.types[lhs.type_];
    let rhs_type = &binder.project.types[rhs.type_];
    match (lhs_type, result, rhs_type) {
        (lhs_type, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, rhs)
            if lhs_type == rhs && lhs.type_ != typeid!(Type::Void) =>
        {
            Some(BoundBinary::same_input(
                lhs.type_,
                result,
                typeid!(Type::Boolean),
            ))
        }
        (
            Type::Integer(_),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_input(
            lhs.type_,
            result,
            typeid!(Type::Boolean),
        )),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Integer(_),
        ) => Some(BoundBinary::same_input(
            rhs.type_,
            result,
            typeid!(Type::Boolean),
        )),
        (_, BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals, _)
            if binder.project.types.noneable_base_type(lhs.type_).is_some()
                || binder.project.types.noneable_base_type(rhs.type_).is_some() => {
            let (noneable_type, other) = binder
                .project
                .types
                .noneable_base_type(lhs.type_)
                .map(|_| (lhs.type_, rhs.type_))
                .or_else(|| binder
                    .project
                    .types
                    .noneable_base_type(rhs.type_)
                    .map(|_| (rhs.type_, lhs.type_))
                ).expect("No noneable types found!");

            if binder.project.types.can_be_converted(other, noneable_type) {
                let noneable_base_type = binder.project.types.noneable_base_type(noneable_type).unwrap();
                let noneable_type = binder.project.types.look_up_type_by_name("Noneable").expect("Failed to load type Noneable from stdlib.").unwrap_generic_type_id();
                let noneable_type = bind_generic_struct_type_for_types(operator_token.location, noneable_type, &[noneable_base_type], binder);
                Some(BoundBinary::same_input(noneable_type, result, typeid!(Type::Boolean)))
            } else {
                binder.diagnostic_bag.report_no_binary_operator(location, lhs.type_, operator_token.location, rhs.type_);
                None
            }
        }
        (
            Type::Pointer | Type::PointerOf(_),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::None,
        )
        | (
            Type::None,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Pointer | Type::PointerOf(_),
        ) => Some(BoundBinary::same_input(
            typeid!(Type::Pointer),
            result,
            typeid!(Type::Boolean),
        )),
        (Type::Struct(s), BoundBinaryOperator::ArithmeticAddition, _) if s.function_table.add_function.is_some() =>
        {
            if let Some(f) = &s.function_table.add_function {
                let f = binder.project.types[f.function_type].as_function_type().unwrap();
                Some(BoundBinary::new(lhs.type_, result, f.parameter_types[0], f.return_type))
            } else {
                binder.diagnostic_bag.report_no_binary_operator(
                    location,
                    lhs.type_,
                    operator_token.location,
                    rhs.type_,
                );
                None
            }
        }
        (
            Type::Integer(lhs_integer_type),
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer(rhs_integer_type),
        ) if lhs_integer_type == rhs_integer_type => {
            Some(BoundBinary::same_output(result, lhs.type_))
        }
        (
            Type::Integer(lhs_integer_type),
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer(rhs_integer_type),
        ) if lhs_integer_type.equals_ignoring_sign(rhs_integer_type) => {
            Some(BoundBinary::same_output(
                result,
                binder
                    .project
                    .types
                    .look_up_type(&Type::Integer(lhs_integer_type.to_signed()))
                    .expect("This type has been looked up. It should already exist."),
            ))
        }
        (
            Type::Integer(_),
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_output(result, lhs.type_)),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::Integer(_),
        ) => Some(BoundBinary::same_output(result, rhs.type_)),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::ArithmeticAddition
            | BoundBinaryOperator::ArithmeticSubtraction
            | BoundBinaryOperator::ArithmeticMultiplication
            | BoundBinaryOperator::ArithmeticDivision,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_output(
            result,
            typeid!(Type::IntegerLiteral),
        )),
        (
            Type::Integer(_),
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer(_) | Type::IntegerLiteral,
        ) => Some(BoundBinary::same_input(
            lhs.type_,
            result,
            typeid!(Type::Boolean),
        )),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::Integer(_),
        ) => Some(BoundBinary::same_input(
            rhs.type_,
            result,
            typeid!(Type::Boolean),
        )),
        (
            Type::IntegerLiteral,
            BoundBinaryOperator::LessThan
            | BoundBinaryOperator::GreaterThan
            | BoundBinaryOperator::LessThanEquals
            | BoundBinaryOperator::GreaterThanEquals,
            Type::IntegerLiteral,
        ) => Some(BoundBinary::same_input(
            typeid!(Type::IntegerLiteral),
            result,
            typeid!(Type::Boolean),
        )),
        (
            Type::Boolean,
            BoundBinaryOperator::LogicalAnd | BoundBinaryOperator::LogicalOr,
            Type::Boolean,
        ) => Some(BoundBinary::same_output(result, typeid!(Type::Boolean))),
        (Type::String, BoundBinaryOperator::ArithmeticAddition, _)
        | (_, BoundBinaryOperator::ArithmeticAddition, Type::String)
        // | (Type::String, BoundBinaryOperator::ArithmeticAddition, Type::String)
         => Some(
            // Currently a call to to$string will be emitted. And for that call
            // both sides need to be of type any. There is an optimization here,
            // where there might be two versions of StringConcat and only one of
            // those works with to$string.
            //
            // BoundBinary::same_output(BoundBinaryOperator::StringConcat, Type::String),
            BoundBinary::same_input(
                typeid!(Type::Any),
                BoundBinaryOperator::StringConcat,
                typeid!(Type::String),
            ),
        ),
        (Type::Struct(_), BoundBinaryOperator::NoneableOrValue, _) => {
            if let Some(base_type) = binder.project.types.noneable_base_type(lhs.type_) {
                if binder.project.types.can_be_converted(rhs.type_, base_type) {
                    Some(BoundBinary::same_input(lhs.type_, result, base_type))
                } else {
                    binder
                        .diagnostic_bag
                        .report_cannot_convert(location, rhs.type_, base_type);
                    None
                }
            } else {
                binder.diagnostic_bag.report_no_binary_operator(location, lhs.type_, operator_token.location, rhs.type_);
                None
            }
        }
        // Special case, where none ?? value is used. This could be optimized
        // away later.
        (Type::None, BoundBinaryOperator::NoneableOrValue, _) => {
            if let Some(noneable_base_type) = binder.project.types.noneable_base_type(rhs.type_) {
                binder.diagnostic_bag.report_cannot_convert(location, rhs.type_, noneable_base_type);
                None
            } else {
                let noneable_type = binder.project.types.look_up_type_by_name("Noneable").expect("Failed to look up type Noneable from stdlib.").unwrap_generic_type_id();
                let input_type = bind_generic_struct_type_for_types(location, noneable_type, &[rhs.type_], binder);
                Some(BoundBinary::same_input(input_type, result, rhs.type_))
            }
        }
        (
            Type::Integer(IntegerType::Signed64) | Type::IntegerLiteral,
            BoundBinaryOperator::Range,
            Type::Integer(IntegerType::Signed64) | Type::IntegerLiteral,
        ) => {
            let range_type = binder
                .project
                .types
                .look_up_type_by_name("Range")
                .expect("Failed to load type Range from stdlib").unwrap_type_id();
            Some(BoundBinary::same_input(
                typeid!(Type::Integer(IntegerType::Signed64)),
                result,
                range_type,
            ))
        }
        (
            Type::Integer(IntegerType::Unsigned64) | Type::IntegerLiteral,
            BoundBinaryOperator::Range,
            Type::Integer(IntegerType::Unsigned64) | Type::IntegerLiteral,
        ) => {
            let range_type = binder
                .project
                .types
                .look_up_type_by_name("UnsignedRange")
                .expect("Failed to load type UnsignedRange from stdlib").unwrap_type_id();
            Some(BoundBinary::same_input(
                typeid!(Type::Integer(IntegerType::Unsigned64)),
                result,
                range_type,
            ))
        }
        (Type::Error, _, _) | (_, _, Type::Error) => None,
        _ => {
            binder.diagnostic_bag.report_no_binary_operator(
                location,
                lhs.type_,
                operator_token.location,
                rhs.type_,
            );
            None
        }
    }
}

fn bind_unary<'a, 'b>(
    span: TextLocation,
    unary: UnaryNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let operand = bind_node(*unary.operand, binder);
    match bind_unary_operator(span, &operand, unary.operator_token, binder) {
        Some((operator_token, type_)) => BoundNode::unary(span, operator_token, operand, type_),
        None => BoundNode::error(span),
    }
}

fn bind_unary_operator<'a, 'b>(
    span: TextLocation,
    operand: &BoundNode,
    operator_token: SyntaxToken,
    binder: &mut BindingState<'b>,
) -> Option<(BoundUnaryOperator, TypeId)> {
    let result = match &operator_token.kind {
        SyntaxTokenKind::Plus => BoundUnaryOperator::ArithmeticIdentity,
        SyntaxTokenKind::Minus => BoundUnaryOperator::ArithmeticNegate,
        SyntaxTokenKind::Bang => BoundUnaryOperator::LogicalNegation,
        _ => unreachable!(),
    };
    let operand_type = &binder.project.types[operand.type_];
    match operand_type {
        Type::Integer(integer_type) if result != BoundUnaryOperator::LogicalNegation => {
            if result == BoundUnaryOperator::ArithmeticNegate {
                Some((
                    result,
                    binder
                        .project
                        .types
                        .look_up_type(&Type::Integer(integer_type.to_signed()))
                        .expect("This is a core type and should always exist."),
                ))
            } else {
                Some((result, operand.type_))
            }
        }
        // FIXME: Is there a conversion of the operand needed?
        Type::IntegerLiteral if result != BoundUnaryOperator::LogicalNegation => {
            if result == BoundUnaryOperator::ArithmeticNegate {
                Some((result, typeid!(Type::Integer(IntegerType::Signed64))))
            } else {
                Some((result, typeid!(Type::IntegerLiteral)))
            }
        }
        Type::Boolean if result == BoundUnaryOperator::LogicalNegation => {
            Some((result, typeid!(Type::Boolean)))
        }
        _ if binder
            .project
            .types
            .noneable_base_type(operand.type_)
            .is_some()
            && result == BoundUnaryOperator::LogicalNegation =>
        {
            Some((result, typeid!(Type::Boolean)))
        }
        Type::Error => None,
        _ => {
            binder.diagnostic_bag.report_no_unary_operator(
                span,
                operator_token.location,
                operand.type_,
            );
            None
        }
    }
}

fn bind_parenthesized<'a, 'b>(
    _: TextLocation,
    parenthesized: ParenthesizedNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    bind_node(*parenthesized.expression, binder)
}

fn function_type(type_: &Type) -> FunctionType<TypeId> {
    match type_ {
        Type::SystemCall(kind) => FunctionType::system_call(*kind),
        Type::Function(result) => result.clone(),
        Type::Closure(closure) => closure.base_function_type.clone(),
        Type::Error => FunctionType::error(),
        _ => FunctionType::error(),
    }
}

fn bind_function_call<'a, 'b>(
    location: TextLocation,
    function_call: FunctionCallNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let argument_span = function_call.argument_span();
    let function = bind_node(*function_call.base, binder);
    let mut arguments = vec![];
    let mut more_arguments = vec![];
    let function = if let BoundNodeKind::Closure(mut closure) = function.kind.clone() {
        more_arguments.append(&mut closure.arguments);
        let type_ = if let Type::Closure(closure_type) = &binder.project.types[function.type_] {
            binder
                .project
                .types
                .look_up_or_add_type(Type::Function(closure_type.base_function_type.clone()))
        } else {
            unreachable!("There is a closure, which is not of type closure???");
        };
        match closure.function {
            typing::FunctionKind::FunctionId(id) => {
                BoundNode::variable(function.location, id, type_)
            }
            typing::FunctionKind::SystemCall(kind) => {
                BoundNode::literal(location, Value::SystemCall(kind))
            }
            typing::FunctionKind::LabelReference(label_reference) => {
                BoundNode::label_reference(location, label_reference, type_)
            }
            typing::FunctionKind::VtableIndex(_) => {
                // We actually do note have more arguments, since they are only
                // accessed during runtime!
                more_arguments.clear();
                function
            }
        }
    } else {
        function
    };
    if function.type_ == typeid!(Type::Error) {
        return BoundNode::error(location);
    }
    let function_type = function_type(&binder.project.types[function.type_]);
    if function_type.is_error() {
        binder
            .diagnostic_bag
            .report_not_a_function(location, function.type_);
        return BoundNode::error(location);
    }
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
            function.type_,
            binder,
        ));
    }
    arguments.append(&mut more_arguments);
    match &binder.project.types[function.type_] {
        Type::Error => BoundNode::error(location),
        &Type::SystemCall(system_call) => {
            BoundNode::system_call(location, system_call, arguments, function_type.return_type)
        }
        Type::Function(_) => BoundNode::function_call(
            location,
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
                BoundNode::system_call(location, system_call, arguments, function_type.return_type)
            } else {
                BoundNode::function_call(
                    location,
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
    location: TextLocation,
    array_index: ArrayIndexNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let index = bind_node(*array_index.index, binder);
    let base_span = array_index.base.location;
    let base = bind_node(*array_index.base, binder);
    let (type_, index) = match binder.project.types[base.type_].clone() {
        Type::PointerOf(base_type) => (
            base_type,
            bind_conversion(
                index,
                typeid!(Type::Integer(IntegerType::Unsigned64)),
                binder,
            ),
        ),
        Type::Struct(struct_type) => match &struct_type.function_table.get_function {
            Some(get_function) => {
                let index_type = binder.project.types[get_function.function_type]
                    .as_function_type()
                    .unwrap()
                    .parameter_types[0];
                let index = bind_conversion(index, index_type, binder);
                let function_base = BoundNode::label_reference(
                    location,
                    get_function.function_label as _,
                    get_function.function_type,
                );
                return BoundNode::function_call(
                    location,
                    function_base,
                    vec![index, base],
                    false,
                    binder.project.types[get_function.function_type]
                        .as_function_type()
                        .unwrap()
                        .return_type,
                );
            }
            None => {
                if base.type_ != typeid!(Type::Error) {
                    binder
                        .diagnostic_bag
                        .report_cannot_index_get(base_span, base.type_);
                }
                (typeid!(Type::Error), index)
            }
        },
        _ => {
            if base.type_ != typeid!(Type::Error) {
                binder
                    .diagnostic_bag
                    .report_cannot_index_get(base_span, base.type_);
            }
            (typeid!(Type::Error), index)
        }
    };
    BoundNode::array_index(location, base, index, type_)
}

fn bind_array_index_for_assignment<'a, 'b>(
    span: TextLocation,
    array_index: ArrayIndexNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let index = bind_node(*array_index.index, binder);
    let base_span = array_index.base.location;
    let base = bind_node(*array_index.base, binder);
    let base_type = binder.project.types[base.type_].clone();
    let (type_, index) = match &base_type {
        Type::PointerOf(base_type) => (
            *base_type,
            bind_conversion(
                index,
                typeid!(Type::Integer(IntegerType::Unsigned64)),
                binder,
            ),
        ),
        Type::Struct(struct_type) => match &struct_type.function_table.set_function {
            Some(set_function) => {
                let type_ = set_function.function_type;
                let index_type = binder.project.types[type_]
                    .as_function_type()
                    .unwrap()
                    .parameter_types[0];
                let index = bind_conversion(index, index_type, binder);
                let type_ = binder.project.types.look_up_or_add_type(Type::closure(
                    binder.project.types[type_].as_function_type().unwrap(),
                    // vec![],
                    vec![base.type_, index.type_],
                ));
                return BoundNode::closure_label(
                    span,
                    vec![base, index],
                    set_function.function_label as _,
                    type_,
                );
            }
            None => {
                if base.type_ != typeid!(Type::Error) {
                    binder
                        .diagnostic_bag
                        .report_cannot_index_set(base_span, base.type_);
                }
                (typeid!(Type::Error), index)
            }
        },
        _ => {
            if base.type_ != typeid!(Type::Error) {
                binder
                    .diagnostic_bag
                    .report_cannot_index_set(base_span, base.type_);
            }
            (typeid!(Type::Error), index)
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_field_access<'a, 'b>(
    location: TextLocation,
    field_access: FieldAccessNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let base_location = field_access.base.location;
    let field = field_access.field;
    let base = bind_node(*field_access.base, binder);
    match binder.project.types[base.type_].clone() {
        Type::Error => base,
        Type::Struct(_) => field_access_in_struct(field, base.type_, base, binder, location),
        Type::Library(index) => {
            let library = &binder.libraries[index];
            let field_name = binder.lexeme(field);
            if let Some(function) = library.look_up_function_by_name(field_name) {
                BoundNode::label_reference(
                    location,
                    function.function_label as _,
                    function.function_type,
                )
            } else if let Some(enum_type) =
                library.look_up_enum_by_name(field_name, &binder.project.types)
            {
                BoundNode::field_access(location, base, 0, enum_type)
            } else {
                dbg!(&library);
                binder.diagnostic_bag.report_no_field_named_on_type(
                    location,
                    field.location,
                    binder
                        .project
                        .types
                        .look_up_or_add_type(Type::Library(index)),
                );
                BoundNode::error(location)
            }
        }
        Type::Enum(_name, values) => {
            let value = values
                .iter()
                .enumerate()
                .find(|e| e.1 == binder.lexeme(field));
            match value {
                Some((literal_value, _)) => {
                    BoundNode::literal(location, Value::EnumValue(literal_value, base.type_))
                }
                None => {
                    binder.diagnostic_bag.report_no_field_named_on_type(
                        location,
                        field.location,
                        base.type_,
                    );
                    BoundNode::error(location)
                }
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
        | Type::GenericType(_, _)
        | Type::PointerOf(_)
        | Type::TypeId => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base_location, base.type_);
            BoundNode::error(location)
        }
        Type::String => {
            if binder.lexeme(field) == "length" {
                let base = bind_conversion(base, typeid!(Type::Any), binder);
                let function_type = FunctionType::system_call(SystemCallKind::ArrayLength);
                let type_ = binder
                    .project
                    .types
                    .look_up_or_add_type(Type::closure(function_type, vec![base.type_]));
                BoundNode::system_call_closure(
                    location,
                    vec![base],
                    SystemCallKind::ArrayLength,
                    type_,
                )
            } else if binder.lexeme(field) == "bytes" {
                let type_ = binder
                    .project
                    .types
                    .look_up_or_add_type(Type::PointerOf(typeid!(Type::Integer(
                        IntegerType::Unsigned8
                    ))));
                BoundNode::binary(
                    location,
                    base,
                    BoundBinaryOperator::ArithmeticAddition,
                    BoundNode::literal(location, Value::Integer(8)),
                    type_,
                )
            } else {
                binder.diagnostic_bag.report_no_field_named_on_type(
                    location,
                    field.location,
                    base.type_,
                );
                BoundNode::error(location)
            }
        }
        Type::StructPlaceholder(it) => {
            crate::debug::print_location_as_source_text(
                location,
                &binder.project.source_text_collection,
            );
            unreachable!(
                "Internal Compiler error, struct placeholder '{}' should already be resolved.",
                it.name
            )
        }
    }
}

fn field_access_in_struct<'a, 'b>(
    field: SyntaxToken,
    id: TypeId,
    base: BoundNode,
    binder: &mut BindingState<'b>,
    span: TextLocation,
) -> BoundNode {
    let field_name = binder.lexeme(field).to_string();
    let bound_struct_type = binder.project.types[id].as_struct_type().unwrap();
    let is_abstract = bound_struct_type.is_abstract;
    // Normally we turn function calls on abstract structs into fat pointers,
    // since the actual call gets decided at runtime, but the base variable is
    // actually of type abstract struct, so we need to call into it directly.
    let is_base_variable = binder.is_struct_function
        && &binder.project.source_text_collection[base.location] == "base";
    let field_symbol = if binder.is_struct_function {
        bound_struct_type.member_fields_first(&field_name, &binder.project.types)
    } else {
        bound_struct_type.member_functions_first(&field_name, &binder.project.types)
    };
    if let Some(field) = field_symbol {
        if let Type::Function(function_type) = binder.project.types[field.type_].clone() {
            let index = bound_struct_type.functions.iter().position(|f| f == field);
            let mut current_type = id;
            loop {
                let function_name = format!(
                    "{}::{}",
                    binder.project.types.name_of_type_id(current_type),
                    field_name
                );
                let mut function_type = function_type.clone();
                // The former this type is now an explicit argument in the closure.
                function_type.this_type = None;
                let type_ = binder
                    .project
                    .types
                    .look_up_or_add_type(Type::closure(function_type, vec![base.type_]));
                let variable = binder.look_up_variable_or_constant_by_name(&function_name);
                break match variable.kind {
                    VariableOrConstantKind::None => {
                        match binder.project.types[current_type]
                            .as_struct_type()
                            .unwrap()
                            .parent
                        {
                            Some(it) => {
                                current_type = it;
                                continue;
                            }
                            // Probably somebody already reported an error here!
                            None => {
                                assert!(binder.diagnostic_bag.has_errors(), "{field_name}");
                                BoundNode::error(span)
                            }
                        }
                    }
                    VariableOrConstantKind::Variable(variable_id) => {
                        BoundNode::closure(span, vec![base], variable_id, type_)
                    }
                    VariableOrConstantKind::Constant(value) => {
                        if is_abstract && !is_base_variable {
                            BoundNode::closure_abstract(span, vec![base], index.expect("We found a valid variable kind, so we expect to also have a field on the current type! (Maybe not?)"), type_)
                        } else {
                            let label_index = value.as_label_pointer().unwrap().0;
                            BoundNode::closure_label(span, vec![base], label_index, type_)
                        }
                    }
                };
            }
        } else {
            BoundNode::field_access(
                span,
                base,
                field.offset_or_address.unwrap_offset() as _,
                field.type_.clone(),
            )
        }
    } else {
        for f in &bound_struct_type.functions {
            println!(
                "  - func {}{}",
                f.name,
                binder.project.types.name_of_type_id_debug(f.type_)
            );
        }
        binder
            .diagnostic_bag
            .report_no_field_named_on_struct(field.location, field.location, id);
        BoundNode::error(span)
    }
}

fn bind_field_access_for_assignment<'a>(
    span: TextLocation,
    field_access: FieldAccessNodeKind,
    binder: &mut BindingState<'_>,
) -> BoundNode {
    let base_is_this = if let SyntaxNodeKind::Variable(identifier) = &field_access.base.kind {
        binder.lexeme(identifier.token) == "this"
    } else {
        false
    };
    let base = bind_node(*field_access.base, binder);
    let field = field_access.field;
    match &binder.project.types[base.type_] {
        Type::Error => base,
        Type::Any
        | Type::Void
        | Type::Integer(_)
        | Type::IntegerLiteral
        | Type::Boolean
        | Type::None
        | Type::SystemCall(_)
        | Type::Function(_)
        | Type::Pointer
        | Type::PointerOf(_)
        | Type::GenericType(_, _)
        | Type::Closure(_)
        | Type::TypeId => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base.location, base.type_);
            BoundNode::error(span)
        }
        Type::String | Type::Library(_) | Type::Enum(..) => {
            binder.diagnostic_bag.report_cannot_assign_to(span);
            BoundNode::error(span)
        }
        Type::Struct(_) => {
            bind_field_in_struct_for_assignment(field, binder, base.type_, span, base_is_this, base)
        }
        Type::StructPlaceholder(it) => unreachable!(
            "Internal Compiler error, struct placeholder '{}' should already be resolved.",
            it.name
        ),
    }
}

fn bind_field_in_struct_for_assignment<'a>(
    field: SyntaxToken,
    binder: &mut BindingState<'_>,
    id: TypeId,
    location: TextLocation,
    base_is_this: bool,
    base: BoundNode,
) -> BoundNode {
    let field_name = binder.lexeme(field).to_owned();
    let bound_struct_type = binder.project.types[id].as_struct_type().unwrap();
    let field_symbol = if binder.is_struct_function {
        bound_struct_type.member_fields_first(&field_name, &binder.project.types)
    } else {
        bound_struct_type.member_functions_first(&field_name, &binder.project.types)
    };
    if let Some(field) = field_symbol {
        if field.is_read_only {
            binder.diagnostic_bag.report_cannot_assign_to(location);
            BoundNode::error(location)
        } else {
            if base_is_this && !binder.assigned_fields.contains(&field_name) {
                binder.assigned_fields.push(field_name);
            }
            BoundNode::field_access(
                location,
                base,
                field.offset_or_address.unwrap_offset() as _,
                field.type_.clone(),
            )
        }
    } else {
        binder
            .diagnostic_bag
            .report_no_field_named_on_struct(field.location, field.location, id);
        BoundNode::error(location)
    }
}

fn bind_arguments_for_function<'a, 'b>(
    span: TextLocation,
    arguments: Vec<SyntaxNode>,
    function_type: TypeId,
    binder: &mut BindingState<'b>,
) -> Vec<BoundNode> {
    let mut result = vec![];
    let parameter_types = binder.project.types[function_type]
        .as_function_type()
        .unwrap()
        .parameter_types
        .clone();
    if parameter_types.len() != arguments.len() {
        binder.diagnostic_bag.report_unexpected_argument_count(
            span,
            arguments.len(),
            parameter_types.len(),
        );
    }
    for (argument, parameter_type) in arguments.into_iter().zip(parameter_types.iter().copied()) {
        let old_expected_type = binder.expected_type.take();
        binder.expected_type = Some(parameter_type.clone());
        let mut argument = bind_node(argument, binder);
        binder.expected_type = old_expected_type;
        if argument.type_ == typeid!(Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.location);
            result.push(BoundNode::error(argument.location));
            continue;
        }
        argument = bind_conversion(argument, parameter_type, binder);
        result.push(argument);
    }
    result
}

// fn bind_arguments_for_generic_function<'a, 'b>(
//     span: TextLocation,
//     arguments: Vec<SyntaxNode>,
//     label: usize,
//     function_type: &FunctionType<GenericTypeId>,
//     function_labels_map: HashMap<usize, usize>,
//     binder: &mut BindingState<'b>,
// ) -> (Vec<BoundNode>, usize) {
//     let mut result = vec![];
//     let mut changed_label = 0;
//     if function_type.parameter_types.len() != arguments.len() {
//         binder.diagnostic_bag.report_unexpected_argument_count(
//             span,
//             arguments.len(),
//             function_type.parameter_types.len(),
//         );
//     }
//     for (argument, parameter_type) in arguments
//         .into_iter()
//         .zip(function_type.parameter_types.iter().copied())
//     {
//         let mut argument = bind_node(argument, binder);
//         if argument.type_ == typeid!(Type::Void) {
//             binder
//                 .diagnostic_bag
//                 .report_invalid_void_expression(argument.span);
//             result.push(BoundNode::error(argument.span));
//             continue;
//         }
//         let parameter_type = if parameter_type == typeid!(Type::GenericType) {
//             if binder.generic_type.is_none() {
//                 binder.generic_type = Some(argument.type_.clone());
//                 let mut generic_function = binder
//                     .find_generic_function_by_label(label)
//                     .unwrap()
//                     .clone();
//                 changed_label = bind_generic_function_for_type(
//                     &mut generic_function,
//                     function_type.this_type.is_some(),
//                     argument.type_,
//                     // Note: this may be wrong if a $ function is compiled. Then
//                     // again, this should not happen, since this gets called by
//                     // function calls and you cannot call al $ function.
//                     None,
//                     function_labels_map.clone(),
//                     binder,
//                 );
//             }
//             binder.generic_type.clone().unwrap()
//         } else {
//             parameter_type.clone()
//         };
//         argument = bind_conversion(argument, parameter_type, binder);
//         result.push(argument);
//     }
//     binder.generic_type = None;
//     (result, changed_label)
// }

fn bind_arguments_for_generic_constructor_on_struct<'a, 'b>(
    location: TextLocation,
    arguments: Vec<SyntaxNode>,
    label: usize,
    function_type: TypeId,
    struct_id: GenericTypeId,
    binder: &mut BindingState<'b>,
) -> (Vec<BoundNode>, usize, TypeId) {
    let mut result = vec![];
    let parameter_types = binder.project.types[function_type]
        .as_function_type()
        .expect("FunctionType")
        .parameter_types
        .clone();
    if parameter_types.len() != arguments.len() {
        binder.diagnostic_bag.report_unexpected_argument_count(
            location,
            arguments.len(),
            parameter_types.len(),
        );
    }

    let generic_struct_parameters =
        if let GenericType::Struct(it) = &binder.project.types[struct_id] {
            it.generic_parameters.clone()
        } else {
            unreachable!("expected generic struct!")
        };
    binder.generic_types = (0..generic_struct_parameters.len())
        .map(|_| typeid!(Type::Void))
        .collect();

    if let Some(expected_type) = binder
        .expected_type
        .map(|t| binder.project.types[t].as_struct_type())
        .flatten()
    {
        assert_eq!(
            binder.generic_types.len(),
            expected_type.applied_types.len()
        );
        binder.generic_types = expected_type.applied_types.clone();
    }

    for (argument, parameter_type) in arguments.into_iter().zip(parameter_types.iter().copied()) {
        let mut argument = bind_node(argument, binder);
        if argument.type_ == typeid!(Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.location);
            result.push(BoundNode::error(argument.location));
            continue;
        }
        let parameter_type =
            if let Type::GenericType(index, _name) = &binder.project.types[parameter_type] {
                if binder.generic_types[*index] == typeid!(Type::Void) {
                    binder.generic_types[*index] = argument.type_;
                    argument.type_
                } else {
                    parameter_type
                }
            } else {
                parameter_type
            };

        argument = bind_conversion(argument, parameter_type, binder);
        result.push(argument);
    }

    if binder
        .generic_types
        .iter()
        .all(|t| *t != typeid!(Type::Void))
    {
        let mut applied_types = vec![typeid!(Type::Void); binder.generic_types.len()];
        binder.generic_types.swap_with_slice(&mut applied_types);
        binder.generic_types.clear();
        let type_id =
            bind_generic_struct_type_for_types(location, struct_id, &applied_types, binder);
        let struct_type = binder.project.types[type_id].as_struct_type().unwrap();
        (
            result,
            struct_type
                .function_table
                .constructor_function
                .as_ref()
                .unwrap()
                .function_label as _,
            type_id,
        )
    } else {
        binder.diagnostic_bag.report_underspecified_generic_struct(
            location,
            binder
                .project
                .types
                .name_of_generic_type_id(struct_id)
                .as_ref(),
        );
        (
            result,
            label,
            binder.project.types.to_fake_type_id(struct_id),
        )
    }
}

// TODO: Move this to after the binding.
fn bind_generic_function_for_type(
    location: TextLocation,
    generic_function: &GenericFunction,
    label: usize,
    function_label_map: &HashMap<usize, usize>,
    types: &[TypeId],
    this_type: TypeId,
    name: &str,
    binder: &mut BindingState<'_>,
) -> TypeId {
    let replace: Vec<_> = (binder.project.types[generic_function.function_type]
        .generic_parameters()
        .to_vec()
        .into_iter()
        .enumerate())
    .map(|(i, n)| {
        binder
            .project
            .types
            .look_up_or_add_type(Type::GenericType(i, n))
    })
    .collect();
    let function_type = binder.project.types[generic_function.function_type]
        .as_function_type()
        .unwrap();
    let parameter_types: Vec<_> = function_type
        .parameter_types
        .iter()
        .copied()
        .map(|t| binder.replace_in_type_for_types(location, t, &replace, &types))
        .collect();
    let return_type =
        binder.replace_in_type_for_types(location, function_type.return_type, &replace, &types);
    let parameters = (0..parameter_types.len() as u64
        + if this_type != typeid!(Type::Void) {
            1
        } else {
            0
        })
        .collect();
    let function_type = binder
        .project
        .types
        .look_up_or_add_type(Type::Function(FunctionType {
            parameter_types,
            this_type: Some(this_type),
            return_type,
            system_call_kind: None,
            name: Some(name.to_owned().into()),
            is_generic: false,
        }));
    binder.register_constant(name.to_owned(), Value::LabelPointer(label, function_type));
    let mut body = function_call_replacer::replace_function_calls(
        generic_function.body.clone(),
        function_label_map,
    );
    // NOTE: You might think, this is not necessary, but the types are used for
    // any conversions, like in print or toString. So the types of the body need
    // to be updated.
    // We also update the labels here, because otherwise we have labels point to
    // multiple places in the code.
    let label_relocations: HashMap<_, _> = generic_function
        .labels
        .iter()
        .copied()
        .map(|l| (l, binder.generate_label()))
        .collect();
    body.for_each_child_mut(&mut |n| {
        n.type_ = binder.replace_in_type_for_types(location, n.type_, &replace, &types);
        match &mut n.kind {
            BoundNodeKind::Label(it) if label_relocations.contains_key(it) => {
                *it = label_relocations[it]
            }
            BoundNodeKind::LabelReference(it) if label_relocations.contains_key(it) => {
                *it = label_relocations[it]
            }
            BoundNodeKind::Conversion(it) => {
                it.type_ = binder.replace_in_type_for_types(location, it.type_, &replace, &types);
            }
            BoundNodeKind::ConstructorCall(it) => {
                it.base_type =
                    binder.replace_in_type_for_types(location, it.base_type, &replace, &types);
                let had_function = it.function.is_some();
                it.function = binder.project.types[it.base_type]
                    .as_struct_type()
                    .map(|s| {
                        s.function_table
                            .constructor_function
                            .as_ref()
                            .map(|f| f.function_label)
                    })
                    .unwrap_or_else(|| {
                        if let Type::StructPlaceholder(it) = &binder.project.types[it.base_type] {
                            it.function_table.constructor_function.map(|it| it as u64)
                        } else {
                            unreachable!("Constructor calls only work for structs!")
                        }
                    });
                assert_eq!(had_function, it.function.is_some());
            }
            _ => {}
        }
    });
    let function = BoundNode::function_declaration(
        generic_function.body.location,
        label,
        false,
        body,
        parameters,
        None,
        function_type,
    );
    binder.bound_functions.push(function);
    function_type
}

fn bind_conversion<'a>(base: BoundNode, type_: TypeId, binder: &mut BindingState<'_>) -> BoundNode {
    if base.type_ == type_ {
        base
    } else if binder.project.types.can_be_converted(base.type_, type_) {
        BoundNode::conversion(base.location, base, type_)
    } else if type_ != typeid!(Type::Error) && base.type_ != typeid!(Type::Error) {
        binder
            .diagnostic_bag
            .report_cannot_convert(base.location, base.type_, type_);
        BoundNode::error(base.location)
    } else {
        BoundNode::error(base.location)
    }
}

fn bind_condition_conversion<'a>(
    base: BoundNode,
    binder: &mut BindingState<'_>,
) -> (BoundNode, SafeNodeInCondition) {
    match &binder.project.types[base.type_] {
        Type::Error => (base, SafeNodeInCondition::None),
        Type::None => (base, SafeNodeInCondition::None),
        Type::Boolean => {
            let safe_node = register_contained_safe_nodes(&base, binder);
            (base, safe_node)
        }
        Type::Struct(_) => {
            if let Some(noneable_base_type) = binder.project.types.noneable_base_type(base.type_) {
                (
                    bind_conversion(base.clone(), typeid!(Type::Boolean), binder),
                    SafeNodeInCondition::IfBody(base, noneable_base_type),
                )
            } else {
                (
                    bind_conversion(base, typeid!(Type::Boolean), binder),
                    SafeNodeInCondition::None,
                )
            }
        }
        Type::Void
        | Type::Any
        | Type::Integer(_)
        | Type::IntegerLiteral
        | Type::SystemCall(_)
        | Type::String
        | Type::Function(_)
        | Type::Closure(_)
        | Type::Pointer
        | Type::PointerOf(_)
        | Type::Enum(..)
        | Type::TypeId => (
            bind_conversion(base, typeid!(Type::Boolean), binder),
            SafeNodeInCondition::None,
        ),
        Type::Library(_) | Type::GenericType(_, _) => unimplemented!(),
        Type::StructPlaceholder(it) => unreachable!(
            "Internal Compiler error, struct placeholder '{}' should already be resolved.",
            it.name
        ),
    }
}

fn register_contained_safe_nodes(
    base: &BoundNode,
    binder: &mut BindingState<'_>,
) -> SafeNodeInCondition {
    match &base.kind {
        BoundNodeKind::UnaryExpression(unary)
            if unary.operator_token == BoundUnaryOperator::LogicalNegation =>
        {
            match &binder.project.types[unary.operand.type_] {
                Type::Boolean => register_contained_safe_nodes(&unary.operand, binder).negate(),
                _ => {
                    if let Some(noneable_base_type) =
                        binder.project.types.noneable_base_type(unary.operand.type_)
                    {
                        SafeNodeInCondition::ElseBody(*unary.operand.clone(), noneable_base_type)
                    } else {
                        SafeNodeInCondition::None
                    }
                }
            }
        }
        BoundNodeKind::BinaryExpression(binary)
            if binary.operator_token == BoundBinaryOperator::Equals
                && binder
                    .project
                    .types
                    .noneable_base_type(binary.lhs.type_)
                    .is_some()
                && binder
                    .project
                    .types
                    .noneable_base_type(binary.rhs.type_)
                    .is_some() =>
        {
            match (&binary.lhs.constant_value, &binary.rhs.constant_value) {
                (None, Some(constant)) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.lhs, binder).negate()
                    } else {
                        register_contained_safe_nodes(&binary.lhs, binder)
                    }
                }
                (Some(constant), None) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.rhs, binder).negate()
                    } else {
                        register_contained_safe_nodes(&binary.rhs, binder)
                    }
                }
                _ => SafeNodeInCondition::None,
            }
        }
        BoundNodeKind::BinaryExpression(binary)
            if binary.operator_token == BoundBinaryOperator::NotEquals
                && binder
                    .project
                    .types
                    .noneable_base_type(binary.lhs.type_)
                    .is_some()
                && binder
                    .project
                    .types
                    .noneable_base_type(binary.rhs.type_)
                    .is_some() =>
        {
            match (&binary.lhs.constant_value, &binary.rhs.constant_value) {
                (None, Some(constant)) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.lhs, binder)
                    } else {
                        SafeNodeInCondition::None
                    }
                }
                (Some(constant), None) => {
                    if constant.value == Value::None {
                        register_contained_safe_nodes(&binary.rhs, binder)
                    } else {
                        SafeNodeInCondition::None
                    }
                }
                _ => SafeNodeInCondition::None,
            }
        }
        BoundNodeKind::FunctionCall(fn_call) => {
            if fn_call.is_eq_function {
                assert_eq!(fn_call.arguments.len(), 2);
                let lhs = &fn_call.arguments[0];
                let rhs = &fn_call.arguments[1];
                match (&lhs.constant_value, &rhs.constant_value) {
                    (None, Some(constant)) => {
                        if constant.value == Value::None {
                            register_contained_safe_nodes(&lhs, binder).negate()
                        } else {
                            register_contained_safe_nodes(&lhs, binder)
                        }
                    }
                    (Some(constant), None) => {
                        if constant.value == Value::None {
                            register_contained_safe_nodes(&rhs, binder).negate()
                        } else {
                            register_contained_safe_nodes(&rhs, binder)
                        }
                    }
                    _ => SafeNodeInCondition::None,
                }
            } else {
                if let Some(noneable_base_type) =
                    binder.project.types.noneable_base_type(base.type_)
                {
                    SafeNodeInCondition::IfBody(base.clone(), noneable_base_type)
                } else {
                    SafeNodeInCondition::None
                }
            }
        }
        _ => {
            if let Some(noneable_base_type) = binder.project.types.noneable_base_type(base.type_) {
                SafeNodeInCondition::IfBody(base.clone(), noneable_base_type)
            } else {
                SafeNodeInCondition::None
            }
        }
    }
}

fn bind_for_statement<'a, 'b>(
    span: TextLocation,
    for_statement: ForStatementNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let variable_count = binder.variable_table.len();
    let collection = bind_node(*for_statement.collection, binder);
    let struct_type = if let Type::Struct(struct_type) = &binder.project.types[collection.type_] {
        let is_iterable = struct_type.function_table.get_function.is_some()
            && struct_type.function_table.element_count_function.is_some();
        if !is_iterable {
            binder
                .diagnostic_bag
                .report_cannot_iterate(collection.location, collection.type_);
            return BoundNode::error(span);
        }
        Some(struct_type.clone())
    } else {
        if collection.type_ != typeid!(Type::Error) {
            binder
                .diagnostic_bag
                .report_cannot_iterate(collection.location, collection.type_);
        }
        return BoundNode::error(span);
    };
    let variable_type = binder.project.types[struct_type
        .as_ref()
        .unwrap()
        .function_table
        .get_function
        .as_ref()
        .unwrap()
        .function_type]
        .as_function_type()
        .unwrap()
        .return_type
        .clone();
    let variable_name = for_statement.variable.location;
    let variable_location = for_statement.variable.location;
    let variable = binder.register_variable(variable_name, variable_type.clone(), true);
    if variable.is_none() {
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(variable_location, variable_location);
        return BoundNode::error(span);
    }
    let variable = variable.unwrap();
    let variable = BoundNode::variable(span, variable, variable_type);
    let index_variable = match for_statement.optional_index_variable.map(|i| i.location) {
        Some(index_variable) => binder.register_variable(
            index_variable,
            typeid!(Type::Integer(IntegerType::Unsigned64)),
            true,
        ),
        None => binder.register_generated_variable(
            format!(
                "{}$index",
                &binder.project.source_text_collection[variable_name]
            ),
            typeid!(Type::Integer(IntegerType::Unsigned64)),
            true,
        ),
    };
    if index_variable.is_none() {
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(variable_location, variable_location);
        return BoundNode::error(span);
    }
    let index_variable = index_variable.unwrap();

    let collection_variable = binder
        .register_generated_variable(
            format!(
                "{}$collection",
                &binder.project.source_text_collection[variable_name]
            ),
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
        &binder.project.types,
    );
    binder.delete_variables_until(variable_count);

    result
}

fn bind_if_statement<'a, 'b>(
    span: TextLocation,
    if_statement: IfStatementNodeKind,
    binder: &mut BindingState<'b>,
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
    span: TextLocation,
    variable_declaration: VariableDeclarationNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    binder.expected_type = variable_declaration
        .optional_type_declaration
        .map(|td| bind_type(td.type_, binder).unwrap_type_id());
    let initializer = bind_node(*variable_declaration.initializer, binder);
    if initializer.type_ == typeid!(Type::Void) {
        binder
            .diagnostic_bag
            .report_invalid_void_expression(initializer.location);
    }
    let type_ = binder
        .expected_type
        .take()
        .unwrap_or_else(|| initializer.type_.clone());
    if type_ == typeid!(Type::None) {
        binder
            .diagnostic_bag
            .report_invalid_variable_type_none(span);
    }
    let type_ = match &binder.project.types[type_] {
        Type::IntegerLiteral => typeid!(Type::Integer(IntegerType::Signed64)),
        _ => type_,
    };
    let initializer = bind_conversion(initializer, type_, binder);
    let variable_index = binder.register_variable(
        variable_declaration.identifier.location,
        type_.clone(),
        false,
    );
    if let Some(variable_index) = variable_index {
        BoundNode::variable_declaration(span, variable_index, initializer, Some(type_))
    } else {
        let location = TextLocation::bounds(
            variable_declaration.let_keyword.location,
            variable_declaration.identifier.location,
        );
        binder
            .diagnostic_bag
            .report_cannot_declare_variable(location, variable_declaration.identifier.location);
        BoundNode::error(location)
    }
}

fn bind_return_statement<'a, 'b>(
    span: TextLocation,
    return_statement: ReturnStatementNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let expression = return_statement
        .optional_expression
        .map(|e| bind_node(*e, binder));
    let function_return_type = binder.function_return_type.clone();
    if expression.is_none()
        && !binder
            .project
            .types
            .can_be_converted(function_return_type, typeid!(Type::Void))
    {
        binder
            .diagnostic_bag
            .report_missing_return_value(span, function_return_type);
    }

    let expression =
        expression.map(|expression| bind_conversion(expression, function_return_type, binder));
    // TODO: This works with implicit returns in main only. Because for main
    // restores_variables must be false. There needs to be a flag in the binder
    // not only knowing the return type of the current function but also if it
    // is the main function or not.
    BoundNode::return_statement(span, expression, true)
}

fn bind_while_statement<'a, 'b>(
    span: TextLocation,
    while_statement: WhileStatementNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let condition = bind_node(*while_statement.condition, binder);
    let condition = bind_conversion(condition, typeid!(Type::Boolean), binder);
    let body = bind_node(*while_statement.body, binder);
    BoundNode::while_statement(span, condition, body)
}

fn bind_match_statement(
    location: TextLocation,
    match_statement: MatchStatementNodeKind,
    binder: &mut BindingState,
) -> BoundNode {
    let expression = bind_node(*match_statement.expression, binder);
    let expression = if expression.type_ == typeid!(Type::IntegerLiteral) {
        bind_conversion(
            expression,
            typeid!(Type::Integer(IntegerType::Signed64)),
            binder,
        )
    } else {
        expression
    };
    // Weird stuff we need to do, so the lowerer doesn't have to do them,
    // because I do not know, if he can do them.
    let dictionary = binder
        .project
        .types
        .look_up_type_by_name("Dict")
        .unwrap()
        .unwrap_generic_type_id();
    bind_generic_struct_type_for_types(
        location,
        dictionary,
        &[expression.type_, typeid!(Type::Pointer)],
        binder,
    );
    let noneable = binder
        .project
        .types
        .look_up_type_by_name("Noneable")
        .unwrap()
        .unwrap_generic_type_id();
    bind_generic_struct_type_for_types(location, noneable, &[typeid!(Type::Pointer)], binder);
    let mut default_case: Option<BoundNode> = None;
    let cases: Vec<_> = match_statement
        .match_cases
        .into_iter()
        .filter_map(|c| match bind_match_case(c, expression.type_, binder) {
            Either::Left(it) => Some(it),
            Either::Right(default_body) => {
                if let Some(default_case) = &default_case {
                    binder.diagnostic_bag.report_found_multiple_else_cases(
                        default_case.location,
                        default_body.location,
                    );
                } else {
                    default_case = Some(default_body);
                }
                None
            }
        })
        .rev()
        .collect();
    if default_case.is_none() {}
    let temporary_variable = binder.generate_temporary_variable("match", location);
    BoundNode::match_statement(
        location,
        expression,
        cases,
        default_case,
        temporary_variable,
    )
}

fn bind_match_case(
    case: MatchCaseNode,
    type_: TypeId,
    binder: &mut BindingState,
) -> either::Either<BoundMatchCase, BoundNode> {
    match case.expression {
        MatchCaseExpression::Expression(expression) => {
            let expression = bind_node(expression, binder);
            let expression = bind_conversion(expression, type_, binder);
            let body = bind_node(case.body, binder);
            Either::Left(BoundMatchCase::new(
                BoundMatchCaseExpression::Expression(expression),
                body,
            ))
        }
        MatchCaseExpression::Else(_) => Either::Right(bind_node(case.body, binder)),
        MatchCaseExpression::Type(t) => {
            let (name, expression_type) = bind_parameter(t, binder);
            let expression_type = expression_type.unwrap_type_id();
            let v = binder
                .register_variable(name, expression_type, true)
                .expect("TODO: Add diagnostic here!");
            assert!(
                binder
                    .project
                    .types
                    .can_be_converted(expression_type, type_),
                "TODO: Add diagnostic here!"
            );
            let body = bind_node(case.body, binder);
            Either::Left(BoundMatchCase::new(
                BoundMatchCaseExpression::Type(v, expression_type),
                body,
            ))
        }
    }
}

fn bind_assignment<'a, 'b>(
    span: TextLocation,
    assignment: AssignmentNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let lhs = bind_node_for_assignment(*assignment.lhs, binder);
    if let BoundNodeKind::VariableExpression(variable) = &lhs.kind {
        let name = binder.get_variable_name_by_id(variable.variable_index);
        if name == Some("this") && binder.is_struct_function {
            binder.diagnostic_bag.report_cannot_assign_to(lhs.location);
        }
    }
    binder.expected_type = Some(lhs.type_);
    let expression = bind_node(*assignment.expression, binder);
    // NOTE: Maybe this could be moved to another stage, since this is not
    // directly checking types and variables, but deconstructing a closure and
    // changing its argument order. On the other hand, we still need to check,
    // that the closure takes the correct type of our expression, otherwise we
    // might call functions with incorrect types.
    if let Type::Closure(closure) = &binder.project.types[lhs.type_].clone() {
        let function_type = &closure.base_function_type;
        if let BoundNodeKind::Closure(mut closure) = lhs.kind {
            let expression = bind_conversion(expression, function_type.parameter_types[1], binder);
            let base = match closure.function {
                typing::FunctionKind::FunctionId(_) => {
                    todo!("Assignment as FunctionCall not implemented for FunctionId")
                }
                typing::FunctionKind::SystemCall(_) => {
                    todo!("Assignment as FunctionCall not implemented for SystemCalls")
                }
                typing::FunctionKind::LabelReference(label_reference) => {
                    BoundNode::label_reference(
                        lhs.location,
                        label_reference,
                        binder
                            .project
                            .types
                            .look_up_or_add_type(Type::Function(function_type.clone())),
                    )
                }
                typing::FunctionKind::VtableIndex(_) => todo!(),
            };
            let mut arguments = Vec::with_capacity(closure.arguments.len() + 1);
            // index
            arguments.push(closure.arguments.pop().unwrap());
            // value
            arguments.push(expression);
            // this
            arguments.push(closure.arguments.pop().unwrap());
            assert!(closure.arguments.is_empty());
            BoundNode::function_call(span, base, arguments, false, typeid!(Type::Void))
        } else {
            // TODO: This is untested and it is actually expected, that this
            // will fail. The order of the arguments will probably be wrong..
            unimplemented!("Found a closure in assignment, which could not be called directly!");
            // let expression = bind_conversion(expression, &lhs.type_, binder);
            // BoundNode::function_call(span, lhs, vec![expression], false, Type::Void)
        }
    } else {
        let expression = bind_conversion(expression, lhs.type_, binder);
        BoundNode::assignment(span, lhs, expression)
    }
}

fn bind_block_statement<'a, 'b>(
    span: TextLocation,
    block_statement: BlockStatementNodeKind,
    binder: &mut BindingState<'b>,
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
    span: TextLocation,
    expression_statement: ExpressionStatementNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let expression = bind_node(*expression_statement.expression, binder);
    BoundNode::expression_statement(span, expression)
}

fn bind_repetition_node(
    location: TextLocation,
    repetition_node: RepetitionNodeNodeKind,
    binder: &mut BindingState,
) -> BoundNode {
    let expression = bind_node(*repetition_node.base_expression, binder);
    let repetition = bind_node(*repetition_node.repetition, binder);
    let repetition = bind_conversion(
        repetition,
        typeid!(Type::Integer(IntegerType::Unsigned64)),
        binder,
    );
    let counting_variable = binder
        .register_variable(
            format!(
                "generated$repetition$variable{}_{}",
                location.source_text.as_raw(),
                location.span.start()
            ),
            typeid!(Type::Integer(IntegerType::Unsigned64)),
            false,
        )
        .expect("Failed to declare compiler intern variable!");
    BoundNode::repetition_node(location, counting_variable, expression, repetition)
}
