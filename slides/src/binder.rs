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

use std::{collections::HashMap, convert::TryFrom, path::Path};

use crate::{
    binder::{
        bound_nodes::{is_same_expression::IsSameExpression, BoundNodeKind},
        operators::{BoundBinary, BoundUnaryOperator},
        symbols::StructFunctionKind,
        typing::{IntegerType, Type},
    },
    diagnostics::DiagnosticBag,
    instruction_converter::{
        instruction::Instruction, InstructionOrLabelReference, LabelReference,
    },
    lexer::syntax_token::{SyntaxToken, SyntaxTokenKind},
    parser::{
        self,
        syntax_nodes::{
            ArrayIndexNodeKind, ArrayLiteralNodeKind, AssignmentNodeKind, BinaryNodeKind,
            BlockStatementNodeKind, CastExpressionNodeKind, ConstDeclarationNodeKind,
            ConstructorCallNodeKind, EnumDeclarationNodeKind, ExpressionStatementNodeKind,
            FieldAccessNodeKind, ForStatementNodeKind, FunctionCallNodeKind,
            FunctionDeclarationNodeKind, FunctionTypeNode, IfStatementNodeKind, LiteralNodeKind,
            ParameterNode, ParenthesizedNodeKind, ReturnStatementNodeKind, StructBodyNode,
            StructDeclarationNodeKind, SyntaxNode, SyntaxNodeKind, TypeNode, UnaryNodeKind,
            VariableDeclarationNodeKind, VariableNodeKind, WhileStatementNodeKind,
        },
    },
    text::{SourceTextCollection, SourceTextId, TextLocation},
    value::Value,
    Project,
};

use self::{
    bound_nodes::BoundNode,
    operators::BoundBinaryOperator,
    symbols::{
        FunctionSymbol, GenericFunction, Library, StructFieldSymbol, StructFunctionTable,
        StructSymbol,
    },
    typing::{
        FunctionType, GenericType, GenericTypeId, Member, MemberOffsetOrAddress, StructType,
        SystemCallKind, TypeCollection, TypeCollectionIndexOutput, TypeId, TypeOrGenericType,
        TypeOrGenericTypeId,
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

#[derive(Clone, Debug)]
struct FunctionDeclarationBody<T> {
    header_location: TextLocation,
    function_name: SourceCow,
    body: SyntaxNode,
    parameters: Vec<(SourceCow, T)>,
    is_main: bool,
    _is_generic: bool,
    function_label: u64,
    function_type: T,
    base_struct: Option<T>,
    struct_function_kind: Option<StructFunctionKind>,
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
struct StructDeclarationBody {
    location: TextLocation,
    name: SourceCow,
    body: StructBodyNode,
    struct_function_table: SimpleStructFunctionTable,
    parent_id: Option<TypeId>,
    is_abstract: bool,
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

// impl From<GenericStructSymbol> for BoundGenericStructSymbol<'_> {
//     fn from(it: GenericStructSymbol) -> Self {
//         Self {
//             struct_type: BoundStructSymbol {
//                 name: it.name.into(),
//                 fields: it.fields.into_iter().map(Into::into).collect(),
//                 functions: it
//                     .functions
//                     .clone()
//                     .into_iter()
//                     .map(|f| BoundStructFieldSymbol {
//                         name: f.function_name.into(),
//                         type_: f.function_type,
//                         offset: 0,
//                         is_read_only: true,
//                     })
//                     .collect(),
//                 function_table: it.function_table,
//                 is_generic: true,
//                 parent: None, // TODO
//             },
//             functions: it.functions,
//         }
//     }
// }

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
    /// When registering a type for the type table, it is also registered in the
    /// struct table if it is a struct. The struct table is used to access field
    /// information on a struct. Since the type of a struct only has the types of
    /// the fields and their offset into the struct it cannot be used to check if
    /// a struct has a field with a certain name. This is what this is used for.
    /// So this is used by constructors or field accesses. Fields can also be
    /// read only, even though this is currently (12.10.2021) only used for
    /// functions on structs since they are also fields in some way. But
    /// functions on structs are still registered in the functions table.
    struct_table: Vec<StructSymbol>,
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
    generic_structs: Vec<StructDeclarationBody>,
    generic_type: Option<TypeId>,
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
    /// The type the current expression is expected to have. This is only used in
    /// bind_array_literal, to convert each expression inside the array literal
    /// to the right type. Everywhere else the expression is converted
    /// afterwards.
    expected_type: Option<TypeId>,
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

    // fn register_struct_name(
    //     &mut self,
    //     name: &'a str,
    //     struct_function_table: SimpleStructFunctionTable,
    // ) -> Option<u64> {
    //     let id = self.type_table.len() as u64;
    //     self.register_type(
    //         name,
    //         Type::StructReference(StructReferenceType {
    //             id,
    //             simple_function_table: struct_function_table,
    //         }),
    //     )
    // }

    // fn register_generated_struct_name(
    //     &mut self,
    //     name: String,
    //     struct_function_table: SimpleStructFunctionTable,
    // ) -> Option<u64> {
    //     let id = self.type_table.len() as u64;
    //     self.register_generated_type(
    //         name,
    //         Type::StructReference(StructReferenceType {
    //             id,
    //             simple_function_table: struct_function_table,
    //         }),
    //     )
    // }

    // fn register_generic_struct(
    //     &mut self,
    //     id: u64,
    //     fields: Vec<BoundStructFieldSymbol<'a>>,
    //     function_table: StructFunctionTable,
    // ) -> u64 {
    //     let name = self.type_table[id as usize].identifier.clone();
    //     let bound_struct_type = BoundGenericStructSymbol {
    //         struct_type: BoundStructSymbol {
    //             name,
    //             fields,
    //             function_table,
    //             is_generic: true,
    //             parent: None, // TODO
    //         },
    //         functions: vec![],
    //     };
    //     let struct_id = id as usize - type_table().len();
    //     while struct_id >= self.struct_table.len() {
    //         self.struct_table.push(BoundMaybeGenericStructSymbol::Empty);
    //     }
    //     assert!(self.struct_table[struct_id].is_empty());
    //     self.struct_table[struct_id] = bound_struct_type.into();
    //     id
    // }

    // fn register_struct(
    //     &mut self,
    //     id: u64,
    //     parent_id: Option<u64>,
    //     fields: Vec<BoundStructFieldSymbol<'a>>,
    //     function_table: StructFunctionTable,
    // ) -> u64 {
    //     let mut struct_type = StructType {
    //         id,
    //         fields: fields
    //             .iter()
    //             .filter(|f| !matches!(f.type_, Type::Function(_)))
    //             .map(|f| f.type_.clone())
    //             .collect(),
    //         functions: fields
    //             .iter()
    //             .filter(|f| matches!(f.type_, Type::Function(_)))
    //             .map(|f| f.type_.clone())
    //             .collect(),
    //         function_table: function_table.clone(),
    //         is_generic: false,
    //         parent_id,
    //         size_in_bytes: 0,
    //     };
    //     self.insert_into_struct_table(
    //         id,
    //         parent_id,
    //         fields.into_iter().map(Into::into).collect(),
    //         function_table,
    //     );
    //     struct_type.size_in_bytes = self.get_struct_type_by_id(id).unwrap().size_in_bytes();
    //     self.type_table[id as usize].type_ = Type::Struct(Box::new(struct_type));
    //     id
    // }

    // pub fn register_maybe_generic_struct_as(
    //     &mut self,
    //     name: &str,
    //     maybe_generic_struct: &MaybeGenericStructSymbol,
    // ) -> Option<u64> {
    //     let id = self.register_generated_struct_name(
    //         name.to_owned(),
    //         maybe_generic_struct.function_table().into(),
    //     )?;
    //     let fields = maybe_generic_struct.fields().to_vec();
    //     let function_table = maybe_generic_struct.function_table().clone();
    //     let struct_type = StructType {
    //         id,
    //         fields: fields
    //             .iter()
    //             .filter(|f| !matches!(f.type_, Type::Function(_)))
    //             .map(|f| f.type_.clone())
    //             .collect(),
    //         functions: fields
    //             .iter()
    //             .filter(|f| matches!(f.type_, Type::Function(_)))
    //             .map(|f| f.type_.clone())
    //             .collect(),
    //         function_table,
    //         is_generic: false,
    //         parent_id: None, // TODO
    //         size_in_bytes: fields
    //             .iter()
    //             .filter(|f| !matches!(f.type_, Type::Function(_)))
    //             .map(|f| f.type_.size_in_bytes())
    //             .sum(),
    //     };
    //     self.type_table[id as usize].type_ = Type::Struct(Box::new(struct_type));

    //     let struct_id = id as usize - type_table().len();
    //     while struct_id >= self.struct_table.len() {
    //         self.struct_table.push(BoundMaybeGenericStructSymbol::Empty);
    //     }

    //     assert!(self.struct_table[struct_id].is_empty());
    //     self.struct_table[struct_id] = maybe_generic_struct.to_owned().into();
    //     self.struct_table[struct_id].set_name(name.to_owned());

    //     Some(id)
    // }

    // pub fn insert_into_struct_table(
    //     &mut self,
    //     id: u64,
    //     parent_id: Option<u64>,
    //     fields: Vec<BoundStructFieldSymbol<'a>>,
    //     function_table: StructFunctionTable,
    // ) {
    //     let name = self.type_table[id as usize].identifier.clone();
    //     let bound_struct_type = BoundStructSymbol {
    //         name,
    //         fields,
    //         function_table,
    //         is_generic: false,
    //         parent: parent_id.map(|id| {
    //             Box::new(
    //                 self.get_struct_type_by_id(id)
    //                     .expect(
    //                         "This should never happen. Since it should already be checked before.",
    //                     )
    //                     .clone(),
    //             )
    //         }), // TODO
    //     };
    //     let struct_id = id as usize - type_table().len();
    //     while struct_id >= self.struct_table.len() {
    //         self.struct_table.push(BoundMaybeGenericStructSymbol::Empty);
    //     }

    //     assert!(self.struct_table[struct_id].is_empty());
    //     self.struct_table[struct_id] = bound_struct_type.into();
    // }

    // fn register_type(&mut self, name: &'a str, type_: Type) -> Option<u64> {
    //     let index = self.type_table.len() as u64;
    //     let type_name_already_registered = self
    //         .type_table
    //         .iter()
    //         .any(|type_| type_.identifier.as_ref() == name);
    //     if type_name_already_registered {
    //         None
    //     } else {
    //         self.type_table.push(BoundVariableName {
    //             identifier: name.into(),
    //             type_,
    //             is_read_only: true,
    //         });
    //         Some(index)
    //     }
    // }

    // fn register_generated_type(&mut self, name: String, type_: Type) -> Option<u64> {
    //     let index = self.type_table.len() as u64;
    //     let type_name_already_registered = self
    //         .type_table
    //         .iter()
    //         .any(|type_| type_.identifier.as_ref() == name);
    //     if type_name_already_registered {
    //         None
    //     } else {
    //         self.type_table.push(BoundVariableName {
    //             identifier: name.into(),
    //             type_,
    //             is_read_only: true,
    //         });
    //         Some(index)
    //     }
    // }

    fn register_constant(&mut self, name: TextLocation, value: Value) -> Option<u64> {
        let index = self.constants.len() as u64;
        let constant_name_already_registered = self.constants.iter().any(|c| {
            c.identifier.as_str(&self.project.source_text_collection)
                == &self.project.source_text_collection[name]
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

    fn register_generated_constant(&mut self, name: String, value: Value) -> Option<u64> {
        let index = self.constants.len() as u64;
        let constant_name_already_registered = self
            .constants
            .iter()
            .any(|c| c.identifier.as_str(&self.project.source_text_collection) == name);
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

    // fn look_up_std_struct_id(&self, kind: StdTypeKind) -> u64 {
    //     self.look_up_struct_id_by_name(kind.name())
    //         .unwrap_or_else(|| panic!("Could not load std type {}.", kind.name()))
    // }
    fn look_up_type_by_source_cow(&self, name: &SourceCow) -> Option<TypeOrGenericTypeId> {
        self.project
            .types
            .look_up_type_by_name(name.as_str(&self.project.source_text_collection))
    }

    fn look_up_type_by_name(&self, name: &str) -> Option<TypeOrGenericTypeId> {
        // if name == "$Type" && self.generic_type.is_some() {
        //     return Some(self.generic_type.clone().unwrap());
        // }
        self.project.types.look_up_type_by_name(name)
        // self.type_table
        //     .iter()
        //     .find(|v| v.identifier.as_ref() == name)
        //     .map(|v| v.type_.clone())
        //     .map(|t| {
        //         if let Type::Struct(struct_type) = t {
        //             Type::StructReference(StructReferenceType {
        //                 id: struct_type.id,
        //                 simple_function_table: (&struct_type.function_table).into(),
        //             })
        //         } else {
        //             t
        //         }
        //     })
    }

    // fn look_up_type_by_name(&&self, name: String) -> Option<Type> {
    //     self.type_table
    //         .iter()
    //         .find(|v| v.identifier.as_ref() == name)
    //         .map(|v| v.type_.clone())
    //         .map(|t| {
    //             if let Type::Struct(struct_type) = t {
    //                 Type::StructReference(StructReferenceType {
    //                     id: struct_type.id,
    //                     simple_function_table: (&struct_type.function_table).into(),
    //                 })
    //             } else {
    //                 t
    //             }
    //         })
    // }

    // fn get_struct_type_by_id(&self, id: u64) -> Option<&BoundStructSymbol<'a>> {
    //     let index = id as usize - type_table().len();

    //     self.struct_table
    //         .get(index)
    //         .filter(|e| !e.is_empty())
    //         .map(|e| {
    //             e.as_struct()
    //                 .or_else(|| e.as_generic_struct().map(|e| &e.struct_type))
    //         })
    //         .flatten()
    // }

    // fn get_generic_struct_type_by_id(&self, id: u64) -> Option<&BoundGenericStructSymbol<'a>> {
    //     self.struct_table
    //         .get(id as usize - type_table().len())
    //         .map(|e| e.as_generic_struct())
    //         .flatten()
    // }

    // fn get_generic_struct_type_by_id_mut(
    //     &mut self,
    //     id: u64,
    // ) -> Option<&mut BoundGenericStructSymbol<'a>> {
    //     self.struct_table
    //         .get_mut(id as usize - type_table().len())
    //         .map(|e| e.as_generic_struct_mut())
    //         .flatten()
    // }

    // fn get_struct_by_id(&self, id: u64) -> Option<StructType> {
    //     let mut iterator =
    //         self.type_table
    //             .iter()
    //             .filter_map(|BoundVariableName { type_, .. }| {
    //                 if let Type::Struct(it) = type_ {
    //                     if it.id == id {
    //                         Some(*it.clone())
    //                     } else {
    //                         None
    //                     }
    //                 } else {
    //                     None
    //                 }
    //             });
    //     let result = iterator.next();
    //     assert!(iterator.next().is_none());
    //     result
    // }

    // pub fn look_up_struct_id_by_name(&self, name: &str) -> Option<u64> {
    //     self.struct_table
    //         .iter()
    //         .enumerate()
    //         .find(|(_, s)| s.name() == name)
    //         .map(|(i, _)| (i + type_table().len()) as u64)
    // }

    // pub fn rename_struct_by_id(&mut self, struct_id: u64, new_name: String) {
    //     self.struct_table[struct_id as usize - type_table().len()].set_name(new_name.clone());
    //     self.type_table
    //         .iter_mut()
    //         .find(|t| match &t.type_ {
    //             Type::Struct(struct_type) => struct_type.id == struct_id,
    //             // NOTE: Is there a reason to format this like it currently is?
    //             Type::StructReference(id) if id.id == struct_id => true,
    //             _ => false,
    //         })
    //         .unwrap()
    //         .identifier = new_name.into();
    // }

    // fn convert_struct_reference_to_struct(&self, type_: Type) -> Type {
    //     match type_ {
    //         Type::Error
    //         | Type::Void
    //         | Type::Any
    //         | Type::Integer(_)
    //         | Type::IntegerLiteral
    //         | Type::Boolean
    //         | Type::None
    //         | Type::String
    //         | Type::Function(_)
    //         | Type::Closure(_)
    //         | Type::Library(_)
    //         | Type::GenericType
    //         | Type::Struct(_)
    //         | Type::TypedGenericStruct(_)
    //         | Type::Pointer
    //         | Type::SystemCall(_)
    //         | Type::Enum(_) => type_,
    //         Type::Noneable(base_type) => {
    //             Type::noneable(self.convert_struct_reference_to_struct(*base_type))
    //         }
    //         Type::StructReference(id) => {
    //             debug_assert!(self.generic_structs.is_empty());
    //             debug_assert!(self.structs.is_empty());
    //             Type::Struct(Box::new(self.get_struct_by_id(id.id).unwrap()))
    //         }
    //         Type::PointerOf(base_type) => {
    //             Type::pointer_of(self.convert_struct_reference_to_struct(*base_type))
    //         }
    //     }
    // }

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
    ) -> Option<TypeId> {
        let name = name.into();
        self.project
            .types
            .add_type(Type::StructPlaceholder(
                name.to_owned(&self.project.source_text_collection),
                struct_function_table,
            ))
            .ok()
    }

    fn register_generic_struct_name(
        &mut self,
        name: impl Into<SourceCow>,
        struct_function_table: SimpleStructFunctionTable,
    ) -> Option<GenericTypeId> {
        let name = name.into();
        self.project
            .types
            .add_generic_type(GenericType::StructPlaceholder(
                name.to_owned(&self.project.source_text_collection),
                struct_function_table,
            ))
            .ok()
    }

    fn lexeme(&self, token: SyntaxToken) -> &str {
        &self.project.source_text_collection[token.location]
    }

    fn directory(&self) -> &str {
        self.project.source_text_collection[self.source_text].directory()
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
    pub exported_structs: Vec<StructSymbol>,
    pub exported_generic_structs: Vec<GenericType>,
}

impl BoundLibrary {
    pub fn error(location: TextLocation) -> Self {
        Self {
            program: BoundProgram::error(location),
            exported_functions: vec![],
            exported_structs: vec![],
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
        max_used_variables: 0,
        function_return_type: typeid!(Type::Error),
        is_struct_function: false,
        assigned_fields: vec![],
        expected_type: None,
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

    while let Some(node) = binder.generic_structs.pop() {
        let name = node.name.clone();
        let struct_type = bind_struct_body(node, &mut binder);
        let placeholder = binder
            .look_up_type_by_source_cow(&name)
            .expect("Failed to preregister type!")
            .unwrap_generic_type_id();
        let type_id_placeholder = binder.project.types.to_fake_type_id(placeholder);
        binder.project.types.overwrite_generic_type(
            placeholder,
            GenericType::Struct(struct_type.clone(), Vec::new()),
        );
        binder.project.types.overwrite_type(
            type_id_placeholder,
            Type::Struct(StructType {
                name: format!("{}<$Type>", struct_type.name),
                applied_type: Some(typeid!(Type::GenericType)),
                ..struct_type
            }),
        );
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
            binder.bound_generic_functions.push(generic_function);
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
        for (id, entry) in binder.struct_table.iter().enumerate() {
            println!("  {}: {}", id, entry.name);
        }
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
        exported_structs: binder.struct_table,
        exported_generic_structs: vec![],
    }
}

fn bind_function_declaration_body<'a>(
    node: FunctionDeclarationBody<TypeId>,
    binder: &mut BindingState<'_>,
) -> BoundNode {
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
            | StructFunctionKind::Equals,
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
    let body_statements = lowerer::flatten(body, &mut binder.label_offset);
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
    let result =
        BoundNode::function_declaration(label, node.is_main, body, parameters, base_register);
    binder.function_return_type = typeid!(Type::Void);
    binder.delete_variables_until(fixed_variable_count);
    result
}

fn bind_generic_function_declaration_body<'a>(
    node: FunctionDeclarationBody<TypeOrGenericTypeId>,
    binder: &mut BindingState<'_>,
) -> (BoundNode, Vec<usize>) {
    let fixed_variable_count = binder.variable_table.len();
    // let label = node.function_label as usize;
    binder
        .project
        .types
        .maybe_print_type_table(binder.project.debug_flags);
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
            | StructFunctionKind::Equals,
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
    let body_statements = lowerer::flatten(body, &mut binder.label_offset);
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
    binder.register_generated_constant("print".into(), Value::SystemCall(SystemCallKind::Print));
    binder.register_generated_constant(
        "heapdump".into(),
        Value::SystemCall(SystemCallKind::HeapDump),
    );
    binder.register_generated_constant("break".into(), Value::SystemCall(SystemCallKind::Break));
    binder.register_generated_constant(
        "reallocate".into(),
        Value::SystemCall(SystemCallKind::Reallocate),
    );
    binder.register_generated_constant(
        "runtimeError".into(),
        Value::SystemCall(SystemCallKind::RuntimeError),
    );
    binder.register_generated_constant(
        "addressOf".into(),
        Value::SystemCall(SystemCallKind::AddressOf),
    );
    binder.register_generated_constant(
        "garbageCollect".into(),
        Value::SystemCall(SystemCallKind::GarbageCollect),
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
                Instruction::function_call(location).into(),
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
            result.push(Instruction::function_call(location).into());
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
    let is_generic = function_declaration.optional_generic_keyword.is_some();
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
        _is_generic: is_generic,
        function_label,
        function_type: type_,
        base_struct: None,
        struct_function_kind: None,
    });
}

fn bind_function_declaration_for_struct<'a, 'b>(
    struct_name: &SourceCow,
    struct_is_generic: bool,
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
                        _is_generic: struct_is_generic,
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
                        binder.register_generated_constant(
                            function_name.clone(),
                            Value::LabelPointer(function_label as usize, type_.clone()),
                        );
                        binder.add_function_declaration(FunctionDeclarationBody {
                            header_location: header_span,
                            function_name: function_name.into(),
                            body: *function_declaration.body,
                            parameters: variables,
                            is_main: false,
                            _is_generic: struct_is_generic,
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
            let type_ = binder
                .project
                .types
                .look_up_or_add_generic_type(GenericType::Function(function_type.clone()));

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
                        _is_generic: struct_is_generic,
                        function_label,
                        function_type: type_.into(),
                        base_struct: Some(struct_type.into()),
                        struct_function_kind: None,
                    });
                    // todo!("Do we need to implement this now??");
                    // TODO:

                    // let function_label = simple_struct_function_table
                    //     .get(struct_function_kind)
                    //     .unwrap() as u64;

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
                            _is_generic: struct_is_generic,
                            function_label,
                            function_type: type_.into(),
                            base_struct: Some(struct_type.into()),
                            struct_function_kind: None,
                        });
                        let type_ = binder.project.types.to_fake_type_id(type_);
                        let (struct_name, function_name) = function_name.split_once("::").unwrap();
                        let function_name = format!("{}<$Type>::{}", struct_name, function_name);
                        binder.register_generated_constant(
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
            if !binder.project.types.can_be_converted(
                function_type.parameter_types[0],
                typeid!(Type::Integer(IntegerType::Unsigned64)),
            ) {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.parameter_types[0],
                    typeid!(Type::Integer(IntegerType::Unsigned64)),
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
            if !binder.project.types.can_be_converted(
                function_type.parameter_types[0],
                typeid!(Type::Integer(IntegerType::Unsigned64)),
            ) {
                binder.diagnostic_bag.report_cannot_convert(
                    location,
                    function_type.parameter_types[0],
                    typeid!(Type::Integer(IntegerType::Unsigned64)),
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
    let is_generic = struct_declaration.optional_generic_keyword.is_some();
    let successfull_struct_declaration = if is_generic {
        binder
            .register_generic_struct_name(
                struct_declaration.identifier.location,
                struct_function_table,
            )
            .map(|_| ())
    } else {
        binder
            .register_struct_name(
                struct_declaration.identifier.location,
                struct_function_table,
            )
            .map(|_| ())
    };
    let is_abstract = struct_declaration.optional_abstract_keyword.is_some();
    if successfull_struct_declaration == Some(()) {
        let parent_id = struct_declaration.optional_parent.map(|i| {
            binder
                .look_up_type_by_name(binder.lexeme(i))
                .expect("Implement error message if parent does not exist!")
                .unwrap_type_id()
        });
        if struct_declaration.optional_generic_keyword.is_some() {
            binder
                .register_struct_name(
                    format!("{}<$Type>", binder.lexeme(struct_declaration.identifier)),
                    struct_function_table,
                )
                .unwrap();
            binder.generic_structs.push(StructDeclarationBody {
                location: struct_declaration.identifier.location,
                name: struct_declaration.identifier.location.into(),
                body: *struct_declaration.body,
                struct_function_table,
                parent_id,
                is_abstract,
            });
        } else {
            binder.structs.push(StructDeclarationBody {
                location: struct_declaration.identifier.location,
                name: struct_declaration.identifier.location.into(),
                body: *struct_declaration.body,
                struct_function_table,
                parent_id,
                is_abstract,
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

// FIXME: We already have struct which contains all the struct_ fields, why are
// not using it directly??
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
    let mut used_generic_type = false;

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
                    struct_is_generic,
                    *function_declaration,
                    &mut function_table,
                    struct_node.struct_function_table,
                    binder,
                )
            }
            SyntaxNodeKind::StructField(struct_field) => {
                is_function = false;
                let type_span = struct_field.field.type_declaration.type_.location();
                let (name, type_) = bind_parameter(struct_field.field, binder);
                let type_ = type_.unwrap_type_id();
                let field_offset = {
                    let type_ = &binder.project.types[type_];
                    offset += type_.size_in_bytes();
                    offset - type_.size_in_bytes()
                };
                if binder
                    .project
                    .types
                    .contains_type(type_, typeid!(Type::GenericType))
                {
                    used_generic_type = true;
                }
                if type_ == typeid!(Type::GenericType) && !struct_is_generic {
                    binder
                        .diagnostic_bag
                        .report_generic_type_in_ungeneric_struct(
                            type_span,
                            struct_node.name.clone(),
                        );
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
    if !used_generic_type && struct_is_generic {
        binder
            .diagnostic_bag
            .report_generic_struct_without_generic_type(
                struct_node.location,
                struct_node.name.clone(),
            )
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
        applied_type: None,
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
    let type_span = type_.location();
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
        binder.look_up_type_by_name(type_name)
    }
    .unwrap_or_else(|| {
        binder.diagnostic_bag.report_unknown_type(
            type_span,
            type_.full_type_name(&binder.project.source_text_collection),
        );
        typeid!(Type::Error).into()
    });
    if let Some(generic_type_qualifier) = type_.generic_type_qualifier {
        let generic_type = bind_type(*generic_type_qualifier, binder);
        let new_type = bind_generic_struct_type_for_type(
            result.unwrap_generic_type_id(),
            generic_type.unwrap_type_id(),
            binder,
        );
        result = new_type.into();
    }
    if type_.optional_question_mark.is_some() {
        result = binder
            .project
            .types
            .create_noneable_version(result.unwrap_type_id())
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
            let new_type =
                bind_generic_struct_type_for_type(array_base_type, result.unwrap_type_id(), binder);
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
        SyntaxNodeKind::Assignment(assignment) => {
            bind_assignment(node.location, assignment, binder)
        }
        SyntaxNodeKind::ExpressionStatement(expression_statement) => {
            bind_expression_statement(node.location, expression_statement, binder)
        }
        _ => unreachable!(),
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
    span: TextLocation,
    mut array_literal: ArrayLiteralNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let array_type = binder
        .project
        .types
        .look_up_type_by_name("Array")
        .expect("Failed to load type Array from std lib.")
        .unwrap_generic_type_id();
    let expected_type = if let Some(struct_type) = binder
        .expected_type
        .map(|t| binder.project.types[t].as_struct_type())
        .flatten()
    {
        Some(struct_type.applied_type.unwrap().clone())
    } else {
        None
    };
    let first_child = array_literal.children.remove(0);
    let (mut type_, mut children) =
        bind_array_literal_first_child(first_child, expected_type, binder);
    for child in array_literal.children {
        let (child, repetition) =
            if let SyntaxNodeKind::RepetitionNode(repetition_node) = child.kind {
                let base_expression = bind_node(*repetition_node.base_expression, binder);
                let repetition = bind_node(*repetition_node.repetition, binder);
                let repetition = bind_conversion(
                    repetition,
                    typeid!(Type::Integer(IntegerType::Unsigned64)),
                    binder,
                );
                let repetition = match repetition.constant_value {
                    Some(value) => value.value.as_integer().unwrap(),
                    None => {
                        binder
                            .diagnostic_bag
                            .report_expected_constant(repetition.location);
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
        if type_ == typeid!(Type::IntegerLiteral) && child.type_ != typeid!(Type::IntegerLiteral) {
            if binder.project.types.can_be_converted(type_, child.type_) {
                // FIXME: Do we need to update all children until now??
                type_ = child.type_.clone();
            }
        }
        let child = bind_conversion(child, type_, binder);
        for _ in 0..repetition {
            children.push(child.clone());
        }
    }
    if type_ == typeid!(Type::IntegerLiteral) {
        type_ = typeid!(Type::Integer(IntegerType::Signed64));
    }
    let type_ = bind_generic_struct_type_for_type(array_type, type_, binder);
    BoundNode::array_literal(span, children, type_)
}

fn bind_array_literal_first_child<'a>(
    first_child: SyntaxNode,
    expected_type: Option<TypeId>,
    binder: &mut BindingState<'_>,
) -> (TypeId, Vec<BoundNode>) {
    let children = if let SyntaxNodeKind::RepetitionNode(repetition_node) = first_child.kind {
        let base_expression = bind_node(*repetition_node.base_expression, binder);
        let repetition = bind_node(*repetition_node.repetition, binder);
        let repetition = bind_conversion(
            repetition,
            typeid!(Type::Integer(IntegerType::Unsigned64)),
            binder,
        );
        let repetition = match repetition.constant_value {
            Some(value) => value.value.as_integer().unwrap(),
            None => {
                binder
                    .diagnostic_bag
                    .report_expected_constant(repetition.location);
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
        .map(|c| bind_conversion(c, type_, binder))
        .collect();
    (type_, children)
}

fn bind_cast_expression<'a>(
    span: TextLocation,
    cast_expression: CastExpressionNodeKind,
    binder: &mut BindingState<'_>,
) -> BoundNode {
    let expression = bind_node(*cast_expression.expression, binder);
    // TODO: Write diagnostic, that it is impossible to cast to generic types!
    let type_ = bind_type(cast_expression.type_, binder).unwrap_type_id();
    // Cast unnecessary and will always return a valid value.
    if binder
        .project
        .types
        .can_be_converted(expression.type_, type_)
    {
        binder
            .diagnostic_bag
            .report_unnecessary_cast(span, expression.type_, type_);
        return bind_conversion(expression, type_, binder);
    }
    if !binder
        .project
        .types
        .can_be_casted_to(expression.type_, type_)
    {
        binder
            .diagnostic_bag
            .report_impossible_cast(span, expression.type_, type_);
        return BoundNode::error(span);
    }
    let type_noneable = binder.project.types.create_noneable_version(type_);
    if binder
        .project
        .types
        .can_be_converted(expression.type_, type_noneable)
    {
        binder
            .diagnostic_bag
            .report_unnecessary_cast(span, expression.type_, type_);
        return bind_conversion(expression, type_noneable, binder);
    }
    BoundNode::conversion(span, expression, type_noneable)
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
                generic_type_qualifier: None, // TODO: we might wanna use this later.
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
                    && it.applied_type.is_none()
                    && expected_struct.applied_type.is_some()
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
        TypeOrGenericType::GenericType(GenericType::Struct(it, _)) => it,
        _ => {
            dbg!(&type_);
            binder
                .diagnostic_bag
                .report_unknown_type(type_name_location, constructor_call.type_name.location);
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
            // HACK: We somehow get functions in our fields, if we use generic
            // structs, so we filter them out, since user can not declare
            // functions as fields of the time being.
            let fields: Vec<_> = struct_type
                .fields_for_constructor(&binder.project.types)
                .into_iter()
                .cloned()
                .collect();
            // let fields: Vec<_> = struct_type
            //     .fields
            //     .iter()
            //     .filter(|f| !matches!(f.type_, Type::Function(_)))
            //     .collect();
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
                binder.expected_type = Some(parameter_type.clone());
                let argument = bind_node(argument, binder);
                binder.expected_type = old_expected_type;
                let parameter_type = if struct_type.is_generic {
                    if parameter_type == typeid!(Type::GenericType) {
                        if binder.generic_type.is_none() {
                            binder.generic_type = Some(argument.type_.clone());
                            resolved_struct_type = Some(
                                bind_generic_struct_type_for_type(
                                    struct_id.unwrap_generic_type_id(),
                                    argument.type_,
                                    binder,
                                )
                                .into(),
                            );
                        }
                        argument.type_.clone()
                    } else if parameter_type
                        == binder
                            .project
                            .types
                            .look_up_or_add_type(Type::PointerOf(typeid!(Type::GenericType)))
                    {
                        if binder.generic_type.is_none() {
                            if let Type::PointerOf(type_) = &binder.project.types[argument.type_] {
                                binder.generic_type = Some(*type_);
                                resolved_struct_type = Some(
                                    bind_generic_struct_type_for_type(
                                        struct_id.unwrap_generic_type_id(),
                                        *type_,
                                        binder,
                                    )
                                    .into(),
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
                let argument = bind_conversion(argument, parameter_type, binder);
                arguments.push(argument);
            }
            binder.generic_type = None;
            match resolved_struct_type {
                Some(it) => (arguments, None, it.unwrap_type_id()),
                // An error should already been reported.
                None => return BoundNode::error(location),
            }
        }
    };

    BoundNode::constructor_call(location, arguments, struct_type, function)
}

fn bind_generic_struct_type_for_type(
    struct_id: GenericTypeId,
    type_: TypeId,
    binder: &mut BindingState,
) -> TypeId {
    let struct_name = format!(
        "{}<{}>",
        binder.project.types.name_of_generic_type_id(struct_id),
        binder.project.types.name_of_type_id(type_)
    );
    let id = if let Some(id) = binder.look_up_type_by_name(&struct_name) {
        id.unwrap_type_id()
    } else {
        let generic_struct = binder.project.types[struct_id]
            .as_struct_type()
            .unwrap()
            .clone();
        // let mut function_labels_map: HashMap<_, _> = generic_struct
        //     .functions
        //     .iter()
        //     .map(|f| {
        //         (
        //             f.offset_or_address.unwrap_address(),
        //             binder.generate_label(),
        //         )
        //     })
        //     .collect();
        let mut function_labels_map: HashMap<usize, usize> = HashMap::default();
        function_labels_map.extend(
            generic_struct
                .function_table
                .function_symbols_iter()
                .map(|f| (f.function_label as usize, binder.generate_label())),
        );
        let mut simple_function_table = SimpleStructFunctionTable::default();
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
                m.type_ = binder.project.types.replace_in_type_for_type(
                    m.type_,
                    typeid!(Type::GenericType),
                    type_,
                );
                m
            })
            .collect();
        let mut functions = Vec::new();
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
        let this_type = binder
            .register_struct_name(struct_name.clone(), SimpleStructFunctionTable::default())
            .unwrap();
        let mut function_table = StructFunctionTable::default();
        for generic_function in generic_functions.iter() {
            let label = function_labels_map[&(generic_function.function_label as usize)];
            let bare_function_name = generic_function.function_name.split_once("::").unwrap().1;
            let function_name = format!("{}::{}", struct_name, bare_function_name,);
            let function = bind_generic_function_for_type(
                generic_function,
                label,
                &function_labels_map,
                type_,
                this_type,
                &function_name,
                binder,
            );
            match StructFunctionKind::try_from(bare_function_name) {
                Ok(it) => function_table.set(
                    it,
                    FunctionSymbol {
                        name: function_name.clone(),
                        function_type: function,
                        function_label: label as u64,
                        is_member_function: false,
                    },
                ),
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
            this_type,
            Type::Struct(StructType {
                name: struct_name,
                fields,
                functions,
                function_table,
                is_generic: false,
                is_abstract: false, // Should there be abstract generic structs??
                parent: None,
                size_in_bytes,
                applied_type: Some(type_),
            }),
        );
        this_type
    };
    if id.as_raw() == 50 {
        panic!();
    }
    id
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
                            BoundNode::function_call(
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
                                BoundNode::function_call(
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
    span: TextLocation,
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
        (
            Type::Noneable(inner),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::None,
        )
        | (
            Type::None,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Noneable(inner),
        ) if *inner != typeid!(Type::Void) => Some(BoundBinary::same_input(
            binder
                .project
                .types
                .look_up_type(&Type::Noneable(inner.clone()))
                .expect("This type has been looked up. It should already exist."),
            result,
            typeid!(Type::Boolean),
        )),
        (
            Type::Noneable(inner),
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            outer,
        )
        | (
            outer,
            BoundBinaryOperator::Equals | BoundBinaryOperator::NotEquals,
            Type::Noneable(inner),
        ) => {
            let noneable_type = binder
                .project
                .types
                .look_up_type(outer)
                .expect("This type has been looked up. It should already exist");
            if binder.project.types.can_be_converted(noneable_type, *inner) && outer != &Type::Void
            {
                let input = binder
                    .project
                    .types
                    .look_up_or_add_type(Type::Noneable(*inner));
                Some(BoundBinary::same_input(
                    input,
                    result,
                    typeid!(Type::Boolean),
                ))
            } else {
                todo!("Report error: cannot convert type to type?.");
                // None
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
        (Type::Noneable(lhs_type), BoundBinaryOperator::NoneableOrValue, _) => {
            if binder.project.types.can_be_converted(rhs.type_, *lhs_type) {
                Some(BoundBinary::new(lhs.type_, result, rhs.type_, *lhs_type))
            } else {
                binder
                    .diagnostic_bag
                    .report_cannot_convert(span, rhs.type_, *lhs_type);
                None
            }
        }
        // Special case, where none ?? value is used. This could be optimized
        // away later.
        (Type::None, BoundBinaryOperator::NoneableOrValue, _) => Some(BoundBinary::new(
            typeid!(Type::None),
            result,
            rhs.type_,
            rhs.type_.clone(),
        )),
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
                span,
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
        Type::Boolean | Type::Noneable(_) if result == BoundUnaryOperator::LogicalNegation => {
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
        Type::Function(result) => result.clone(),
        Type::Closure(closure) => closure.base_function_type.clone(),
        Type::Error => FunctionType::error(),
        _ => unimplemented!("Not implemented for {:?}", type_),
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
            typing::FunctionKind::VtableIndex(_) => function,
        }
    } else {
        function
    };
    if function.type_ == typeid!(Type::Error) {
        return BoundNode::error(location);
    }
    let function_type = function_type(&binder.project.types[function.type_]);
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
    let index = bind_conversion(
        index,
        typeid!(Type::Integer(IntegerType::Unsigned64)),
        binder,
    );
    let base_span = array_index.base.location;
    let base = bind_node(*array_index.base, binder);
    let type_ = match binder.project.types[base.type_].clone() {
        Type::PointerOf(base_type) => base_type,
        Type::Struct(struct_type) => match &struct_type.function_table.get_function {
            Some(get_function) => {
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
                binder
                    .diagnostic_bag
                    .report_cannot_index_get(base_span, base.type_);
                typeid!(Type::Error)
            }
        },
        _ => {
            binder
                .diagnostic_bag
                .report_cannot_index_get(base_span, base.type_);
            typeid!(Type::Error)
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
    let index = bind_conversion(
        index,
        typeid!(Type::Integer(IntegerType::Unsigned64)),
        binder,
    );
    let base_span = array_index.base.location;
    let base = bind_node(*array_index.base, binder);
    let base_type = binder.project.types[base.type_].clone();
    let type_ = match &base_type {
        Type::PointerOf(base_type) => *base_type,
        Type::Struct(struct_type) => match &struct_type.function_table.set_function {
            Some(set_function) => {
                let type_ = set_function.function_type;
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
                binder
                    .diagnostic_bag
                    .report_cannot_index_set(base_span, base.type_);
                typeid!(Type::Error)
            }
        },
        _ => {
            binder
                .diagnostic_bag
                .report_cannot_index_get(base_span, base.type_);
            typeid!(Type::Error)
        }
    };
    BoundNode::array_index(span, base, index, type_)
}

fn bind_field_access<'a, 'b>(
    span: TextLocation,
    field_access: FieldAccessNodeKind,
    binder: &mut BindingState<'b>,
) -> BoundNode {
    let base_location = field_access.base.location;
    let field = field_access.field;
    let base = bind_node(*field_access.base, binder);
    match binder.project.types[base.type_].clone() {
        Type::Error => base,
        Type::Struct(_) => field_access_in_struct(field, base.type_, base, binder, span),
        Type::Library(index) => {
            let library = &binder.libraries[index];
            let function_name = binder.lexeme(field);
            if let Some(function) = library.look_up_function_by_name(function_name) {
                BoundNode::label_reference(
                    span,
                    function.function_label as _,
                    function.function_type,
                )
            } else {
                binder.diagnostic_bag.report_no_field_named_on_type(
                    span,
                    field.location,
                    binder
                        .project
                        .types
                        .look_up_or_add_type(Type::Library(index)),
                );
                BoundNode::error(span)
            }
        }
        Type::Enum(_name, values) => {
            let value = values
                .iter()
                .enumerate()
                .find(|e| e.1 == binder.lexeme(field));
            match value {
                Some((literal_value, _)) => {
                    BoundNode::literal(span, Value::EnumValue(literal_value, base.type_))
                }
                None => {
                    binder.diagnostic_bag.report_no_field_named_on_type(
                        span,
                        field.location,
                        base.type_,
                    );
                    BoundNode::error(span)
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
        | Type::GenericType
        | Type::PointerOf(_)
        | Type::Noneable(_) => {
            binder
                .diagnostic_bag
                .report_no_fields_on_type(base_location, base.type_);
            BoundNode::error(span)
        }
        Type::String => {
            if binder.lexeme(field) == "length" {
                let base = bind_conversion(base, typeid!(Type::Any), binder);
                let function_type = FunctionType::system_call(SystemCallKind::ArrayLength);
                let type_ = binder
                    .project
                    .types
                    .look_up_or_add_type(Type::closure(function_type, vec![base.type_]));
                BoundNode::system_call_closure(span, vec![base], SystemCallKind::ArrayLength, type_)
            } else {
                binder.diagnostic_bag.report_no_field_named_on_type(
                    span,
                    field.location,
                    base.type_,
                );
                BoundNode::error(span)
            }
        }
        Type::StructPlaceholder(name, ..) => unreachable!(
            "Internal Compiler error, struct placeholder '{}' should already be resolved.",
            name
        ),
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
                        current_type = binder.project.types[current_type]
                            .as_struct_type()
                            .unwrap()
                            .parent
                            .expect("Failed to resolve a function.");
                        continue;
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
        | Type::Noneable(_)
        | Type::Function(_)
        | Type::Pointer
        | Type::PointerOf(_)
        | Type::GenericType
        | Type::Closure(_) => {
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
        Type::StructPlaceholder(name, ..) => unreachable!(
            "Internal Compiler error, struct placeholder '{}' should already be resolved.",
            name
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
    span: TextLocation,
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
            span,
            arguments.len(),
            parameter_types.len(),
        );
    }

    let mut struct_type = None;

    for (argument, parameter_type) in arguments.into_iter().zip(parameter_types.iter().copied()) {
        let mut argument = bind_node(argument, binder);
        if argument.type_ == typeid!(Type::Void) {
            binder
                .diagnostic_bag
                .report_invalid_void_expression(argument.location);
            result.push(BoundNode::error(argument.location));
            continue;
        }
        let parameter_type = if parameter_type == typeid!(Type::GenericType) {
            struct_type = if binder.generic_type.is_none() {
                let _struct_type =
                    bind_generic_struct_type_for_type(struct_id, argument.type_, binder);
                binder.generic_type = Some(argument.type_.clone());
                todo!("Update label!");
                // Some(_struct_type)
            } else {
                struct_type
            };
            binder.generic_type.clone().unwrap()
        } else {
            parameter_type.clone()
        };
        argument = bind_conversion(argument, parameter_type, binder);
        result.push(argument);
    }
    binder.generic_type = None;
    (result, label, struct_type.unwrap())
}

// TODO: Move this to after the binding.
fn bind_generic_function_for_type(
    generic_function: &GenericFunction,
    label: usize,
    function_label_map: &HashMap<usize, usize>,
    type_: TypeId,
    this_type: TypeId,
    name: &str,
    binder: &mut BindingState<'_>,
) -> TypeId {
    let function_type = binder.project.types[generic_function.function_type]
        .as_function_type()
        .unwrap();
    binder
        .project
        .types
        .maybe_print_type_table(binder.project.debug_flags);
    let parameter_types: Vec<_> = function_type
        .parameter_types
        .iter()
        .copied()
        .map(|t| {
            binder
                .project
                .types
                .replace_in_type_for_type(t, typeid!(Type::GenericType), type_)
        })
        .collect();
    let return_type = binder.project.types.replace_in_type_for_type(
        function_type.return_type,
        typeid!(Type::GenericType),
        type_,
    );
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
    binder.register_generated_constant(name.into(), Value::LabelPointer(label, function_type));
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
        n.type_ = binder.project.types.replace_in_type_for_type(
            n.type_,
            typeid!(Type::GenericType),
            type_,
        );
        match &mut n.kind {
            BoundNodeKind::Label(it) if label_relocations.contains_key(it) => {
                *it = label_relocations[it]
            }
            BoundNodeKind::LabelReference(it) if label_relocations.contains_key(it) => {
                *it = label_relocations[it]
            }
            _ => {}
        }
    });
    let function = BoundNode::function_declaration(label, false, body, parameters, None);
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
        Type::Noneable(base_type) => {
            let safe_type = *base_type;
            let node = base.clone();
            (base, SafeNodeInCondition::IfBody(node, safe_type))
        }
        Type::Boolean => {
            let safe_node = register_contained_safe_nodes(&base, binder);
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
        | Type::Pointer
        | Type::PointerOf(_)
        | Type::Enum(..) => (
            bind_conversion(base, typeid!(Type::Boolean), binder),
            SafeNodeInCondition::None,
        ),
        Type::Library(_) | Type::GenericType => unimplemented!(),
        Type::StructPlaceholder(name, ..) => unreachable!(
            "Internal Compiler error, struct placeholder '{}' should already be resolved.",
            name
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
                Type::Noneable(base_type) => {
                    SafeNodeInCondition::ElseBody(*unary.operand.clone(), *base_type)
                }
                _ => SafeNodeInCondition::None,
            }
        }
        BoundNodeKind::BinaryExpression(binary)
            if binary.operator_token == BoundBinaryOperator::Equals
                && matches!(
                    (
                        &binder.project.types[binary.lhs.type_],
                        &binder.project.types[binary.rhs.type_]
                    ),
                    (Type::Noneable(_), Type::Noneable(_))
                ) =>
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
                && matches!(
                    (
                        &binder.project.types[binary.lhs.type_],
                        &binder.project.types[binary.rhs.type_]
                    ),
                    (Type::Noneable(_), Type::Noneable(_))
                ) =>
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
        _ => {
            if let Type::Noneable(base_type) = &binder.project.types[base.type_] {
                SafeNodeInCondition::IfBody(base.clone(), *base_type)
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
