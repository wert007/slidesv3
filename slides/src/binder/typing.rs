use std::{fmt::Display, ops::Index, borrow::Cow};

use num_enum::TryFromPrimitive;

use crate::evaluator::memory::WORD_SIZE_IN_BYTES;

use super::{symbols::StructFunctionTable, SimpleStructFunctionTable};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(u64);

impl TypeId {
    pub unsafe fn from_raw(id: u64) -> Self {
        Self(id)
    }

    pub(crate) fn as_raw(&self) -> u64 {
        self.0
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "type#{}", self.0)
    }
}

macro_rules! typeid {
    (Type::Error) => {
        unsafe { TypeId::from_raw(0) }
    };
    (Type::Void) => {
        unsafe { TypeId::from_raw(1) }
    };
    (Type::Any) => {
        unsafe { TypeId::from_raw(2) }
    };
    (Type::Boolean) => {
        unsafe { TypeId::from_raw(3) }
    };
    (Type::String) => {
        unsafe { TypeId::from_raw(4) }
    };
    (Type::Pointer) => {
        unsafe { TypeId::from_raw(5) }
    };
    (Type::IntegerLiteral) => {
        unsafe { TypeId::from_raw(6) }
    };
    (Type::Integer(IntegerType::Signed64)) => {
        unsafe { TypeId::from_raw(7) }
    };
    (Type::Integer(IntegerType::Unsigned8)) => {
        unsafe { TypeId::from_raw(8) }
    };
    (Type::Integer(IntegerType::Unsigned64)) => {
        unsafe { TypeId::from_raw(9) }
    };
    (Type::GenericType) => {
        unsafe { TypeId::from_raw(10) }
    };
    (Type::None) => {
        unsafe { TypeId::from_raw(11) }
    };
    (Type::SystemCall($kind:expr)) => {
        unsafe {
            TypeId::from_raw(match $kind {
                SystemCallKind::Print => 12,
                SystemCallKind::ToString => 13,
                SystemCallKind::ArrayLength => 14,
                SystemCallKind::HeapDump => 15,
                SystemCallKind::Break => 16,
                SystemCallKind::Reallocate => 17,
                SystemCallKind::RuntimeError => 18,
                SystemCallKind::AddressOf => 19,
                SystemCallKind::GarbageCollect => 20,
            })
        }
    };
    ($($ignore:tt)*) => {
        compile_error!("Invalid type supplied!")
    };
}

#[derive(Debug, Clone)]
pub struct TypeCollection {
    types: Vec<Type>,
}

impl TypeCollection {
    pub fn new() -> Self {
        Self {
            types: vec![
                Type::Error,
                Type::Void,
                Type::Any,
                Type::Boolean,
                Type::String,
                Type::Pointer,
                Type::IntegerLiteral,
                Type::Integer(IntegerType::Signed64),
                Type::Integer(IntegerType::Unsigned8),
                Type::Integer(IntegerType::Unsigned64),
                Type::GenericType,
                Type::None,
                Type::SystemCall(SystemCallKind::Print),
                Type::SystemCall(SystemCallKind::ToString),
                Type::SystemCall(SystemCallKind::ArrayLength),
                Type::SystemCall(SystemCallKind::HeapDump),
                Type::SystemCall(SystemCallKind::Break),
                Type::SystemCall(SystemCallKind::Reallocate),
                Type::SystemCall(SystemCallKind::RuntimeError),
                Type::SystemCall(SystemCallKind::AddressOf),
                Type::SystemCall(SystemCallKind::GarbageCollect),
            ],
        }
    }

    pub fn look_up_type(&self, type_: &Type) -> Option<TypeId> {
        self.types
            .iter()
            .position(|t| t == type_)
            .map(|i| TypeId(i as u64))
    }

    pub fn look_up_or_add_type(&mut self, type_: Type) -> TypeId {
        let id = match self.types.iter().position(|t| t == &type_) {
            Some(it) => it,
            None => {
                let result = self.types.len();
                self.types.push(type_);
                result
            }
        };
        TypeId(id as u64)
    }

    pub fn look_up_type_by_name(&self, name: &str) -> Option<TypeId> {
        self.types
            .iter()
            .position(|t| self.name_of_type(t) == name)
            .map(|i| TypeId(i as u64))
    }

    pub fn name(&self, type_id: TypeId) -> Cow<str> {
        self.name_of_type(&self[type_id])
    }

    pub fn create_noneable_version(&mut self, id: TypeId) -> TypeId {
        self.look_up_or_add_type(Type::Noneable(id))
    }

    pub fn create_pointer_of_version(&mut self, id: TypeId) -> TypeId {
        self.look_up_or_add_type(Type::PointerOf(id))
    }

    pub fn can_be_converted(&self, from_id: TypeId, to_id: TypeId) -> bool {
        let from = &self[from_id];
        let to = &self[to_id];
        match (from, to) {
            (Type::Library(_), _) | (_, Type::Library(_)) => false,
            _ if from_id == to_id => true,
            (_, Type::Any) => true,
            (Type::Pointer, Type::PointerOf(_)) => true,
            (Type::PointerOf(_), Type::Pointer) => true,
            (Type::None, Type::PointerOf(_)) => true,
            (Type::None, Type::Pointer) => true,
            (Type::None, Type::Noneable(_)) => true,
            (_, Type::Noneable(other)) => self.can_be_converted(from_id, *other),
            (Type::TypedGenericStruct(id), Type::Struct(other)) if id.struct_type == **other => true,
            (Type::Struct(id), Type::TypedGenericStruct(other)) if **id == other.struct_type => true,
            // FIXME: This means that 9999 would be a valid u8?
            (Type::IntegerLiteral, Type::Integer(_)) => true,
            (Type::Integer(from_integer_type), Type::Integer(to_integer_type)) => {
                from_integer_type.to_signed() == *to_integer_type
            }
            _ => false,
        }
    }

    pub fn can_be_casted_to(&self, from_id: TypeId, to_id: TypeId) -> bool {
        let from = &self[from_id];
        let to = &self[to_id];
        match (from, to) {
            (a, b) if a == b => false,
            (Type::Any, _) => true,
            // FIXME: Can it though?
            // (Type::Pointer, Type::Integer) => true,
            (Type::PointerOf(inner), Type::Noneable(other)) => self.can_be_converted(*inner, *other),
            (Type::Noneable(base_type), _) => self.can_be_converted(*base_type, to_id),
            (Type::Integer(signed_type), Type::Integer(unsigned_type))
                if signed_type.is_signed()
                    && !unsigned_type.is_signed()
                    && unsigned_type.to_signed() == *signed_type =>
            {
                true
            }
            _ => false,
        }
    }

    fn name_of_type<'a>(&self, type_: &'a Type) -> Cow<'a, str> {
        match type_ {
            Type::Error => "error".into(),
            Type::Void => "void".into(),
            Type::Any => "any".into(),
            Type::IntegerLiteral => "integer literal".into(),
            Type::Integer(int_type) => format!("{int_type}").into(),
            Type::Boolean => "bool".into(),
            Type::None => "any?".into(),
            Type::SystemCall(call) => format!("{call}").into(),
            Type::Noneable(inner) => format!("{}?", self.name(*inner)).into(),
            Type::String => "string".into(),
            Type::Function(_) => "function".into(),
            Type::Closure(_) => "closure".into(),
            Type::Struct(s) => format!("{}", s.name).into(),
            Type::Library(_) => "library".into(),
            Type::Pointer => "ptr".into(),
            Type::PointerOf(inner) => format!("&{}", self.name(*inner)).into(),
            Type::GenericType => "$Type".into(),
            Type::TypedGenericStruct(_) => format!("????").into(),
            Type::Enum(name, _) => name.into(),
            Type::StructPlaceholder(name, _) => name.into(),
        }
    }

    pub(crate) fn add_type(&mut self, type_: Type) -> Result<TypeId, ()> {
        if self.look_up_type_by_name(&self.name_of_type(&type_)).is_some() {
            return Err(());
        }
        Ok(self.look_up_or_add_type(type_))
    }
}

impl Index<TypeId> for TypeCollection {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index.0 as usize]
    }
}

#[derive(TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum IntegerType {
    Signed64,
    Unsigned8,
    Unsigned64,
}

impl IntegerType {
    pub fn size_in_bytes(&self) -> u64 {
        match self {
            IntegerType::Signed64 => 8,
            IntegerType::Unsigned8 => 1,
            IntegerType::Unsigned64 => 8,
        }
    }

    pub fn to_signed(&self) -> IntegerType {
        match self {
            IntegerType::Signed64 => *self,
            IntegerType::Unsigned8 => todo!(),
            IntegerType::Unsigned64 => Self::Signed64,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            IntegerType::Signed64 => true,
            IntegerType::Unsigned8 => false,
            IntegerType::Unsigned64 => false,
        }
    }

    pub fn equals_ignoring_sign(&self, other: &Self) -> bool {
        self.to_signed() == other.to_signed()
    }
}

impl std::fmt::Display for IntegerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerType::Signed64 => write!(f, "int"),
            IntegerType::Unsigned8 => write!(f, "byte"),
            IntegerType::Unsigned64 => write!(f, "uint"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Error,
    Void,
    Any,
    IntegerLiteral,
    Integer(IntegerType),
    Boolean,
    None,
    SystemCall(SystemCallKind),
    Noneable(TypeId),
    String,
    Function(Box<FunctionType>),
    Closure(Box<ClosureType>),
    Struct(Box<StructType>),
    Library(usize),
    Pointer,
    PointerOf(TypeId),
    GenericType,
    StructPlaceholder(String, SimpleStructFunctionTable),
    // FIXME: This is only used to save the type of the generic array struct
    // during binding in the variable declaration. If this stays this way there
    // is probably a better solution to store that type then this type here.
    TypedGenericStruct(Box<TypedGenericStructType>),
    Enum(String, Vec<String>),
}

impl Type {
    pub fn function(function_type: FunctionType) -> Self {
        Self::Function(Box::new(function_type))
    }

    pub fn closure(base_function_type: FunctionType) -> Self {
        Self::Closure(Box::new(base_function_type.into()))
    }

    pub fn convert_typed_generic_struct_to_struct(self) -> Self {
        if let Type::TypedGenericStruct(typed_generic_struct_type) = self {
            Type::Struct(Box::new(typed_generic_struct_type.struct_type))
        } else {
            self
        }
    }

    pub fn noneable_base_type(&self) -> Option<TypeId> {
        if let Self::Noneable(v) = self {
            Some(*v)
        } else {
            None
        }
    }

    pub fn type_identifier_size_in_words(&self) -> u64 {
        1
    }

    pub fn size_in_bytes(&self) -> u64 {
        match self {
            Type::Library(_) => panic!("Libraries should only be accessed during binding!"),
            Type::Any => unreachable!(),
            Type::StructPlaceholder(..) => panic!("This is only a placeholder type!"),
            Type::Enum(..) => todo!("Implement enums at runtime"),
            Type::Error => 0,
            Type::Void => 0,
            Type::Integer(integer_type) => integer_type.size_in_bytes(),
            Type::None
            | Type::Struct(_)
            | Type::TypedGenericStruct(_)
            | Type::Function(_)
            | Type::Closure(_)
            | Type::IntegerLiteral
            | Type::Boolean
            | Type::SystemCall(_)
            | Type::Noneable(_)
            | Type::Pointer
            | Type::PointerOf(_)
            | Type::GenericType
            | Type::String => WORD_SIZE_IN_BYTES,
        }
    }

    pub fn array_element_size_in_bytes(&self) -> u64 {
        match self {
            Type::Library(_) => panic!("Libraries should only be accessed during binding!"),
            Type::GenericType => panic!("Generic Types should only be accessed during binding!"),
            Type::Void | Type::Any | Type::Error => unreachable!(),
            Type::String => 1,
            _ => unreachable!("TODO: String should be a struct I think, then this function would not be needed anymore at all!"),
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Library(_) => panic!("Libraries should only be accessed during binding!"),
            Type::GenericType => panic!("Generic Types should only be accessed during binding!"),
            Type::Enum(..) => todo!("Implement enums at runtime"),
            Type::StructPlaceholder(..) => panic!("This is only a placeholder type!"),
            Type::Error
            | Type::Void
            | Type::Any
            // Technically a pointer. But it does not get dereferenced,
            // instead the value itself is assigned to the program counter.
            | Type::Function(_)
            | Type::SystemCall(_)
            | Type::Integer(_)
            | Type::IntegerLiteral
            | Type::Boolean => false,
            // A none Pointer should never be dereferenced.
            Type::None
            | Type::Noneable(_)
            | Type::String
            | Type::Closure(_)
            | Type::Struct(_)
            | Type::TypedGenericStruct(_)
            | Type::Pointer
            | Type::PointerOf(_) => true,
        }
    }

    pub(crate) fn as_struct_type(&self) -> Option<&StructType> {
        if let Type::Struct(it) = self {
            Some(it)
        } else if let Type::TypedGenericStruct(it) = self {
            Some(&it.struct_type)
        } else {
            None
        }
    }
}

#[derive(TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum SystemCallKind {
    Print,
    ToString,
    ArrayLength,
    HeapDump,
    Break,
    Reallocate,
    RuntimeError,
    AddressOf,
    GarbageCollect,
}

impl std::fmt::Display for SystemCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SystemCallKind::Print => "print",
                SystemCallKind::ToString => "toString",
                SystemCallKind::ArrayLength => "array$length",
                SystemCallKind::HeapDump => "heapDump",
                SystemCallKind::Break => "break",
                SystemCallKind::Reallocate => "reallocate",
                SystemCallKind::RuntimeError => "runtimeError",
                SystemCallKind::AddressOf => "addressOf",
                SystemCallKind::GarbageCollect => "garbageCollect",
            }
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionType {
    pub parameter_types: Vec<TypeId>,
    pub this_type: Option<TypeId>,
    pub return_type: TypeId,
    pub system_call_kind: Option<SystemCallKind>,
    pub is_generic: bool,
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut is_first = true;
        for parameter in &self.parameter_types {
            if !is_first {
                write!(f, ", ")?;
            }
            is_first = false;
            write!(f, "{}", parameter)?;
        }
        write!(f, ")")?;
        if self.return_type != typeid!(Type::Void) {
            write!(f, " -> {}", self.return_type)?;
        }
        Ok(())
    }
}

impl FunctionType {
    pub fn error() -> Self {
        Self {
            parameter_types: vec![],
            this_type: None,
            return_type: typeid!(Type::Void),
            system_call_kind: None,
            is_generic: false,
        }
    }

    pub fn system_call(system_call_kind: SystemCallKind) -> Self {
        match system_call_kind {
            SystemCallKind::Print => Self {
                parameter_types: vec![typeid!(Type::Any)],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::ToString => Self {
                parameter_types: vec![typeid!(Type::Any)],
                this_type: None,
                return_type: typeid!(Type::String),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::ArrayLength => Self {
                parameter_types: vec![],
                this_type: Some(typeid!(Type::Any)),
                return_type: typeid!(Type::Integer(IntegerType::Unsigned64)),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::HeapDump => Self {
                parameter_types: vec![typeid!(Type::String)],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::Break => Self {
                parameter_types: vec![],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::Reallocate => Self {
                parameter_types: vec![
                    typeid!(Type::Pointer),
                    typeid!(Type::Integer(IntegerType::Unsigned64)),
                ],
                this_type: None,
                return_type: typeid!(Type::Pointer),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::RuntimeError => Self {
                parameter_types: vec![typeid!(Type::String)],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::AddressOf => Self {
                parameter_types: vec![typeid!(Type::Pointer)],
                this_type: None,
                return_type: typeid!(Type::Integer(IntegerType::Unsigned64)),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::GarbageCollect => Self {
                parameter_types: vec![],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
        }
    }

    pub fn function(
        parameter_types: Vec<TypeId>,
        this_type: Option<TypeId>,
        return_type: TypeId,
        is_generic: bool,
    ) -> Self {
        Self {
            parameter_types,
            this_type,
            return_type,
            system_call_kind: None,
            is_generic,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ClosureType {
    pub base_function_type: FunctionType,
}

impl From<FunctionType> for ClosureType {
    fn from(mut it: FunctionType) -> Self {
        it.this_type = None;
        Self {
            base_function_type: it,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct StructType {
    pub name: String,
    pub fields: Vec<Member>,
    pub functions: Vec<Member>,
    pub function_table: StructFunctionTable,
    pub is_generic: bool,
    pub parent: Option<TypeId>,
    pub size_in_bytes: u64,
}

impl StructType {
    fn parent<'this, 'types: 'this>(&'this self, types: &'types TypeCollection) -> Option<&'this StructType> {
        self.parent.map(|p| types[p].as_struct_type().expect("Parent must be a struct!"))
    }

    pub(crate) fn fields_for_constructor<'this, 'types: 'this>(&'this self, types: &'types TypeCollection) -> Vec<&'this Member> {
        let mut result = if let Some(parent) = self.parent(types) {
            parent.fields_for_constructor(types)
        } else {
            Vec::new()
        };
        result.extend(&self.fields);
        result
    }

    pub(crate) fn member_fields_first<'this, 'types: 'this>(&'this self, field_name: &str, types: &'types TypeCollection) -> Option<&'this Member> {
        self.lookup_field_by_name(field_name, types).or_else(|| self.lookup_function_by_name(field_name, types))
    }

    pub(crate) fn member_functions_first<'this, 'types: 'this>(&'this self, field_name: &str, types: &'types TypeCollection) -> Option<&'this Member> {
        self.lookup_function_by_name(field_name, types).or_else(|| self.lookup_field_by_name(field_name, types))
    }

    fn lookup_field_by_name<'this, 'types: 'this>(&'this self, field_name: &str, types: &'types TypeCollection) -> Option<&'this Member> {
        self.fields.iter().find(|f| f.name == field_name).or_else(||self.parent(types).map(|p| p.lookup_field_by_name(field_name, types)).flatten())
    }

    fn lookup_function_by_name<'this, 'types: 'this>(&'this self, field_name: &str, types: &'types TypeCollection) -> Option<&'this Member> {
        self.functions.iter().find(|f| f.name == field_name).or_else(||self.parent(types).map(|p| p.lookup_function_by_name(field_name, types)).flatten())
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum MemberOffsetOrAddress {
    Offset(usize),
    Address(usize),
}

impl MemberOffsetOrAddress {
    pub(crate) fn unwrap_address(&self) -> usize {
        if let Self::Address(it) = self {
            *it
        } else {
            panic!("Expected address, but found offset!");
        }
    }

    pub(crate) fn unwrap_offset(&self) -> usize {
        if let Self::Offset(it) = self {
            *it
        } else {
            panic!("Expected offset, but found address!");
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Member {
    pub name: String,
    pub type_: TypeId,
    pub offset_or_address: MemberOffsetOrAddress,
    pub is_read_only: bool,
}

// impl StructType {
//     pub fn size_in_bytes(&self) -> u64 {
//         let mut result = 0;
//         for field in &self.fields {
//             // FIXME: This is only true if all sizes are a multiple of words.
//             result += field.size_in_bytes();
//         }
//         result
//     }
// }

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.name)?;
        for field in &self.fields {
            writeln!(f, "{}: {};", field.name, field.type_)?;
        }
        write!(f, "}}")
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct StructReferenceType {
    pub id: u64,
    pub simple_function_table: SimpleStructFunctionTable,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct TypedGenericStructType {
    pub type_: TypeId,
    pub struct_type: StructType,
    pub function_table: StructFunctionTable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    FunctionId(u64),
    SystemCall(SystemCallKind),
    LabelReference(usize),
}

impl std::fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::FunctionId(id) => write!(f, "fn#{}", id),
            FunctionKind::SystemCall(kind) => write!(f, "{}", kind),
            FunctionKind::LabelReference(label_reference) => write!(f, "L{:X}", label_reference),
        }
    }
}
