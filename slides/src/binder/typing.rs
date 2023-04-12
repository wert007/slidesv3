use std::{borrow::Cow, fmt::Display, ops::Index};

use num_enum::TryFromPrimitive;

use crate::{evaluator::memory::WORD_SIZE_IN_BYTES, DebugFlags};

use super::{
    symbols::{GenericFunction, StructFunctionTable},
    SimpleStructFunctionTable,
};

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

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct GenericTypeId(u64);

impl GenericTypeId {
    pub unsafe fn from_raw(id: u64) -> Self {
        Self(id)
    }
}

impl Display for GenericTypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "generic type#{}", self.0)
    }
}

pub enum TypeOrGenericType {
    Type(Type),
    GenericType(GenericType),
}

impl TypeCollectionIndexOutput<TypeOrGenericTypeId> for TypeOrGenericType {
    fn as_struct_type(&self) -> Option<&StructType> {
        match self {
            TypeOrGenericType::Type(t) => t.as_struct_type(),
            TypeOrGenericType::GenericType(gt) => gt.as_struct_type(),
        }
    }

    fn as_function_type(&self) -> Option<FunctionType<TypeOrGenericTypeId>> {
        match self {
            TypeOrGenericType::Type(it) => it.as_function_type().map(|it| it.into()),
            TypeOrGenericType::GenericType(it) => it.as_function_type().map(|it| it.into()),
        }
    }
}

impl From<Type> for TypeOrGenericType {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}

impl From<GenericType> for TypeOrGenericType {
    fn from(value: GenericType) -> Self {
        Self::GenericType(value)
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeOrGenericTypeId {
    Type(TypeId),
    GenericType(GenericTypeId),
}

impl TypeOrGenericTypeId {
    pub(crate) fn unwrap_type_id(&self) -> TypeId {
        if let Self::Type(it) = self {
            *it
        } else {
            panic!("Expected type id, but found generic type id!");
        }
    }

    pub(crate) fn unwrap_generic_type_id(&self) -> GenericTypeId {
        if let Self::GenericType(it) = self {
            *it
        } else {
            panic!("Expected generic type id, but found type id!");
        }
    }

    /// This converts [`GenericTypeId`]s to [`TypeId`]s via the
    /// [`TypeCollection::to_fake_type_id`] function.
    pub(crate) fn convert_to_type_id(&self, types: &mut TypeCollection) -> TypeId {
        match self {
            TypeOrGenericTypeId::Type(it) => *it,
            TypeOrGenericTypeId::GenericType(it) => types.to_fake_type_id(*it),
        }
    }
}

impl From<TypeId> for TypeOrGenericTypeId {
    fn from(value: TypeId) -> Self {
        Self::Type(value)
    }
}

impl From<GenericTypeId> for TypeOrGenericTypeId {
    fn from(value: GenericTypeId) -> Self {
        Self::GenericType(value)
    }
}

pub trait TypeCollectionIndexOutput<T> {
    fn as_struct_type(&self) -> Option<&StructType>;
    fn as_function_type(&self) -> Option<FunctionType<T>>;
}

pub trait TypeCollectionIndex
where
    Self::Output: TypeCollectionIndexOutput<Self>,
    Self: Sized + Copy,
{
    type Output;

    fn get_element<'a>(&self, types: &'a TypeCollection) -> &'a Self::Output;
}

impl TypeCollectionIndex for TypeId {
    type Output = Type;

    fn get_element<'a>(&self, types: &'a TypeCollection) -> &'a Self::Output {
        &types.types[self.0 as usize]
    }
}

impl TypeCollectionIndex for GenericTypeId {
    type Output = GenericType;

    fn get_element<'a>(&self, types: &'a TypeCollection) -> &'a Self::Output {
        &types.generic_types[self.0 as usize]
    }
}

impl TypeCollectionIndex for TypeOrGenericTypeId {
    type Output = TypeOrGenericType;

    fn get_element<'a>(&self, types: &'a TypeCollection) -> &'a Self::Output {
        let owned: TypeOrGenericType = match *self {
            TypeOrGenericTypeId::Type(it) => types[it].clone().into(),
            TypeOrGenericTypeId::GenericType(it) => types[it].clone().into(),
        };
        // TODO: Remove leakage here!
        let leaked = Box::new(owned);
        Box::leak(leaked)
        // panic!("Find better trait constraints, then these...")
    }
}

impl<T: TypeCollectionIndex> Index<T> for TypeCollection {
    type Output = T::Output;

    fn index(&self, index: T) -> &Self::Output {
        index.get_element(self)
    }
}

#[derive(Debug, Clone)]
pub struct TypeCollection {
    pub types: Vec<Type>,
    pub generic_types: Vec<GenericType>,
}

impl TypeCollection {
    pub fn new() -> Self {
        Self {
            generic_types: Vec::new(),
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
                // if result == 30 {
                //     panic!();
                // }
                result
            }
        };
        TypeId(id as u64)
    }

    pub fn look_up_or_add_generic_type(&mut self, generic_type: GenericType) -> GenericTypeId {
        let id = match self.generic_types.iter().position(|t| t == &generic_type) {
            Some(it) => it,
            None => {
                let result = self.generic_types.len();
                self.generic_types.push(generic_type);
                result
            }
        };
        GenericTypeId(id as u64)
    }

    pub fn look_up_type_by_name(&self, name: &str) -> Option<TypeOrGenericTypeId> {
        self.generic_types
            .iter()
            .position(|t| self.name_of_generic_type(t) == name)
            .map(|i| GenericTypeId(i as u64).into())
            .or_else(|| {
                self.types
                    .iter()
                    .position(|t| self.name_of_type(t) == name)
                    .map(|i| TypeId(i as u64).into())
            })
    }

    pub fn name(&self, type_id: TypeId) -> Cow<str> {
        self.name_of_type(&self[type_id])
    }

    pub fn name_of_generic_type_id(&self, generic_type_id: GenericTypeId) -> Cow<str> {
        self.name_of_generic_type(&self[generic_type_id])
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
            (Type::PointerOf(inner), Type::Noneable(other)) => {
                self.can_be_converted(*inner, *other)
            }
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
            Type::Function(f) => format!("function {}", f.display(self)).into(),
            Type::Closure(_) => "closure".into(),
            Type::Struct(s) => format!("{}", s.name).into(),
            Type::Library(_) => "library".into(),
            Type::Pointer => "ptr".into(),
            Type::PointerOf(inner) => format!("&{}", self.name(*inner)).into(),
            Type::GenericType => "$Type".into(),
            Type::Enum(name, _) => name.into(),
            Type::StructPlaceholder(name, _) => name.into(),
        }
    }

    fn name_of_type_debug<'a>(&self, type_: &'a Type) -> Cow<'a, str> {
        match type_ {
            Type::StructPlaceholder(name, _) => format!("placeholder {}", name).into(),
            type_ => self.name_of_type(type_),
        }
    }

    fn name_of_generic_type<'a>(&self, generic_type: &'a GenericType) -> Cow<'a, str> {
        match generic_type {
            GenericType::StructPlaceholder(name, _) => name.as_str().into(),
            GenericType::Struct(it, _) => it.name.as_str().into(),
            GenericType::Function(it) => it.name().into(),
        }
    }

    fn name_of_generic_type_debug<'a>(&self, generic_type: &'a GenericType) -> Cow<'a, str> {
        match generic_type {
            GenericType::StructPlaceholder(name, _) => format!("placeholder {}", name).into(),
            generic_type => self.name_of_generic_type(generic_type),
        }
    }

    pub(crate) fn add_type(&mut self, type_: Type) -> Result<TypeId, ()> {
        if self
            .look_up_type_by_name(&self.name_of_type(&type_))
            .is_some()
        {
            return Err(());
        }
        Ok(self.look_up_or_add_type(type_))
    }

    pub(crate) fn add_generic_type(
        &mut self,
        generic_type: GenericType,
    ) -> Result<GenericTypeId, ()> {
        if self
            .look_up_type_by_name(&self.name_of_generic_type(&generic_type))
            .is_some()
        {
            return Err(());
        }
        Ok(self.look_up_or_add_generic_type(generic_type))
    }

    pub fn maybe_print_type_table(&self, debug_flags: DebugFlags) {
        if !debug_flags.print_type_table {
            return;
        }
        println!("=== There are {} types registered. ===", self.types.len());
        for (index, type_) in self.types.iter().enumerate() {
            println!("  {:2}. {}", index, self.name_of_type_debug(type_));
        }
        println!(
            "\n=== There are {} generic types registered. ===",
            self.generic_types.len()
        );
        for (index, generic_type) in self.generic_types.iter().enumerate() {
            println!(
                "  {:2}. {}",
                index,
                self.name_of_generic_type_debug(generic_type)
            );
        }
    }

    pub(crate) fn overwrite_type(&mut self, type_id: TypeId, new_type: Type) {
        self.types[type_id.0 as usize] = new_type;
    }

    pub(crate) fn overwrite_generic_type(
        &mut self,
        generic_type_id: GenericTypeId,
        new_generic_type: GenericType,
    ) {
        self.generic_types[generic_type_id.0 as usize] = new_generic_type;
    }

    pub(crate) fn add_generic_function_to_generic_struct(
        &mut self,
        generic_type: GenericTypeId,
        generic_function: GenericFunction,
    ) -> Result<(), ()> {
        if let GenericType::Struct(_, generic_functions) =
            &mut self.generic_types[generic_type.0 as usize]
        {
            generic_functions.push(generic_function);
            Ok(())
        } else {
            Err(())
        }
    }

    pub(crate) fn replace_in_type_for_type(
        &mut self,
        type_: TypeId,
        replace: TypeId,
        replace_with: TypeId,
    ) -> TypeId {
        if type_ == replace {
            return replace_with;
        }
        match &self[type_] {
            Type::StructPlaceholder(name, _) => {
                dbg!(name);
                todo!()
            }
            Type::Function(it) => {
                let it = it.clone();
                let parameter_types = it
                    .parameter_types
                    .into_iter()
                    .map(|t| self.replace_in_type_for_type(t, replace, replace_with))
                    .collect();
                let this_type = it
                    .this_type
                    .map(|t| self.replace_in_type_for_type(t, replace, replace_with));
                let return_type =
                    self.replace_in_type_for_type(it.return_type, replace, replace_with);
                self.look_up_or_add_type(Type::Function(FunctionType {
                    parameter_types,
                    this_type,
                    return_type,
                    system_call_kind: it.system_call_kind,
                    name: it.name,
                    is_generic: it.is_generic,
                }))
            }
            Type::Closure(_) => todo!(),
            Type::Noneable(it) => {
                let it = self.replace_in_type_for_type(*it, replace, replace_with);
                self.look_up_or_add_type(Type::Noneable(it))
            }
            Type::PointerOf(it) => {
                let it = self.replace_in_type_for_type(*it, replace, replace_with);
                self.look_up_or_add_type(Type::PointerOf(it))
            }
            Type::Struct(struct_type) if struct_type.applied_type.is_some() => {
                if Some(replace) == struct_type.applied_type {
                    self.look_up_type_by_name(&format!(
                        "{}<{}>",
                        // Get the base part of the generic struct
                        struct_type.name.split_once('<').unwrap().0,
                        self.name(replace_with)
                    ))
                    .expect(
                        "Convert the applied type of the struct in a better way then currently!",
                    )
                    .unwrap_type_id()
                } else {
                    type_
                }
            }
            _ => type_,
        }
    }

    /// Turns a generic struct into a generic struct with a generic type
    /// applied. E.g. `Array` becomes `Array<$Type>`, thus turning the
    /// GenericTypeId into a TypeId. This is used to bind parameters to a type.
    pub(crate) fn to_fake_type_id(&mut self, it: GenericTypeId) -> TypeId {
        match self[it].clone() {
            GenericType::StructPlaceholder(name, struct_function_table) => self
                .look_up_or_add_type(Type::StructPlaceholder(
                    format!("{}<$Type>", name),
                    struct_function_table,
                )),
            GenericType::Struct(struct_type, _) => {
                let struct_type = StructType {
                    name: format!("{}<$Type>", struct_type.name),
                    applied_type: Some(typeid!(Type::GenericType)),
                    ..struct_type
                };
                self.look_up_or_add_type(Type::Struct(struct_type))
            }
            GenericType::Function(function_type) => {
                let name = function_type.name.unwrap();
                let (struct_name, function_name) = name.split_once("::").unwrap();
                let name = format!("{}<$Type>::{}", struct_name, function_name);
                let function_type = FunctionType {
                    parameter_types: function_type.parameter_types,
                    this_type: function_type.this_type.map(|it| self.to_fake_type_id(it)),
                    return_type: function_type.return_type,
                    system_call_kind: function_type.system_call_kind,
                    name: Some(name.into()),
                    is_generic: false,
                };
                self.look_up_or_add_type(Type::Function(function_type))
            }
        }
    }

    pub(crate) fn for_each_type(&mut self, mut cb: impl FnMut(&mut Type)) {
        for ty in &mut self.types {
            cb(ty);
        }
    }

    pub(crate) fn for_each_generic_type(&mut self, mut cb: impl FnMut(&mut GenericType)) {
        for ty in &mut self.generic_types {
            cb(ty);
        }
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
pub enum GenericType {
    StructPlaceholder(String, SimpleStructFunctionTable),
    Struct(StructType, Vec<GenericFunction>),
    Function(FunctionType<GenericTypeId>),
}
impl GenericType {
    pub(crate) fn as_generic_functions(&self) -> Option<&[GenericFunction]> {
        match self {
            GenericType::Struct(_, it) => Some(it),
            _ => None,
        }
    }

    fn as_struct_type(&self) -> Option<&StructType> {
        match self {
            GenericType::Struct(it, _) => Some(it),
            _ => None,
        }
    }

    pub(crate) fn as_struct_type_mut(&mut self) -> Option<&mut StructType> {
        match self {
            GenericType::Struct(it, _) => Some(it),
            _ => None,
        }
    }
}

impl TypeCollectionIndexOutput<GenericTypeId> for GenericType {
    fn as_struct_type(&self) -> Option<&StructType> {
        match self {
            GenericType::Struct(it, _) => Some(it),
            _ => None,
        }
    }

    fn as_function_type(&self) -> Option<FunctionType<GenericTypeId>> {
        match self {
            GenericType::Function(it) => Some(it.clone()),
            _ => None,
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
    Function(FunctionType<TypeId>),
    Closure(Box<ClosureType>),
    Struct(StructType),
    Library(usize),
    Pointer,
    PointerOf(TypeId),
    GenericType,
    StructPlaceholder(String, SimpleStructFunctionTable),
    // FIXME: This is only used to save the type of the generic array struct
    // during binding in the variable declaration. If this stays this way there
    // is probably a better solution to store that type then this type here.
    // TypedGenericStruct(Box<TypedGenericStructType>),
    Enum(String, Vec<String>),
}

impl Type {
    pub fn closure(base_function_type: FunctionType<TypeId>) -> Self {
        Self::Closure(Box::new(base_function_type.into()))
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

    pub fn raw_size_in_bytes(&self) -> u64 {
        if let Type::Struct(s) = self {
            s.size_in_bytes
        } else {
            self.size_in_bytes()
        }
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
            | Type::Pointer
            | Type::PointerOf(_) => true,
        }
    }

    pub(crate) fn as_struct_type(&self) -> Option<&StructType> {
        if let Type::Struct(it) = self {
            Some(it)
        } else {
            None
        }
    }

    pub(crate) fn as_struct_type_mut(&mut self) -> Option<&mut StructType> {
        if let Type::Struct(it) = self {
            Some(it)
        } else {
            None
        }
    }

    pub(crate) fn as_function_type(&self) -> Option<FunctionType<TypeId>> {
        if let Type::Function(it) = self {
            Some(it.clone())
        } else if let Type::SystemCall(it) = self {
            Some(FunctionType::system_call(*it))
        } else {
            None
        }
    }
}

impl TypeCollectionIndexOutput<TypeId> for Type {
    fn as_struct_type(&self) -> Option<&StructType> {
        if let Type::Struct(it) = self {
            Some(it)
        } else {
            None
        }
    }

    fn as_function_type(&self) -> Option<FunctionType<TypeId>> {
        if let Type::Function(it) = self {
            Some(it.clone())
        } else if let Type::SystemCall(it) = self {
            Some(FunctionType::system_call(*it))
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
pub struct FunctionType<T> {
    pub parameter_types: Vec<TypeId>,
    pub this_type: Option<T>,
    pub return_type: TypeId,
    pub system_call_kind: Option<SystemCallKind>,
    pub name: Option<Cow<'static, str>>,
    pub is_generic: bool,
}

impl From<FunctionType<TypeId>> for FunctionType<TypeOrGenericTypeId> {
    fn from(value: FunctionType<TypeId>) -> Self {
        Self {
            this_type: value.this_type.map(|it| it.into()),
            parameter_types: value.parameter_types,
            return_type: value.return_type,
            system_call_kind: value.system_call_kind,
            name: value.name,
            is_generic: value.is_generic,
        }
    }
}

impl From<FunctionType<GenericTypeId>> for FunctionType<TypeOrGenericTypeId> {
    fn from(value: FunctionType<GenericTypeId>) -> Self {
        Self {
            this_type: value.this_type.map(|it| it.into()),
            parameter_types: value.parameter_types,
            return_type: value.return_type,
            system_call_kind: value.system_call_kind,
            name: value.name,
            is_generic: value.is_generic,
        }
    }
}

impl<T> std::fmt::Display for FunctionType<T> {
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

impl FunctionType<TypeId> {
    pub fn system_call(system_call_kind: SystemCallKind) -> FunctionType<TypeId> {
        match system_call_kind {
            SystemCallKind::Print => Self {
                parameter_types: vec![typeid!(Type::Any)],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                name: None,
                is_generic: false,
            },
            SystemCallKind::ToString => Self {
                parameter_types: vec![typeid!(Type::Any)],
                this_type: None,
                return_type: typeid!(Type::String),
                name: None,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::ArrayLength => Self {
                parameter_types: vec![],
                this_type: Some(typeid!(Type::Any)),
                return_type: typeid!(Type::Integer(IntegerType::Unsigned64)),
                system_call_kind: Some(system_call_kind),
                name: None,
                is_generic: false,
            },
            SystemCallKind::HeapDump => Self {
                parameter_types: vec![typeid!(Type::String)],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                name: None,
                is_generic: false,
            },
            SystemCallKind::Break => Self {
                parameter_types: vec![],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                name: None,
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
                name: None,
                is_generic: false,
            },
            SystemCallKind::RuntimeError => Self {
                parameter_types: vec![typeid!(Type::String)],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                name: None,
                is_generic: false,
            },
            SystemCallKind::AddressOf => Self {
                parameter_types: vec![typeid!(Type::Pointer)],
                this_type: None,
                return_type: typeid!(Type::Integer(IntegerType::Unsigned64)),
                system_call_kind: Some(system_call_kind),
                name: None,
                is_generic: false,
            },
            SystemCallKind::GarbageCollect => Self {
                parameter_types: vec![],
                this_type: None,
                return_type: typeid!(Type::Void),
                system_call_kind: Some(system_call_kind),
                name: None,
                is_generic: false,
            },
        }
    }
}

impl FunctionType<GenericTypeId> {
    pub(crate) fn to_fake_type_id(&self, types: &mut TypeCollection) -> Cow<FunctionType<TypeId>> {
        let result = FunctionType {
            parameter_types: self.parameter_types.clone(),
            this_type: self.this_type.map(|t| types.to_fake_type_id(t)),
            return_type: self.return_type,
            system_call_kind: self.system_call_kind,
            name: self.name.clone(),
            is_generic: self.is_generic,
        };
        Cow::Owned(result)
    }
}

impl<T> FunctionType<T> {
    pub fn error() -> Self {
        Self {
            parameter_types: vec![],
            this_type: None,
            return_type: typeid!(Type::Void),
            system_call_kind: None,
            name: Some("undefined function".into()),
            is_generic: false,
        }
    }

    pub fn function(
        parameter_types: Vec<TypeId>,
        this_type: Option<T>,
        name: Cow<'static, str>,
        return_type: TypeId,
        is_generic: bool,
    ) -> Self {
        Self {
            parameter_types,
            this_type,
            return_type,
            system_call_kind: None,
            name: Some(name),
            is_generic,
        }
    }

    pub fn name(&self) -> String {
        self.name
            .as_ref()
            .map(|n| n.clone().into_owned())
            .unwrap_or_else(|| self.system_call_kind.unwrap().to_string())
    }

    pub fn display(&self, types: &TypeCollection) -> String {
        use std::fmt::Write;
        let mut result = self.name();
        write!(result, "(").unwrap();
        let mut is_first = true;
        for parameter in self.parameter_types.iter().copied() {
            if !is_first {
                write!(result, ", ").unwrap();
            }
            is_first = false;
            write!(result, "{}", types.name(parameter)).unwrap();
        }
        write!(result, ")").unwrap();
        if self.return_type != typeid!(Type::Void) {
            write!(result, " -> {}", types.name(self.return_type)).unwrap();
        }
        result
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ClosureType {
    pub base_function_type: FunctionType<TypeId>,
}

impl From<FunctionType<TypeId>> for ClosureType {
    fn from(mut it: FunctionType<TypeId>) -> Self {
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
    /// If this struct was a generic struct, this is the type, that was applied
    /// to this generic struct.
    pub applied_type: Option<TypeId>,
}

impl StructType {
    fn parent<'this, 'types: 'this>(
        &'this self,
        types: &'types TypeCollection,
    ) -> Option<&'this StructType> {
        self.parent
            .map(|p| types[p].as_struct_type().expect("Parent must be a struct!"))
    }

    pub(crate) fn fields_for_constructor<'this, 'types: 'this>(
        &'this self,
        types: &'types TypeCollection,
    ) -> Vec<&'this Member> {
        let mut result = if let Some(parent) = self.parent(types) {
            parent.fields_for_constructor(types)
        } else {
            Vec::new()
        };
        result.extend(&self.fields);
        result
    }

    pub(crate) fn member_fields_first<'this, 'types: 'this>(
        &'this self,
        field_name: &str,
        types: &'types TypeCollection,
    ) -> Option<&'this Member> {
        self.lookup_field_by_name(field_name, types)
            .or_else(|| self.lookup_function_by_name(field_name, types))
    }

    pub(crate) fn member_functions_first<'this, 'types: 'this>(
        &'this self,
        field_name: &str,
        types: &'types TypeCollection,
    ) -> Option<&'this Member> {
        self.lookup_function_by_name(field_name, types)
            .or_else(|| self.lookup_field_by_name(field_name, types))
    }

    fn lookup_field_by_name<'this, 'types: 'this>(
        &'this self,
        field_name: &str,
        types: &'types TypeCollection,
    ) -> Option<&'this Member> {
        self.fields
            .iter()
            .find(|f| f.name == field_name)
            .or_else(|| {
                self.parent(types)
                    .map(|p| p.lookup_field_by_name(field_name, types))
                    .flatten()
            })
    }

    fn lookup_function_by_name<'this, 'types: 'this>(
        &'this self,
        field_name: &str,
        types: &'types TypeCollection,
    ) -> Option<&'this Member> {
        self.functions
            .iter()
            .find(|f| f.name == field_name)
            .or_else(|| {
                self.parent(types)
                    .map(|p| p.lookup_function_by_name(field_name, types))
                    .flatten()
            })
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum MemberOffsetOrAddress {
    Offset(usize),
    Address(usize),
}

impl MemberOffsetOrAddress {
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
