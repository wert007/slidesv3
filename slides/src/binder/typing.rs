#[cfg(test)]
mod tests;

use num_enum::TryFromPrimitive;

use crate::evaluator::memory::WORD_SIZE_IN_BYTES;

use super::{symbols::StructFunctionTable, SimpleStructFunctionTable};

#[derive(TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum IntegerType {
    Signed8,
    Signed64,
    Unsigned8,
    Unsigned64,
}

impl IntegerType {
    pub fn size_in_bytes(&self) -> u64 {
        match self {
            IntegerType::Signed8 => 1,
            IntegerType::Signed64 => 8,
            IntegerType::Unsigned8 => 1,
            IntegerType::Unsigned64 => 8,
        }
    }

    pub fn to_signed(&self) -> IntegerType {
        match self {
            IntegerType::Signed8 |
            IntegerType::Signed64 => *self,
            IntegerType::Unsigned8 => Self::Signed8,
            IntegerType::Unsigned64 => Self::Signed64,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            IntegerType::Signed8 => true,
            IntegerType::Signed64 => true,
            IntegerType::Unsigned8 => false,
            IntegerType::Unsigned64 => false,
        }
    }

    pub fn equals_ignoring_sign(&self, other: &Self) -> bool {
        self.to_signed() == other.to_signed()
    }

    fn can_be_converted_to(&self, other: IntegerType) -> bool {
        if self.to_signed() == other.to_signed() {
            !self.is_signed()
        }
        else if self.is_signed() == other.is_signed() {
            self.size_in_bytes() <= other.size_in_bytes()
        } else {
            false
        }
    }
}

impl std::fmt::Display for IntegerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerType::Signed8 => write!(f, "int8"),
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
    IgnoreTypeChecking,
    Any,
    IntegerLiteral,
    Integer(IntegerType),
    Boolean,
    None,
    SystemCall(SystemCallKind),
    Noneable(Box<Type>),
    Function(Box<FunctionType>),
    Closure(Box<ClosureType>),
    Struct(Box<StructType>),
    StructReference(StructReferenceType),
    Library(usize),
    Pointer,
    PointerOf(Box<Type>),
    GenericType,
    // FIXME: This is only used to save the type of the generic array struct
    // during binding in the variable declaration. If this stays this way there
    // is probably a better solution to store that type then this type here.
    TypedGenericStruct(Box<TypedGenericStructType>),
}

impl Type {
    pub fn function(function_type: FunctionType) -> Self {
        Self::Function(Box::new(function_type))
    }

    pub fn closure(base_function_type: FunctionType) -> Self {
        Self::Closure(Box::new(base_function_type.into()))
    }

    pub fn noneable(base_type: Type) -> Self {
        Self::Noneable(Box::new(base_type))
    }

    pub fn pointer_of(base_type: Type) -> Self {
        Self::PointerOf(Box::new(base_type))
    }

    pub fn convert_typed_generic_struct_to_struct(self) -> Self {
        if let Type::TypedGenericStruct(typed_generic_struct_type) = self {
            Type::Struct(Box::new(typed_generic_struct_type.struct_type	))
        } else {
            self
        }
    }

    pub fn can_be_converted_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Library(_), _) | (_, Type::Library(_)) => false,
            _ if self == other => true,
            (_, Type::Any) => true,
            (_, Type::IgnoreTypeChecking) => true,
            (Type::IgnoreTypeChecking, _) => true,
            (Type::Pointer, Type::PointerOf(_)) => true,
            (Type::PointerOf(_), Type::Pointer) => true,
            (Type::None, Type::PointerOf(_)) => true,
            (Type::None, Type::Pointer) => true,
            (Type::None, Type::Noneable(_)) => true,
            (type_, Type::Noneable(other)) => type_.can_be_converted_to(other),
            (Type::Struct(id), Type::StructReference(other_id)) if id.id == other_id.id => true,
            (Type::StructReference(id), Type::Struct(other)) if id.id == other.id => true,
            (Type::TypedGenericStruct(id), Type::Struct(other)) if id.id == other.id => true,
            (Type::TypedGenericStruct(id), Type::StructReference(other)) if id.id == other.id => true,
            (Type::StructReference(id), Type::TypedGenericStruct(other)) if id.id == other.id => true,
            (Type::Struct(id), Type::TypedGenericStruct(other)) if id.id == other.id => true,
            // FIXME: This means that 9999 would be a valid u8?
            (Type::IntegerLiteral, Type::Integer(_)) => true,
            (Type::Integer(from_integer_type), Type::Integer(to_integer_type)) => from_integer_type.can_be_converted_to(*to_integer_type),
            _ => false,
        }
    }

    pub fn can_be_casted_to(&self, other: &Type) -> bool {
        match (self, other) {
            (a, b) if a == b => false,
            (Type::Any, _) => true,
            // FIXME: Can it though?
            // (Type::Pointer, Type::Integer) => true,
            (Type::PointerOf(inner), Type::Noneable(other)) => inner.can_be_converted_to(other),
            (Type::Noneable(base_type), other) => base_type.can_be_converted_to(other),
            (Type::Integer(signed_type), Type::Integer(unsigned_type)) if signed_type.is_signed() && !unsigned_type.is_signed() && unsigned_type.to_signed() == *signed_type => true,
            _ => false,
        }
    }

    pub fn noneable_base_type(&self) -> Option<&Type> {
        if let Self::Noneable(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_struct_id(&self) -> Option<u64> {
        match self {
            Type::Struct(struct_type) => Some(struct_type.id),
            Type::StructReference(struct_reference_type) => Some(struct_reference_type.id),
            _ => None,
        }
    }

    pub fn type_identifier_size_in_words(&self) -> u64 {
        match self {
            Type::Library(_) => panic!("Libraries should only be accessed during binding!"),
            Type::GenericType => panic!("GenericTypes should only be accessed during binding!"),
            Type::IgnoreTypeChecking => panic!("IgnoreTypeChecking should only be accessed during binding!"),
            Type::Error => 1,
            Type::Void => 1,
            Type::Any => 1,
            Type::IntegerLiteral => 1,
            Type::Integer(_) => 1,
            Type::Boolean => 1,
            Type::None => 1,
            Type::SystemCall(_) => 1,
            Type::Pointer => 1,
            Type::PointerOf(base_type) => base_type.type_identifier_size_in_words() + 1,
            Type::Noneable(base_type) => base_type.type_identifier_size_in_words() + 1,
            Type::Function(function_type) => {
                let mut result = 2;
                result += &function_type
                    .this_type
                    .as_ref()
                    .unwrap_or(&Type::Void)
                    .type_identifier_size_in_words();
                result += function_type.return_type.type_identifier_size_in_words();
                for parameter in &function_type.parameter_types {
                    result += parameter.type_identifier_size_in_words();
                }
                result
            }
            Type::Closure(closure_type) => {
                let mut result = 2;
                result += closure_type
                    .base_function_type
                    .this_type
                    .as_ref()
                    .unwrap_or(&Type::Void)
                    .type_identifier_size_in_words();
                result += closure_type
                    .base_function_type
                    .return_type
                    .type_identifier_size_in_words();
                for parameter in &closure_type.base_function_type.parameter_types {
                    result += parameter.type_identifier_size_in_words();
                }
                result
            }
            Type::Struct(_) => 1,
            Type::StructReference(_) => 1,
            Type::TypedGenericStruct(_) => 1,
        }
    }

    pub const TYPE_IDENTIFIER_ERROR: u64 = 0;
    pub const TYPE_IDENTIFIER_VOID: u64 = 1;
    pub const TYPE_IDENTIFIER_ANY: u64 = 2;
    pub const TYPE_IDENTIFIER_BOOLEAN: u64 = 5;
    pub const TYPE_IDENTIFIER_NONE: u64 = 6;
    pub const TYPE_IDENTIFIER_NONEABLE: u64 = 7;
    pub const TYPE_IDENTIFIER_FUNCTION: u64 = 9;
    pub const TYPE_IDENTIFIER_CLOSURE: u64 = 10;
    pub const TYPE_IDENTIFIER_STRUCT: u64 = 11;
    pub const TYPE_IDENTIFIER_STRUCT_REFERENCE: u64 = 12;
    pub const TYPE_IDENTIFIER_TYPED_GENERIC_STRUCT: u64 = 13;
    pub const TYPE_IDENTIFIER_POINTER: u64 = 14;
    pub const TYPE_IDENTIFIER_POINTER_OF: u64 = 15;


    pub fn type_identifier_kind(&self) -> u64 {
        match self {
            Type::Library(_) => panic!("Libraries should only be accessed during binding!"),
            Type::GenericType => panic!("Generic Types should only be accessed during binding!"),
            Type::IgnoreTypeChecking => panic!("IgnoreTypeChecking should only be accessed during binding!"),
            Type::IntegerLiteral => panic!("Only literals have this type! And they should be bound to a specific type at some point!"),
            Type::Error => Self::TYPE_IDENTIFIER_ERROR,
            Type::Void => Self::TYPE_IDENTIFIER_VOID,
            Type::Any => Self::TYPE_IDENTIFIER_ANY,
            Type::Integer(integer_type) => (1 + *integer_type as u8 as u64) << 16,
            Type::Boolean => Self::TYPE_IDENTIFIER_BOOLEAN,
            Type::None => Self::TYPE_IDENTIFIER_NONE,
            Type::Noneable(_) => Self::TYPE_IDENTIFIER_NONEABLE,
            Type::Function(_) => Self::TYPE_IDENTIFIER_FUNCTION,
            Type::Closure(_) => Self::TYPE_IDENTIFIER_CLOSURE,
            Type::Struct(_) => Self::TYPE_IDENTIFIER_STRUCT,
            Type::StructReference(_) => Self::TYPE_IDENTIFIER_STRUCT_REFERENCE,
            Type::TypedGenericStruct(_) => Self::TYPE_IDENTIFIER_TYPED_GENERIC_STRUCT,
            Type::Pointer => Self::TYPE_IDENTIFIER_POINTER,
            Type::PointerOf(_) => Self::TYPE_IDENTIFIER_POINTER_OF,
            Type::SystemCall(system_call_kind) => (1 + *system_call_kind as u8 as u64) << 8,
        }
    }

    pub fn simple_type_from_type_identifier(type_identifier_kind: u64) -> Option<Self> {
        match type_identifier_kind {
            Self::TYPE_IDENTIFIER_ERROR => Some(Type::Error),
            Self::TYPE_IDENTIFIER_VOID => Some(Type::Void),
            Self::TYPE_IDENTIFIER_ANY => Some(Type::Any),
            Self::TYPE_IDENTIFIER_BOOLEAN => Some(Type::Boolean),
            Self::TYPE_IDENTIFIER_NONE => Some(Type::None),
            Self::TYPE_IDENTIFIER_NONEABLE => None,
            Self::TYPE_IDENTIFIER_FUNCTION => None,
            Self::TYPE_IDENTIFIER_CLOSURE => None,
            Self::TYPE_IDENTIFIER_STRUCT => None,
            Self::TYPE_IDENTIFIER_STRUCT_REFERENCE => None,
            Self::TYPE_IDENTIFIER_POINTER => Some(Type::Pointer),
            Self::TYPE_IDENTIFIER_POINTER_OF => None,
            unknown => {
                if unknown >= 1 << 16 {
                    let integer_type = (unknown >> 16) - 1;
                    Some(Type::Integer(IntegerType::try_from_primitive(integer_type as u8).ok()?))
                } else if unknown >= 1 << 8 {
                    let system_call_kind = (unknown >> 8) - 1;
                    Some(Type::SystemCall(SystemCallKind::try_from_primitive(system_call_kind as u8).ok()?))
                } else {
                    None
                }
            }
        }
    }

    pub fn size_in_bytes(&self) -> u64 {
        match self {
            Type::Library(_) => panic!("Libraries should only be accessed during binding!"),
            Type::Any => unreachable!(),
            Type::IgnoreTypeChecking => unreachable!(),
            Type::Error => 0,
            Type::Void => 0,
            Type::Integer(integer_type) => integer_type.size_in_bytes(),
            Type::None
            | Type::Struct(_)
            | Type::StructReference(_)
            | Type::TypedGenericStruct(_)
            | Type::Function(_)
            | Type::Closure(_)
            | Type::IntegerLiteral
            | Type::Boolean
            | Type::SystemCall(_)
            | Type::Noneable(_)
            | Type::Pointer
            | Type::PointerOf(_)
            | Type::GenericType => WORD_SIZE_IN_BYTES,
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
            Type::IgnoreTypeChecking => panic!("IgnoreTypeChecking should only be accessed during binding!"),
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
            | Type::Closure(_)
            | Type::Struct(_)
            | Type::StructReference(_)
            | Type::TypedGenericStruct(_)
            | Type::Pointer
            | Type::PointerOf(_) => true,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Library(_) => write!(f, "library"),
            Type::GenericType => write!(f, "$Type"),
            Type::IgnoreTypeChecking => write!(f, "unsafe any type"),
            Type::Error => write!(f, "error"),
            Type::Void => write!(f, "void"),
            Type::Any => write!(f, "any"),
            Type::Integer(integer_type) => integer_type.fmt(f),
            Type::IntegerLiteral => write!(f, "int literal"),
            Type::Boolean => write!(f, "bool"),
            Type::None => Type::noneable(Type::Any).fmt(f),
            Type::SystemCall(system_call) => write!(f, "system call {}", system_call),
            Type::Noneable(base) => write!(f, "{}?", base),
            Type::Function(function_type) => {
                write!(f, "fn {}", function_type)
            }
            Type::Closure(closure) => {
                write!(f, "closure {}", closure.base_function_type)
            }
            Type::Struct(struct_type) => {
                write!(f, "struct {}", struct_type)
            }
            Type::StructReference(struct_id) => {
                write!(f, "struct#{}", struct_id.id)
            }
            Type::TypedGenericStruct(typed_generic_struct_type) => {
                write!(
                    f,
                    "struct<{}> {}",
                    typed_generic_struct_type.type_, typed_generic_struct_type.struct_type
                )
            }
            Type::Pointer => write!(f, "pointer"),
            Type::PointerOf(base) => write!(f, "&{}", base),
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
    IgnoreTypeChecking,
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
                SystemCallKind::IgnoreTypeChecking => "ignoreTypeChecking",
            }
        )
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub this_type: Option<Type>,
    pub return_type: Type,
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
        if self.return_type != Type::Void {
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
            return_type: Type::Void,
            system_call_kind: None,
            is_generic: false,
        }
    }

    pub fn system_call(system_call_kind: SystemCallKind, string_type: Type) -> Self {
        match system_call_kind {
            SystemCallKind::Print => Self {
                parameter_types: vec![Type::Any],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::ToString => Self {
                parameter_types: vec![Type::Any],
                this_type: None,
                return_type: string_type,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::ArrayLength => Self {
                parameter_types: vec![],
                this_type: Some(Type::Any),
                return_type: Type::Integer(IntegerType::Unsigned64),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::HeapDump => Self {
                parameter_types: vec![string_type],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::Break => Self {
                parameter_types: vec![],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::Reallocate => Self {
                parameter_types: vec![Type::Pointer, Type::Integer(IntegerType::Unsigned64)],
                this_type: None,
                return_type: Type::Pointer,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::RuntimeError => Self {
                parameter_types: vec![string_type],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::AddressOf => Self {
                parameter_types: vec![Type::Pointer],
                this_type: None,
                return_type: Type::Integer(IntegerType::Unsigned64),
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::GarbageCollect => Self {
                parameter_types: vec![],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
            SystemCallKind::IgnoreTypeChecking => Self {
                parameter_types: vec![Type::IgnoreTypeChecking],
                this_type: None,
                return_type: Type::IgnoreTypeChecking,
                system_call_kind: Some(system_call_kind),
                is_generic: false,
            },
        }
    }

    pub fn function(
        parameter_types: Vec<Type>,
        this_type: Option<Type>,
        return_type: Type,
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

    pub fn relocate_structs(&mut self, struct_offset: usize) {
        for type_ in self
            .parameter_types
            .iter_mut()
            .chain(self.this_type.iter_mut())
        {
            if let Type::StructReference(index) = type_ {
                index.id += struct_offset as u64;
            }
        }
        if let Type::StructReference(index) = &mut self.return_type {
            index.id += struct_offset as u64;
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
    pub id: u64,
    pub fields: Vec<Type>,
    pub functions: Vec<Type>,
    pub function_table: StructFunctionTable,
    pub is_generic: bool,
}

impl StructType {
    pub fn size_in_bytes(&self) -> u64 {
        let mut result = 0;
        for field in &self.fields {
            // FIXME: This is only true if all sizes are a multiple of words.
            result += field.size_in_bytes();
        }
        result
    }
}

impl std::fmt::Display for StructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "t{} {{", self.id)?;
        for (index, field_type) in self.fields.iter().enumerate() {
            writeln!(f, "field{}: {};", index, field_type)?;
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
    pub id: u64,
    pub type_: Type,
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
