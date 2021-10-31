#[cfg(test)]
mod tests;

use num_enum::TryFromPrimitive;

use crate::evaluator::memory::WORD_SIZE_IN_BYTES;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Error,
    Void,
    Any,
    Integer,
    Boolean,
    None,
    SystemCall(SystemCallKind),
    Array(Box<Type>),
    Noneable(Box<Type>),
    String,
    Function(Box<FunctionType>),
    Closure(Box<ClosureType>),
    Struct(Box<StructType>),
    StructReference(u64),
}

impl Type {
    pub fn array(base_type: Type) -> Self {
        Self::Array(Box::new(base_type))
    }

    pub fn function(function_type: FunctionType) -> Self {
        Self::Function(Box::new(function_type))
    }

    pub fn closure(base_function_type: FunctionType) -> Self {
        Self::Closure(Box::new(base_function_type.into()))
    }

    pub fn noneable(base_type: Type) -> Self {
        Self::Noneable(Box::new(base_type))
    }

    pub fn can_be_converted_to(&self, other: &Type) -> bool {
        match (self, other) {
            _ if self == other => true,
            (_, Type::Any) => true,
            (Type::None, Type::Noneable(_)) => true,
            (type_, Type::Noneable(other)) => type_.can_be_converted_to(other),
            (Type::Array(base_type), Type::Array(other)) => base_type.can_be_converted_to(other),
            (Type::Struct(id), Type::StructReference(other_id)) if id.id == *other_id => true,
            (Type::StructReference(id), Type::Struct(other)) if *id == other.id => true,
            _ => false,
        }
    }

    pub fn can_be_casted_to(&self, other: &Type) -> bool {
        match (self, other) {
            (a, b) if a == b => false,
            (Type::Any, _) => true,
            (Type::Noneable(base_type), other) => base_type.can_be_converted_to(other),
            _ => false,
        }
    }

    pub fn array_base_type(&self) -> Option<&Type> {
        if let Self::Array(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn noneable_base_type(&self) -> Option<&Type> {
        if let Self::Noneable(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn type_identifier_size_in_words(&self) -> u64 {
        match self {
            Type::Error => 1,
            Type::Void => 1,
            Type::Any => 1,
            Type::Integer => 1,
            Type::Boolean => 1,
            Type::None => 1,
            Type::String => 1,
            Type::SystemCall(_) => 1,
            Type::Array(inner) => {
                let mut base_type = inner;
                while let Type::Array(inner) = base_type.as_ref() {
                    base_type = inner;
                }
                base_type.type_identifier_size_in_words() + 2
            }
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
        }
    }

    pub const TYPE_IDENTIFIER_ERROR: u64 = 0;
    pub const TYPE_IDENTIFIER_VOID: u64 = 1;
    pub const TYPE_IDENTIFIER_ANY: u64 = 2;
    pub const TYPE_IDENTIFIER_INTEGER: u64 = 3;
    pub const TYPE_IDENTIFIER_BOOLEAN: u64 = 4;
    pub const TYPE_IDENTIFIER_NONE: u64 = 5;
    pub const TYPE_IDENTIFIER_ARRAY: u64 = 6;
    pub const TYPE_IDENTIFIER_NONEABLE: u64 = 7;
    pub const TYPE_IDENTIFIER_STRING: u64 = 8;
    pub const TYPE_IDENTIFIER_FUNCTION: u64 = 9;
    pub const TYPE_IDENTIFIER_CLOSURE: u64 = 10;
    pub const TYPE_IDENTIFIER_STRUCT: u64 = 11;
    pub const TYPE_IDENTIFIER_STRUCT_REFERENCE: u64 = 12;
    pub const TYPE_IDENTIFIER_SYSTEM_CALL_PRINT: u64 =
        (1 + SystemCallKind::Print as u8 as u64) << 8;
    pub const TYPE_IDENTIFIER_SYSTEM_CALL_TO_STRING: u64 =
        (1 + SystemCallKind::ToString as u8 as u64) << 8;
    pub const TYPE_IDENTIFIER_SYSTEM_CALL_ARRAY_LENGTH: u64 =
        (1 + SystemCallKind::ArrayLength as u8 as u64) << 8;
    pub const TYPE_IDENTIFIER_SYSTEM_CALL_DEBUG_HEAP_DUMP: u64 =
        (1 + SystemCallKind::DebugHeapDump as u8 as u64) << 8;

    pub fn type_identifier_kind(&self) -> u64 {
        match self {
            Type::Error => Self::TYPE_IDENTIFIER_ERROR,
            Type::Void => Self::TYPE_IDENTIFIER_VOID,
            Type::Any => Self::TYPE_IDENTIFIER_ANY,
            Type::Integer => Self::TYPE_IDENTIFIER_INTEGER,
            Type::Boolean => Self::TYPE_IDENTIFIER_BOOLEAN,
            Type::None => Self::TYPE_IDENTIFIER_NONE,
            Type::Array(_) => Self::TYPE_IDENTIFIER_ARRAY,
            Type::Noneable(_) => Self::TYPE_IDENTIFIER_NONEABLE,
            Type::String => Self::TYPE_IDENTIFIER_STRING,
            Type::Function(_) => Self::TYPE_IDENTIFIER_FUNCTION,
            Type::Closure(_) => Self::TYPE_IDENTIFIER_CLOSURE,
            Type::Struct(_) => Self::TYPE_IDENTIFIER_STRUCT,
            Type::StructReference(_) => Self::TYPE_IDENTIFIER_STRUCT_REFERENCE,
            Type::SystemCall(SystemCallKind::Print) => Self::TYPE_IDENTIFIER_SYSTEM_CALL_PRINT,
            Type::SystemCall(SystemCallKind::ToString) => {
                Self::TYPE_IDENTIFIER_SYSTEM_CALL_TO_STRING
            }
            Type::SystemCall(SystemCallKind::ArrayLength) => {
                Self::TYPE_IDENTIFIER_SYSTEM_CALL_ARRAY_LENGTH
            }
            Type::SystemCall(SystemCallKind::DebugHeapDump) => {
                Self::TYPE_IDENTIFIER_SYSTEM_CALL_DEBUG_HEAP_DUMP
            }
        }
    }

    pub fn simple_type_from_type_identifier(type_identifier_kind: u64) -> Option<Self> {
        match type_identifier_kind {
            Self::TYPE_IDENTIFIER_ERROR => Some(Type::Error),
            Self::TYPE_IDENTIFIER_VOID => Some(Type::Void),
            Self::TYPE_IDENTIFIER_ANY => Some(Type::Any),
            Self::TYPE_IDENTIFIER_INTEGER => Some(Type::Integer),
            Self::TYPE_IDENTIFIER_BOOLEAN => Some(Type::Boolean),
            Self::TYPE_IDENTIFIER_NONE => Some(Type::None),
            Self::TYPE_IDENTIFIER_ARRAY => None,
            Self::TYPE_IDENTIFIER_NONEABLE => None,
            Self::TYPE_IDENTIFIER_STRING => Some(Type::String),
            Self::TYPE_IDENTIFIER_FUNCTION => None,
            Self::TYPE_IDENTIFIER_CLOSURE => None,
            Self::TYPE_IDENTIFIER_STRUCT => None,
            Self::TYPE_IDENTIFIER_STRUCT_REFERENCE => None,
            Self::TYPE_IDENTIFIER_SYSTEM_CALL_PRINT => {
                Some(Type::SystemCall(SystemCallKind::Print))
            }
            Self::TYPE_IDENTIFIER_SYSTEM_CALL_TO_STRING => {
                Some(Type::SystemCall(SystemCallKind::ToString))
            }
            Self::TYPE_IDENTIFIER_SYSTEM_CALL_ARRAY_LENGTH => {
                Some(Type::SystemCall(SystemCallKind::ArrayLength))
            }
            Self::TYPE_IDENTIFIER_SYSTEM_CALL_DEBUG_HEAP_DUMP => {
                Some(Type::SystemCall(SystemCallKind::DebugHeapDump))
            }
            _ => None,
        }
    }

    pub fn size_in_bytes(&self) -> u64 {
        match self {
            Type::Any => unreachable!(),
            Type::Error => 0,
            Type::Void => 0,
            Type::None
            | Type::Struct(_)
            | Type::StructReference(_)
            | Type::Function(_)
            | Type::Closure(_)
            | Type::Integer
            | Type::Boolean
            | Type::SystemCall(_)
            | Type::Array(_)
            | Type::Noneable(_)
            | Type::String => WORD_SIZE_IN_BYTES,
        }
    }

    pub fn array_element_size_in_bytes(&self) -> u64 {
        match self {
            Type::Void | Type::Any | Type::Error => unreachable!(),
            Type::String => 1,
            Type::Integer
            | Type::Boolean
            | Type::None
            | Type::SystemCall(_)
            | Type::Array(_)
            | Type::Noneable(_)
            | Type::Function(_)
            | Type::Closure(_)
            | Type::Struct(_)
            | Type::StructReference(_) => WORD_SIZE_IN_BYTES,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            Type::Error
            | Type::Void
            | Type::Any
            // Technically a pointer. But it does not get dereferenced, but
            // instead the value itself is assigned to the program counter.
            | Type::Function(_)
            | Type::SystemCall(_)
            | Type::Integer
            | Type::Boolean => false,
            // A none Pointer should never be dereferenced.
            Type::None
            | Type::Array(_)
            | Type::Noneable(_)
            | Type::String
            | Type::Closure(_)
            | Type::Struct(_)
            | Type::StructReference(_) => true,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Error => write!(f, "error"),
            Type::Void => write!(f, "void"),
            Type::Any => write!(f, "any"),
            Type::Integer => write!(f, "int"),
            Type::Boolean => write!(f, "bool"),
            Type::None => Type::noneable(Type::Any).fmt(f),
            Type::SystemCall(system_call) => write!(f, "system call {}", system_call),
            Type::Array(base) => write!(f, "{}[]", base),
            Type::Noneable(base) => write!(f, "{}?", base),
            Type::String => write!(f, "string"),
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
                write!(f, "struct#{}", struct_id)
            }
        }
    }
}

#[derive(TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum SystemCallKind {
    Print,
    ToString,
    ArrayLength,
    DebugHeapDump,
}

impl std::fmt::Display for SystemCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SystemCallKind::Print => "print",
                SystemCallKind::ToString => "to$string",
                SystemCallKind::ArrayLength => "array$length",
                SystemCallKind::DebugHeapDump => "debug$heap$dump",
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
        }
    }

    pub fn system_call(system_call_kind: SystemCallKind) -> Self {
        match system_call_kind {
            SystemCallKind::Print => Self {
                parameter_types: vec![Type::Any],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
            },
            SystemCallKind::ToString => Self {
                parameter_types: vec![Type::Any],
                this_type: None,
                return_type: Type::String,
                system_call_kind: Some(system_call_kind),
            },
            SystemCallKind::ArrayLength => Self {
                parameter_types: vec![],
                this_type: Some(Type::Any),
                return_type: Type::Integer,
                system_call_kind: Some(system_call_kind),
            },
            SystemCallKind::DebugHeapDump => Self {
                parameter_types: vec![Type::String],
                this_type: None,
                return_type: Type::Void,
                system_call_kind: Some(system_call_kind),
            },
        }
    }

    pub fn function(
        parameter_types: Vec<Type>,
        this_type: Option<Type>,
        return_type: Type,
    ) -> Self {
        Self {
            parameter_types,
            this_type,
            return_type,
            system_call_kind: None,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    FunctionId(u64),
    SystemCall(SystemCallKind),
}

impl std::fmt::Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::FunctionId(id) => write!(f, "fn#{}", id),
            FunctionKind::SystemCall(kind) => write!(f, "{}", kind),
        }
    }
}
