#[cfg(test)]
mod tests;

use std::ops::Deref;

use num_enum::TryFromPrimitive;

use crate::evaluator::WORD_SIZE_IN_BYTES;

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

    pub fn as_system_call(&self) -> Option<SystemCallKind> {
        if let Self::SystemCall(v) = *self {
            Some(v)
        } else {
            None
        }
    }

    pub fn array_base_type(&self) -> Option<&Type> {
        if let Self::Array(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn type_identifier(&self) -> u64 {
        match self {
            Type::Array(base_type) => {
                let mut array_count = 0u8;
                let mut base_type = base_type;
                while let Type::Array(child) = base_type.deref() {
                    base_type = &child;
                    array_count += 1;
                }
                let base_type: u64 = base_type.type_identifier();
                (base_type << 8) + (array_count << 2) as u64 + 1
            }
            Type::Error => 0 << 2,
            Type::Any => 1 << 2,
            Type::Void => 2 << 2,
            Type::Integer => 3 << 2,
            Type::Boolean => 4 << 2,
            Type::None => Type::noneable(Type::Any).type_identifier(),
            Type::String => 5 << 2,
            Type::SystemCall(kind) => (((*kind as u8) as u64) << 5) + 8 << 2,
            Type::Noneable(base) => base.type_identifier() + 2,
            Type::StructReference(_) | Type::Struct(_) | Type::Function(_) | Type::Closure(_) => {
                eprintln!("Unimplented type_identifer for functions or structs expected..");
                u64::MAX
            }
        }
    }

    pub fn from_type_identifier(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::Error),
            4 => Some(Self::Any),
            8 => Some(Self::Void),
            12 => Some(Self::Integer),
            16 => Some(Self::Boolean),
            20 => Some(Self::String),
            _ => {
                if value & 1 == 1 {
                    let base_type = Self::from_type_identifier(value >> 8)?;
                    let mut result = Type::array(base_type);
                    let array_count = (value >> 1) & 0xFF;
                    for _ in 0..array_count {
                        result = Type::array(result);
                    }
                    Some(result)
                } else if value & 63 == 32 {
                    let kind =
                        SystemCallKind::try_from_primitive(((value >> 5) & 0xFF) as u8).ok()?;
                    Some(Self::SystemCall(kind))
                } else if value & 2 == 2 {
                    let base_type = Self::from_type_identifier(value & !2)?;
                    Some(Type::noneable(base_type))
                } else {
                    None
                }
            }
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

#[derive(Debug, Clone, Copy)]
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
