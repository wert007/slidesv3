#[cfg(test)]
mod tests;

use std::ops::Deref;

use num_enum::TryFromPrimitive;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Error,
    Void,
    Any,
    Integer,
    Boolean,
    SystemCall(SystemCallKind),
    Array(Box<Type>),
    String,
}

impl Type {
    pub fn array(base_type: Type) -> Self {
        Self::Array(Box::new(base_type))
    }

    pub fn can_be_converted_to(&self, other: &Type) -> bool {
        self == other || other == &Type::Any
    }

    pub fn as_system_call(&self) -> Option<SystemCallKind> {
        if let Self::SystemCall(v) = *self {
            Some(v)
        } else {
            None
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array(_))
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
                while let &Type::Array(child) = &base_type.deref() {
                    base_type = &child;
                    array_count += 1;
                }
                let base_type : u64 = base_type.type_identifier();
                (base_type << 8) + (array_count << 1) as u64 + 1
            }
            Type::Error => 0,
            Type::Any => 2,
            Type::Void => 4,
            Type::Integer => 6,
            Type::Boolean => 8,
            Type::String => 10,
            Type::SystemCall(kind) => (((*kind as u8) as u64) << 5) + 16,
        }
    }

    pub fn from_type_identifier(value: u64) -> Option<Self> {
        match value {
            0 => Some(Self::Error),
            2 => Some(Self::Any),
            4 => Some(Self::Void),
            6 => Some(Self::Integer),
            8 => Some(Self::Boolean),
            10 => Some(Self::String),
            _ => {
                if value & 1 == 1 {
                    let base_type = Self::from_type_identifier(value >> 8)?;
                    let mut result = Type::array(base_type);
                    let array_count = (value >> 1) & 0xFF;
                    for _ in 0..array_count {
                        result = Type::array(result);
                    }
                    Some(result)
                } else if value & 31 == 16 {
                    let kind = SystemCallKind::try_from_primitive(((value >> 5) & 0xFF) as u8).ok()?;
                    Some(Self::SystemCall(kind))
                } else {
                    None
                }
            },
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
            Type::SystemCall(system_call) => write!(f, "system call {}", system_call),
            Type::Array(base) => write!(f, "{}[]", base),
            Type::String => write!(f, "string"),
        }
    }
}

#[derive(TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum SystemCallKind {
    Print,
    ArrayLength,
}

impl std::fmt::Display for SystemCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SystemCallKind::Print => "print",
                SystemCallKind::ArrayLength => "array length",
            }
        )
    }
}

pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
    pub system_call_kind: Option<SystemCallKind>,
}
