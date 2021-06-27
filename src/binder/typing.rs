use crate::value::Value;
use num_enum::TryFromPrimitive;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Error,
    Void,
    Any,
    Integer,
    Boolean,
    SystemCall(SystemCallKind),
}

impl Type {
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
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Integer(_) => Type::Integer,
            Value::Boolean(_) => Type::Boolean,
            Value::SystemCall(kind) => Type::SystemCall(kind),
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
        }
    }
}

#[derive(TryFromPrimitive, PartialEq, Eq, Debug, Clone, Copy)]
#[repr(u8)]
pub enum SystemCallKind {
    Print,
}

impl std::fmt::Display for SystemCallKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SystemCallKind::Print => "print",
            }
        )
    }
}

pub struct FunctionType {
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
    pub system_call_kind: Option<SystemCallKind>,
}
