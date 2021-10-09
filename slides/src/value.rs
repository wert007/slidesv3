use crate::binder::typing::{SystemCallKind, Type};

#[derive(Debug, Clone)]
pub enum Value {
    None,
    Integer(i64),
    Boolean(bool),
    SystemCall(SystemCallKind),
    String(String),
}

#[allow(dead_code)]
impl Value {
    pub fn as_integer(&self) -> Option<i64> {
        if let Self::Integer(v) = *self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        if let Self::Boolean(v) = *self {
            Some(v)
        } else {
            None
        }
    }

    pub fn infer_type(&self) -> Type {
        match self {
            Value::Integer(_) => Type::Integer,
            Value::Boolean(_) => Type::Boolean,
            Value::SystemCall(kind) => Type::SystemCall(*kind),
            Value::String(_) => Type::String,
            Value::None => Type::None,
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Integer(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::SystemCall(value) => write!(f, "system call {}", value),
            Value::String(value) => write!(f, "'{}'", value),
            Value::None => write!(f, "none"),
        }
    }
}
