use crate::binder::typing::{SystemCallKind, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Error,
    None,
    Integer(i64),
    Boolean(bool),
    SystemCall(SystemCallKind),
    String(String),
    LabelPointer(usize, Type),
    Library(usize),
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

    pub fn as_string(&self) -> Option<&str> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_label_pointer(&self) -> Option<(usize, &Type)> {
        if let Self::LabelPointer(label_reference, type_) = self {
            Some((*label_reference, type_))
        } else {
            None
        }
    }

    pub fn infer_type(&self, string_type: Type) -> Type {
        match self {
            Value::Error => Type::Error,
            Value::Integer(_) => Type::IntegerLiteral,
            Value::Boolean(_) => Type::Boolean,
            Value::SystemCall(kind) => Type::SystemCall(*kind),
            Value::String(_) => string_type,
            Value::None => Type::None,
            Value::LabelPointer(_, type_) => type_.clone(),
            Value::Library(index) => Type::Library(*index),
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
            Value::Error => write!(f, "error"),
            Value::Integer(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::SystemCall(value) => write!(f, "system call {}", value),
            Value::String(value) => write!(f, "'{}'", value),
            Value::None => write!(f, "none"),
            Value::LabelPointer(label, type_) => write!(f, "L{:X} : {}", label, type_),
            Value::Library(index) => write!(f, "library#{}", index),
        }
    }
}
