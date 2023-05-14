use std::borrow::Cow;

use crate::binder::typing::{SystemCallKind, TypeId, TypeCollection};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    None,
    Integer(i64),
    Boolean(bool),
    SystemCall(SystemCallKind),
    String(String),
    LabelPointer(usize, TypeId),
    EnumType(Vec<String>, TypeId),
    EnumValue(usize, TypeId),
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

    pub fn as_label_pointer(&self) -> Option<(usize, &TypeId)> {
        if let Self::LabelPointer(label_reference, type_) = self {
            Some((*label_reference, type_))
        } else {
            None
        }
    }

    pub fn infer_type(&self) -> TypeId {
        match self {
            Value::Integer(_) => typeid!(Type::IntegerLiteral),
            Value::Boolean(_) => typeid!(Type::Boolean),
            Value::String(_) => typeid!(Type::String),
            Value::None => typeid!(Type::None),
            Value::SystemCall(kind) => typeid!(Type::SystemCall(kind)),
            Value::LabelPointer(_, it) | Value::EnumType(_, it) | Value::EnumValue(_, it) => *it,
        }
    }

    pub(crate) fn display(&self, types: &TypeCollection) -> Cow<str> {
        match self {
            Value::None => "none".into(),
            Value::Integer(it) => it.to_string().into(),
            Value::Boolean(it) => it.to_string().into(),
            Value::SystemCall(it) => it.to_string().into(),
            Value::String(it) => format!("'{it}'").into(),
            Value::LabelPointer(label, type_) => format!("L{:X} : {}", label, types.name_of_type_id(*type_)).into(),
            Value::EnumType(_, t) => types.name_of_type_id(*t).to_string().into(),
            Value::EnumValue(v, t) => format!("{}.value{v}", types.name_of_type_id(*t)).into(),
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
            Value::LabelPointer(label, type_) => write!(f, "L{:X} : {}", label, type_),
            Value::EnumType(_, t) => write!(f, "anomynous enum type {t}"),
            Value::EnumValue(v, t) => write!(f, "anomynous enum type {t}.value{v}"),
        }
    }
}
