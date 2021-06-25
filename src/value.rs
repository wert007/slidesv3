#[derive(Debug, Clone, Copy)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", value),
        }
    }
}