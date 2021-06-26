use crate::value::Value;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Error,
    Void,
    Integer,
    Boolean,
}

impl Type {
    pub fn can_be_converted_to(&self, other: &Type) -> bool {
        self == other
    }
}

impl From<Value> for Type {
    fn from(value: Value) -> Self {
        match value {
            Value::Integer(_) => Type::Integer,
            Value::Boolean(_) => Type::Boolean,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Error => "error",
                Type::Void => "void",
                Type::Integer => "int",
                Type::Boolean => "bool",
            }
        )
    }
}
