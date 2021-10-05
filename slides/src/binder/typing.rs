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
    SystemCall(SystemCallKind),
    Array(Box<Type>),
    String,
    Function(Box<FunctionType>),
    Struct(Box<StructType>),
    StructReference(u64),
}

impl Type {
    pub fn array(base_type: Type) -> Self {
        Self::Array(Box::new(base_type))
    }

    pub fn can_be_converted_to(&self, other: &Type) -> bool {
        match (self, other) {
            _ if self == other => true,
            (_, Type::Any) => true,
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
                (base_type << 8) + (array_count << 1) as u64 + 1
            }
            Type::Error => 0,
            Type::Any => 2,
            Type::Void => 4,
            Type::Integer => 6,
            Type::Boolean => 8,
            Type::String => 10,
            Type::SystemCall(kind) => (((*kind as u8) as u64) << 5) + 16,
            Type::StructReference(_) | Type::Struct(_) | Type::Function(_) => {
                eprintln!("Unimplented type_identifer for functions or structs expected..");
                u64::MAX
            }
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
                    let kind =
                        SystemCallKind::try_from_primitive(((value >> 5) & 0xFF) as u8).ok()?;
                    Some(Self::SystemCall(kind))
                } else {
                    None
                }
            }
        }
    }

    pub fn size_in_bytes(&self) -> u64 {
        match self {
            Type::Any | Type::Function(_) => unreachable!(),
            Type::Error => 0,
            Type::Void => 0,
            Type::Struct(_)
            | Type::StructReference(_)
            | Type::Integer
            | Type::Boolean
            | Type::SystemCall(_)
            | Type::Array(_)
            | Type::String => WORD_SIZE_IN_BYTES,
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
            Type::Function(function_type) => {
                write!(f, "fn {}", function_type)
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
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct StructType {
    pub id: u64,
    pub fields: Vec<Type>,
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
