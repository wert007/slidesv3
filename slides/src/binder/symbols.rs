use std::{
    convert::TryFrom,
    path::{Path, PathBuf},
};

use crate::instruction_converter::{
    instruction::{op_codes::OpCode, Instruction},
    InstructionOrLabelReference, LabelReference, Program,
};

use super::{
    bound_nodes::BoundNode,
    typing::{GenericTypeId, TypeCollection, TypeId},
};

#[derive(Clone)]
pub struct Library {
    pub name: String,
    pub instructions: Vec<InstructionOrLabelReference>,
    pub startup: Vec<InstructionOrLabelReference>,
    pub program: Program,
    pub functions: Vec<FunctionSymbol>,
    pub structs: Vec<TypeId>,
    pub enums: Vec<TypeId>,
    pub generic_structs: Vec<GenericTypeId>,
    pub has_errors: bool,
    pub path: PathBuf,
    pub referenced_libraries: Vec<Library>,
    pub is_already_loaded: bool,
}

impl Library {
    pub fn error() -> Self {
        Self {
            name: "error".into(),
            instructions: vec![],
            startup: vec![],
            program: Program::error(),
            functions: vec![],
            structs: vec![],
            enums: vec![],
            generic_structs: vec![],
            has_errors: true,
            path: PathBuf::new(),
            referenced_libraries: vec![],
            is_already_loaded: false,
        }
    }

    pub fn with_is_already_loaded(self, is_already_loaded: bool) -> Self {
        Self {
            is_already_loaded,
            ..self
        }
    }

    pub fn find_imported_library_by_path(&self, path: &Path) -> Option<(String, Library)> {
        if self.path == path {
            return Some((self.name.clone(), self.clone().with_is_already_loaded(true)));
        }
        for lib in &self.referenced_libraries {
            if let Some((rel_path, lib)) = lib.find_imported_library_by_path(path) {
                return Some((format!("{}.{}", self.name, rel_path), lib));
            }
        }
        None
    }

    pub fn look_up_function_by_name(&self, name: &str) -> Option<&FunctionSymbol> {
        self.functions.iter().find(|f| f.name == name)
    }

    pub fn look_up_enum_by_name(&self, name: &str, types: &TypeCollection) -> Option<TypeId> {
        self.enums
            .iter()
            .find(|e| types.name_of_type_id(**e) == name)
            .copied()
    }

    pub fn relocate_labels(&mut self, label_offset: usize) {
        if label_offset == 0 {
            return;
        }
        for function in self.functions.iter_mut() {
            function.function_label += label_offset as u64;
        }
        for inst in self.instructions.iter_mut().chain(self.startup.iter_mut()) {
            match inst {
                InstructionOrLabelReference::Instruction(Instruction {
                    arg,
                    op_code: OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::Label,
                    ..
                }) => {
                    *arg += label_offset as u64;
                }
                InstructionOrLabelReference::LabelReference(LabelReference {
                    label_reference,
                    ..
                }) => {
                    *label_reference += label_offset;
                }
                _ => {}
            }
        }
    }
}

impl std::fmt::Debug for Library {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Library")
            .field("name", &self.name)
            .field("functions", &self.functions)
            .field("structs", &self.structs)
            .field("generic_structs", &self.generic_structs)
            .field("has_errors", &self.has_errors)
            .field("path", &self.path)
            .field("referenced_libraries", &self.referenced_libraries)
            .field("is_already_loaded", &self.is_already_loaded)
            .finish()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSymbol {
    pub name: String,
    pub function_type: TypeId,
    pub function_label: u64,
    pub is_member_function: bool,
}

#[derive(Debug, Clone)]
pub enum MaybeGenericStructSymbol {
    Struct(StructSymbol),
    GenericStruct(GenericStructSymbol),
}

impl From<StructSymbol> for MaybeGenericStructSymbol {
    fn from(it: StructSymbol) -> Self {
        Self::Struct(it)
    }
}

impl From<GenericStructSymbol> for MaybeGenericStructSymbol {
    fn from(it: GenericStructSymbol) -> Self {
        Self::GenericStruct(it)
    }
}

#[derive(Debug, Clone)]
pub struct StructSymbol {
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
    pub functions: Vec<StructFieldSymbol>,
    pub function_table: StructFunctionTable,
}

#[derive(Debug, Clone)]
pub struct GenericStructSymbol {
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
    pub function_table: StructFunctionTable,
    pub functions: Vec<GenericFunction>,
}

#[derive(Debug, Clone)]
pub struct StructFieldSymbol {
    pub name: String,
    pub type_: TypeId,
    pub offset: u64,
    pub is_read_only: bool,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StructFunctionTable {
    pub constructor_function: Option<FunctionSymbol>,
    pub to_string_function: Option<FunctionSymbol>,
    pub get_function: Option<FunctionSymbol>,
    pub set_function: Option<FunctionSymbol>,
    pub element_count_function: Option<FunctionSymbol>,
    pub equals_function: Option<FunctionSymbol>,
    pub add_function: Option<FunctionSymbol>,
    pub label_relocation: Vec<(u64, u64)>,
}

impl StructFunctionTable {
    pub fn set(&mut self, kind: StructFunctionKind, function: FunctionSymbol) {
        match kind {
            StructFunctionKind::Constructor => self.constructor_function = Some(function),
            StructFunctionKind::ToString => self.to_string_function = Some(function),
            StructFunctionKind::Get => self.get_function = Some(function),
            StructFunctionKind::Set => self.set_function = Some(function),
            StructFunctionKind::ElementCount => self.element_count_function = Some(function),
            StructFunctionKind::Equals => self.equals_function = Some(function),
            StructFunctionKind::Add => self.add_function = Some(function),
        }
    }

    pub fn get(&self, kind: StructFunctionKind) -> Option<&FunctionSymbol> {
        match kind {
            StructFunctionKind::Constructor => self.constructor_function.as_ref(),
            StructFunctionKind::ToString => self.to_string_function.as_ref(),
            StructFunctionKind::Get => self.get_function.as_ref(),
            StructFunctionKind::Set => self.set_function.as_ref(),
            StructFunctionKind::ElementCount => self.element_count_function.as_ref(),
            StructFunctionKind::Equals => self.equals_function.as_ref(),
            StructFunctionKind::Add => self.add_function.as_ref(),
        }
    }

    pub fn function_symbols_iter_mut(&mut self) -> impl Iterator<Item = &mut FunctionSymbol> {
        self.constructor_function
            .iter_mut()
            .chain(self.to_string_function.iter_mut())
            .chain(self.get_function.iter_mut())
            .chain(self.set_function.iter_mut())
            .chain(self.element_count_function.iter_mut())
            .chain(self.equals_function.iter_mut())
            .chain(self.add_function.iter_mut())
    }

    pub(crate) fn function_symbols_iter(&self) -> impl Iterator<Item = &FunctionSymbol> {
        self.constructor_function
            .iter()
            .chain(self.to_string_function.iter())
            .chain(self.get_function.iter())
            .chain(self.set_function.iter())
            .chain(self.element_count_function.iter())
            .chain(self.equals_function.iter())
            .chain(self.add_function.iter())
    }

    pub fn available_struct_function_kinds(&self) -> Vec<StructFunctionKind> {
        let mut result = Vec::with_capacity(16);
        if self.constructor_function.is_some() {
            result.push(StructFunctionKind::Constructor);
        }
        if self.to_string_function.is_some() {
            result.push(StructFunctionKind::ToString);
        }
        if self.get_function.is_some() {
            result.push(StructFunctionKind::Get);
        }
        if self.set_function.is_some() {
            result.push(StructFunctionKind::Set);
        }
        if self.element_count_function.is_some() {
            result.push(StructFunctionKind::ElementCount);
        }
        if self.equals_function.is_some() {
            result.push(StructFunctionKind::Equals);
        }
        if self.add_function.is_some() {
            result.push(StructFunctionKind::Add);
        }
        result
    }

    // fn relocate_labels(&mut self, label_offset: usize) {
    //     if label_offset == 0 {
    //         return;
    //     }
    //     self.function_symbols_iter_mut()
    //         .for_each(|f| f.function_label += label_offset as u64);
    // }

    // pub fn replace_labels(mut self, label_relocation: Vec<(u64, u64)>) -> Self {
    //     let find_label = |lbl: &mut u64| {
    //         *lbl = *label_relocation
    //             .iter()
    //             .find(|(old, _)| *old == *lbl)
    //             .map(|(_, new)| new)
    //             .unwrap();
    //     };
    //     self.function_symbols_iter_mut()
    //         .for_each(|c| find_label(&mut c.function_label));
    //     self.label_relocation = label_relocation;
    //     self
    // }
}

#[derive(Debug, Clone, Copy)]
pub enum StructFunctionKind {
    Constructor,
    ToString,
    Get,
    Set,
    ElementCount,
    Equals,
    Add,
}

impl<'a> TryFrom<&'a str> for StructFunctionKind {
    type Error = &'a str;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "$constructor" => Ok(Self::Constructor),
            "$toString" => Ok(Self::ToString),
            "$get" => Ok(Self::Get),
            "$set" => Ok(Self::Set),
            "$elementCount" => Ok(Self::ElementCount),
            "$equals" => Ok(Self::Equals),
            "$add" => Ok(Self::Add),
            _ => Err(value),
        }
    }
}

#[derive(Clone)]
pub struct GenericFunction {
    pub function_label: u64,
    pub function_name: String,
    pub function_type: GenericTypeId,
    pub body: BoundNode,
    pub labels: Vec<usize>,
}

impl PartialEq for GenericFunction {
    fn eq(&self, other: &Self) -> bool {
        self.function_label == other.function_label
            && self.function_name == other.function_name
            && self.function_type == other.function_type
            && self.labels == other.labels
    }
}

impl Eq for GenericFunction {}

impl std::fmt::Debug for GenericFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GenericFunction")
            .field("function_label", &self.function_label)
            .field("function_name", &self.function_name)
            .field("function_type", &self.function_type)
            .field("labels", &self.labels)
            .finish()
    }
}
