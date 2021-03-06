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
    typing::{FunctionType, Type},
    BoundGenericStructSymbol, BoundMaybeGenericStructSymbol, BoundStructFieldSymbol,
    BoundStructSymbol, FunctionDeclarationBody,
};

#[derive(Debug, Clone)]
pub struct Library {
    pub name: String,
    pub instructions: Vec<InstructionOrLabelReference>,
    pub startup: Vec<InstructionOrLabelReference>,
    pub program: Program,
    pub functions: Vec<FunctionSymbol>,
    pub structs: Vec<MaybeGenericStructSymbol>,
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

    pub fn relocate_labels(&mut self, label_offset: usize, unmoveable_label_range: usize) {
        if label_offset == 0 {
            return;
        }
        for function in self.functions.iter_mut() {
            function.function_label += label_offset as u64;
        }
        for strct in self.structs.iter_mut() {
            strct
                .function_table_mut()
                .relocate_labels(label_offset, unmoveable_label_range);
        }
        for inst in self.instructions.iter_mut().chain(self.startup.iter_mut()) {
            match inst {
                InstructionOrLabelReference::Instruction(Instruction {
                    arg,
                    op_code: OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::Label,
                    ..
                }) => {
                    if *arg < unmoveable_label_range as u64 {
                        continue;
                    }
                    *arg += label_offset as u64;
                }
                InstructionOrLabelReference::LabelReference(LabelReference {
                    label_reference,
                    ..
                }) => {
                    if *label_reference < unmoveable_label_range {
                        continue;
                    }
                    *label_reference += label_offset;
                }
                _ => {}
            }
        }
    }

    pub fn relocate_structs(&mut self, struct_offset: usize) {
        for function in self.functions.iter_mut() {
            function.relocate_structs(struct_offset);
        }
        for strct in self.structs.iter_mut() {
            for field in strct.fields_mut() {
                match &mut field.type_ {
                    Type::Function(function_type) => function_type.relocate_structs(struct_offset),
                    Type::Struct(_) => {
                        unreachable!("relocate_structs currently only works for StructReferences")
                    }
                    Type::StructReference(index) => index.id += struct_offset as u64,
                    _ => {}
                }
            }
            strct.function_table_mut().relocate_structs(struct_offset);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSymbol {
    pub name: String,
    pub function_type: FunctionType,
    pub function_label: u64,
    pub is_member_function: bool,
}

impl FunctionSymbol {
    pub fn relocate_structs(&mut self, struct_offset: usize) {
        for parameter in self
            .function_type
            .parameter_types
            .iter_mut()
            .chain(self.function_type.this_type.iter_mut())
        {
            if let Type::StructReference(index) = parameter {
                index.id += struct_offset as u64;
            }
        }
        if let Type::StructReference(index) = &mut self.function_type.return_type {
            index.id += struct_offset as u64;
        }
    }
}

impl From<FunctionDeclarationBody<'_>> for FunctionSymbol {
    fn from(it: FunctionDeclarationBody) -> Self {
        Self {
            name: it.function_name.into(),
            function_type: it.function_type,
            function_label: it.function_label,
            is_member_function: it.base_struct.is_some(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum MaybeGenericStructSymbol {
    Struct(StructSymbol),
    GenericStruct(GenericStructSymbol),
}

impl MaybeGenericStructSymbol {
    pub fn name(&self) -> &str {
        match self {
            MaybeGenericStructSymbol::Struct(it) => &it.name,
            MaybeGenericStructSymbol::GenericStruct(it) => &it.name,
        }
    }

    pub fn fields(&self) -> &[StructFieldSymbol] {
        match self {
            MaybeGenericStructSymbol::Struct(it) => &it.fields,
            MaybeGenericStructSymbol::GenericStruct(it) => &it.fields,
        }
    }

    pub fn fields_mut(&mut self) -> core::slice::IterMut<StructFieldSymbol> {
        match self {
            MaybeGenericStructSymbol::Struct(it) => it.fields.iter_mut(),
            MaybeGenericStructSymbol::GenericStruct(it) => it.fields.iter_mut(),
        }
    }

    pub fn function_table(&self) -> &StructFunctionTable {
        match self {
            MaybeGenericStructSymbol::Struct(it) => &it.function_table,
            MaybeGenericStructSymbol::GenericStruct(it) => &it.function_table,
        }
    }

    pub fn function_table_mut(&mut self) -> &mut StructFunctionTable {
        match self {
            MaybeGenericStructSymbol::Struct(it) => &mut it.function_table,
            MaybeGenericStructSymbol::GenericStruct(it) => &mut it.function_table,
        }
    }
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

impl From<BoundMaybeGenericStructSymbol<'_>> for MaybeGenericStructSymbol {
    fn from(it: BoundMaybeGenericStructSymbol) -> Self {
        match it {
            BoundMaybeGenericStructSymbol::Struct(it) => StructSymbol::from(it).into(),
            BoundMaybeGenericStructSymbol::GenericStruct(it) => {
                GenericStructSymbol::from(it).into()
            }
            BoundMaybeGenericStructSymbol::Empty => {
                unreachable!("Tried to convert/export an empty symbol")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructSymbol {
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
    pub function_table: StructFunctionTable,
}

impl From<BoundStructSymbol<'_>> for StructSymbol {
    fn from(it: BoundStructSymbol<'_>) -> Self {
        Self {
            name: it.name.into(),
            fields: it.fields.into_iter().map(Into::into).collect(),
            function_table: it.function_table,
        }
    }
}

impl StructSymbol {
    pub fn field(&self, name: &str) -> Option<&StructFieldSymbol> {
        self.fields.iter().find(|f| f.name == name)
    }
}

#[derive(Debug, Clone)]
pub struct GenericStructSymbol {
    pub name: String,
    pub fields: Vec<StructFieldSymbol>,
    pub function_table: StructFunctionTable,
    pub functions: Vec<GenericFunction>,
}

impl From<BoundGenericStructSymbol<'_>> for GenericStructSymbol {
    fn from(it: BoundGenericStructSymbol<'_>) -> Self {
        Self {
            name: it.struct_type.name.into(),
            fields: it.struct_type.fields.into_iter().map(Into::into).collect(),
            function_table: it.struct_type.function_table,
            functions: it.functions,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructFieldSymbol {
    pub name: String,
    pub type_: Type,
    pub offset: u64,
    pub is_read_only: bool,
}

impl From<BoundStructFieldSymbol<'_>> for StructFieldSymbol {
    fn from(it: BoundStructFieldSymbol<'_>) -> Self {
        Self {
            name: it.name.into(),
            type_: it.type_,
            offset: it.offset,
            is_read_only: it.is_read_only,
        }
    }
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
    pub add_to_function: Option<FunctionSymbol>,
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
            StructFunctionKind::AddTo => self.add_to_function = Some(function),
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
            .chain(self.add_to_function.iter_mut())
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
        if self.add_to_function.is_some() {
            result.push(StructFunctionKind::AddTo);
        }
        result
    }

    fn relocate_labels(&mut self, label_offset: usize, unmoveable_label_range: usize) {
        if label_offset == 0 {
            return;
        }
        self.function_symbols_iter_mut().for_each(|f| {
            if f.function_label >= unmoveable_label_range as u64 {
                f.function_label += label_offset as u64
            }
        });
    }

    fn relocate_structs(&mut self, struct_offset: usize) {
        self.function_symbols_iter_mut()
            .for_each(|f| f.relocate_structs(struct_offset));
    }

    pub fn replace_labels(mut self, label_relocation: Vec<(u64, u64)>) -> Self {
        let find_label = |lbl: &mut u64| {
            *lbl = *label_relocation
                .iter()
                .find(|(old, _)| *old == *lbl)
                .map(|(_, new)| new)
                .unwrap();
        };
        self.function_symbols_iter_mut()
            .for_each(|c| find_label(&mut c.function_label));
        self.label_relocation = label_relocation;
        self
    }
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
    AddTo,
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
            "$addTo" => Ok(Self::AddTo),
            _ => Err(value),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GenericFunction {
    pub function_label: u64,
    pub function_name: String,
    pub function_type: FunctionType,
    pub body: BoundNode,
    pub labels: Vec<usize>,
}
