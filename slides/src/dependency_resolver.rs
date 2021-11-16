use crate::text::TextSpan;

#[derive(Debug, Clone)]
pub struct BoundImportStatement<'a> {
    pub span: TextSpan,
    pub name: &'a str,
    pub function: ImportFunction,
}

#[derive(Debug, Clone)]
pub enum ImportFunction {
    Library(ImportLibraryFunction)
}

#[derive(Debug, Clone)]
pub struct ImportLibraryFunction {
    pub path: String,
}