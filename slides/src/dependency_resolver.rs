#[derive(Debug, Clone)]
pub struct BoundImportStatement<'a> {
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