#[derive(Debug, Clone)]
pub enum ImportFunction {
    Library(ImportLibraryFunction)
}

#[derive(Debug, Clone)]
pub struct ImportLibraryFunction {
    pub path: String,
}