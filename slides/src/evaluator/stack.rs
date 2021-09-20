pub struct Stack {
    pub data : Vec<u64>,
    pub flags : Vec<Flags>,
}



pub struct Flags {
    pub is_pointer: bool,
}