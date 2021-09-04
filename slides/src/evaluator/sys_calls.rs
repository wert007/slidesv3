use crate::binder::typing::Type;

use super::TypedU64;

pub fn print(type_: Type, argument: TypedU64, stack: &[u64]) {
    print!("PRINT ");
    match type_ {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer => {
            println!("{}", argument.value as i64)
        },
        Type::Boolean => {
            if argument.value == 0 {
                println!("false");
            } else {
                println!("true");
            }
        },
        Type::SystemCall(kind) => println!("system call {}", kind),
        Type::Array(base_type) => {
            print_array(*base_type, argument, stack);
            println!();
        }
        Type::String => {
            print_string(argument, stack);
            println!();
        }
    }
}

fn print_array(base_type: Type, argument: TypedU64, stack: &[u64]) {
    print!("[ ");
    assert!(argument.is_pointer);
    let array_length_in_bytes = stack[argument.value as usize];
    match base_type {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer => {
            let count = array_length_in_bytes / 4;
            for i in (argument.value - count..argument.value).rev() {
                print!("{}, ", stack[i as usize] as i64);
            }
        },
        Type::Boolean => {
            let count = array_length_in_bytes / 4;
            for i in (argument.value - count..argument.value).rev() {
                if stack[i as usize] == 0 {
                    print!("false, ");
                } else {
                    print!("true, ");
                }
            }
        },
        Type::SystemCall(kind) => {
            let count = array_length_in_bytes / 4;
            for _ in (argument.value - count..argument.value).rev() {
                print!("system call {}, ", kind);
            }
        },
        Type::Array(base_type) => {
            let count = array_length_in_bytes / 4;
            for i in (argument.value - count..argument.value).rev() {
                let argument = TypedU64 {
                    value: stack[i as usize],
                    is_pointer: true,
                }; // FIXME
                print_array(*base_type.clone(), argument, stack);
                print!(", ");
            }
        },
        Type::String => {
            let count = array_length_in_bytes / 4;
            for i in (argument.value - count..argument.value).rev() {
                let argument = TypedU64 { value: stack[i as usize], is_pointer: true, }; //FIXME
                print_string(argument, stack);
                print!(", ");
            }
        },
    }
    print!("]");
}

fn print_string(argument: TypedU64, stack: &[u64]) {
    let string_length_in_bytes = stack[argument.value as usize];
    let mut string_buffer : Vec<u8> = Vec::with_capacity(string_length_in_bytes as usize);
    let count = (string_length_in_bytes + 3) / 4;
    for i in (argument.value - count..argument.value).rev() {
        let word = stack[i as usize];
        string_buffer.push((word & 0xFF) as u8);
        string_buffer.push((word >> 8 & 0xFF) as u8);
        string_buffer.push((word >> 16 & 0xFF) as u8);
        string_buffer.push((word >> 24 & 0xFF) as u8);
    }
    print!("'{}'", String::from_utf8_lossy(&string_buffer));
}
