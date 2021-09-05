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
    let array_length_in_words = (array_length_in_bytes + 3) / 4;
    let array_start = argument.value;
    let array_end = array_start - array_length_in_words;
    match base_type {
        Type::Error => todo!(),
        Type::Void => todo!(),
        Type::Any => todo!(),
        Type::Integer => {
            for i in (array_end..array_start).rev() {
                print!("{}, ", stack[i as usize] as i64);
            }
        },
        Type::Boolean => {
            for i in (array_end..array_start).rev() {
                if stack[i as usize] == 0 {
                    print!("false, ");
                } else {
                    print!("true, ");
                }
            }
        },
        Type::SystemCall(kind) => {
            for _ in (array_end..array_start).rev() {
                print!("system call {}, ", kind);
            }
        },
        Type::Array(base_type) => {
            // After the length is the address to the array (- 1). The address
            // points to the length of another array. But print_array expects
            // the address right before that, so the address is decreased by
            // one.
            let mut address = stack[array_start as usize - 1] + 1;
            while address > array_end {
                address -= 1;
                let argument = TypedU64 {
                    value: address,
                    is_pointer: true,
                }; // FIXME
                print_array(*base_type.clone(), argument, stack);
                print!(", ");

                if address > (stack[address as usize] + 3) / 4 + 1 {
                    address -= (stack[address as usize] + 3) / 4 + 1;
                } else {
                    address = array_end;
                }
            }
        },
        Type::String => {
            // After the length is the address to the array (- 1). The address
            // points to the length of another array. But print_array expects
            // the address right before that, so the address is decreased by
            // one.
            let mut address = stack[array_start as usize - 1] + 1;
            while address > array_end {
                address -= 1;

                let argument = TypedU64 { value: address, is_pointer: true, }; //FIXME
                print_string(argument, stack);
                print!(", ");

                if address > (stack[address as usize] + 3) / 4 + 1 {
                    address -= (stack[address as usize] + 3) / 4 + 1;
                } else {
                    address = array_end;
                }
            }
        },
    }
    print!("]");
}

fn print_string(argument: TypedU64, stack: &[u64]) {
    let string_length_in_bytes = stack[argument.value as usize];
    let string_length_in_words = (string_length_in_bytes + 3) / 4;
    let string_start = argument.value; // Includes the length of the string
    let string_end = string_start - string_length_in_words;
    let mut string_buffer : Vec<u8> = Vec::with_capacity(string_length_in_bytes as usize);

    // The exclusive end of the range excludes the length of the string.
    for i in (string_end..string_start).rev() {
        let word = stack[i as usize];
        string_buffer.push((word & 0xFF) as u8);
        string_buffer.push((word >> 8 & 0xFF) as u8);
        string_buffer.push((word >> 16 & 0xFF) as u8);
        string_buffer.push((word >> 24 & 0xFF) as u8);
    }
    print!("'{}'", String::from_utf8_lossy(&string_buffer));
}
