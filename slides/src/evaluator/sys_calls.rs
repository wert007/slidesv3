use super::TypedU64;

pub fn print(argument: TypedU64, stack: &[u64]) {
    print!("PRINT ");
    if argument.is_pointer {
        // Word size is 4 bytes in this vm. But in doubt we always want do round up.
        let count = (stack[argument.value as usize] + 3) / 4;
        print!("[ ");
        for i in (argument.value - count..argument.value).rev() {
            print!("{}, ", stack[i as usize]);
        }
        println!("]")
    } else {
        println!("{}", argument.value);
    }
}
