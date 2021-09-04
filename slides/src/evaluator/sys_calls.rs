use super::TypedU64;

pub fn print(argument: TypedU64, stack: &[u64]) {
    print!("PRINT ");
    if argument.is_pointer {
        dbg!(stack);
        let count = stack[argument.value as usize] / 4;
        print!("[ ");
        for i in (argument.value - count..argument.value).rev() {
            print!("{}, ", stack[i as usize]);
        }
        println!("]")
    } else {
        println!("{}", argument.value);
    }
}
