use super::TypedU64;

pub fn print(argument: TypedU64, stack: &[u64]) {
    print!("PRINT ");
    if argument.is_pointer {
        dbg!(stack);
        let count = stack[argument.value as usize] / 4;
        print!("[ ");
        for i in argument.value..argument.value + count {
            print!("{}, ", stack[i as usize + 1]);
        }
        println!("]")
    } else {
        println!("{}", argument.value);
    }
}
