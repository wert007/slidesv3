// func printAny(value: any) {
//     print('printAny: ' + value);
// }

// generic func printGeneric(value: $Type) {
//     print('printGeneric: ' + value);
// }

// func printIfEqual(a: any, b: any) {
//     if a == b {
//         print(a);
//     }
// }

// generic func printIfEqualGeneric(a: $Type, b: $Type) {
//     if a == b {
//         print(a);
//     }
// }

generic struct Value {
    value: $Type;

    // func set(value: $Type) {
    //     this.replace(value);
    // }

    func replace(value: $Type) -> $Type {
        let old = this.value;
        this.value = value;
        return old;
    }

    func $toString() -> string {
        return this.value + '';
    }
}

// generic struct Array {
//     length: int;
//     buffer: &$Type;

//     func $get(index: int) -> $Type {
//         return this.buffer[index];
//     }

//     func $set(index: int, value: $Type) {
//         this.buffer[index] = value;
//     }

//     func set(index: int, value: $Type) {
//         this.buffer[index] = value;
//     }

//     func get(index: int) -> $Type {
//         return this.buffer[index];
//     }
// }

func main() {
    // This does and should not compile
    // printIfEqualGeneric(true, 0);
    let v = new Value(99);
    print(v.replace(12));
    print(v);
    // let arrayLength = 3;
    // let arrayPtr : &int = reallocate(none, arrayLength * 8);
    // let array = new Array(arrayLength, arrayPtr);
    // // array[0] = 42;
    // array.set(0, 42);
    // print(array.get(0));
    // printIfEqualGeneric(v, v);
}
