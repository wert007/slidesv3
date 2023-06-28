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

struct Value<Type> {
    value: $Type;

    func set(value: $Type) {
        this.replace(value);
    }

    func replace(value: $Type) -> $Type {
        let old = this.value;
        this.value = value;
        return old;
    }

    func $toString() -> string {
        return this.value + '';
    }
}

// generic struct IterSingle {
//     value: $Type;

//     func $get(index: int) -> $Type {
//         return this.value;
//     }

//     func $elementCount() -> int {
//         // This does crash the compiler for some reason???
//         // print('value was ' + this[0]);
//         return 1;
//     }
// }

func main() {
    // This does and should not compile
    // printIfEqualGeneric(true, 0);
    let v = new Value(99);
    print('Value generics:');
    print(v.replace(12));
    print(v);
    // let array = new Array(0, 3);
    // array[0] = 42;
    // array[1] = v.replace(array[0]);
    // print('Generic Array for-loop:');
    // for i in array {
    //     print(i);
    // }
}
