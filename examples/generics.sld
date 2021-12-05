// func printAny(value: any) {
//     print('printAny: ' + value);
// }

// generic func printGeneric(value: $Type) {
//     let a = value;
//     print('printGeneric: ' + value);
// }

// // TODO: This crashes equals
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

// func main() {
//     // This does not compile
//     // printIfEqualGeneric(true, 0);
//     // This does.
//     // printIfEqual(true, 0);
// }


generic func printGeneric(value: $Type) {
    let a = value;
    print('printGeneric: ' + value);
}

generic func printGeneric2(value: $Type) {
    let a = value;
    print('printGeneric2: ' + value);
}

func main() {
    // TODO: This calls printGeneric2...
    printGeneric(9);
    printGeneric2(true);
}