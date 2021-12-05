func printAny(value: any) {
    print('printAny: ' + value);
}

generic func printGeneric(value: $Type) {
    print('printGeneric: ' + value);
}

func printIfEqual(a: any, b: any) {
    if a == b {
        print(a);
    }
}

generic func printIfEqualGeneric(a: $Type, b: $Type) {
    if a == b {
        print(a);
    }
}

func main() {
    // This does not compile
    // printIfEqualGeneric(true, 0);
    // This does.
    printIfEqual(true, 0);
}
