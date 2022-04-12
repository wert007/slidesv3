func printUintArray(array: uint[]) {
    print(array);
}

func main() {
    let a = 42;
    a = -32 + a;

    print(a);

    // TODO:
    let b : int? = 32;
    if b == 32 {
        print('yay!');
    }

    let c : uint? = 32;
    if c == 32 {
        print('uyay!');
    }

    // TODO:
    let d : uint = 3;
    let e : uint[] = [1, 2, d];
    printUintArray(e);
}