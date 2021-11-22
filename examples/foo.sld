struct Value {
    value: int;

    func value() -> int {
        return this.value;
    }

    // func value(value: int) {
    //     this.value = value;
    // }

    func $toString() -> string {
        return 'Value(' + this.value + ')';
    }
}

func main() {
    let value = new Value(42);
    print(value);
    let a : Value? = new Value(1337);
    print(a);
    a = none;
    print(a);
    // TODO: Implement $toString for any types.
    let b : any = new Value(13);
    print(b);
    // TODO: Implement $toString for array types.
    let c = [new Value(1), new Value(2)];
    print(c);
}