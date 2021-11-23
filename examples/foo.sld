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
    let b : any = new Value(13);
    print(b);
    let c = [new Value(1), new Value(2)];
    print(c);
    let d : any = a;
    print(d);
}