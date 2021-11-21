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
}