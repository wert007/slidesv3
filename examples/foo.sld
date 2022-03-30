struct B {
    values: A[];

    func $toString() -> string {
        return this.values + '';
    }
}

struct A {
    value: int;

    func $toString() -> string {
        return 'A(' + this.value + ')';
    }
}

func main() {
    let b = new B([new A(1)]);
    print(b);
    // print('A(42) = ' + new A(42));
}