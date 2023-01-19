generic struct A {
    value: &$Type;

    func $constructor() {
        this.value = reallocate(none, 8);
    }

    func $set(index: uint, value: $Type) {
        this.value[0] = value;
    }
}

func main() {
    let a: A<int> = new A();
    a[0] = 5;
}