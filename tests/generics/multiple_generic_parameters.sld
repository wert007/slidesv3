struct Foo<Bar, Baz> {
    field: $Bar;
    field2: $Baz;

    func $toString() -> string {
        return this.field + ' - ' + this.field2;
    }
}

func main() {
    let foo = new Foo(-10, false);
    print(foo);
    let ha = new Foo('Hello', 'Hallo');
    print(ha);
}