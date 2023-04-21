struct A {
    field: B;
}

struct B {
    a: int;
    b: int;
    c: bool;
}

func main() {
    let a = new A(new B(1, 2, true));
    print(a);
}