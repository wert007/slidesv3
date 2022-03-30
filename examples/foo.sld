struct A {
    value: int;
}

struct B {
    values: A[];
}

func main() {
    let b = new B([new A(1)]);
    // print('Hello World!');
}