struct A<Type> {
    a: int;
}

func main() {
    let a: A<string> = new A(99);
    let b = new A(3);
}