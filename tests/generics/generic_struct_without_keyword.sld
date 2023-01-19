// This should be either a compile error or work. right now it does neither.
struct A {
    value: $Type;
}

func main() {
    let a = new A(3);
}