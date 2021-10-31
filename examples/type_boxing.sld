struct Person {
    name: string;
    age: int;
}

func fib(n: int) -> int {
    if n <= 0 {
        return 1;
    } else if n <= 2 {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

func main() {
    print(42);
    print(true);
    print(none);
    let n : int? = 13;
    print(n);
    n = none;
    print(n);
    print([1, 2, 3,]);
    print([[1], [2], [3],]);
    print(print);
    print('Hello World!');
    // TODO
    print(fib);
    // let s : string? = 'This would always fail i think?';
    // print(s);
    // print(new Person('John Doe', 32));
}
