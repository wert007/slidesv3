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

func printLog(obj: any) {
    print('Log: ' + obj);
}

func main() {
    let log = printLog;
    // let fs = [
    //     print,
    //     printLog
    // ];
    log(42);
    log(true);
    log(none);
    let n : int? = 13;
    log(n);
    n = none;
    log(n);
    log([1, 2, 3,]);
    log([[1], [2], [3],]);
    log(print);
    log('Hello World!');
    log(fib);
    let s : string? = 'Noneable strings work just fine';
    log(s);
    log(new Person('John Doe', 32));
}
