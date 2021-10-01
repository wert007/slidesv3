func sum(values: int[]) {
    let result = 0;
    for value in values {
        result = result + value;
    }
    print(result);
}

func count_up(n: int) {
    if n > 0 {
        count_up(n - 1);
    }
    print(n);
}

func add(a: int, b: int) -> int {
    if a == 0 {
        return b;
    } else if b == 0 {
        return a;
    } else {
        return a + b;
    }
}

func sub(a: int, b: int) -> int {
    return a - b;
}

func mul(a: int, b: int) -> int {
    return a * b;
}

func div(a: int, b: int) -> int {
    return a / b;
}

func fac(n: int) -> int {
    if n <= 1 {
        return 1;
    } else {
        return n * fac(n - 1);
    }
}

func main() {
    let basic_arith = [
        add,
        sub,
        mul,
        div,
        // Throws rightfully diagnostic
        // fac,
    ];

    for index, arithmetic_function in basic_arith {
        let result = arithmetic_function(5, 3);
        print('fn(5, 3) = ' + result);
        print('fn(fn(5, 3), ' + index + ') = ' + arithmetic_function(result, index));
    }
}