// func add(a: int, b: int) {
//     print(a + b);
// }

// func sum(values: int[]) {
//     // print(values.length());
//     let result = 0;
//     for value in values {
//         result = result + value;
//         // print('value = ' + value);
//     }
//     print(result);
// }

// func count_up(n: int) {
//     if n > 0 {
//         count_up(n - 1);
//         // Same as n % 2 == 0
//         if n / 2 == (n + 1) / 2 {
//             return;
//         }
//     }
//     print(n);
// }

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

// func addr(n: string) -> string {
//     return n;
// }

// func with_while(n: int) -> int {
//     let og = n;
//     while n >= og / 10 {
//         n = n / 2;
//     }
//     return n;
// }

func main() {
    let basic_arith = [
        add,
        sub,
        mul,
        div,
        // Throws rightfully diagnostic
        // fac,
    ];

    for arithmetic_function in basic_arith {
        let result = arithmetic_function(5, 3);
        // Make printing of functions illegal. Maybe later print the type, but
        // the time is currently already known during compile time, so that
        // there still would be no need for a type identifier. On the other
        // hand, this is always true. So it would be possible to have a
        // print(msg: string) and call int$to$string(arg) or
        // int$array$to&string(arg, dimension).
        //
        //      print(arithmetic_function);
        print('fn(5, 3) = ' + result);
    }

    // count_up(10);
    // print('add(1, 2)');
    // add(1, 2);
    // print('sum([1, 2])');
    // sum([1, 2]);
    // print('sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])');
    // sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
}