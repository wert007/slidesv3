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

func fac(n: int) -> int {
    if n <= 1 {
        return 1;
    } else {
        return n * fac(n - 1);
    }
}

func with_while(n: int) -> int {
    let og = n;
    while n >= og / 10 {
        n = n / 2;
    }
    return n;
}

func main() {
    print(fac(add(1, 2)));
    print(with_while(1000));
    // count_up(10);
    // print('add(1, 2)');
    // add(1, 2);
    // print('sum([1, 2])');
    // sum([1, 2]);
    // print('sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])');
    // sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
}