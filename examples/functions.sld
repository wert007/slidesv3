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
    return a + b;
}

func main() {
    print(add(1, 2));
    // count_up(10);
    // print('add(1, 2)');
    // add(1, 2);
    // print('sum([1, 2])');
    // sum([1, 2]);
    // print('sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])');
    // sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
}