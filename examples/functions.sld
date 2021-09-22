func add(a: int, b: int) {
    print(a + b);
}

func sum(values: int[]) {
    // print(values.length());
    let result = 0;
    for value in values {
        result = result + value;
        // print('value = ' + value);
    }
    print(result);
}

func main() {
    print('add(1, 2)');
    add(1, 2);
    print('sum([1, 2])');
    sum([1, 2]);
    print('sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])');
    sum([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
}