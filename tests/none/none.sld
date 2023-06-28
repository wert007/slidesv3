func print_noneable(arg: int?) {
    print('Arg was ' + arg);
}

func main() {
    // Step 1. Type Descriptors
    // let i : int = 42;

    // // Step 2. Optional Types
    let m : int? = 99;

    // // Step 3. None Keyword
    let n : int? = none;
    print(n);
    print(m);

    print_noneable(0);
    print_noneable(none);

    // Step 3.0.1 Make diagnostic for
    // let error = none;

    // Step 3.1 ?? Operator
    let a = n ?? 13;
    let foo = none ?? 12;


    // // Step 3.2 if
    if m {
        print(m + 3);
    }


    // // Step 3.3 unary !
    if !!n {
        print(n + 1);
    } else {
        print('n has sadly no value :/');
    }
    let hui = !n;

    // Step 3.4 binary operations
    if m == 99 {
        print('Shoud have gotten here?!');
        print(m - 99);
    }

    // if (n != none) == false
    if n == none {
        n = 12;
        print('Shoud have gotten here as well?!');
    }

    // // Step 4. ???

    // // Step 5. Profit
}