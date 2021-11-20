func main() {
    let a = ['Hello', 'World', 'how are you', '?'];
    for i in a {
        print(i);
    }
    for i in a {
        print(i);
    }
    for j in [0, 1] {
        for i in a {
            print(i);
        }
    }

    // let z = [ [1], [2], [3], ];
    // for i, it in z {
    //     it[0] = i;
    // }
    // print(z);

    // {
    //     let i$index = 0;
    //     let i$collection = a;
    //     while i$index < i$collection.length() {
    //         let i = i$collection[i$index];
    //         {
    //             print(i);
    //         }
    //         i$index += 1;
    //     }
    // }

    let a2 = [1, 2, 3, 4];
    let b = [false, false, false, false, ];
    for i, it in a2 {
        b[i] = it - i == 1;
    }
    print(b);

    let max = 9;
    for i, j in 3..max {
        print(i + ' ' + j);

    }

    for i, j in stepBy(3..12, 5) {
        print(i + ' ' + j);
    }

    let changed = [1, 2, 3, 4];
    for i in 0..changed.length() {
        changed[i] = i;
    }

    print(changed);

    // {
    //     let i = 3;
    //     let i$increase = 12 >= 3;
    //     while i < 12 {
    //         {
    //             print(i * i);
    //         }
    //         if i$increase {
    //             i = i + 1;
    //         } else {
    //             i = i - 1;
    //         }
    //     }
    // }
}