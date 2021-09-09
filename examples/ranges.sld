{
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


    // TODO: Ranges
    // for i in 0..12 {
    //     print(i * i);
    // }
}