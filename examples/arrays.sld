func main() {
    let a = [1, 2, 3];
    print(a[0]);
    print(a[1]);
    print(a[2]);
    let a_1 = a[1];
    a[1] = 5;
    print(a);
    print('a_1 == 2?');
    print(a_1 == 2);

    let b = [true, false, ];
    let b2 = [true, false, ];
    let b_1 = b[1];
    print('b_1?');
    print(b_1);
    // print(b == a);
    print('b == b2?');
    print(b == b2);
    print('b == [true, false]?');
    print(b == [true, false]);

    let c = [1, 2, 3, 4,];
    print('[4, 3, 2, 1] == [1, 2, 3, 4]?');
    print([4, 3, 2, 1] == [1, 2, 3, 4]);
    print('[1, 2, 3, 4] == c?');
    print([1, 2, 3, 4] == c);
    let c2 = c;
    print('c == c2?');
    print(c == c2);
    let check = c[3] == c.length();
    print('c[3] == c.length()?');
    print(check);
    print('c.length()');
    print(c.length());

    // let d = [5;999] // Array with 999 elements, each being 5
    // print(d.length());
    // print(d[998]);

    // let e = [7, 3;5, 1] // [7, 3, 3, 3, 3, 3, 1]
    // print(e);
    // print(e == [7, 3, 3, 3, 3, 3, 1])

    let f = [ [ 1 ], [ 2, 3 ], [ 4, 5, 6, ], ];
    print(f[2]);
    print(f[2][2]);
    f[2][2] = 999;
    f[1] = [ 42, 1337, 876 ];
    print(f);

    let g = ['Red', 'Blue', 'Yellow', 'Green'];
    print(g);

    let h = [a, c, f[0], f[1], f[2], [843, 43], [321, 32]];
    print(h);

    // let list = new List<int>();
    // list.add(1);
    // list.add([1, 2, 3]);
    // list.concat(list);
    // let length = list.length();
    // let list2 = new List([1, 2, 3, 4]);
    // let list3 = new List(list2);
}