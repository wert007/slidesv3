func main() {
    arrays_in_arrays();
    arrays();
    weird_arrays();
}

func arrays_in_arrays() {
    let a = [ [ 1, ], [ 2, ], ];
    print(a);
    let b = [ [ [ 1, ], ], [ [ 2, ], ], ];
    b[0][0][0] = 2;
    print(b);
    let c = [ [ ['This'] ], [ [ 'is'] ], [ [ 'very' ]], [['mean']], [['!']]];
    print(c);
}

func arrays() {
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
    let check = clampToUnsigned(c[3]) == c.length();
    print('c[3] == c.length()?');
    print(check);
    print('c.length()');
    print(c.length());

    let d = [5;120]; // Array with 120 elements, each being 5
    print(d.length());
    print(d[119]);

    let e = [7, 3;5, 1]; // [7, 3, 3, 3, 3, 3, 1]
    print(e);
    print(e == [7, 3, 3, 3, 3, 3, 1]);

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
}

struct A {
    value: int;

    func add(a: int) -> int {
        return a + this.value;
    }
}

struct B {
    value: int;

    func sub(a: int) -> int {
        return a - this.value;
    }
}

struct C {
    value: int;

    func mul(a: int) -> int {
        return a * this.value;
    }
}

struct D {
    value: int;

    func div(a: int) -> int {
        return a / this.value;
    }
}

func weird_arrays() {
    let a = new A(1);
    let b = new B(2);
    let c = new C(3);
    let d = new D(4);
    let weird_closure_array = [
        a.add,
        b.sub,
        c.mul,
        d.div,
    ];

    // let i = 0;
    // while i < 4 {
    //     print('closure(5) = ' + weird_closure_array[i](5));
    //     i = i + 1;
    // }

    for closure in weird_closure_array {
        print('closure(5) = ' + closure(5));
        // print(closure + '(5) = ' + closure(5));
    }
}

