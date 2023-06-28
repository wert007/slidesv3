struct Point {
    x: int;
    y: int;

    func length_squared() -> int {
        return this.x * this.x + this.y * this.y;
    }
}

struct Rect {
    top_left: Point;
    bottom_right: Point;

    func $constructor(x: int, y: int, width: int, height: int) {
        this.top_left = new Point(x, y);
        this.bottom_right = new Point(x + width, y + height);
    }
}

struct Cube {
    // TODO: Arrays of structs do not work, since there is two representations
    // of structs and each generates a different version of the generic array
    // struct.
    sides: Rect[];
}

struct Bar {
    a: Foo;

    func $constructor(a: int) {
        this.a = new Foo(a);
    }
}

struct Foo {
    a: int;
}

struct ListNode {
    value: int;
    next: ListNode;
}

struct Square {
    side: int;

    func area() -> int {
        return this.side * this.side;
    }

    func circumference() -> int {
        return 4 * this.side;
    }

    func resize(side: int) {
        this.side = side;
    }
}

struct A {
    // Maybe this should be illegal.
    this : int;
}

func main() {
    let p = new Point(43, 87);
    let r = new Rect(10, 10, 200, 300);
    let c = new Cube([r, r, r, r, r, r]);
    print('Point.x = ' + p.x);
    print('Rect.bottom_right.y = ' + r.bottom_right.y);
    r.bottom_right.x = 99;
    print('Cube.sides[0].bottom_right.x = ' + c.sides[0].bottom_right.x);

    let b = new Bar(2312);
    print('Bar.Foo.a = ' + b.a.a);

    let s = new Square(5);
    print('Square.side = ' + s.side);
    s.resize(7);
    print('Square.area() = ' + s.area());
    print('Square.circumference() = ' + s.circumference());
    let area = s.area;
    print('area as closure() = ' + area());

    let this = 'Should this be a keyword??';
    this = 'Idk';

    // This needs noneable pointers!
    // let impossible = new ListNode(0, new ListNode(1, new ListNode(2, '')));
}