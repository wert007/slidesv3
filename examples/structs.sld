struct Point {
    x: int;
    y: int;

    func length_squared() -> int {
        this = 5;
        return this.x * this.x + this.y * this.y;
    }
}

struct Rect {
    top_left: Point;
    bottom_right: Point;
}

struct Cube {
    // sides: Rect[];
    top: Rect;
    bottom: Rect;
    north: Rect;
    east: Rect;
    south: Rect;
    west: Rect;
}

struct Bar {
    a: Foo;
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
        // TODO: make this an error:
        this = new Square(side);
    }
}

func main() {
    let p = new Point(43, 87);
    let r = new Rect(p, new Point(1200, 3400));
    let c = new Cube(r, r, r, r, r, r);
    print('Point.x = ' + p.x);
    print('Rect.bottom_right.y = ' + r.bottom_right.y);
    r.bottom_right.x = 99;
    print('Cube.north.bottom_right.x = ' + c.north.bottom_right.x);

    let b = new Bar(new Foo(2312));
    print('Bar.Foo.a = ' + b.a.a);

    let s = new Square(5);
    print('Square.side = ' + s.side);
    s.resize(7);
    print('Square.area() = ' + s.area());
    print('Square.circumference() = ' + s.circumference());
    // This doesn't work right now!
    // let area = s.area;
    // print(area());

    // This needs noneable pointers!
    // let impossible = new ListNode(0, new ListNode(1, new ListNode(2, '')));
}