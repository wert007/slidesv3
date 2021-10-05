struct Point {
    x: int;
    y: int;
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

// TODO: Any order
// struct Bar {
//     a: Foo;
// }

// struct Foo {
//     a: int;
// }

// TODO: Recursion?
// struct ListNode {
//     value: int;
//     next: ListNode?;
// }

// TODO: Functions on structs
// struct Square {
//     side: int;

//     func area() -> int {
//         return this.side * this.side;
//     }

//     func circumference() -> int {
//         return 4 * this.side;
//     }
// }

func main() {
    print('Hello World!');
    let p = new Point(43, 87);
    let r = new Rect(p, new Point(1200, 3400));
    let c = new Cube(r, r, r, r, r, r);
    print(p.x);
    print(r.bottom_right.y);
    r.bottom_right.x = 99;
    print(c.north.bottom_right.x);
}