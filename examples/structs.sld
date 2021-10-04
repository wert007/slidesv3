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

// TODO: Field Access
// func field_access(rect: Rect) -> int {
//     return rect.top_left.x;
// }

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
// func Rect::area() -> int {
//     let w = this.bottom_right.x - this.top_left.x;
//     let h = this.bottom_right.y - this.top_left.y;
//     return w * h;
// }

func main() {
    print('Hello World!');
    let p = new Point(43, 87);
    let r = new Rect(p, new Point(1200, 3400));
    let c = new Cube(r, r, r, r, r, r);
}