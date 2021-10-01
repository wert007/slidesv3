struct Point {
    x: int;
    y: int;
}

struct Rect {
    top_left: Point;
    bottom_right: Point;
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

// TODO: Constructors
// func example(a: int) -> Bar {
//     return new Bar(new Foo(a));
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
}