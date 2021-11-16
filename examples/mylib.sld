import lib('math') as math;
// library mylib {
//     use func;
// }

func sayHello(name: string) {
    print('Hello, ' + name + '!');
}

struct Point {
    x : int;
    y : int;

    func toVec2() -> math.Vec2 {
        return new math.Vec2(this.x, this.y);
    }
}

func printPoint(pt: Point) {
    print('Point(' + pt.x + ', ' + pt.y + ')');
}