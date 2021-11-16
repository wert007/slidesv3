// library mylib {
//     use func;
// }

func sayHello(name: string) {
    print('Hello, ' + name + '!');
}

struct Point {
    x : int;
    y : int;
}

func printPoint(pt: Point) {
    pt = 5;
    print('Point(' + pt.x + ', ' + pt.y + ')');
}