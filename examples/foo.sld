import lib('mylib') as mylib;
import lib('math') as math;

func main() {
    print('Hello from the main :)');

    mylib.sayHello('World!');

    let pt = new mylib.Point(12, 34);
    // This should not work.
    // pt = new BadPoint(-12, -34);
    mylib.printPoint(pt);

    let vel = new math.Vec2(1, 1);
}

struct AnyTypeThatIsDeclaredHere {
    x: int;
    y: int;
    tellinYa: string;
}