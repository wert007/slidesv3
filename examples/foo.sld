import lib('mylib') as mylib;

func main() {
    print('Hello from the main :)');

    mylib.sayHello('World!');

    let pt = new mylib.Point(12, 34);
    // This should not work.
    // pt = new BadPoint(-12, -34);
    mylib.printPoint(pt);

    let foo = new pt.HasNoType();
}

struct AnyTypeThatIsDeclaredHere {
    x: int;
    y: int;
    tellinYa: string;
}