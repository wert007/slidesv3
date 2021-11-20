import lib('mylib') as mylib;
import lib('math') as math;

func main() {
    print('Hello from the main :)');
    let pt = new mylib.Point(3, 4);
    // This seems to call mylib.printPoint(pt) somehow and does not return the vector
    let v = pt.toVec2();
    mylib.printPoint(pt);
    // Accessing fields on this value does not work.
    print(v.lengthSquared());
    printVec(v);
}

func printVec(v: math.Vec2) {
    print('Vec(' + v.x + ', ' + v.y + ')');
}