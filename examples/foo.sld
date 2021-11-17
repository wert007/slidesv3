import lib('mylib') as mylib;
import lib('math') as math;

func main() {
    print('Hello from the main :)');
    let pt = new mylib.Point(12, 43);
    // print(pt);
    // mylib.printPoint(pt);
    // pt.toVec2();
    print(pt.toVec2());
}

// func printVec(v: math.Vec2) {
//     print('Vec(' + v.x + ', ' + v.y + ')');
// }