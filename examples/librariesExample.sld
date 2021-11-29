import lib('mylib') as mylib;
import lib('math') as math;

func main() {
    print('Hello from the main :)');
    let pt = new mylib.Point(3, 4);
    let v = pt.toVec2();
    mylib.printPoint(pt);
    print(v.lengthSquared());
    print(v);
}