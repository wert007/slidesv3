abstract struct Parent {
    field1: int;

    func sayHello() {
        print('Hello ' + this.field1);
    }
}

struct Child: Parent {
    func $toString() -> string {
        base.sayHello();
        return 'child' + base.field1;
    }
}

// struct Child: Parent {
//     func $toString() -> string {
//         return 'child' + base;
//     }
// }

func main() {
    let p = new Child(42);
    print(p);
    p.field1 = 4;
    p.sayHello();
}