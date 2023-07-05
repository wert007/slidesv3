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


abstract struct Element {
    position: Position;

    func $constructor(x: int, y: int) {
        this.position = new Position(x, y);
    }
}

struct Label: Element {
    text: string;

    func $constructor(text: string) {
        this.text = text;
        base.constructor(0, 0);
    }
}

struct Position {
    x: int;
    y: int;
}

func main() {
    let p = new Child(42);
    print(p);
    p.field1 = 4;
    p.sayHello();

    print(new Cat(4));

    let l: Element = new Label('hello');
    print(l);
}

abstract struct Animal {
    v: int;
}

struct Cat: Animal {
    
}