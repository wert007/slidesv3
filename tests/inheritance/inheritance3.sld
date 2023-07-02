abstract struct Animal {
    v: int;
}

struct Cat: Animal {
    func $constructor() {
        this.v = 0;
    }

    func a() {
        this.v = 999;
    }

    func $toString() -> string {
        return base + 'Im a Cat!';
    }
}


func main() {
    let a: Animal = new Cat();
    print(a.v == 0);
    let ac = cast a: Cat;
    if ac {
        ac.a();
        print(ac);
    }
    print(a.v == 999);
}