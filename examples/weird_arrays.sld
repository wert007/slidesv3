struct A {
    value: int;

    func add(a: int) -> int {
        return a + this.value;
    }
}

struct B {
    value: int;

    func sub(a: int) -> int {
        return a - this.value;
    }
}

struct C {
    value: int;

    func mul(a: int) -> int {
        return a * this.value;
    }
}

struct D {
    value: int;

    func div(a: int) -> int {
        return a / this.value;
    }
}

func main() {
    let a = new A(1);
    let b = new B(2);
    let c = new C(3);
    let d = new D(4);
    let weird_closure_array = [
        a.add,
        b.sub,
        c.mul,
        d.div,
    ];

    for closure in weird_closure_array {
        print('closure(5) = ' + closure(5));
        // print(closure + '(5) = ' + closure(5));
    }
}

