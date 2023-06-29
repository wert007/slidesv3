struct A<Type> {
    value: $Type;

    func print() {
        print(this.value);
    }

    func printBoxed() {
        boxedPrint(this.value);
    }

    func printGreeted() {
        print('Hello:');
        this.print();
    }
}

func boxedPrint(msg: any) {
    print('BOX: ' + msg);
}

func main() {
    let intVersion = new A(3);
    let boolVersion = new A(false);
    let stringVersion = new A('Hello');
    intVersion.print();
    boolVersion.print();
    stringVersion.print();
    intVersion.printBoxed();
    boolVersion.printBoxed();
    stringVersion.printBoxed();
    intVersion.printGreeted();
    boolVersion.printGreeted();
    stringVersion.printGreeted();

    let a = new TwoLevelGeneric(1, [1, 2, 1, 1]);
    a.check();
}

struct TwoLevelGeneric<Type> {
    expected_mean: $Type;
    values: $Type[];

    func check() {
        for index, value in this.values {
            if value == this.expected_mean {
                print('Index ' + index + ' passed check!');
            }
        }
    }
}
