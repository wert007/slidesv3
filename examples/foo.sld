abstract struct Element {
    func printAsHtml() {
        runtimeError('This is an abstract function, and should never be called!');
    }
}

struct Label : Element {
    text: string;

    func printAsHtml() {
        print('<p>' + this.text + '</p>');
    }
}

struct BoldLabel : Element {
    boldText: string;

    func printAsHtml() {
        print('<b>' + this.boldText + '</b>');
    }
}

func main() {
    let elements: List<Element> = new List();
    let e: Element = new Label('test');
    match e {
        l: Label => {
            print(l.text);
        }
        b: BoldLabel => {
            print(b.boldText);
        }
        else => {
            runtimeError('Should be unreachable, but the compiler does not check this!');
        }
    }
}