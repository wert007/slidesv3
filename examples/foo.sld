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
    text: string;

    func printAsHtml() {
        print('<b>' + this.text + '</b>');
    }
}

func main() {
    let elements: List<Element> = new List();
    elements.add(new Label('Hello World!'));
    elements.add(new Label('test' + ' haha!'));
    elements.add(new BoldLabel('Hello World!'));
    for e in elements {
        e.printAsHtml();
    }
}