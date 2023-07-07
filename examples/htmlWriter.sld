import lib('presentationDatatypes') as pd;

struct Head {
    cssLinks: List<string>;

    func $constructor() {
        this.cssLinks = new List();
    }
}

struct HtmlWriter {
    writer: StringBuilder;
    openTags: List<string>;
    attributes: Dict<string, string>;
    head: Head?;

    func $constructor() {
        this.writer = new StringBuilder(1024);
        this.openTags = new List();
        this.attributes = new Dict();
        this.head = new Head();
    }

    func openTag(tag: string) {
        this.openTags.add(tag);
        this.writer.write('<');
        this.writer.write(tag);
        if this.attributes.length > 0 {
            for attribute in this.attributes {
                this.writer.write(' ');
                this.writer.write(attribute.key);
                this.writer.write('="');
                this.writer.write(attribute.value);
                this.writer.write('"');
            }
            this.attributes.clear();
        }
        this.writer.write('>');
    }

    func closeTag() {
        let tag = this.openTags.pop();
        if tag {
            this.writer.write('</');
            this.writer.write(tag);
            this.writer.write('>');
        }
    }

    func pushAttribute(name: string, value: string) {
        if this.attributes.contains(name) {
            runtimeError('Can only set each attribute once!');
        }
        this.attributes[name] = value;
    }

    func addCssLink(fileName: string) {
        if this.head {
            this.head.cssLinks.add(fileName);
        } else {
            runtimeError('Wrote head already!');
        }
    }

    func openHtmlTag() {
        this.openTag('html');
    }

    func writeHead() {
        if !this.head {
            runtimeError('Wrote head already!');
            return;
        } else {
            this.openTag('head');
            for cssLink in this.head.cssLinks {
                this.writer.write('<link rel="stylesheet" type="text/css" href="');
                this.writer.write(cssLink);
                this.writer.write('" />');
            }
            this.closeTag();
        }
    }

    func openBodyTag() {
        this.openTag('body');
    }

    func writePTag(content: string) {
        this.openTag('p');
        this.writer.write(content);
        this.closeTag();
    }

    func pushIdAttribute(id: string) {
        this.pushAttribute('id', id);
    }

    func finish() -> string {
        while this.openTags.length > 0 {
            this.closeTag();
        }
        return this.writer + '';
    }
}

abstract struct Selector {}

struct IdSelector: Selector {
    id: string;

    func $toString() -> string {
        return '#' + this.id;
    }
}

struct CssWriter {
    writer: StringBuilder;
    selectors: List<Selector>;
    wrote_any_fields: bool;

    func $constructor() {
        this.writer = new StringBuilder(1024);
        this.selectors = new List();
        this.wrote_any_fields = false;
    }

    func addSelector(selector: Selector) {
        if this.wrote_any_fields {
            runtimeError('Make sure to call finishRule() before calling this again!');
        }
        this.selectors.add(selector);
        this.wrote_any_fields = false;
    }

    func emitSelectors() {
        for i, selector in this.selectors {
            if i != 0 {
                this.writer.write(', ');
            }
            this.writer.write(selector);
        }
        this.writer.write('{');
        this.selectors.clear();
        this.wrote_any_fields = true;
    }

    func addField(name: string, value: pd.UnitLiteral) {
        this.emitSelectors();
        this.writer.write(name);
        this.writer.write(': ');
        this.writer.write(value);
        this.writer.write(';');
    }

    func finishRule() {
        if this.wrote_any_fields {
            this.writer.write('}');
        }
        this.wrote_any_fields = false;
        this.selectors.clear();
    }

    func finish() -> string {
        this.finishRule();
        return this.writer + '';
    }
}