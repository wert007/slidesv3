struct HtmlWriter {
    writer: StringBuilder;
    openTags: List<string>;

    func $constructor() {
        this.writer = new StringBuilder(1024);
        this.openTags = new List();
    }

    func openTag(tag: string) {
        this.openTags.add(tag);
        this.writer.write('<');
        this.writer.write(tag);
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

    func openHtmlTag() {
        this.openTag('html');
    }

    func openBodyTag() {
        this.openTag('body');
    }

    func writePTag(content: string) {
        this.openTag('p');
        this.writer.write(content);
        this.closeTag();
    }

    func finish() -> string {
        while this.openTags.length > 0 {
            this.closeTag();
        }
        return this.writer + '';
    }
}