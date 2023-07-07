import lib('htmlWriter') as html;
import lib('presentationDatatypes') as pd;

struct Presentation {
    slides: List<Slide>;
    flags: PresentationFlags;
    styles: List<Style>;
    custom_filter: List<SvgFilterArgs>;
    css_imports: List<string>;
    referenced_files: List<string>;
}

struct PresentationFlags {
    color_theme: PresentationColorTheme;
    uses_youtube: bool;
    code_highlighting: PrismSyntaxHighlighting;
}

enum PresentationColorTheme {
    Dark,
    Light,
}

enum PrismSyntaxHighlighting {
    None,
    Coy,
    Dark,
    Funky,
    Okaidia,
    SolarizedLight,
    Tomorrow,
    Twilight,
    Default,
}

struct Slide {
    steps: List<Step>;
}
struct Step {
    elements: List<Element>;
}
abstract struct Element {
    name: string;

    func $constructor() {
        this.name = '';
    }
}

struct Label: Element {
    text: string;
    fontSize: pd.UnitLiteral?;

    func $constructor(text: string) {
        this.text = text;
        this.fontSize = none;
        base.constructor();
    }
}

struct Position {
    x: int;
    y: int;
}

struct Style {}
struct SvgFilterArgs {}

struct HtmlEmitter {
    presentation: Presentation;
    htmlWriter: html.HtmlWriter;
    cssWriter: html.CssWriter;

    func $constructor(presentation: Presentation) {
        this.presentation = presentation;
        this.htmlWriter = new html.HtmlWriter();
        this.cssWriter = new html.CssWriter();
    }

    func emit(directory: string) {
        this.htmlWriter.openHtmlTag();
        this.htmlWriter.addCssLink('style.css');
        this.htmlWriter.writeHead();
        this.htmlWriter.openBodyTag();
        for slide in this.presentation.slides {
            this.emitSlide(slide);
        }
        writeToFile(directory + '/index.html', this.htmlWriter.finish());
        writeToFile(directory + '/style.css', this.cssWriter.finish());
    }

    func emitSlide(slide: Slide) {
        for step in slide.steps {
            this.emitStep(step);
        }
    }

    func emitStep(step: Step) {
        for element in step.elements {
            this.emitElement(element);
        }
    }

    func emitElement(element: Element) {
        this.htmlWriter.pushIdAttribute(element.name);
        this.cssWriter.addSelector(new html.IdSelector(element.name));
        match element {
            label: Label => {
                this.emitElementLabel(label);
            }
            else => {
                runtimeError('Unhandled element kind: ' + typeOfValue(element));
            }
        }
        this.cssWriter.finishRule();
    }

    func emitElementLabel(label: Label) {
        this.htmlWriter.writePTag(label.text);
        if label.fontSize {
            this.cssWriter.addField('font-size', label.fontSize);
        }
    }
}
