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
    position: Position;

    func $constructor(x: int, y: int) {
        this.position = new Position(x, y);
    }
}

struct Label: Element {
    text: string;

    func $constructor(text: string) {
        this.text = text;
        base.constructor(1, 65);
    }
}

struct Position {
    x: int;
    y: int;
}

struct Style {}
struct SvgFilterArgs {}
