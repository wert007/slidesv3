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

struct Slide {}
struct Style {}
struct SvgFilterArgs {}
