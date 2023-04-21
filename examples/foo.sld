import lib('presentation.sld') as presentation;

func main() {
    let pf = new presentation.PresentationFlags(
        presentation.PresentationColorTheme.Dark,
        false,
        presentation.PrismSyntaxHighlighting.Tomorrow
    );
    print(pf);
}