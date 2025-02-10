import lib('presentation') as presentation;
import lib('presentationDatatypes') as pd;

func main() {
    let lbl = new presentation.Label('Hello World!');
    lbl.name = 'lbl';
    lbl.fontSize = pd.pixel(99.0);
    let slide1 = new presentation.Slide(list [new presentation.Step(list [lbl])]);
    let p = new presentation.Presentation(
        list [slide1],
        new presentation.PresentationFlags(
            presentation.PresentationColorTheme.Light,
            false,
            presentation.PrismSyntaxHighlighting.None
        ),
        list [],
        list [],
        list [],
        list []
    );
    let emitter = new presentation.HtmlEmitter(p);
    emitter.emit('foo');
}
