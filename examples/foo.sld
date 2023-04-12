// struct Presentation {
//     slides: List<Slide>;
// }
// 
// struct Slide {
//     steps: List<Step>;
// }
// 
// struct Step {
//     elements: List<Label>;
// }
// 
// struct Element {}
// 
// struct Label : Element {
//     text: string;
// }
// 
// func emitHtml(presentation: Presentation) {
//     for slide in presentation.slides {
//         emitHtmlForSlide(slide);
//     }
// }
// 
// func emitHtmlForSlide(slide: Slide) {
//     for step in slide.steps {
//         emitHtmlForStep(step);
//     }
// }
// 
// func emitHtmlForStep(step: Step) {
//     for element in step.elements {
//         emitHtmlForElementLabel(element);
//     }
// }
// 
// func emitHtmlForElementLabel(label: Label) {
//     print('<p>' + label.text + '</p>');
// }
// 
// func main() {
//     let presentation = new Presentation(new List());
//     let slide = new Slide(new List());
//     let step = new Step(new List());
//     let hello_world = new Label('Hello World!');
//     step.elements.add(hello_world);
//     slide.steps.add(step);
//     presentation.slides.add(slide);
//     emitHtml(presentation);
// }

// TODO: doesnt work currently
// struct Parent {}
// 
// struct Child : Parent {}
// 
// func main() {
//     let p: Parent? = new Child();
// 
// }


func main() {
    let a = [1, 2, 3];
    // print(a[99]);
    // a[99] = -55;
    print(a);
}