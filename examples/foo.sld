func main() {
    let l: List<int> = new List();
    l.add(5);
    for _ in 0..4 {
        l.add(8);
    }
    print(l);
}

// TODO: This crashes the compiler
// func main() {
//     let l = new List();
//     l.add(5);
//     l.add(9);
// }