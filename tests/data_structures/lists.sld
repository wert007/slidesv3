func main() {
    let l: List<string> = new List();
    let a = 'he';
    for _ in 0..95 {
        l.add(a);
    }
    print(l);

    let c: uint = 5;
    let b = list ['hello'; c];
    print(b.length());
    print(b.capacity);
    print(b);
}