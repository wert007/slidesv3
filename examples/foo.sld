func main() {
    // Crashes???
    let l = new List('');
    let a = 'he';
    for _ in 0..5 {
        l.add(a);
    }
    print(l);
}