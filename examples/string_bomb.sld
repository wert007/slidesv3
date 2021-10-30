func main() {
    let i = 0;
    let a = 'a';
    while i < 200000 {
        a = a + '';
        i = i + 1;
    }
    print(a);
}