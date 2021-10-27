func trigger_gc() -> int {
    return 42;
}

// This already works, since chunks can easily be reused.
func simple() {
    let a = 'a';
    a = a + 'b'; // This should be freed.
    a = a + 'c';
    a = '0' + a;
    trigger_gc();
}

// TODO: Since chunks are only splitted and never combined back into bigger
// chunks, this does not work yet. (a needs twice the memory then b does. And b
// fills all the memory with small chunks)
func splitted() {
    let a = 'Very long text!!';
    // let a = 'works';
    let b = 'a' + '';
    b = 'a' + '';
    b = 'a' + '';
    b = 'a' + '';
    trigger_gc();
    a = a + '';
}

func main() {
    splitted();
}
