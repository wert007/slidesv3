func main() {
    print('Hello');
    let count = 10;
    let dict: Dict<string, int> = new Dict();
    for i in 0..count {
        dict['tom' + i] = i;
    }
    for i in 0..count {
        let value = dict['tom' + 1];
        if value != i {
            print('value was ' + value);
            print('i was ' + i);
            runtimeError('Resize test: failed');
        }
    }
    print('Resize test: okay');
}