func main() {
    // Crashes???
    // let l = new List('');
    // for _ in 0..5 {
    //     l.add('he');
    // }
    // print(l);

    let l = new List(0);
    for i in 0..5 {
        l.add(i);
    }
    // let ha = 'ha' + 'ha' + 'ha!';
    // print(ha);
    heapdump('foo_array_list');
    // print('main:');
    // for e in l {
    //     print(e);
    // }
    print(l);
}