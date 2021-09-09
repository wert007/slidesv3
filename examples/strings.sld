{
    print('Im a literal!');

    let h = 'Hello World!';
    print(h);

    let array_test = ['Hello', 'World', '!'];
    print(array_test);

    let a = 'Hewwo';
    let b = ' Meowrld! o//';
    let c = a + b;
    print(c);

    let d = a + ' :3';
    print(d);

    print(c);

    print('uWu' + b);

    print('lol' + 'lal');

    let e = 'ha' + 'he';
    print(e);

    let f = [a, b, c, d, e];
    print(f);

    let g = [ 'maybe' + 'this', 'works' + 'maybe??', 'literal', 'heap' + 'allocated'];
    print(g);
    let len = h.length();
    print(len);

// TODO: This has the wrong result
    let len_fn = 'This is interesting'.length;
    print(len_fn());

// TODO: String concatination with any type
    // print('Len was ' + len);
}