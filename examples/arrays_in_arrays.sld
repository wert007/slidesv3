func main() {
    let a = [ [ 1, ], [ 2, ], ];
    print(a);
    let b = [ [ [ 1, ], ], [ [ 2, ], ], ];
    // b[0][0][0] = 2;
    print(b);
    let c = [ [ ['This'] ], [ [ 'is'] ], [ [ 'very' ]], [['mean']], [['!']]];
    // let c = [ ['This'], [ 'is'], [ 'very' ], ['mean'], ['!'], ];
    print(c);
}