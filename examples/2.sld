{
    let a = print;
    a(false);
    print(true);
    // too many arguments
    print(-999999, 8, 23 * 6);
    // wrong argument type
    print(print(-1));
}