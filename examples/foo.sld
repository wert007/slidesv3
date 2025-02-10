func main() {
    print(foo(2));
    print(foo(1));
}

func foo(value: uint) -> string {
    if mod(value, 2) == 0 {
        return value + '';
    } else {
        runtimeError('ERROR!');
    }
    print('I am unreachable, but does the program now that?');
}
