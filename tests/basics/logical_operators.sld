func main() {
    print('false && loudlyFalse()');
    print(false && loudlyFalse());
    print('true || loudlyTrue()');
    print(true || loudlyTrue());
    print('true && loudlyFalse()');
    print(true && loudlyFalse());
    print('false || loudlyTrue()');
    print(false || loudlyTrue());
}

func loudlyFalse() -> bool {
    print('FALSE');
    return false;
}

func loudlyTrue() -> bool {
    print('TRUE');
    return true;
}