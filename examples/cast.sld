func fuzzyAdd(value: int, add: any) -> int {
    if cast add : int {
        return value + cast add : int;
    } else if cast add : bool {
        if cast add : bool == true {
            return value + 1;
        } else {
            return value;
        }
    } else {
        return value;
    }
}

func main() {
    let a : any = 42;
    // print(a);
    print(cast a : int);
    print(cast a : bool);
    print(fuzzyAdd(23, a));
}