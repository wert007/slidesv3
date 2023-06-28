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
    print(cast a : int);
    print(cast a : bool);
    print(fuzzyAdd(23, a));
    let b = 13;

    let c = -5;
    print('c (-5) is not uint? ' + (cast c : uint == none));
    c = 5;
    print('c (5) is uint and 5? ' + (cast c : uint == 5));
    c = 0;
    print('c (0) is uint and 0? ' + (cast c : uint == 0));

    let d: int? = 32;
    print(d);
    let e: &int = d;
    print(e);
    let f: int? = e;
    print(f);
}
