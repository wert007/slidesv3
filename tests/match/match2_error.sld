func main() {
    match 5 {
        'hello' => {
            print('expected a type error!');
        }
        true => {
            print('expected a type error!');
        }
        else => {
            print('this is the only normal thing here');
        }
    }

    match 5 {
        else => {
            print('all good!');
        }
        else => {
            print('but we cannot have multiple of these!');
        }
    }

    let b = 99 + 5;

    match b {
        b => {
            print('illegal, since b is not a constant!');
        }
        else => {
            print('hello!');
        }
    }

    match 5 {
        5 => {
            print('This is a five');
        }
        5 => {
            print('This is also a five');
        }
        5 => {
            print('This is still a five');
        }
        else => {}
    }

    match false {
        true => {
            print('We did not match all possible cases!');
        }
    }

    match print {
        else => {
            print('cannot match on system calls!');
        }
    }

    match none {
        else => {
            print('cannot match on value none!');
        }
    }
}