
struct MyDict<Key, Value> {
    bucket: List<List<KeyValuePair<$Key, $Value> > >;
    length: uint;

    func $constructor() {
        let hack3_1: List<KeyValuePair<$Key, $Value> > = new List();
        this.bucket = list [hack3_1.clone(); 4];
        this.length = 0;
    }

    func resize() {
        let length = 2 * this.bucket.length;
        let oldBucket = this.bucket;
        let hack3_1: List<KeyValuePair<$Key, $Value> > = new List();
        this.bucket = list [hack3_1.clone(); length];
        this.length = 0;

        for b in this.bucket {
            for e in b {
                this[e.key] = e.value;
            }
        }
    }

    func $set(index: $Key, value: $Value) {
        if this.length >= this.bucket.length {
            this.resize();
        }
        let indexHash = hash(index);
        let bucket = this.bucket[mod(indexHash, this.bucket.length)];
        for e in bucket {
            if e.key == index {
                e.value = value;
                return;
            }
        }
        // Key is not yet in the bucket!
        bucket.add(new KeyValuePair(index, value));
        this.length = this.length + 1;
    }

    func get(index: $Key) -> $Value? {
        let hash = hash(index);
        let bucket = this.bucket[mod(hash, this.bucket.length)];
        for e in bucket {
            if e.key == index {
                return e.value;
            }
        }
        return none;
    }

    func $toString() -> string {
        let result = '[';
        for bucket in this.bucket {
            for value in bucket {
                result = result + value.key + ' => ' + value.value + ', ';
            }
        }
        result = result + ']';
        return result;
    }

    func $elementCount() -> uint {
        return this.length;
    }

    func $get(index: uint) -> KeyValuePair<$Key, $Value> {
        for bucket in this.bucket {
            if bucket.length > index {
                return bucket[index];
            } else {
                index = index - bucket.length;
            }
        }
        runtimeError('Index out of bounds exception!');
        return this.bucket[0][0];
    }

    func length() -> uint {
        return this.length;
    }
}

struct KeyValuePair<Key, Value> {
    key: $Key;
    value: $Value;
}



func main() {
    let a: MyDict<string, int> = new MyDict();
    a['tom'] = 5;
    a['roger'] = 2;
    a['steve'] = 8;
    print(a);

    for kv in a {
        print(kv.key + ' is the name of ' + kv.value + ' people');
    }
}
