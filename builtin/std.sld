struct MyString {
    length: uint;
    bytes: &byte;

    func $constructor() {
        this.length = 0;
        this.bytes = none;
    }

    func $equals(other: MyString) -> bool {
        if this.length != other.length {
            return false;
        }
        for i in 0..this.length {
            if this.bytes[i] != other.bytes[i] {
                return false;
            }
        }
        return true;
    }

    func $add(rhs: any) -> MyString {
        let rhsString = rhs + '';
        let length = this.length + rhsString.length();
        let bytes : &byte = reallocate(none, length);
        if bytes == none {
            runtimeError('Could not allocate string!');
            return this;
        }
        for i in 0..this.length {
            bytes[i] = this.bytes[i];
        }
        for j, i in this.length..length {
            bytes[i] = rhsString.bytes[j];
        }
        let result = new MyString();
        result.length = length;
        result.bytes = bytes;
        return result;
    }

    func length() -> uint {
        // TODO: This crashed the compiler!
        // return this.length();
        return this.length;
    }

    func $toString() -> string {
        let result = '';
        for i in 0..this.length {
            result = result + byteToChar(this.bytes[i]);
        }
        return result;
    }
}

struct Range {
    start: int;
    end: int;
    stepSize: int;

    func $constructor(start: int, end: int) {
        this.start = start;
        this.end = end;
        if start > end {
            this.stepSize = -1;
        } else {
            this.stepSize = 1;
        }
    }

    func $get(index: uint) -> int {
        return this.start + this.stepSize * index;
    }

    func $elementCount() -> uint {
        let result = (this.end - this.start) / this.stepSize;
        return clampToUnsigned(result);
    }

    func $equals(other: Range) -> bool {
        return this.start == other.start && this.end == other.end && this.stepSize == other.stepSize;
    }
}

struct UnsignedRange {
    start: uint;
    end: uint;
    stepSize: int;

    func $constructor(start: uint, end: uint) {
        this.start = start;
        this.end = end;
        if start > end {
            this.stepSize = -1;
        } else {
            this.stepSize = 1;
        }
    }

    func $get(index: uint) -> uint {
        return this.start + clampToUnsigned(this.stepSize * index);
    }

    func $elementCount() -> uint {
        let result = (this.end - this.start) / this.stepSize;
        return clampToUnsigned(result);
    }

    func $equals(other: UnsignedRange) -> bool {
        return this.start == other.start && this.end == other.end && this.stepSize == other.stepSize;
    }
}

func stepBy(range: Range, stepSize: int) -> Range {
    let result = new Range(range.start, range.end);
    result.stepSize = stepSize;
    return result;
}

struct Array<Type> {
    length: uint;
    buffer: &$Type;

    func $constructor(basis: $Type, length: uint) {
        this.length = length;
        this.buffer = reallocate(none, 8 * length);
        if this.buffer == none {
            runtimeError('Could not allocate array on the heap!');
        }
        for i in 0..length {
            // FIXME: This crashes the compiler for some reason?
            // Maybe because, at the time of binding, the $get function has not
            // been bound yet?
            //
            // Not anymore! But also doesn't work!
            this[i] = basis;
            // this.buffer[i] = basis;
        }
    }

    /* inline */ func checkArrayBounds(index: uint) {
        if index >= this.length {
            runtimeError('Index ' + index + ' is out of bounds of array with length ' + this.length + '.');
        }
    }

    func $get(index: uint) -> $Type {
        this.checkArrayBounds(index);
        return this.buffer[index];
    }

    func $set(index: uint, value: $Type) {
        this.checkArrayBounds(index);
        this.buffer[index] = value;
    }

    func $elementCount() -> uint {
        return this.length;
    }

    func $toString() -> string {
        let result = '[ ';
        let isFirst = true;
        for i in 0..this.length {
            if isFirst {
                isFirst = false;
            } else {
                result = result + ', ';
            }
            result = result + this[i];
        }
        result = result + ' ]';
        return result;
    }

    func $equals(other: Array<$Type>) -> bool {
        if this.length != other.length {
            return false;
        }
        for i in 0..this.length {
            if this.buffer[i] != other.buffer[i] {
                return false;
            }
        }
        return true;
    }

    func length() -> uint {
        return this.length;
    }
}

func clampToUnsigned(value: int) -> uint {
    return cast value : uint ?? 0;
}

func mod(lhs: uint, rhs: uint) -> uint {
    let tooMuch : uint = lhs / rhs;
    return lhs - rhs * tooMuch;
}

func toHex(value: uint) -> string {
    if value == 0 {
        return '0x0';
    }
    let result = '';
    let digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ];
    while value > 0 {
        let digit = mod(value, 16);
        result = digits[digit] + result;
        value = value / 16;
    }
    return '0x' + result;
}

struct List<Type> {
    capacity: uint;
    length: uint;
    buffer: &$Type;

    func $constructor() {
        this.length = 0;
        this.buffer = reallocate(none, 8 * 4);
        if this.buffer == none {
            runtimeError('Could not allocate list on the heap!');
        }
        this.capacity = 4;
    }

    func ensureEnoughCapacity(addedElements: uint) {
        if this.capacity >= this.length + addedElements {
            return;
        }
        let capacity = 2 * this.capacity + 1;

        let placeholder: &$Type = reallocate(this.buffer, capacity * 8);
        if placeholder == none {
            runtimeError('Could not allocate enough space for more elements!');
        }
        this.buffer = placeholder;
        this.capacity = capacity;
    }

    func add(element: $Type) {
        this.ensureEnoughCapacity(1);
        this.buffer[this.length] = element;
        this.length = this.length + 1;
    }

    func $get(index: uint) -> $Type {
        if index >= this.length {
            runtimeError('Index ' + index + ' is out of bounds of array with length ' + this.length + '.');
        }
        return this.buffer[index];
    }

    func $set(index: uint, value: $Type) {
        if index >= this.length {
            runtimeError('Index ' + index + ' is out of bounds of array with length ' + this.length + '.');
        }
        this.buffer[index] = value;
    }

    func $elementCount() -> uint {
        return this.length;
    }

    func $toString() -> string {
        let result = '[ ';
        let isFirst = true;
        for i in 0..this.length {
            if isFirst {
                isFirst = false;
            } else {
                result = result + ', ';
            }
            result = result + this.buffer[i];
        }
        result = result + ' ]';
        return result;
    }

    func $equals(other: List<$Type>) -> bool {
        if this.length != other.length {
            return false;
        }
        for i in 0..this.length {
            if this.buffer[i] != other.buffer[i] {
                return false;
            }
        }
        return true;
    }

    func length() -> uint {
        return this.length;
    }

    func clone() -> List<$Type> {
        let buffer: &$Type = reallocate(none, this.capacity * 8);
        for i in 0..this.length {
            buffer[i] = this.buffer[i];
        }
        let result: List<$Type> = new List();
        result.capacity = this.capacity;
        result.length = this.length;
        result.buffer = buffer;
        return result;
    }

    // func $cast<$Type[]>() -> $Type[] {
    //     return new Array(this.length, this.buffer);
    // }
}


struct Dict<Key, Value> {
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

        for b in oldBucket {
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

struct Noneable<Type> {
    value: &$Type;

    func $constructor() {
        this.value = none;
    }

    func set(value: $Type) {
        if this.value == none {
            this.value = reallocate(this.value, 8);
        }
        this.value[0] = value;
    }

    func reset() {
        this.value = none;
    }

    func $equals(other: Noneable<$Type>) -> bool {
        if this.value == none || other.value == none {
            return this.value == other.value;
        } else {
            return this.value[0] == other.value[0];
        }
    }

    func $toString() -> string {
        if this.value == none {
            return 'none';
        } else {
            return this.value[0] + '';
        }
    }
}

// generic struct Noneable {
//     buffer: &$Type;

//     func $equals(other: Noneable) -> bool {
//         if this.buffer == none || other.buffer == none {
//             return this.buffer == other.buffer;
//         } else {
//             return this.buffer[0] == other.buffer[0];
//         }
//     }

//     func $toString() -> string {
//         if this.buffer == none {
//             return 'none';
//         } else {
//             return this.buffer[0] + '';
//         }
//     }
// }

// generic struct any {
//     value: pointer;
//     type_identifier: TypeIdentifier;

//     func $constructor(value: $Type) {
//         this.value = reallocate(none, 8);
//         this.value[0] = value;
//         this.type_identifier = typeOf : $Type;
//     }

//     func $cast<Type>() -> $Type {
//         if this.type_identifier == typeOf : $Type {
//             return ignoreTypechecking(this.value);
//         } else {
//             return none;
//         }
//     }
// }

// generic cast any : $Type? {
//     if this.type_identifier == typeOf : $Type {
//         // Trust me bro...
//         return this.value[0];
//     } else {
//         return none;
//     }
// }

// struct TypeIdentifier {
//     simple_type_identifier: uint;
//     struct_id: uint?;
//     struct_function_table: FunctionTable?;
// }
