struct string {
    length: uint;
    bytes: &byte;

    func $equals(other: string) -> bool {
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

    func $addTo(lhs: any) -> string {
        let lhsString = toString(lhs);
        let length = this.length + lhsString.length;
        let bytes : &byte = reallocate(lhsString.bytes, length);
        for j, i in lhsString.length..length {
            bytes[i] = this.bytes[j];
        }
        return new string(length, bytes);
    }

    func $add(rhs: any) -> string {
        let rhsString = toString(rhs);
        let length = this.length + rhsString.length;
        let bytes : &byte = reallocate(this.bytes, length);
        for j, i in this.length..length {
            bytes[i] = rhsString.bytes[j];
        }
        return new string(length, bytes);
    }

    func length() -> uint {
        // TODO: This crashed the compiler!
        // return this.length();
        return this.length;
    }

    func $toString() -> string {
        return this;
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

generic struct Array {
    length: uint;
    buffer: &$Type;

    func $constructor(basis: $Type, length: uint) {
        this.length = length;
        this.buffer = reallocate(none, 8 * length);
        for i in 0..length {
            // FIXME: This crashes the compiler for some reason?
            // Maybe because, at the time of binding, the $get function has not
            // been bound yet?
            // this[i] = basis;
            this.buffer[i] = basis;
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
            result = result + this.buffer[i];
        }
        result = result + ' ]';
        return result;
    }

    func $equals(other: Array) -> bool {
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

generic struct List {
    capacity: uint;
    length: uint;
    buffer: &$Type;

    func $constructor(phantom: $Type) {
        this.length = 0;
        this.buffer = reallocate(none, 8 * 4);
        this.capacity = 4;
    }

    func ensureEnoughCapacity(addedElements: uint) {
        if this.capacity >= this.length + addedElements {
            return;
        }
        let capacity = 2 * this.capacity + 1;

        let placeholder : &$Type = reallocate(this.buffer, capacity * 8);
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

    func $equals(other: List) -> bool {
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

// generic cast List<$Type> : Array<$Type> {
//     return new Array(this.length, this.buffer);
// }

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
