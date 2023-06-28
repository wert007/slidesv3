// Those crash the compiler right now. Add check, that there are no circular
// dependencies.
// import lib('mylib') as mylib;
// import lib('math') as math;

struct Vec2 {
    // There are no floats currently.
    x: int;
    y: int;

    func lengthSquared() -> int {
        return this.x * this.x + this.y * this.y;
    }

    func $toString() -> string {
        return 'Vec2(' + this.x + ', ' + this.y + ')';
    }
}