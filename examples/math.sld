struct Vec2 {
    // There are no floats currently.
    x: int;
    y: int;

    func length_squared() -> int {
        return this.x * this.x + this.y * this.y;
    }
}