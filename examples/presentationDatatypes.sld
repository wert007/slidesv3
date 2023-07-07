struct UnitLiteral {
    value: float;
    kind: UnitKind;

    func $toString() -> string {
        return this.value + cssName(this.kind);
    }
}

func pixel(value: float) -> UnitLiteral {
    return new UnitLiteral(value, UnitKind.Pixel);
}

func percent(value: float) -> UnitLiteral {
    return new UnitLiteral(value, UnitKind.Percent);
}

func point(value: float) -> UnitLiteral {
    return new UnitLiteral(value, UnitKind.Point);
}

enum UnitKind {
    Pixel,
    Percent,
    Point,
}


func cssName(unitKind: UnitKind) -> string {
    match unitKind {
        UnitKind.Pixel => {
            return 'px';
        }
        UnitKind.Percent => {
            return '%';
        }
        UnitKind.Point => {
            return 'pt';
        }
    }
    return 'unreachable!';
}