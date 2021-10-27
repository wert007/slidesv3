struct ListNode {
    value: int;
    next: ListNode?;

    func add(value: int) {
        if this.next {
            this.next.add(value);
        } else {
            this.next = new ListNode(value, none);
        }
    }

    func print() {
        print('[ ' + this.to_string() + ' ]');
    }

    func to_string() -> string {
        let next = '';
        if this.next {
            next = this.next.to_string();
        }
        if next.length() > 0 {
            return this.value + ', ' + next;
        } else {
            return this.value + '';
        }
    }
}

func main() {
    let i = 0;
    let list = new ListNode(i, none);
    while i < 64 {
        i = i + 1;
        list.add(i);
    }
    list.print();
}