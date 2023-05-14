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

    func delete_range(start: int, count: int) {
        if start > 0 {
            if this.next {
                this.next.delete_range(start - 1, count);
            }
        } else {
            this.next = this.node(count);
        }
    }

    func node(index: int) -> ListNode? {
        if index == 0 {
            return this;
        } else if index == 1 {
            return this.next;
        } else if this.next {
            return this.next.node(index - 1);
        } else {
            return none;
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

func gc() {}

func main() {
    let i = 0;
    {
        let l = new ListNode(i, none);
        while i < 95 {
            i = i + 1;
            l.add(i);
        }
        // heapdump('heap_before_print');
        l.print();
    }
    {
        i = 0;
        let l: List<int> = new List();
        while i < 95 {
            i = i + 1;
            l.add(i);
        }
        print(l);
    }
}