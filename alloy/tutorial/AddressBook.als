sig Name, Addr {}

sig Book {
    addr: Name -> lone Addr
}

one sig AllBooks {
    book: set Book
}

pred show (b: Book) {
    #b.addr > 1
    #Name.(b.addr) > 1
}

pred add (b, b": Book, n: Name, a: Addr) {
    b".addr = b.addr + n -> a
}

pred showAdd (b, b": Book, n: Name, a: Addr) {
    add[b, b", n, a]
    #Name.(b".addr) > 1
}

pred del (b, b": Book, n: Name) {
    b".addr = b.addr - n -> Addr
}

fun lookup (b: Book, n: Name): set Addr {
    n.(b.addr)
}

assert delUndoesAdd {
    all b: Book, bb: Book, b": Book, n: Name, a: Addr | 
        add[b, b", n, a] and del[bb, b", n] implies b.addr = b".addr
}

assert addIdempotent {
    all b, bb, b": Book, n: Name, a: Addr |
        add [b, bb, n, a] and add [bb, b", n, a] implies bb.addr = b".addr
}

assert addLocal {
    all b, b": Book, n, n": Name, a: Addr |
        add [b, b", n, a] and n != n" implies lookup [b, n"] = lookup [b", n"]
}

check delUndoesAdd for 10 but 3 Book

run show for 3 but 2 Book

run showAdd for 3 but 2 Book
