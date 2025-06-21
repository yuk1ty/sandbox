abstract sig Target {}
sig Addr extends Target {}
abstract sig Name extends Target {}

sig Alias, Group extends Name {}
sig Book { addr: Name -> Target }
    { no n: Name | n in n.^addr }

pred show (b: Book) { some b.addr }
run show for 3 but 1 Book
