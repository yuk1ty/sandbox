sig TitleCharacter {}

sig Title {
	contents: set TitleCharacter,
}

sig Author {}

sig Isbn {}

sig Book {
	title: Title,
	author: Author,
	isbn: Isbn,
}

sig System {
	catalog: set Book,
	copies: catalog -> Int,
} {
	all b: Book | copies[b] >= 0
}

// The first argument represents the initial state of the sytem,
// the second argument represents the final state of the system
pred addBook [sys: System, sys": System, b: Book] {
	// Represents a transition of the sytem state from sys to sys"
	// `b not in sys.catalog` means that the book b is not in the initial state of the system
	// `sys".catalog = sys.catalog + b` means that the final state of the system is the initial state of the system plus the book b
	// `=` is just `==` in other programming languages
	b not in sys.catalog => sys".catalog = sys.catalog + b
}

pred addCopy [sys, sys":System, b: Book] {
	b in sys.catalog =>
		// -> means key and value
		sys".copies = sys.copies + (b -> (sys.copies[b] + 1))
}

assert addNewBookSuccess {
	all sys, sys": System, b: Book |
		b not in sys.catalog
		implies addBook[sys, sys", b]
		implies b in sys".catalog
}

assert addExistingBookFail {
    all sys, sys": System, b: Book |
        b in sys.catalog
    	implies addBook[sys, sys", b]
        implies b in sys".catalog
}

assert addCopyExistingSuccess {
	all sys, sys": System, b: Book |
		b in sys.catalog
		implies addCopy[sys, sys", b]
		implies sys".copies[b] = sys.copies[b] + 1
}

assert addCopyNewFail {
	all sys, sys": System, b: Book |
		b not in sys.catalog
		implies addCopy[sys, sys", b]
		implies sys".copies[b] = sys.copies[b]
}

fun findBookByIsbn [sys: System, i: Isbn]: set Book {
    {b: sys.catalog | b.isbn = i}
}

assert findBookByIsbnSuccess {
    all sys: System, i: Isbn, b: Book | 
        b in sys.catalog and b.isbn = i 
        implies b in findBookByIsbn[sys, i]
}

assert findBookByIsbnFail {
    all sys: System, i: Isbn, b: Book | 
        b not in sys.catalog and b.isbn = i 
        implies b not in findBookByIsbn[sys, i]
}

check addNewBookSuccess for 5
check addExistingBookFail for 5

check addCopyExistingSuccess for 5

check addCopyNewFail for 5

check findBookByIsbnSuccess for 5

check findBookByIsbnFail for 5

run addBook

fact singletonSystem {
	#System = 1
}

