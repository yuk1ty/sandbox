sig A {}

// sig B {
//     parent: A
// }

// pred show {}

pred atLeastOne {
    #A > 0
}

pred notASingleOne {
    #A != 1
}

pred moreThanOne {
    atLeastOne && notASingleOne
}


run {} for 3

run moreThanOne
