const expect = @import("std").testing.expect;

test "while" {
    var i: u8 = 2;
    while (i < 100) {
        i += 2;
    }
    try expect(i == 100);
}
