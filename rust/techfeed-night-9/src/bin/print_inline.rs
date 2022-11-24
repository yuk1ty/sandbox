fn main() {
    let a = 42;
    let b = 31;
    let ans = a + b;
    // 従来
    println!("{} + {} = {}", a, b, ans);
    // 以降
    println!("{a} + {b} = {ans}");
    // 下記はできない。
    // (関数や関数の適用、演算を含めることはできない)
    // println!("{a} + {b} = {a + b}");
}
