fn main() {
    let boxed_str: Box<str> = "text".into();
    println!("{}", boxed_str);

    let string = "text".to_string();
    println!("{}", string);

    // size checking
    println!(
        "スタック上のboxed_strのサイズ: {}",
        std::mem::size_of_val(&boxed_str)
    );
    println!(
        "スタック上のstringのサイズ: {}",
        std::mem::size_of_val(&string)
    );
}
