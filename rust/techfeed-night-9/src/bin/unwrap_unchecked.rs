fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let num: i32 = args[1].parse().unwrap();
    println!("{}", num);
}
