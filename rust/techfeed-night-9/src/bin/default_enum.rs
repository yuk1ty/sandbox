#[derive(Debug, Default)]
enum State {
    #[default]
    Idle,
    Fetch,
    Exec,
}

// できそうでできない
// #[derive(Default)]
// struct Instruction;

// #[derive(Default)]
// enum State2 {
//     Idle,
//     Fetch,
//     #[default]
//     Exec(Instruction),
// }

fn main() {
    println!("{:?}", State::default());
}
