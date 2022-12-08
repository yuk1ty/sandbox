use std::thread;

// NG case
fn main() {
    let greeting = String::from("Hello world!");

    // thread::spwan は static なライフタイムを保つため、スレッドは
    // ローカル変数を借用できないという規則がある。
    let handle1 = thread::spawn({
        // そのため、1度クローンしておいてスコープ外の greeting の
        // 所有権が移動しないようにする必要がある。
        let greeting = greeting.clone();
        move || {
            println!("thread #1 says: {}", greeting);
        }
    });

    let handle2 = thread::spawn(move || {
        println!("thread #2 says: {}", greeting);
    });

    handle1.join().unwrap();
    handle2.join().unwrap();
}

// OK case
fn main() {
    let greeting = String::from("Hello world!");

    // scope を使うと greeting を clone せずに済んでいることがわかる。
    thread::scope(|s| {
        s.spawn(|| {
            println!("thread #1 says: {}", greeting);
        });

        s.spawn(|| {
            println!("thread #2 says: {}", greeting);
        });

        // スコープの終わりに起動済みのスレッドのジョインが走るので、join も不要になる。
    });
}
