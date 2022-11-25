use std::sync::Mutex;
use std::thread;

static DATA: Mutex<Vec<u32>> = Mutex::new(Vec::new());

fn main() {
    let t1 = thread::spawn(|| {
        for i in 0..10 {
            let mut v = DATA.lock().unwrap();
            v.push(i);
        }
    });

    let t2 = thread::spawn(|| {
        for i in 10..20 {
            let mut v = DATA.lock().unwrap();
            v.push(i);
        }
    });

    t1.join().unwrap();
    t2.join().unwrap();

    println!("{:?}", DATA.lock());
}
