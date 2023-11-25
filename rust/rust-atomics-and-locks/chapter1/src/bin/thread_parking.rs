use std::{collections::VecDeque, sync::Mutex, thread, time::Duration};

fn main() {
    let queue = Mutex::new(VecDeque::new());

    thread::scope(|s| {
        // `t` represents the "consumer thread".
        let t = s.spawn(|| loop {
            let item = queue.lock().unwrap().pop_front();
            if let Some(item) = item {
                dbg!(item);
            } else {
                // `park` means, the thread gets to sleep.
                thread::park();
            }
        });

        // Here represents the "producer thread".
        for i in 0.. {
            queue.lock().unwrap().push_back(i);
            // `unpark` means, the (consumer) thread wakes up.
            t.thread().unpark();
            thread::sleep(Duration::from_secs(1));
        }
    });
}
