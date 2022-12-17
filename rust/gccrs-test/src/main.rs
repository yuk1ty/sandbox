mod core;

extern "C" {
    fn write(fd: usize, buf: *const u8, len: usize);
}

fn main() -> i32 {
    unsafe {
        let msg = "Hello, world!\n";
        let buf = msg.as_ptr();
        let len = msg.len();
        write(1, buf, len);
    }
    0
}
