use std::{
    io::{Read, Write},
    net::{TcpListener, TcpStream},
    thread,
};

fn handler(mut stream: TcpStream) -> std::io::Result<()> {
    let mut buffer = [0; 1024];
    stream.read(&mut buffer)?;
    let response = "HTTP/1.1 200 OK\r\n\r\n";
    stream.write_all(response.as_bytes())?;
    stream.flush()
}

fn start() -> std::io::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:9999")?;
    while let Ok((stream, _)) = listener.accept() {
        thread::spawn(|| handler(stream));
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    start()
}
