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
    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                let handler = thread::spawn(move || handler(stream));
                handler.join().unwrap()?;
            }
            Err(err) => eprintln!("{:?}", err),
        }
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    start()
}
