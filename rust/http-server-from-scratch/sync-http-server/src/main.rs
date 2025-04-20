use std::{
    io::{BufRead, BufReader, Write},
    net::{TcpListener, TcpStream},
};

fn main() {
    let listener = TcpListener::bind("127.0.0.1:9999").unwrap();
    loop {
        let (stream, _) = listener.accept().unwrap();
        handle_connection(stream);
    }
}

fn handle_connection(mut stream: TcpStream) {
    let buf_reader = BufReader::new(&stream);

    let mut req = vec![];
    let mut lines = buf_reader.lines();
    while let Some(line) = lines.next() {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        req.push(line);
    }

    let res = format!("HTTP/1.1 200 OK\r\n\r\n{:#?}", req);
    stream.write_all(res.as_bytes()).unwrap();
    stream.flush().unwrap();
}
