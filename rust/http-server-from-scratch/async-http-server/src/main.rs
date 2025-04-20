use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream};

#[tokio::main]
async fn main() {
    let listener = TcpListener::bind("127.0.0.1:9999").await.unwrap();
    loop {
        let (stream, _) = listener.accept().await.unwrap();
        handle_connection(stream).await;
    }
}

async fn handle_connection(mut stream: TcpStream) {
    let buf_reader = BufReader::new(&mut stream);

    let mut req = vec![];
    let mut lines = buf_reader.lines();
    while let Some(line) = lines.next_line().await.unwrap() {
        if line.is_empty() {
            break;
        }
        req.push(line);
    }

    let res = format!("HTTP/1.1 200 OK\r\n\r\n{:#?}", req);
    stream.write_all(res.as_bytes()).await.unwrap();
    stream.flush().await.unwrap();
}
