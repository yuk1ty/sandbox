use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio::io::BufWriter;
use tokio::net::TcpListener;
use tokio::net::TcpStream;

#[tokio::main]
async fn main() {
    let listener = TcpListener::bind("127.0.0.1:9999").await.unwrap();
    loop {
        let (stream, _) = listener.accept().await.unwrap();
        handle_connection(stream).await;
    }
}

async fn handle_connection(stream: TcpStream) {
    let (reader, writer) = stream.into_split();
    let buf_reader = BufReader::new(reader);
    let mut buf_writer = BufWriter::new(writer);

    let mut req: Vec<String> = vec![];
    let mut lines = buf_reader.lines();
    while let Some(line) = lines.next_line().await.unwrap() {
        if line.is_empty() {
            break;
        }
        req.push(line);
    }

    let res = format!("HTTP/1.1 200 OK\r\n\r\n{:#?}", req);
    buf_writer.write_all(res.as_bytes()).await.unwrap();
    buf_writer.flush().await.unwrap();
}
