use smol::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    net::{TcpListener, TcpStream},
    stream::StreamExt,
};

fn main() {
    smol::block_on(async {
        let listener = TcpListener::bind("127.0.0.1:9999").await.unwrap();
        loop {
            let (stream, _) = listener.accept().await.unwrap();
            handle_connection(stream).await;
        }
    })
}

async fn handle_connection(mut stream: TcpStream) {
    let buf_reader = BufReader::new(&mut stream);

    let mut req = vec![];
    let mut lines = buf_reader.lines();
    while let Some(line) = lines.next().await {
        let line = line.unwrap();
        if line.is_empty() {
            break;
        }
        req.push(line);
    }

    let res = format!("HTTP/1.1 200 OK\r\n\r\n{:#?}", req);
    stream.write_all(res.as_bytes()).await.unwrap();
    stream.flush().await.unwrap();
}
