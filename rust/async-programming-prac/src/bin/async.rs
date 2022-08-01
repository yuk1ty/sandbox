use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::{TcpListener, TcpStream},
    runtime::Runtime,
};

async fn handler(mut stream: TcpStream) -> std::io::Result<()> {
    let mut buffer = [0; 1024];
    stream.read(&mut buffer).await?;
    let response = "HTTP/1.1 200 OK\r\n\r\n";
    stream.write_all(response.as_bytes()).await?;
    stream.flush().await
}

async fn start() -> std::io::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:9999").await?;
    loop {
        match listener.accept().await {
            Ok((stream, _)) => handler(stream).await?,
            Err(err) => eprintln!("{:?}", err),
        }
    }
}

fn main() -> std::io::Result<()> {
    let rt = Runtime::new()?;
    rt.block_on(start())
}
