use futures_lite::{AsyncReadExt, AsyncWriteExt};
use glommio::{
    net::{TcpListener, TcpStream},
    LocalExecutorBuilder, Placement, Task,
};

async fn handler(mut stream: TcpStream) -> std::io::Result<()> {
    let mut buffer = [0; 1024];
    stream.read(&mut buffer)?;

    let get = b"GET / HTTP/1.1\r\n\r\n";

    let response = if buffer.starts_with(get) {
        "HTTP/1.1 200 OK\r\n\r\n"
    } else {
        "HTTP/1.1 404 NotFound\r\n\r\n"
    };
    stream.write_all(response.as_bytes())?;
    stream.flush()
}

async fn start() -> std::io::Result<()> {
    let listener = TcpListener::bind("0.0.0.0:9999")?;
    while let Ok(stream) = listener.accept().await {
        std::thread::spawn(handler(stream));
    }
    Ok(())
}

fn main() -> std::io::Result<()> {
    let ex = LocalExecutorBuilder::new(Placement::Fixed(1)).make()?;
    ex.run(start())
}
