#[tokio::main]
async fn main() {
    tokio::select! {
        _ = tokio::spawn(async_backtrace::frame!(pending())) => {}
        _ = foo() => {}
    };
}

#[async_backtrace::framed]
async fn pending() {
    std::future::pending::<()>().await
}

#[async_backtrace::framed]
async fn foo() {
    bar().await;
}

#[async_backtrace::framed]
async fn bar() {
    futures::join!(fiz(), buz());
}

#[async_backtrace::framed]
async fn fiz() {
    tokio::task::yield_now().await;
}

// #[async_backtrace::framed]
async fn buz() {
    println!("{}", baz().await);
}

#[async_backtrace::framed]
async fn baz() -> String {
    async_backtrace::taskdump_tree(true)
}
