use std::future::Future;

async fn foo<'a, T>(x: &'a (), y: T) {
    _ = (x, y)
}

trait Captures<U> {}
impl<T: ?Sized, U> Captures<U> for T {}

fn bar<'a, T>(x: &'a (), y: T) -> impl Future<Output = ()> + Captures<&'a ()> {
    async move { _ = (x, y) }
}

fn baz<'a, T>(x: &'a (), y: T) -> impl Future<Output = ()> + 'a {
    async move {
        _ = (x, y);
    }
}

fn main() {
    println!("Hello, world!");
}
