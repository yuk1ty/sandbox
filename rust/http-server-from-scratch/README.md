# http-server-from-scratch

...in Rust. A trial of making an HTTP server that just echoes incoming requests. This toy example effectively illustrates

- What is the difference between sync and async implementation (both tokio and smol) in Rust.
- How we can migrate a sync implementation to an async one that leverages on tokio.

## Launch

Start the server first,

```
# Run the sync server
cargo run -p sync-http-server

# Run the async server
cargo run -p async-http-server

# Run the smol version async server
cargo run -p async-smol-http-server
```

Then you can check it works on the browser with accessing `127.0.0.1:9999`.
