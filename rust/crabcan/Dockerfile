FROM ubuntu:22.04

# RUN apt update && apt -y install cargo
RUN curl https://sh.rustup.rs -sSf | sh
ENV PATH="/root/.cargo/bin:${PATH}"

COPY ./ ./

RUN cargo install cargo-watch

# RUN cargo build

# CMD ["./target/debug/crabcan", "--command", "bash", "--mount", "./mount-dir", "--uid", "0"]
