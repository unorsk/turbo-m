default: install

build:
    cargo build --release

install: build
    cargo install --path .

check:
    cargo fmt && cargo clippy -- -D warnings && cargo test
