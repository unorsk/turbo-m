default: install

build:
    cargo build --release

install: build
    cargo install --path .
