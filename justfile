test:
    cargo test

fmt:
    cargo fmt

clippy:
    cargo clippy

ci:
    cargo fmt
    cargo clippy
    cargo test
