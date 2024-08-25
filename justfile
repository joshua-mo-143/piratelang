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

repl:
    cargo run -p piratelang-repl
