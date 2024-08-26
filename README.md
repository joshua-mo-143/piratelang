## Piratelang - A language for those who sail the seven seas!
Piratelang is a dynamically-typed interpreted scripting language based on pirates.

This is *primarily* a hobby project, but with the eventual aim of being usable for more than hobbyist workloads.

## Usage
To try out Piratelang, you can run the REPL in this repo (`cargo run -p repl` from the root folder). You can also look at the examples folder (within `_examples`) if you want to have a cursory look at what code currently looks like, as they are used for tests.

You can also try the CLI with `cargo run -p piratelang-cli run <file-name>`. Currently there's only one command to run a file, but if you don't want to use the REPL and want to run a file instead, the CLI is there!


## Roadmap
- [x] Add basic type primitives
- [x] Declare variables
- [x] If statements
- [x] Mathematical operators
- [x] Functions
- [x] Chained functions
- [x] Add lists
- [x] Add structs
- [x] "Extends" syntax for adding chained methods to structs
- [ ] File imports
- [x] Ability to use Rust functions under the hood for stdlib
- [ ] Design a standard library
- [ ] Write a standard library
- [ ] Write a web server using Piratelang
- [ ] (Github-based) Package management
