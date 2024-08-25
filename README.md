## Piratelang - A language for those who sail the seven seas!
Piratelang is a dynamically-typed interpreted scripting language based on pirates.

This is *primarily* a hobby project, but with the eventual aim of being usable for more than hobbyist workloads.

## Usage
To try out Piratelang, you can run the REPL in this repo (`cargo run -p repl` from the root folder). You can also look at the examples folder (within `_examples`) if you want to have a cursory look at what code currently looks like, as they are used for tests.

## Roadmap
- [x] Add basic type primitives
- [x] Declare variables
- [x] If statements
- [x] Mathematical operators
- [x] Functions
- [x] Add lists
- [x] Add structs
- [ ] File imports
- [x] Ability to use Rust functions under the hood for stdlib
- [x] Chained methods
- [ ] Design a standard library
- [ ] Write a standard library
- [ ] Write a web server using Piratelang
- [ ] (Github-based) Package management

## Language Quirks
- It's currently recommended to put all of your functions at the top and then execution should either be in a different file or at the bottom of the file.
