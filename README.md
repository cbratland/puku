# wasmlang (not yet named)

## Notes
For now, this can be run with `cargo run -- run add.wlang`, which will produce a wasm binary called `out.wasm` and will then be run with [wasmer](https://wasmer.io/) automatically. Tests can be run with `cargo test`. See `examples` directory for some examples.

## Purpose
- Target WebAssembly first and foremost
- Easier to use than Rust, but still performance oriented

## Future Goals (not implemented)
Similar to Rust/Swift/Kotlin.
- Readable
- No double colons for namespaces
- No semicolon expression separators
- Most things are expressions (e.g. an if else can return a value into a variable)
- Nice structs and enums and something like traits/protocols/interfaces
- Swift/Kotlin's null coalescing operator instead of Rust's `?` operator
- Optionals built in to the language like swift (e.g. `Type?` instead of `Option<Type>` in rust)
- Pattern matching
- Some sort of lifetime analysis
- LLVM/CraneLift backend

## Current Roadmap
- [x] variable mutability
- [x] function calls
- [x] while loops
- [x] comparison ops (<, <=, >, >=, etc.)
- [x] break and continue keywords
- [x] else if branches
- [x] operator equals (+=, -=, etc.)
- [x] start function
- [x] import functions
- [x] import namespace and export name specification
- [x] plain loops
- [ ] for loops
- [ ] specify variable mutability (semantic analysis?)
- [ ] evaluatable blocks (blocks return a value)
- [ ] optionals
