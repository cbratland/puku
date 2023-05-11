# wasmlang (not yet named)

## Notes
For now, this can be run with `cargo run -- run path/to/file.wlang`, which will produce a wasm binary called `out.wasm` and will then be run with [wasmer](https://wasmer.io/) automatically. Tests can be run with `cargo test`. See `examples` directory for some examples.

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
- [x] import and export functions
- [x] mutable/immutable variables
- [x] function calls
- [x] plain loops and while loops
- [x] break and continue keywords
- [x] comparison operators (<, <=, >, >=, etc.)
- [x] assignment operators (+=, -=, etc.)
- [x] if else branches
- [x] start function (entry point)
- [x] static analysis
    - [x] basic type checking
    - [x] variable mutability
    - [x] function body existence
    - [x] dividing by zero
    - [ ] argument count and type checking
    - [ ] unused variables/imports/functions (warning)
- [ ] arrays
- [ ] strings
- [ ] for loops
- [ ] evaluatable blocks (blocks return a value)
- [ ] optionals
