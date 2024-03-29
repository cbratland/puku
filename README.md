# Puku

A simple programming language that compiles directly to WebAssembly.

### Notes
For now, this can be run with `cargo run -- run path/to/file.pk`, which will produce a wasm binary called `out.wasm` and will then be run with [wasmer](https://wasmer.io/) automatically. Tests can be run with `cargo test`. See `examples` directory for some examples and [LANGUAGE.md](LANGUAGE.md) for the formal grammar.

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
    - [ ] function type and argument count checking
    - [ ] unused variables/imports/functions (warning)
- [x] arrays: i32 memory offsets, with the first element being the i32 length of the array
    - [x] literals
    - [x] indexing
    - [ ] type specifying (e.g. [i32])
    - [ ] assignment (e.g. a[0] = 1)
    - [ ] size specifying? (e.g. [i32; 10])
- [ ] strings
- [ ] for loops
- [ ] bitwise operators (^, |, &, etc.)
- [ ] evaluatable blocks (blocks return a value)
- [ ] optionals
