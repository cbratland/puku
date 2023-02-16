# wasmlang (not yet named)

## notes
for now, this can be run with `cargo run -- add.wlang`, which will produce a wasm binary called `tmp.wasm` which will then be run with [wasmtime](https://github.com/bytecodealliance/wasmtime) automatically.

a lot of the code is placeholder right now and very hacked together (a million `unwrap()`s) just so I could get something working

## purpose
an easy language with wasm support first. goal is to support the full wasm spec. ideally, something easy like javascript, but with types and compiles to wasm and is more performance oriented.
maybe: exportable ast and efficient embeddable scripting support

## grammar
like rust/swift/kotlin.
goals: rust, but
- more readable
- no double colons for namespaces
- no semicolon expression separators
- nice structs and enums and something like traits/protocols/interfaces
- swift/kotlin's null coalescing operator instead of rust's (`?` operator)
- optionals built in to the language like swift (like `Type?` instead of `Option<Type>` in rust)

## extra language features
- inferred type checking
- const evaluation (for compile time expressions)
- lifetime analysis (at least something basic)
	- I want something like rust's Drop trait where I can have my own kind of memory mangement for when things go out of scope
	- I don't want a garbage collector (the WasmGC proposal isn't finalized yet, and I wouldn't want to use it anyways, and languages like AssemblyScript that ship their own garbage collector in wasm are absolutely terrible)
