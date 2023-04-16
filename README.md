# wasmlang (not yet named)

## notes
for now, this can be run with `cargo run -- add.wlang`, which will produce a wasm binary called `tmp.wasm` and will then be run with [wasmer](https://wasmer.io/) automatically.

## purpose
- target webassembly first and foremost
- easier to use than rust, but still performance oriented
- maybe: exportable ast and efficient embeddable scripting support

## basics
like rust/swift/kotlin.
goals: rust, but
- more readable
- no double colons for namespaces
- no semicolon expression separators
also want
- most things to be expressions (e.g. an if else can return a value into a variable)
- nice structs and enums and something like traits/protocols/interfaces
- swift/kotlin's null coalescing operator instead of rust's `?` operator
- optionals built in to the language like swift (e.g. `Type?` instead of `Option<Type>` in rust)
- pattern matching

## possible future extra language features
- inferred type checking
- const evaluation (for compile time expressions)
- lifetime analysis (at least something basic)
	- I want something like rust's Drop trait where I can have my own kind of memory mangement for when things go out of scope
	- I don't want a garbage collector (the WasmGC proposal isn't finalized yet, and I wouldn't want to use it anyways, and languages like AssemblyScript that ship their own garbage collector in wasm are bad)
