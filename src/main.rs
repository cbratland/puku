// wasm lang
use std::env;
use std::io::{BufWriter, Read, Write};
use std::process;

mod analyzer;
#[allow(dead_code)]
mod ast;
mod codegen;
#[allow(dead_code)]
mod lexer;
mod parser;
mod typechecker;

fn usage() -> ! {
    eprintln!("Usage: wasmlangc [build|run] <file>");
    process::exit(1)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 || (args[1] != "build" && args[1] != "run") {
        usage();
    }

    let out_path = "./out.wasm";

    // read input file
    let file_name = args.last().unwrap();
    let mut src = String::new();
    let mut input_file = std::fs::File::open(file_name).expect("no file found");
    input_file
        .read_to_string(&mut src)
        .expect("failed to read file");

    // tokenize
    let tokens = lexer::tokenize(&src);
    // println!("tokens: {:?}", tokens);
    // parse into ast
    let mut ast = match parser::parse(&src, tokens) {
        Ok(ast) => ast,
        Err(err) => {
            err.emit(file_name, &src);
            return;
        }
    };
    // typecheck
    if let Err(type_error) = typechecker::check(&src, &mut ast) {
        type_error.emit(file_name, &src);
        return;
    }
    // static analysis
    if let Err(semantic_error) = analyzer::analyze(&src, &mut ast) {
        semantic_error.emit(file_name, &src);
        return;
    }
    // println!("ast: {:#?}", ast);
    let mut compiler = codegen::wasm::WasmCompiler::new();
    let module = compiler.compile(ast);

    // emit to out.wasm
    let file = std::fs::File::create(out_path).expect("failed to create file");
    let mut buffer = BufWriter::new(file);

    codegen::wasm::emit(module, &mut buffer).expect("failed to write codegen to buffer");

    buffer.flush().expect("couldn't flush buffer to file");

    if args[1] != "run" {
        return;
    }

    // read stuff
    let mut file = std::fs::File::open(out_path).expect("no file found");
    let metadata = std::fs::metadata(out_path).expect("unable to read metadata");
    let mut buffer = vec![0; metadata.len() as usize];
    file.read_exact(&mut buffer).expect("file read failed");
    // println!("bytes: {:02X?}", &buffer);

    // run wasm
    let mut store = wasmer::Store::default();
    let module = wasmer::Module::new(&store, &buffer).expect("invalid wasm generated");

    let print_int = wasmer::Function::new_typed(&mut store, |num: i32| {
        println!("{}", num);
    });
    let print_float = wasmer::Function::new_typed(&mut store, |num: f32| {
        println!("{}", num);
    });
    let _instance = match wasmer::Instance::new(
        &mut store,
        &module,
        &wasmer::imports! {
            "env" => {
                "print_int" => print_int,
                "print_float" => print_float,
            }
        },
    ) {
        Ok(instance) => instance,
        Err(err) => {
            eprintln!("error instantiating wasm module: {}", err);
            return;
        }
    };

    // if let Ok(add) = instance.exports.get::<wasmer::Function>("add") {
    //     println!(
    //         "add(1,2): {}",
    //         add.call(&mut store, &[wasmer::Value::I32(1), wasmer::Value::I32(2)])
    //             .unwrap()[0]
    //             .i32()
    //             .unwrap()
    //     );
    // }
}
