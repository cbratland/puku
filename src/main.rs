// wasm lang
use std::env;
use std::io::{BufWriter, Read, Write};
use std::process;

#[allow(dead_code)]
mod ast;
mod codegen;
#[allow(dead_code)]
mod lexer;
mod parser;
mod typechecker;

use parser::ParseError;

fn usage() -> ! {
    eprintln!("Usage: wasmlangc <file>");
    process::exit(1)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        usage();
    }

    // read input file
    let file_name = args.last().unwrap();
    let mut input_str = String::new();
    let mut input_file = std::fs::File::open(file_name).expect("no file found");
    input_file
        .read_to_string(&mut input_str)
        .expect("failed to read file");

    // pipeline
    let tokens = lexer::tokenize(&input_str);
    // println!("tokens: {:?}", tokens);
    let mut ast = match parser::parse(&input_str, tokens) {
        Ok(ast) => ast,
        Err(err) => {
            match err {
                ParseError::Unhandled => eprintln!("unhandled parser error!"),
                ParseError::UnexpectedToken(span) => {
                    let loc = span.loc as usize;
                    let source_up_to = &input_str[..loc];
                    let line = bytecount::count(source_up_to.as_bytes(), b'\n') + 1;
                    let col = loc - source_up_to.rfind('\n').unwrap_or(0);
                    let culprit = &input_str[loc..loc + span.len as usize];
                    eprintln!("{file_name}:{line}:{col}: unexpected token `{culprit}`");
                }
            }
            return;
        }
    };
    typechecker::check(&mut ast);
    // println!("ast: {:#?}", ast);
    let module = codegen::wasm::gen_ir(ast);

    // emit to tmp.wasm
    let file = std::fs::File::create("./tmp.wasm").expect("failed to create file");
    let mut buffer = BufWriter::new(file);

    codegen::wasm::emit(module, &mut buffer).expect("failed to write codegen to buffer");

    buffer.flush().expect("couldn't flush buffer to file");

    // read stuff
    let mut file = std::fs::File::open("./tmp.wasm").expect("no file found");
    let metadata = std::fs::metadata("./tmp.wasm").expect("unable to read metadata");
    let mut buffer = vec![0; metadata.len() as usize];
    file.read(&mut buffer).expect("buffer overflow");
    println!("bytes: {:02X?}", &buffer);

    // run wasm
    let store = wasmer::Store::default();
    let module = wasmer::Module::new(&store, &buffer).expect("invalid wasm generated");
    let instance = wasmer::Instance::new(&module, &wasmer::imports! {}).unwrap();
    if let Ok(add) = instance.exports.get_function("add") {
        println!(
            "add(1,2): {}",
            add.call(&[wasmer::Value::I32(1), wasmer::Value::I32(2)])
                .unwrap()[0]
                .i32()
                .unwrap()
        );
    } else {
        eprintln!("add function not exported");
    }
}
