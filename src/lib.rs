use futures::executor;
use std::{
    fs,
    io::{self, Write},
};
#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

use compiler::IrCompiler;
use parser::Parser;

#[cfg(feature = "gccjit")]
mod codegen;
mod compiler;
mod interpreter;
mod lexer;
mod parser;

#[cfg(feature = "gccjit")]
pub fn compile(filename: &str) {
    let function = IrCompiler::new().compile(
        &Parser::new(lexer::tokenize(
            &fs::read_to_string(filename).expect("error reading file"),
        ))
        .parse(),
    );
    codegen::compile(function);
}

pub fn run(filename: &str) {
    let function = IrCompiler::new().compile(
        &Parser::new(lexer::tokenize(
            &fs::read_to_string(filename).expect("error reading file"),
        ))
        .parse(),
    );
    executor::block_on(interpreter::run(
        function,
        |s| print!("{s}"),
        || async {
            io::stdout().flush().expect("IO error");
            let mut line = String::new();
            io::stdin().read_line(&mut line).expect("IO error");
            line
        },
    ));
}

#[cfg(feature = "wasm")]
#[wasm_bindgen(module = "/web/io.mjs")]
extern "C" {
    fn output(s: &str);
    async fn input() -> JsValue;
}

#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub async fn wasm_run(code: &str) {
    let function = IrCompiler::new().compile(&Parser::new(lexer::tokenize(code)).parse());
    interpreter::run(function, output, || async {
        input().await.as_string().unwrap()
    })
    .await;
}
