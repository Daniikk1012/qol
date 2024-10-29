use std::fs;
#[cfg(not(debug_assertions))]
use std::{env, panic};

use compiler::IrCompiler;
use parser::Parser;

mod codegen;
mod compiler;
mod lexer;
mod parser;

fn main() {
    #[cfg(not(debug_assertions))]
    {
        let prefix = if let Some(name) = env::args_os().next() {
            format!("{}: ", name.to_string_lossy())
        } else {
            String::new()
        };
        panic::set_hook(Box::new(move |info| {
            if let Some(s) = info.payload().downcast_ref::<&str>() {
                eprintln!("{prefix}{s}");
            } else if let Some(string) = info.payload().downcast_ref::<String>() {
                eprintln!("{prefix}{string}");
            } else {
                eprintln!("{prefix}unknown error occured");
            }
        }));
    }
    let function = IrCompiler::new().compile(
        &Parser::new(lexer::tokenize(
            &fs::read_to_string("main.qol").expect("error reading file"),
        ))
        .parse(),
    );
    println!("{function:?}");
    codegen::compile(function);
}
