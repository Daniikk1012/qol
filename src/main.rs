use std::{env, fs, panic};

use parser::Parser;

mod compiler;
mod lexer;
mod parser;

fn main() {
    let prefix = if let Some(name) = env::args_os().next() {
        format!("{}: ", name.to_string_lossy())
    } else {
        String::new()
    };
    //panic::set_hook(Box::new(move |info| {
    //    if let Some(s) = info.payload().downcast_ref::<&str>() {
    //        eprintln!("{prefix}{s}");
    //    } else if let Some(string) = info.payload().downcast_ref::<String>() {
    //        eprintln!("{prefix}{string}");
    //    } else {
    //        eprintln!("{prefix}unknown error occured");
    //    }
    //}));
    compiler::compile(
        &Parser::new(lexer::tokenize(
            &fs::read_to_string("main.qol").expect("error reading file"),
        ))
        .parse(),
    );
}
