use std::fs;

use compiler::IrCompiler;
use parser::Parser;

mod codegen;
mod compiler;
mod interpreter;
mod lexer;
mod parser;

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
    interpreter::run(function);
}
