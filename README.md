# QOL - Qazaq Oriented Language

This is the source code of the QOL programming language. Right now, the project
is under development, and only the bare minimum to make the language
Turing-complete is available. You can use the language using the compiler (Using
[`libgccjit`][libgccjit] backend) or the interpreter (Available both as
executable and as a [webpage][webpage]).

## Installation

There are two options for Windows users: build manually, or use 
[prebuilt package][releases] (Only built for releases, and currently weighs more
than 1GB. Exists solely because dealing with MinGW is not worth it if you just
want to try out the language).

If you are on Linux, macOS, or any other OS that is supported by
[`libgccjit`][libgccjit] and [Rust][rust], read the [building](#building)
section.

## Building

### Dependencies

Install [Rust][rust] along with its dependencies. If you want to build a
compiler, also install [`libgccjit`][libgccjit]. If you want to build the web
version, install [`wasm-pack`][wasm-pack] like so:
```sh
$ cargo install wasm-pack
```
After you do so, ensure that the `.cargo/bin` directory is in your `PATH`
environment variable ([Rust][rust] installer should do that by default, but you
might need to do it manually if you installed it through other means).

### Build process

Following commands should be done with the root of the source code as the
current working directory.

If you want to only build the interpreter, run:
```sh
$ cargo build --release --no-default-features
```
The executable will be located in `target/release/`.

If you want to build both the compiler and the interpreter, run:
```sh
$ cargo build --release
```
The executable will be located in `target/release/`.

If you want to build the web version, run:
```sh
$ wasm-pack build --no-typescript -t web --release -d web/pkg --no-pack --no-default-features --features wasm
```
The webpage will be located in `web/`.

After you've successfully built the interpreter/compiler, check out the
[usage](#usage) section.

## Usage

To run the interpreter, use:
```sh
$ <QOL Executable> run
```
This will run the program contained in `main.qol` in the current working
directory.

To run the compiler, use:
```sh
$ <QOL Executable>
```
This will compile the program contained in `main.qol` in the current working
directory and output the executable to `main`.

To get started with learning the language, check out the
[language spec](#language-spec) section.

## Language spec

Currently, there is no language spec

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the [LICENSE][license] for more details.

[libgccjit]: https://gcc.gnu.org/wiki/JIT
[webpage]: https://daniikk1012.github.io/qol
[releases]: /../../releases
[rust]: https://www.rust-lang.org/
[wasm-pack]: https://crates.io/crates/wasm-pack
[license]: /LICENSE
