# Gucci Lang

[![Build](https://github.com/coszio/gucci-lang/workflows/Build/badge.svg)](https://github.com/coszio/gucci-lang/actions/workflows/build.yml)
[![Test](https://github.com/coszio/gucci-lang/workflows/Test/badge.svg)](https://github.com/coszio/gucci-lang/actions/workflows/build.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## What is Gucci Lang?

Gucci Lang is an toy language made with the purpose of learning language design.

It was made using [chumsky](https://github.com/zesterer/chumsky) and [ariadne](https://github.com/zesterer/ariadne), and aims to have a simple syntax for didactic purposes.

## Documentation

Run `cargo doc --open`


## How to use it

To create the executable, run `cargo build`. It will create a binary in `target/debug/gucci-lang.exe`.

To compile a gucci file, create a file ending with `.gu` and run `gucci_lang.exe compile <filename>`.

To run an already compiled gucci file, run `gucci_lang.exe run <filename>`, the filename must end in `.gu.bs`.

To compile and run in the same command, run `gucci_lang.exe compile_run <filename>`.

It is possible to use the short version of the actions: `c`, `r`, `cr`, respectively.

## Implemented features

- [x] Lexical analysis
- [x] Syntactic analysis
- [x] Basic variables semantics: Variables table and functions directory
- [x] Basic expression semantics: semantic cube and evaluation
- [x] Code generation for conditional statements and loops
- [x] Code generation for functions
- [x] Memory map for virtual machine execution. 
- [x] Virtual Machine: execution of arithmetic expressions and sequential statements.
- [ ] Code generation for structured data 
- [x] Virtual Machine: execution of conditional statements.
- [x] 1st version of documentation

## Made by

[Luis Cossio](https://github.com/coszio)

[Miguel Peralta](https://github.com/MiguelPeraltaP)

## License
The project is available under the [MIT](https://opensource.org/licenses/MIT) license.
