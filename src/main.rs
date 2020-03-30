#![feature(fs_read_write)]

mod lexer;
mod runtime;
mod parser;

use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;

use lexer::Lexer;

use parser::Parser;

use runtime::Runtime;

fn source(source: &str, pos: usize) {
    let mut buf = Vec::new();

    let mut line = 1;
    let mut col = 1;
    let mut error_col = 1;

    for (i, c) in source.chars().enumerate() {
        if i == pos {
            error_col = col;
        }

        match c {
            '\n' | '\r' => {
                if i >= pos {
                    break;
                } else if buf.len() > 0 {
                    line += 1;
                    col = 1;
                    buf.clear();
                }
            }
            _ => {
                buf.push(c);
                col += 1;
            }
        }
    }

    println!("LINE: {}, COLUMN: {}", line, col);
    println!("{}", buf.into_iter().collect::<String>());
    println!("{}^", " ".repeat(error_col - 1));
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Invalid number of arguments.");
    }

    let filename = &args[1];

    let mut f = File::open(filename).expect("File not found.");

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .expect("Something went wrong reading the file.");
    let lex = Lexer::lex(&contents);

    let mut parser = Parser::new(lex.into_iter());
    let tree = parser.parse();
    for warning in parser.warnings {
        source(&contents, warning.pos);
        println!("Warning: {}", warning.msg);
        println!("");
    }
    match tree {
        Ok(t) => {
            let mut runtime = Runtime::new();
            runtime.run(&t);
        }
        Err(e) => {
            for error in e.errors {
                source(&contents, error.pos);
                println!("Syntax Error: {}", error.msg);
                println!("");
            }
        }
    };
}
