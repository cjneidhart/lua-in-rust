use std::env::args;
use std::fs::read_to_string;
use std::io;
use std::io::Write;

use lua::State;

fn main() {
    let mut args = args();
    match args.nth(1) {
        None => run_prompt(),
        Some(s) => run_file(s.as_str()),
    }
}

fn run_file(filename: &str) {
    let source = read_to_string(filename).unwrap();
    lua::run_string(source.as_str());
}

fn run_prompt() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();
    let mut state = State::new();
    loop {
        print!("> ");
        stdout.flush().unwrap();
        buf.clear();
        stdin.read_line(&mut buf).unwrap();
        if buf.is_empty() {
            println!();
            break;
        }

        if let Err(e) = state.loadstring(&buf) {
            println!("stdin: {}", e.get_msg());
        }
    }
}
