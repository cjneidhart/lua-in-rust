use std::env::args;
use std::fs::read_to_string;
use std::io::{self, Write};
use std::process::exit;

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
    let result = lua::run_string(source.as_str());
    if let Err(e) = result {
        eprintln!("{}", e);
        exit(1);
    }
}

fn run_prompt() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut state = State::new();
    loop {
        let mut bytes_read = 0;
        let mut is_first_time = true;
        let load_result = State::load(|buffer| {
            if is_first_time {
                print!("> ");
                is_first_time = false;
            } else {
                print!(">> ");
            }
            let _ = stdout.flush();

            let n = stdin.read_line(buffer)?;
            if n == 0 {
                std::process::exit(0);
            }
            bytes_read += n;
            Ok(n)
        });

        match load_result {
            Ok(chunk) => {
                let eval_result = state.eval_chunk(chunk);
                if let Err(e) = eval_result {
                    eprintln!("eval error: {}", e);
                }
            }
            Err(e) => {
                eprintln!("stdin: {}", e);
            }
        }

        if bytes_read == 0 {
            break;
        }
    }
}
