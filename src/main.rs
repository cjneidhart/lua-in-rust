use std::env::args;
use std::io;
use std::process::exit;

use lua::State;

fn main() {
    let mut args = args();
    match args.nth(1) {
        None => run_prompt(),
        Some(s) => run_file(&s),
    }
}

fn run_file(filename: &str) {
    let mut state = State::new();
    let result = state.do_file(filename);
    if let Err(e) = result {
        eprintln!("{}", e);
        exit(1);
    }
}

fn run_prompt() {
    let mut state = State::new();
    loop {
        let load_result = read_stdin(&mut state);
        match load_result {
            Ok(0) => {
                println!();
                return;
            }
            Err(e) => {
                eprintln!("stdin: {}", e);
                continue;
            }
            Ok(_) => (),
        }

        let run_result = state.call(0, 0);
        if let Err(e) = run_result {
            eprintln!("stdin: {}", e);
        }
    }
}

fn read_stdin(state: &mut State) -> lua::Result<usize> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut is_first_line = true;
    let mut buffer = String::new();
    let mut total_bytes_read = 0;
    loop {
        if is_first_line {
            print!("> ");
            is_first_line = false;
        } else {
            print!(">> ");
        }
        let _ = io::Write::flush(&mut stdout);

        total_bytes_read += stdin.read_line(&mut buffer)?;
        if total_bytes_read == 0 {
            return Ok(0);
        }

        let load_result = state.load_string(&buffer);
        match load_result {
            Ok(()) => {
                return Ok(total_bytes_read);
            }
            Err(e) => {
                if e.is_recoverable() {
                    continue;
                } else {
                    return Err(e);
                }
            }
        }
    }
}
