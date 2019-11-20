use lua::Result;
use lua::State;

fn run_file(filename: &str) -> Result<()> {
    let mut state = State::new();
    state.do_file(filename)
}

#[test]
fn test01() -> Result<()> {
    run_file("tests/test01.lua")
}

#[test]
fn test02() -> Result<()> {
    run_file("tests/test02.lua")
}

#[test]
fn test03() -> Result<()> {
    run_file("tests/test03.lua")
}

#[test]
fn test04() -> Result<()> {
    run_file("tests/test04.lua")
}

#[test]
fn test05() -> Result<()> {
    run_file("tests/test05.lua")
}
