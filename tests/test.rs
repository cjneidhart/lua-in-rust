use lua::Result;

#[test]
fn test01() -> Result<()> {
    lua::run_file("tests/test01.lua")
}

#[test]
fn test02() -> Result<()> {
    lua::run_file("tests/test02.lua")
}
