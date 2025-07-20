use comp_base::AnyResult;
use line_ending::LineEnding;
use super::*;

#[test]
fn test_whole() -> AnyResult<()> {
    let s = std::fs::read_to_string("./test.plir")?;

    let file = file_parser::file(&LineEnding::normalize(s.as_str()))?;
    dbg!(&file);

    Ok(())
}