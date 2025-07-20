use comp_base::AnyResult;
use crate::str_parser::file_parser;
use super::*;

#[test]
fn test_higher_whole() -> AnyResult<()> {
    let s = std::fs::read_to_string("./test.plir")?;

    let file = {
        let file = file_parser::file(s.as_str())?;
        File::try_from(file)?
    };
    dbg!(&file);

    Ok(())
}