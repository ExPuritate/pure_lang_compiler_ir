use comp_base::AnyResult;
use crate::ast::File;
use crate::Error;
use crate::output::Output;
use crate::str_parser::file_parser;

#[test]
fn final_test() -> AnyResult<()> {
    let s = std::fs::read_to_string("./test.plir")?;

    let file = File::try_from(file_parser::file(s.as_str())?)?;
    dbg!(&file);
    let mut out_file = std::fs::File::create("./test.plb")?;
    Output::new(file).out(&mut out_file)?;
    Ok(())
}