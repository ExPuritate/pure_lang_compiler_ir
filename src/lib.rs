#![allow(non_snake_case)]
#![feature(is_ascii_octdigit)]
#![feature(new_range_api)]
#![feature(trait_alias)]
#![feature(const_trait_impl)]
#![feature(decl_macro)]
#![feature(iterator_try_collect)]
#![feature(impl_trait_in_assoc_type)]
#![feature(inherent_associated_types)]
#![feature(stmt_expr_attributes)]
#![allow(incomplete_features)]
#![cfg_attr(test, allow(warnings))]

mod ast;
mod output;
mod str_parser;
#[cfg(test)]
mod tests;

use crate::output::Output;
use comp_base::AnyResult;
use comp_base::global::configs::compiler::CompilerConfig;
use derive_more::Display;
use line_ending::LineEnding;
use std::collections::HashMap;

pub struct Compiler {
    sources: HashMap<String, Compiler::File>,
}

#[allow(dead_code)]
impl Compiler {
    type File = str_parser::ast::File;
}

impl comp_shared::Compiler for Compiler {
    fn add_file(&mut self, p: &str) -> AnyResult<()> {
        self.sources.insert(
            p.to_owned(),
            str_parser::file_parser::file(&LineEnding::normalize(&std::fs::read_to_string(p)?))?,
        );
        Ok(())
    }

    fn compile(&self, map: &HashMap<String, String>) -> AnyResult<()> {
        for (path, file) in &self.sources {
            let out_path = map
                .get(path)
                .ok_or(Error::PathNotFoundInMapWhileCompiling)?;
            let mut out_file = std::fs::File::create(out_path)?;
            out_file.lock()?;
            let output = Output {
                file: ast::File::try_from(file.clone())?,
            };
            output.out(&mut out_file)?;
            out_file.unlock()?;
        }
        Ok(())
    }

    fn paths(&self) -> Vec<String> {
        self.sources.keys().cloned().collect()
    }
}

#[derive(PartialEq, Debug, Clone, Display, thiserror::Error)]
pub enum Error {
    PathNotFoundInMapWhileCompiling,
}
#[derive(PartialEq, Debug, Clone, Display, thiserror::Error)]
pub enum ParsingError {
    UnknownModifier(String),
}

#[allow(nonstandard_style)]
#[unsafe(no_mangle)]
pub extern "Rust" fn INIT_COMPILER(
    #[allow(unused)] config: &CompilerConfig,
) -> Box<dyn comp_shared::Compiler> {
    Box::new(Compiler {
        sources: HashMap::new(),
    })
}

#[unsafe(no_mangle)]
pub static SOURCE_SUFFIX: &str = "plir";
