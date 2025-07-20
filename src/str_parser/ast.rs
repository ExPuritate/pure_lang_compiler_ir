#![allow(clippy::too_many_arguments)]

use crate::str_parser::token::{Punctuated, StringToken, WithSpan};
use comp_base::global::StringTypeReference;
use comp_base::global::derive_ctor::ctor;
use comp_base::global::instruction::StringInstruction;

#[allow(unused)]
#[derive(Debug, ctor, Clone)]
pub struct File {
    pub asts: Vec<Ast>,
}

#[allow(unused, clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum Ast {
    TypeDef(TypeDef),
    SetAssemblyName(SetAssemblyNameAst),
}

#[allow(unused)]
#[derive(Debug, ctor, Clone)]
pub struct SetAssemblyNameAst {
    pub ch_exclamation_mark: StringToken,
    pub kw_assembly_name: StringToken,
    pub ch_colon: StringToken,
    pub name: StringToken,
    pub ch_semicolon: StringToken,
}

#[allow(unused)]
#[derive(Debug, Clone)]
pub enum TypeDef {
    Class(ClassDef),
}

#[allow(unused)]
#[derive(ctor, Debug, Clone)]
pub struct ClassDef {
    pub kw_class: StringToken,

    pub ch_bracket_open0: StringToken,
    pub modifiers: Vec<StringToken>,
    pub ch_bracket_close0: StringToken,

    pub name: StringToken,

    pub generics: Option<(
        /* AngleBracketOpen */ StringToken,
        Punctuated<GenericDef, StringToken>,
        /*AngleBracketClose*/ StringToken,
    )>,

    pub parent_with_colon: Option<(StringToken, WithSpan<StringTypeReference, ()>)>,

    pub where_clauses: Option<WhereClauses>,

    pub ch_brace_open0: StringToken,
    pub class_items: Vec<ClassItem>,
    pub ch_brace_close0: StringToken,
}

#[allow(unused)]
#[derive(ctor, Debug, Clone)]
pub struct GenericDef {
    pub ch_at: StringToken,
    pub name: StringToken,
}

#[allow(unused)]
#[derive(ctor, Debug, Clone)]
pub struct WhereClauses {
    pub kw_where: StringToken,
    pub clauses: Vec<(SingleWhereClause, /* Semicolon */ StringToken)>,
}

#[allow(unused)]
#[derive(ctor, Debug, Clone)]
pub struct SingleWhereClause {
    pub name: StringToken,
    pub ch_colon: StringToken,
    /// First must be parent binding.
    pub bindings: Punctuated<WithSpan<StringTypeReference, ()>, /* Comma */ StringToken>,
}
#[allow(unused, clippy::large_enum_variant)]
#[derive(ctor, Debug, Clone)]
pub enum ClassItem {
    Field(Field),
    Method(Method),
}

#[allow(unused)]
#[derive(ctor, Debug, Clone)]
pub struct Field {
    pub kw_field: StringToken,

    pub ch_bracket_open0: StringToken,
    pub modifiers: Vec<StringToken>,
    pub ch_bracket_close0: StringToken,

    pub name: StringToken,

    pub ch_colon: StringToken,

    pub ty: WithSpan<StringTypeReference, ()>,

    pub ch_semicolon: StringToken,
}

#[allow(unused)]
#[derive(ctor, Debug, Clone)]
pub struct Method {
    pub kw_method: StringToken,

    pub ch_bracket_open0: StringToken,
    pub modifiers: Vec<StringToken>,
    pub ch_bracket_close0: StringToken,

    pub name: StringToken,

    pub generics: Option<(
        /* AngleBracketOpen */ StringToken,
        Punctuated<GenericDef, StringToken>,
        /*AngleBracketClose*/ StringToken,
    )>,

    pub ch_parenthesis_open0: StringToken,
    pub arguments: Punctuated<WithSpan<StringTypeReference, ()>, StringToken>,
    pub ch_parenthesis_close0: StringToken,

    pub register_len: StringToken,

    pub ch_arrow: StringToken,

    pub ret_ty: WithSpan<StringTypeReference, ()>,

    pub where_clauses: Option<WhereClauses>,

    pub ch_brace_open0: StringToken,
    pub statements: Vec<(WithSpan<StringInstruction, ()>, StringToken)>,
    pub ch_brace_close0: StringToken,
}
