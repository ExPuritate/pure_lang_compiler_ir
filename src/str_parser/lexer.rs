#![allow(nonstandard_style)]

use crate::str_parser::ast::{GenericDef, SingleWhereClause};
use crate::str_parser::token::{CR, Comment, CommentStyle, LF, Punctuated, TAB, WithSpan};
use crate::str_parser::token::{Keyword, SpecialChar, StringToken, TokenKind};
use comp_base::global::StringTypeReference;
use comp_base::global::instruction::StringInstruction;
use comp_base::{lit_escaper, unicode_xid::UnicodeXID};
use peg::error::ErrorState;
use peg::{ParseLiteral, RuleResult};
use std::fmt::Debug;
use std::sync::LazyLock;

#[const_trait]
trait TokenMatcherExt<Res> {
    fn ok_or<F: Fn(&str, usize) -> RuleResult<Res>>(
        self,
        f: F,
    ) -> impl Fn(&str, usize) -> RuleResult<Res>;
    fn and_then<Res1, F: Fn(&str, usize) -> RuleResult<Res1>>(
        self,
        f: F,
    ) -> impl Fn(&str, usize) -> RuleResult<(Res, Res1)>;
}

impl<T: Fn(&str, usize) -> RuleResult<Res>, Res> const TokenMatcherExt<Res> for T {
    fn ok_or<F: Fn(&str, usize) -> RuleResult<Res>>(
        self,
        f: F,
    ) -> impl Fn(&str, usize) -> RuleResult<Res> {
        move |input, pos| {
            if let RuleResult::Matched(pos, t) = self(input, pos) {
                RuleResult::Matched(pos, t)
            } else {
                f(input, pos)
            }
        }
    }
    fn and_then<Res1, F: Fn(&str, usize) -> RuleResult<Res1>>(
        self,
        f: F,
    ) -> impl Fn(&str, usize) -> RuleResult<(Res, Res1)> {
        move |input, pos| {
            if let RuleResult::Matched(pos, t1) = self(input, pos) {
                if let RuleResult::Matched(pos, t2) = f(input, pos) {
                    return RuleResult::Matched(pos, (t1, t2));
                }
            }
            RuleResult::Failed
        }
    }
}

fn is_whitespace(c: char) -> bool {
    ('\u{0009}'..='\u{000D}').contains(&c)
        || c == '\u{0020}'
        || c == '\u{0085}'
        || c == '\u{200E}'
        || c == '\u{200F}'
        || c == '\u{2028}'
        || c == '\u{2029}'
}

pub fn WHITESPACE(input: &str, pos: usize) -> RuleResult<()> {
    let mut chars = input[pos..].chars();
    let _pos = pos;
    let mut pos = pos;
    while let Some(c) = chars.next()
        && is_whitespace(c)
    {
        pos += c.len_utf8();
    }
    if pos == _pos {
        RuleResult::Failed
    } else {
        RuleResult::Matched(pos, ())
    }
}

pub fn KEYWORD(input: &str, pos: usize, kind: Keyword) -> RuleResult<StringToken> {
    if cfg!(feature = "trace_parser") {
        println!("Matching {kind:?}");
    }
    let _pos = pos;
    if let RuleResult::Matched(pos, _) = input.parse_string_literal(pos, kind.as_str()) {
        if cfg!(feature = "trace_parser") {
            println!("Matched");
        }
        RuleResult::Matched(
            pos,
            StringToken::new(
                TokenKind::Keyword(kind),
                kind.as_str().to_owned(),
                _pos,
                kind.len(),
            ),
        )
    } else {
        if cfg!(feature = "trace_parser") {
            println!("Failed to match");
        }
        RuleResult::Failed
    }
}

pub const fn keyword_matcher(kind: Keyword) -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    move |input, pos| KEYWORD(input, pos, kind)
}

#[allow(unused)]
pub const fn keyword_array_matcher<const N: usize>(
    kinds: [Keyword; N],
) -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    move |input, pos| {
        for kind in kinds {
            if let RuleResult::Matched(pos, t) = keyword_matcher(kind)(input, pos) {
                return RuleResult::Matched(pos, t);
            }
        }
        RuleResult::Failed
    }
}

pub const fn visibility_modifier_matcher() -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    keyword_matcher(Keyword::Public)
        .ok_or(keyword_matcher(Keyword::Private))
        .ok_or(keyword_matcher(Keyword::InAssembly))
}

pub const fn type_modifier_matcher() -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    visibility_modifier_matcher()
}

pub const fn class_modifier_matcher() -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    type_modifier_matcher().ok_or(keyword_matcher(Keyword::Static))
}

pub const fn field_modifier_matcher() -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    visibility_modifier_matcher().ok_or(keyword_matcher(Keyword::Static))
}

pub const fn method_modifier_matcher() -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    visibility_modifier_matcher()
        .ok_or(keyword_matcher(Keyword::Static))
        .ok_or(INTEGER_LITERAL)
}

pub fn SPECIAL_CHAR(input: &str, pos: usize, kind: SpecialChar) -> RuleResult<StringToken> {
    let _pos = pos;
    if let RuleResult::Matched(pos, _) = input.parse_string_literal(pos, kind.as_str()) {
        println!("Matched {kind}");
        RuleResult::Matched(
            pos,
            StringToken::new(
                TokenKind::SpecialChar(kind),
                kind.as_str().to_owned(),
                _pos,
                kind.len(),
            ),
        )
    } else {
        println!("Failed to match {kind}");
        RuleResult::Failed
    }
}

pub fn special_char_matcher(kind: SpecialChar) -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    move |input, pos| SPECIAL_CHAR(input, pos, kind)
}

peg::parser! {
    pub(crate) grammar comment_parser() for str {
        #[no_trace]
        pub rule LINE_COMMENT() -> StringToken =
        p:position!()
        s:(
            "//"
            a:(c:[^'/'|'!'|'\u{000A}'] { c.to_string() } / "//" { "//".to_owned() })
            b:[^'\u{000A}']* {
            let mut res = a;
            res.extend(&b);
            res
        }) {
            StringToken {
                kind: TokenKind::Comment(Comment::Line(None)),
                pos: p,
                len: s.len(),
                content: s,
            }
        }
        #[no_trace]
        pub rule BLOCK_COMMENT() -> StringToken =
        p:position!()
        a:(
            (
                "/*"
                a:(
                    a:[^'*'|'!'] {a.to_string()} /
                    "**" { "**".to_owned() } /
                    a:BlockCommentOrDoc() { a.content }
                )
                b:(
                    a:BlockCommentOrDoc() { a.content } /
                    (!"*/" a:[_] { a.to_string() })
                )*
                "*/" {
                    let mut res = "/*".to_owned();
                    res.push_str(&a);
                    res.push_str(&b.join(""));
                    res.push_str("*/");
                    res
                }
            ) /
            "/**/" { "/**/".to_owned() } /
            "/***/" { "/***/".to_owned() }
        ) {
            StringToken {
                kind: TokenKind::Comment(Comment::Block(None)),
                pos: p,
                len: a.len(),
                content: a,
            }
        }

        #[no_trace]
        pub rule INNER_LINE_DOC() -> StringToken =
        p:position!() "//!" s:[^'\u{000A}'|'\u{000D}']* {
            let mut res = "//!".to_owned();
            res.extend(s);
            StringToken {
                kind: TokenKind::Comment(Comment::Line(Some(CommentStyle::Inner))),
                pos: p,
                len: res.len(),
                content: res,
            }
        }

        #[no_trace]
        pub rule INNER_BLOCK_DOC() -> StringToken =
        p:position!()
        "/*!"
        a:(a:BlockCommentOrDoc() { a.content } / (!"*/" a:[^CR] { a.to_string() }))*
        "*/" {
            let mut res = "/*!".to_owned();
            res.push_str(&a.join(""));
            res.push_str("*/");
            StringToken {
                kind: TokenKind::Comment(Comment::Block(Some(CommentStyle::Inner))),
                pos: p,
                len: res.len(),
                content: res,
            }
        }

        #[no_trace]
        pub rule OUTER_LINE_DOC() -> StringToken =
        p:position!()
        "///"
        a:(
            a:[^'/'] b:[^LF|CR]* {
                let mut res = a.to_string();
                res.extend(b);
                res
            }
        )? "\u{000A}"? {
            let mut s = "///".to_owned();
            s.push_str(&a.unwrap_or("".to_owned()));
            StringToken {
                kind: TokenKind::Comment(Comment::Line(Some(CommentStyle::Outer))),
                pos: p,
                len: s.len(),
                content: s,
            }
        }

        #[no_trace]
        pub rule OUTER_BLOCK_DOC() -> StringToken =
        p:position!()
        "/**"
        a:(a:[^'*'] { a.to_string() } / b:BlockCommentOrDoc() { b.content })
        b:(
            a:(a:BlockCommentOrDoc() { a.content } /
            a:(!"*/" !"\u{000D}" a:[_] {a}) {a.to_string()})* { a.join("") }
        )
        "*/" {
            let mut res = "/**".to_owned();
            res.push_str(&a);
            res.push_str(&b);
            res.push_str("*/");
            StringToken {
                kind: TokenKind::Comment(Comment::Block(Some(CommentStyle::Outer))),
                pos: p,
                len: res.len(),
                content: res,
            }
        }

        #[no_trace]
        pub rule BlockCommentOrDoc() -> StringToken =
        BLOCK_COMMENT() / OUTER_BLOCK_DOC() / INNER_BLOCK_DOC()
    }
}

macro_rules! gen_comment_parser_fn {
    ($(
        $v:vis $i:ident
    )+) => {$(
        #[allow(unused)]
        $v fn $i(input: &str, pos: usize) -> ::peg::RuleResult<super::token::StringToken> {
            paste::paste! {
                comment_parser::[<__parse_ $i>](
                    input,
                    &mut comment_parser::ParseState::new(),
                    &mut ::peg::error::ErrorState::new(pos),
                    pos,
                )
            }
        }
    )+}
}

gen_comment_parser_fn! {
    pub LINE_COMMENT
    pub BLOCK_COMMENT
    pub INNER_LINE_DOC
    pub INNER_BLOCK_DOC
    pub OUTER_LINE_DOC
    pub OUTER_BLOCK_DOC
    pub BlockCommentOrDoc
}

pub const fn COMMENT() -> impl Fn(&str, usize) -> RuleResult<StringToken> {
    LINE_COMMENT
        .ok_or(INNER_LINE_DOC)
        .ok_or(OUTER_LINE_DOC)
        .ok_or(BlockCommentOrDoc)
}

pub const fn IGNORE<TOut: Debug>(
    f: impl Fn(&str, usize) -> RuleResult<TOut>,
) -> impl Fn(&str, usize) -> RuleResult<()> {
    move |input, pos| match f(input, pos) {
        RuleResult::Matched(pos, _) => RuleResult::Matched(pos, ()),
        RuleResult::Failed => RuleResult::Failed,
    }
}

pub fn HEX_DIGIT(input: &str, pos: usize) -> RuleResult<char> {
    if input.as_bytes()[pos].is_ascii_hexdigit() {
        RuleResult::Matched(pos + 1, input.as_bytes()[pos] as char)
    } else {
        RuleResult::Failed
    }
}

pub fn OCT_DIGIT(input: &str, pos: usize) -> RuleResult<char> {
    if input.as_bytes()[pos].is_ascii_octdigit() {
        RuleResult::Matched(pos + 1, input.as_bytes()[pos] as char)
    } else {
        RuleResult::Failed
    }
}

peg::parser! {
    pub grammar char_str_parser() for str {
        pub rule CHAR_LITERAL() -> StringToken =
        p:position!()
        "'"
        a:(
            a:[^'\''|'\\'|LF|CR|TAB] { a.to_string() } /
            s:QUOTE_ESCAPE() { s.to_owned() } /
            ASCII_ESCAPE() /
            UNICODE_ESCAPE()
        )
        "'" {?
            Ok(StringToken {
                kind: TokenKind::Char,
                pos: p,
                len: a.len(),
                content: match lit_escaper::unescape_char(&a) {
                    Ok(c) => c.to_string(),
                    Err(_) => return Err("UNESCAPE_FAILED"),
                },
            })
        }
        pub rule STRING_LITERAL() -> StringToken =
        p:position!()
        "\"" s:(
            s:[^'"' | '\\' | CR] { s.to_string() } /
            s:QUOTE_ESCAPE() { s.to_owned() } /
            ASCII_ESCAPE() /
            UNICODE_ESCAPE() /
            s:STRING_CONTINUE() { s.to_owned() }
        )* "\"" {?
            let s = s.join("");
            let mut res_str = s.clone();
            let len = s.len();
            let mut error = None;
            lit_escaper::unescape_str(&s, |r, res| {
                if error.is_some() {
                    return;
                }
                if let Ok(c) = res {
                    res_str.replace_range(r, &c.to_string());
                    return;
                }
                error = res.err();
            });
            if error.is_some() {
                return Err("UNESCAPE_FAILED");
            }
            Ok(StringToken {
                kind: TokenKind::String,
                pos: p,
                len,
                content: s,
            })
        }

        pub rule RAW_STRING_LITERAL() -> StringToken =
        p:position!()
        "r" s:RAW_STRING_CONTENT() {
            StringToken {
                kind: TokenKind::RawString,
                pos: p,
                len: s.len() + 1,
                content: {
                    s.trim_matches('#').trim_matches('"').to_owned()
                }
            }
        }

        #[no_trace]
        rule QUOTE_ESCAPE() -> &'static str = "\'" { r"\'" } / r#"\""# { r#"\""# }
        #[no_trace]
        rule ASCII_ESCAPE() -> String =
        r"\x" a:#{OCT_DIGIT} b:#{HEX_DIGIT} { format!(r"\x{a}{b}") } /
        r"\n" { r"\n".to_owned() } /
        r"\r" { r"\r".to_owned() } /
        r"\t" { r"\t".to_owned() } /
        r"\\" { r"\\".to_owned() } /
        r"\0" { r"\0".to_owned() }
        #[no_trace]
        rule UNICODE_ESCAPE() -> String =
        r"\u{" a:(
            a:#{HEX_DIGIT} b:("_" {'_'})* {
                let mut res = vec![a];
                res.extend_from_slice(&b);
                String::from_iter(res)
            }
        )*<1, 5> "}" {
            let mut res = r"\u{".to_owned();
            res.push_str(&a.join(""));
            res.push('}');
            res
        }
        #[no_trace]
        rule STRING_CONTINUE() -> &'static str = r"\" "\u{000A}" { "\\\u{000A}" }
        #[no_trace]
        rule RAW_STRING_CONTENT() -> String =
        "\"" s:#{|input, pos| {
            static REGEX: LazyLock<fancy_regex::Regex> = LazyLock::new(|| {
                fancy_regex::Regex::new(const_format::formatcp!("^[^{CR}]*?")).unwrap()
            });
            if let Ok(Some(res)) = REGEX.find_from_pos(input, pos) {
                RuleResult::Matched(
                    res.end(),
                    res.as_str().to_owned(),
                )
            } else {
                RuleResult::Failed
            }
        }} "\"" { format!("\"{s}\"") } /
        "#" a:RAW_STRING_CONTENT() "#" { format!("#{a}#") }
    }
}

macro_rules! gen_char_str_parser_fn {
    ($(
        $v:vis $i:ident
    )+) => {$(
        #[allow(unused)]
        $v fn $i(input: &str, pos: usize) -> ::peg::RuleResult<super::token::StringToken> {
            paste::paste! {
                char_str_parser::[<__parse_ $i>](
                    input,
                    &mut char_str_parser::ParseState::new(),
                    &mut ::peg::error::ErrorState::new(pos),
                    pos,
                )
            }
        }
    )+}
}

gen_char_str_parser_fn! {
    pub RAW_STRING_LITERAL
    pub STRING_LITERAL
    pub CHAR_LITERAL
}

pub fn IDENTIFIER(input: &str, pos: usize) -> RuleResult<StringToken> {
    if cfg!(feature = "trace_parser") {
        println!("Matching IDENTIFIER");
    }
    let s = &input[pos..];
    if !s.starts_with(|x| UnicodeXID::is_xid_start(x) || x == '_') {
        return STRING_LITERAL
            .ok_or(RAW_STRING_LITERAL)
            .ok_or(special_char_matcher(SpecialChar::ExclamationMark))(input, pos);
    }
    let mut chars = s.chars();
    let mut res = String::new();
    res.push(chars.next().unwrap_or_default());
    while let Some(c) = chars.next()
        && UnicodeXID::is_xid_continue(c)
    {
        res.push(c);
    }
    if cfg!(feature = "trace_parser") {
        println!("Matched `{res}`");
    }
    RuleResult::Matched(
        pos + res.len(),
        StringToken {
            kind: TokenKind::Identifier,
            pos,
            len: res.len(),
            content: res,
        },
    )
}

peg::parser! {
    pub(crate) grammar string_reference_parser() for str {
        use comp_base::global::{StringTypeReference, StringName};
        use super::super::token::WithSpan;
        use std::sync::Arc;
        pub(super) rule ty() -> WithSpan<StringTypeReference, ()> =
        p:position!()
        t:(
            ("@" a:#{IDENTIFIER} {
                let s = format!("@{}", &a.content);
                let len = s.len();
                (
                    StringTypeReference::Generic(StringName::from_string(s)),
                    len,
                )
            }) /
            (
                p_start:position!()
                "[" __ assem:#{IDENTIFIER} __ "]"
                a:((
                    ty:#{IDENTIFIER} __ "[" generics: (
                        __ "@" n:#{IDENTIFIER} __ ":" __ t:ty() __
                         { (format!("@{}", &n.content),t.content) }
                    ) ** "|"
                    __ "]" __ p_end:position!() {(
                        StringTypeReference::WithGeneric {
                            assem: StringName::from_string(assem.content.clone()),
                            ty: StringName::from_string(ty.content),
                            type_vars: Arc::new(
                                generics
                                    .into_iter()
                                    .map(|(k, v)| {
                                        (StringName::from_string(k), v)
                                    })
                                    .collect(),
                            ),
                        },
                        p_end - p_start + 1
                    )}
                ) / (ty:#{IDENTIFIER} p_end:position!() {
                        (
                            StringTypeReference::Single {
                                assem: StringName::from_string(assem.content),
                                ty: StringName::from_string(ty.content),
                            },
                            p_end - p_start + 1
                        )
                    })
                ) { a }
            )
        ) {
            WithSpan {
                kind: (),
                pos: p,
                len: t.1,
                content: t.0,
            }
        }

        #[no_trace]
        rule __ -> () = #{SHOULD_SKIP}
    }
}

pub fn STRING_TYPE_REFERENCE(
    input: &str,
    pos: usize,
) -> RuleResult<WithSpan<StringTypeReference, ()>> {
    string_reference_parser::__parse_ty(
        input,
        &mut string_reference_parser::ParseState::new(),
        &mut ErrorState::new(pos),
        pos,
    )
}

pub fn SHOULD_SKIP(input: &str, pos: usize) -> RuleResult<()> {
    let mut pos = pos;
    while let RuleResult::Matched(_pos, _) = IGNORE(COMMENT()).ok_or(WHITESPACE)(input, pos) {
        pos = _pos;
    }
    RuleResult::Matched(pos, ())
}

pub const fn PUNCTUATED<TValue, TSeparator>(
    value_parser: impl Fn(&str, usize) -> RuleResult<TValue>,
    separator_parser: impl Fn(&str, usize) -> RuleResult<TSeparator>,
) -> impl Fn(&str, usize) -> RuleResult<Punctuated<TValue, TSeparator>> {
    move |input, pos| {
        let pos = SHOULD_SKIP(input, pos)?.0;
        let val1_res = value_parser(input, pos);
        let RuleResult::Matched(pos, val1) = val1_res else {
            return RuleResult::Matched(pos, Punctuated::new(vec![], None));
        };
        let pos = SHOULD_SKIP(input, pos)?.0;
        let sep1_res = separator_parser(input, pos);
        let RuleResult::Matched(pos, sep1) = sep1_res else {
            return RuleResult::Matched(pos, Punctuated::new(vec![], Some(val1)));
        };
        let pos = SHOULD_SKIP(input, pos)?.0;
        let mut res = vec![(val1, sep1)];
        let mut _pos = pos;
        while let RuleResult::Matched(pos, val) = value_parser(input, _pos) {
            _pos = SHOULD_SKIP(input, pos)?.0;
            if let RuleResult::Matched(pos, sep) = separator_parser(input, _pos) {
                let pos = SHOULD_SKIP(input, pos)?.0;
                _pos = pos;
                res.push((val, sep));
            } else {
                return RuleResult::Matched(_pos, Punctuated::new(res, Some(val)));
            }
        }
        RuleResult::Matched(_pos, Punctuated::new(res, None))
    }
}

peg::parser! {
    pub grammar integer_parser() for str {
        pub(super) rule INTEGER_LITERAL() -> StringToken =
        p:position!()
        x:(DEC_LITERAL() / BIN_LITERAL() / OCT_LITERAL() / HEX_LITERAL()) {
            StringToken {
                kind: TokenKind::Integer,
                pos: p,
                len: x.len(),
                content: x
            }
        }


        rule DEC_LITERAL() -> String =
            begin:DEC_DIGIT() rest:(DEC_DIGIT() / "_" { '_' })* {
            let mut res = vec![begin];
            res.extend_from_slice(&rest);
            String::from_iter(res)
        }
        rule BIN_LITERAL() -> String =
            "0b" _0:(BIN_DIGIT() / "_" { '_' })* _1:BIN_DIGIT() _2:(BIN_DIGIT() / "_" { '_' })* {
            let mut res = vec!['0', 'b'];
            res.extend_from_slice(&_0);
            res.push(_1);
            res.extend_from_slice(&_2);
            String::from_iter(res)
        }
        rule OCT_LITERAL() -> String =
            "0o" _0:(OCT_DIGIT() / "_" { '_' })* _1:OCT_DIGIT() _2:(OCT_DIGIT() / "_" { '_' })* {
            let mut res = vec!['0', 'o'];
            res.extend_from_slice(&_0);
            res.push(_1);
            res.extend_from_slice(&_2);
            String::from_iter(res)
        }
        rule HEX_LITERAL() -> String =
            "0x" _0:(HEX_DIGIT() / "_" { '_' })* _1:HEX_DIGIT() _2:(HEX_DIGIT() / "_" { '_' })* {
            let mut res = vec!['0', 'x'];
            res.extend_from_slice(&_0);
            res.push(_1);
            res.extend_from_slice(&_2);
            String::from_iter(res)
        }


        rule BIN_DIGIT() -> char =
            ['0'|'1']
        rule OCT_DIGIT() -> char =
            [c if c.is_ascii_octdigit()]
        rule DEC_DIGIT() -> char =
            [c if c.is_ascii_digit()]
        rule HEX_DIGIT() -> char =
            [c if c.is_ascii_hexdigit()]
    }
}

pub fn INTEGER_LITERAL(input: &str, pos: usize) -> RuleResult<StringToken> {
    integer_parser::__parse_INTEGER_LITERAL(
        input,
        &mut integer_parser::ParseState::new(),
        &mut ErrorState::new(pos),
        pos,
    )
}

pub fn parse_int(s: String) -> Result<u64, &'static str> {
    if s.len() <= 2 {
        return s.parse::<u64>().map_err(|_| "FAILED_PARSE_INT");
    }
    match &s[..2] {
        "0x" => u64::from_str_radix(&s[2..], 16).map_err(|_| "FAILED_PARSE_INT"),
        "0o" => u64::from_str_radix(&s[2..], 8).map_err(|_| "FAILED_PARSE_INT"),
        "0b" => u64::from_str_radix(&s[2..], 2).map_err(|_| "FAILED_PARSE_INT"),
        _ => s.parse::<u64>().map_err(|_| "FAILED_PARSE_INT"),
    }
}

peg::parser! {
    pub grammar statement_parser() for str {
        use comp_base::global::{instruction::StringInstruction, StringMethodReference};
        use super::super::token::WithSpan;

        pub(super) rule STATEMENT() -> (WithSpan<StringInstruction, ()>, StringToken) =
        pos:position!()
        ins:((__ "LoadTrue" __ register_addr:#{INTEGER_LITERAL} __ {
                StringInstruction::LoadTrue {
                    register_addr: parse_int(register_addr.content).unwrap(),
                }
            }) /
            (__ "LoadFalse" __ register_addr:#{INTEGER_LITERAL} __ {
                StringInstruction::LoadFalse {
                    register_addr: parse_int(register_addr.content).unwrap(),
                }
            }) /
            (__ "Load_u8" __ register_addr:#{INTEGER_LITERAL} __ val:#{INTEGER_LITERAL} __ {?
                let val = parse_int(val.content).unwrap();
                if val > u8::MAX as u64 {
                    return Err("GREATER_THAN_MAX");
                }
                let register_addr = parse_int(register_addr.content).unwrap();
                match val as u8 {
                    0 => Ok(StringInstruction::Load_u8_0 { register_addr }),
                    1 => Ok(StringInstruction::Load_u8_1 { register_addr }),
                    2 => Ok(StringInstruction::Load_u8_2 { register_addr }),
                    3 => Ok(StringInstruction::Load_u8_3 { register_addr }),
                    4 => Ok(StringInstruction::Load_u8_4 { register_addr }),
                    5 => Ok(StringInstruction::Load_u8_5 { register_addr }),
                    val => Ok(StringInstruction::Load_u8 { register_addr, val }),
                }
            }) /
            (__ "Load_u64" __ register_addr:#{INTEGER_LITERAL} __ val:#{INTEGER_LITERAL} {
                let val = parse_int(val.content).unwrap();
                let register_addr = parse_int(register_addr.content).unwrap();
                // May add `Load_u64_*`
                #[allow(clippy::match_single_binding)]
                match val {
                    val => StringInstruction::Load_u64 { register_addr, val },
                }
            }) /
            (
                __ "NewObject" __
                __ ty:#{STRING_TYPE_REFERENCE} __
                __ ctor_name:#{IDENTIFIER} __
                __ register_addr:#{INTEGER_LITERAL} __
                __ args:#{INTEGER_LITERAL}* __ {
                let ty = ty.content;
                let ctor_name = ctor_name.content.into();
                let args = args
                    .into_iter()
                    .map(|x| parse_int(x.content))
                    .try_collect::<Vec<_>>()
                    .unwrap();
                let register_addr = parse_int(register_addr.content).unwrap();
                StringInstruction::NewObject {
                    ty,
                    ctor_name,
                    args,
                    register_addr,
                }
            }) /
            (
                __ "InstanceCall" __
                __ val:#{INTEGER_LITERAL} __
                // TODO: Change to `StringMethodReference`
                __ method:#{IDENTIFIER} __
                __ ret_at:#{INTEGER_LITERAL} __
                __ args:#{INTEGER_LITERAL}* __ {?
                let val = parse_int(val.content)?;
                let method =
                    StringMethodReference::from_string_repr(method.content)
                        .map_err(|_| "FAILED_PARSE_STRING_METHOD_REFERENCE")?;
                let ret_at = parse_int(ret_at.content).unwrap();
                let args = args
                    .into_iter()
                    .map(|x| parse_int(x.content))
                    .try_collect::<Vec<_>>().unwrap();
                Ok(StringInstruction::InstanceCall {
                    val,
                    method,
                    args,
                    ret_at,
                })
            }) /
            (
                __ "StaticCall" __
                __ ty:#{STRING_TYPE_REFERENCE} __
                // TODO: Change to `StringMethodReference`
                __ method:#{IDENTIFIER} __
                __ ret_at:#{INTEGER_LITERAL} __
                __ args:#{INTEGER_LITERAL}* __ {?
                let ty = ty.content;
                let method =
                    StringMethodReference::from_string_repr(method.content)
                        .map_err(|_| "FAILED_PARSE_STRING_METHOD_REFERENCE")?;
                let ret_at = parse_int(ret_at.content).unwrap();
                let args = args
                    .into_iter()
                    .map(|x| parse_int(x.content))
                    .try_collect::<Vec<_>>()
                    .unwrap();
                Ok(StringInstruction::StaticCall {
                    ty,
                    method,
                    args,
                    ret_at,
                })
            }) /
            (
                __ "LoadArg" __
                __ register_addr:#{INTEGER_LITERAL} __
                __ arg:#{INTEGER_LITERAL} __ {
                let register_addr = parse_int(register_addr.content).unwrap();
                let arg = parse_int(arg.content).unwrap();
                StringInstruction::LoadArg { register_addr, arg }
            }) /
            (__ "LoadAllArgsAsArray" __ register_addr:#{INTEGER_LITERAL} __ {
                let register_addr = parse_int(register_addr.content).unwrap();
                StringInstruction::LoadAllArgsAsArray { register_addr }
            }) /
            (
                __ "LoadStatic" __
                __ register_addr:#{INTEGER_LITERAL} __
                __ ty:#{STRING_TYPE_REFERENCE} __
                __ name:#{IDENTIFIER} {
                let register_addr = parse_int(register_addr.content).unwrap();
                let ty = ty.content;
                let name = name.content.into();
                StringInstruction::LoadStatic {
                    register_addr,
                    ty,
                    name,
                }
            }) /
            (
                __ "SetField" __
                __ register_addr:#{INTEGER_LITERAL} __
                __ field:#{IDENTIFIER} __ {
                let register_addr = parse_int(register_addr.content).unwrap();
                let field = field.content.into();
                StringInstruction::SetField {
                    register_addr,
                    field,
                }
            }) /
            (__ "ReturnVal" __ register_addr:#{INTEGER_LITERAL} __ {
                let register_addr = parse_int(register_addr.content).unwrap();
                StringInstruction::ReturnVal { register_addr }
            })
        ) p_end:position!() semicolon:#{special_char_matcher(SpecialChar::Semicolon)} {?
            Ok((WithSpan::with_span_no_kind(ins, pos, p_end), semicolon))
        }

        rule __ -> () = #{SHOULD_SKIP}
    }
}

pub fn METHOD_STATEMENT(
    input: &str,
    pos: usize,
) -> RuleResult<(WithSpan<StringInstruction, ()>, StringToken)> {
    statement_parser::__parse_STATEMENT(
        input,
        &mut statement_parser::ParseState::new(),
        &mut ErrorState::new(pos),
        pos,
    )
}

pub fn GENERIC_DEF(input: &str, pos: usize) -> RuleResult<GenericDef> {
    if let RuleResult::Matched(pos, (ch_at, name)) =
        special_char_matcher(SpecialChar::At).and_then(IDENTIFIER)(input, pos)
    {
        RuleResult::Matched(pos, GenericDef { ch_at, name })
    } else {
        RuleResult::Failed
    }
}
