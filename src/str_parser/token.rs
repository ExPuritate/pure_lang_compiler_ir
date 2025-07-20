use std::{fmt::Display, iter::{Chain, Map}};

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Keyword(Keyword),
    SpecialChar(SpecialChar),
    Comment(Comment),
    Char,
    String,
    RawString,
    Identifier,
    Integer,
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    Class,
    Field,
    Method,

    Public,
    Private,
    Static,
    InAssembly,
    AssemblyName,

    Where,
}

impl PartialEq<str> for Keyword {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq(other)
    }
}

impl<T: AsRef<str>> PartialEq<T> for Keyword {
    fn eq(&self, other: &T) -> bool {
        self.as_str().eq(other.as_ref())
    }
}

impl Keyword {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Keyword::Class => "class",
            Keyword::Field => "field",
            Keyword::Public => "public",
            Keyword::Private => "private",
            Keyword::Static => "static",
            Keyword::InAssembly => "in-assembly",
            Keyword::Method => "method",
            Keyword::AssemblyName => "assembly-name",
            Keyword::Where => "where",
        }
    }
    pub const fn len(&self) -> usize {
        self.as_str().len()
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum SpecialChar {
    ParenthesisOpen,
    ParenthesisClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    AngleBracketOpen,
    AngleBracketClose,

    Colon,
    Semicolon,

    Arrow,
    ThinArrow,

    Comma,
    ExclamationMark,
    At,
}

impl Display for SpecialChar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl SpecialChar {
    pub const fn as_str(&self) -> &'static str {
        match self {
            SpecialChar::ParenthesisOpen => "(",
            SpecialChar::ParenthesisClose => ")",
            SpecialChar::BracketOpen => "[",
            SpecialChar::BracketClose => "]",
            SpecialChar::BraceOpen => "{",
            SpecialChar::BraceClose => "}",
            SpecialChar::AngleBracketOpen => "<",
            SpecialChar::AngleBracketClose => ">",
            SpecialChar::Colon => ":",
            SpecialChar::Semicolon => ";",
            SpecialChar::Arrow => "=>",
            SpecialChar::ThinArrow => "->",
            SpecialChar::Comma => ",",
            SpecialChar::ExclamationMark => "!",
            SpecialChar::At => "@",
        }
    }
    pub const fn len(&self) -> usize {
        self.as_str().len()
    }
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Comment {
    Line(Option<CommentStyle>),
    Block(Option<CommentStyle>),
}

#[allow(unused)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CommentStyle {
    Outer,
    Inner,
}

pub type StringToken = WithSpan<String, TokenKind>;

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WithSpan<T, TKind> {
    pub kind: TKind,
    pub content: T,
    pub pos: usize,
    pub len: usize,
}

#[allow(unused)]
impl<T, TKind> WithSpan<T, TKind> {
    pub const fn new(kind: TKind, content: T, pos: usize, len: usize) -> Self {
        Self {
            kind,
            content,
            pos,
            len,
        }
    }
    pub const fn with_span(kind: TKind, content: T, start: usize, end: usize) -> Self {
        Self {
            kind,
            content,
            pos: start,
            len: end - start,
        }
    }
}

impl<T> WithSpan<T, ()> {
    pub const fn with_span_no_kind(content: T, start: usize, end: usize) -> Self {
        Self {
            kind: (),
            content,
            pos: start,
            len: end - start,
        }
    }
}

pub(crate) const NUL: char = '\u{0000}';
pub(crate) const TAB: char = '\u{0009}';
pub(crate) const LF: char = '\u{000A}';
pub(crate) const CR: char = '\u{000D}';

#[allow(unused)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punctuated<TValue, TSeparator> {
    inner: Vec<(TValue, TSeparator)>,
    last: Option<TValue>,
}

impl<TValue, TSeparator> Punctuated<TValue, TSeparator> {
    pub fn new(inner: Vec<(TValue, TSeparator)>, last: Option<TValue>) -> Self {
        Self { inner, last }
    }
    pub fn iter(&self) -> PunctuatedIterator<'_, TValue, TSeparator> {
        PunctuatedIterator {
            inner: self,
            pos: 0,
        }
    }
}

impl<TValue, TSeparator> IntoIterator for Punctuated<TValue, TSeparator> {
    type Item = TValue;
    type IntoIter = impl Iterator<Item = TValue>;
    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.inner.into_iter().map(|x| x.0).chain(self.last)
    }
}

pub struct PunctuatedIterator<'a, TValue, TSeparator> {
    inner: &'a Punctuated<TValue, TSeparator>,
    pos: usize,
}

impl<'a, TValue, TSeparator> Iterator for PunctuatedIterator<'a, TValue, TSeparator> {
    type Item = &'a TValue;
    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.pos == self.inner.inner.len() {
            self.pos += 1;
            return self.inner.last.as_ref();
        }
        self.pos += 1;
        self.inner.inner.get(self.pos - 1).map(|x| &x.0)
    }
}
