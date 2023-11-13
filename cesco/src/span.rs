use std::fmt::Display;


#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub beg: u32,
    pub end: u32,
    pub line: u32,
    pub file: String,
}

impl Span {
    pub fn new(beg: u32, end: u32, line: u32, file: &str) -> Self {
        Self{ beg, end, line, file: file.to_string() }
    }

    pub fn merge(a: Self, b: Self) -> Self {
        Self{ beg: a.beg, end: b.end, ..a }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.cco {}:{}", self.file, self.line, self.beg - 1)
    }
}


#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Item<T> {
    pub val: T,
    pub span: Span,
}

impl<T> Item<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self{ val, span }
    }
}

impl<T: Default> Item<T> {
    pub fn mock(val: T) -> Self {
        Self{ val, ..Self::default() }
    }
}
