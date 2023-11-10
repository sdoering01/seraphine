pub type Pos = usize;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub len: usize,
}

impl Span {
    pub fn new(start: Pos, len: usize) -> Span {
        Span { start, len }
    }

    pub fn until(self, other: Span) -> Span {
        Span::new(self.start, other.start - self.start + other.len)
    }
}
