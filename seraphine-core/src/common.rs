pub type Pos = usize;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub len: usize,
}

impl Span {
    pub fn new(start: Pos, len: usize) -> Span {
        Span { start, len }
    }
}
