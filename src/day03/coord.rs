#[derive(Copy, Clone, Default, Eq, Hash, PartialEq)]
pub(super) struct Coord {
    pub(super) x: isize,
    pub(super) y: isize,
}

impl std::fmt::Debug for Coord {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({},{})", self.x, self.y)
    }
}
