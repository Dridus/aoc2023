use crate::coord::Coord;
use crate::dim::Dim;
use crate::span::Span;

pub(super) struct Image<A> {
    data: Vec<A>,
    dim: Dim,
}

impl<A> Image<A> {
    pub(super) fn new_from_data(data: Vec<A>, dim: Dim) -> Self {
        Image { data, dim }
    }

    pub(super) fn offset(&self, coord: Coord) -> usize {
        (coord.y * self.dim.width + coord.x) as usize
    }

    pub(super) fn dim(&self) -> Dim {
        self.dim
    }
}

impl<A> std::ops::Index<Coord> for Image<A> {
    type Output = A;
    fn index(&self, coord: Coord) -> &A {
        &self.data[self.offset(coord)]
    }
}

impl<A> std::ops::IndexMut<Coord> for Image<A> {
    fn index_mut(&mut self, coord: Coord) -> &mut A {
        let offs = self.offset(coord);
        &mut self.data[offs]
    }
}

impl<A> std::ops::Index<Span> for Image<A> {
    type Output = [A];
    fn index(&self, span: Span) -> &[A] {
        let start = self.offset(span.start());
        let end = self.offset(span.end());
        &self.data[start..end]
    }
}
