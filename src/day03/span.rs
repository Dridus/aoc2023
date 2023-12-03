use crate::coord::Coord;
use crate::dim::Dim;

#[derive(Copy, Clone, Default, PartialEq)]
pub(super) struct Span {
    pub(super) x_start: isize,
    pub(super) x_end: isize,
    pub(super) y: isize,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}..{},{})", self.x_start, self.x_end, self.y)
    }
}

impl Span {
    pub(super) fn start(&self) -> Coord {
        Coord {
            x: self.x_start,
            y: self.y,
        }
    }

    pub(super) fn end(&self) -> Coord {
        Coord {
            x: self.x_end,
            y: self.y,
        }
    }

    pub(super) fn expand_left(&mut self, dx: usize) {
        self.x_start = (self.x_start - (dx as isize)).max(0);
    }

    pub(super) fn expand_right(&mut self, dx: usize, dim: &Dim) {
        self.x_end = (self.x_end + (dx as isize)).min(dim.width);
    }

    pub(super) fn iter<'a>(&'a self) -> Iter<'a> {
        Iter {
            span: self,
            coord: self.start(),
        }
    }

    pub(super) fn neighborhood<'a>(&'a self, dim: &'a Dim) -> Neighborhood<'a> {
        Neighborhood {
            span: self,
            dim,
            next: Some(Side::Top),
        }
    }
}

pub(super) struct Iter<'a> {
    span: &'a Span,
    coord: Coord,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Coord;
    fn next(&mut self) -> Option<Coord> {
        if self.coord.x >= self.span.x_end {
            return None;
        }

        let coord = self.coord;
        self.coord.x += 1;
        Some(coord)
    }
}

enum Side {
    Top,
    Right,
    Bottom,
    Left,
}

pub(super) struct Neighborhood<'a> {
    span: &'a Span,
    dim: &'a Dim,
    next: Option<Side>,
}

impl<'a> Iterator for Neighborhood<'a> {
    type Item = Span;
    fn next(&mut self) -> Option<Span> {
        let Some(next) = self.next.take() else {
            return None;
        };
        let mut s = self.span.clone();
        match next {
            Side::Top => {
                self.next = Some(Side::Right);
                if s.y == 0 {
                    return self.next();
                }
                s.y -= 1;
                s.expand_left(1);
                s.expand_right(1, self.dim);
            }
            Side::Right => {
                self.next = Some(Side::Bottom);
                if s.x_end >= self.dim.width {
                    return self.next();
                }
                s.x_start = s.x_end;
                s.expand_right(1, self.dim);
            }
            Side::Bottom => {
                self.next = Some(Side::Left);
                if s.y + 1 >= self.dim.height {
                    return self.next();
                }
                s.y += 1;
                s.expand_left(1);
                s.expand_right(1, self.dim);
            }
            Side::Left => {
                self.next = None;
                if s.x_start == 0 {
                    return self.next();
                }
                s.x_end = s.x_start;
                s.expand_left(1);
            }
        }
        if s.x_start == s.x_end {
            return self.next();
        }
        Some(s)
    }
}

