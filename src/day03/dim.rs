use crate::coord::Coord;

#[derive(Copy, Clone, Debug, PartialEq)]
pub(super) struct Dim {
    pub(super) width: isize,
    pub(super) height: isize,
}

impl Dim {
    pub(super) fn coords<'a>(&'a self) -> Coords<'a> {
        Coords {
            dim: self,
            next: Coord { x: 0, y: 0 },
        }
    }
}

pub(super) struct Coords<'a> {
    dim: &'a Dim,
    next: Coord,
}

impl<'a> Iterator for Coords<'a> {
    type Item = Coord;
    fn next(&mut self) -> Option<Coord> {
        if self.next.y >= self.dim.height {
            return None;
        }
        if self.next.x >= self.dim.width {
            self.next.x = 0;
            self.next.y += 1;
            return self.next();
        }
        let coord = self.next;
        self.next.x += 1;
        Some(coord)
    }
}

