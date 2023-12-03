use std::collections::HashMap;

mod coord;
mod dim;
mod image;
mod span;

use crate::coord::Coord;
use crate::dim::Dim;
use crate::image::Image;
use crate::span::Span;

fn main() {
    let input = load();

    let mut run_start_opt: Option<Coord> = None;
    let number_spans = input
        .dim()
        .coords()
        .filter_map(|c| {
            let on_digit = input[c].is_ascii_digit();
            let ret = if let Some(run_start) = run_start_opt {
                if c.y != run_start.y || !on_digit {
                    let mut run_end = c;
                    if run_end.y != run_start.y {
                        run_end.y = run_start.y;
                        run_end.x = input.dim().width;
                    };
                    run_start_opt = None;
                    Some(Span {
                        x_start: run_start.x,
                        x_end: run_end.x,
                        y: run_start.y,
                    })
                } else {
                    None
                }
            } else {
                None
            };

            if run_start_opt.is_none() && on_digit {
                run_start_opt = Some(c);
            }

            ret
        })
        .collect::<Vec<_>>();

    let part_numbers = number_spans
        .iter()
        .filter_map(|s| {
            let is_part_number = s
                .neighborhood(&input.dim())
                .any(|bs| input[bs].iter().any(|c| is_symbol(*c)));
            let gear_symbols = s
                .neighborhood(&input.dim())
                .flat_map(|bs| bs.iter().filter(|c| input[*c] == b'*').collect::<Vec<_>>())
                .collect::<Vec<_>>();

            (is_part_number || !gear_symbols.is_empty()).then(|| {
                (
                    *s,
                    u32::from_str_radix(&*String::from_utf8_lossy(&input[*s]), 10)
                        .expect("valid part numbers"),
                    gear_symbols,
                )
            })
        })
        .collect::<Vec<_>>();

    for pn in part_numbers.iter() {
        println!("found part number: {:?}", pn);
    }

    let mut valid_gears: HashMap<Coord, Vec<u32>> = HashMap::new();
    for (_, pn, gs) in part_numbers.iter() {
        for g in gs.iter() {
            valid_gears
                .entry(*g)
                .and_modify(|pns| pns.push(*pn))
                .or_insert_with(|| vec![*pn]);
        }
    }
    let valid_gears = valid_gears
        .into_iter()
        .filter(|(_, pns)| pns.len() == 2)
        .collect::<HashMap<_, _>>();

    for (g, pns) in valid_gears.iter() {
        println!("found valid gear at {:?}: {:?}", g, pns);
    }

    println!(
        "sum of part numbers {:?}",
        part_numbers.iter().map(|(_, pn, _)| pn).sum::<u32>()
    );

    println!(
        "sum of gear 'ratios' {:?}",
        valid_gears
            .iter()
            .map(|(_, pns)| pns.iter().product::<u32>())
            .sum::<u32>()
    );
}

fn is_symbol(ch: u8) -> bool {
    ch != b'.' && !ch.is_ascii_digit()
}

fn load() -> Image<u8> {
    let mut lines = std::io::stdin().lines();
    let Some(first_line) = lines.next() else {
        panic!("empty input");
    };
    let width = first_line.as_ref().expect("UTF-8").chars().count() as isize;
    let mut data = Vec::new();
    let mut dim = Dim { width, height: 0 };
    for line in std::iter::once(first_line).chain(lines) {
        let line = line.expect("UTF-8");
        if (line.chars().count() as isize) != width {
            panic!("ragged grid");
        }
        data.extend(line.chars().map(|c| c as u8));
        dim.height += 1;
    }
    Image::new_from_data(data, dim)
}
