use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::error::Simple;
use chumsky::primitive::{choice, end, just};
use chumsky::text;
use chumsky::Parser;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::ops::Range;

fn sp() -> impl Parser<char, (), Error = Simple<char>> + Copy {
    just(' ').repeated().at_least(1).ignored()
}

type Number = usize;

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct Rule {
    source_start: Number,
    dest_start: Number,
    length: Number,
}

impl Rule {
    fn source_range(&self) -> Range<Number> {
        self.source_start..self.source_start + self.length
    }

    fn dest_range(&self) -> Range<Number> {
        let mut r = self.source_range();
        self.remap(&mut r);
        r
    }

    fn remap(&self, r: &mut Range<Number>) {
        r.start = r.start + self.dest_start - self.source_start;
        r.end = r.end + self.dest_start - self.source_start;
    }
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "({})→({})",
            DispRange(&self.source_range()),
            DispRange(&self.dest_range())
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum MapId {
    SeedToSoil,
    SoilToFertilizer,
    FertilizerToWater,
    WaterToLight,
    LightToTemperature,
    TemperatureToHumidity,
    HumidityToLocation,
}

#[derive(Debug)]
struct Map {
    id: MapId,
    rules: Vec<Rule>,
}

impl PartialEq for Map {
    fn eq(&self, rhs: &Self) -> bool {
        self.id == rhs.id
    }
}

impl Eq for Map {}

impl PartialOrd for Map {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for Map {
    fn cmp(&self, rhs: &Self) -> Ordering {
        self.id.cmp(&rhs.id)
    }
}

impl Map {
    fn new(id: MapId, mut rules: Vec<Rule>) -> Self {
        (&mut rules[..]).sort();
        Self { id, rules }
    }

    fn apply(&self, mut remaining: Vec<Range<Number>>) -> Vec<Range<usize>> {
        println!("  ===============");
        println!("  map {:?} apply to {}:", self.id, DispRanges(&remaining));
        let mut res = Vec::new();
        for rule in self.rules.iter() {
            let src = rule.source_range();
            if remaining.is_empty() {
                break;
            }
            let inp = std::mem::replace(&mut remaining, Vec::new());
            print!("    remaining = {}, rule = {}: ", DispRanges(&inp), rule,);
            remaining = inp
                .into_iter()
                .flat_map(|mut r| {
                    if r.is_empty() {
                        print!("r = {}, empty. ", DispRange(&r));
                        vec![]
                    } else if r.end <= src.start || r.start >= src.end {
                        // range outside rule entirely. should never occur unless rules overlap,
                        // and one subsumed another's overlap with the input
                        print!("r = {}, disjunct. ", DispRange(&r));
                        vec![r]
                    } else if r.start >= src.start && r.end <= src.end {
                        // input totally contained within rule source
                        print!("r = {}, subset. ", DispRange(&r));
                        rule.remap(&mut r);
                        res.push(r);
                        vec![]
                    } else if r.start < src.start && r.end >= src.end {
                        // input totally contains rule source
                        print!("r = {}, superset. ", DispRange(&r));
                        let mut out = src.clone();
                        rule.remap(&mut out);
                        res.push(out);
                        vec![r.start..src.start, src.end..r.end]
                    } else if r.start <= src.start && r.end <= src.end {
                        // input left of rule source
                        print!("r = {}, left. ", DispRange(&r));
                        let mut out = src.start..r.end;
                        rule.remap(&mut out);
                        res.push(out);
                        vec![r.start..src.start]
                    } else if r.start >= src.start && r.end > src.end {
                        // input right of rule source
                        print!("r = {}, right. ", DispRange(&r));
                        let mut out = r.start..src.end;
                        rule.remap(&mut out);
                        res.push(out);
                        vec![src.end..r.end]
                    } else {
                        panic!("r = {:?}, src = {:?} for {:?}", r, src, rule);
                    }
                })
                .collect();
            println!();
        }
        res.extend(remaining.into_iter());
        let () = &res[..].sort_by_key(|r| r.start);
        println!("  done: {}", DispRanges(&res));
        res
    }
}

struct DispRange<'a, A>(&'a Range<A>);

impl<'a, A: std::fmt::Display> std::fmt::Display for DispRange<'a, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}..{}", self.0.start, self.0.end)
    }
}

struct DispRanges<'a, A>(&'a Vec<Range<A>>);

impl<'a, A: std::fmt::Display> std::fmt::Display for DispRanges<'a, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for r in self.0.iter() {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}..{}", r.start, r.end)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

fn parse_file() -> impl Parser<char, (Vec<Number>, Vec<Map>), Error = Simple<char>> {
    let sp = sp();

    (just("seeds:").ignore_then(sp).ignore_then(
        text::int(10)
            .map(|cs: String| Number::from_str_radix(&*cs, 10).unwrap())
            .separated_by(sp)
            .then_ignore(just("\n")),
    ))
    .then(parse_maps())
    .then_ignore(end())
}

fn parse_maps() -> impl Parser<char, Vec<Map>, Error = Simple<char>> {
    just("\n").ignore_then(parse_map().repeated().map(|mut maps| {
        (&mut maps[..]).sort();
        maps
    }))
}

fn parse_map() -> impl Parser<char, Map, Error = Simple<char>> {
    parse_map_id()
        .then_ignore(just(" map:\n"))
        .then(parse_map_rules())
        .then_ignore(just("\n"))
        .map(|(id, rules)| Map::new(id, rules))
}

fn parse_map_id() -> impl Parser<char, MapId, Error = Simple<char>> {
    choice((
        just("seed-to-soil").to(MapId::SeedToSoil),
        just("soil-to-fertilizer").to(MapId::SoilToFertilizer),
        just("fertilizer-to-water").to(MapId::FertilizerToWater),
        just("water-to-light").to(MapId::WaterToLight),
        just("light-to-temperature").to(MapId::LightToTemperature),
        just("temperature-to-humidity").to(MapId::TemperatureToHumidity),
        just("humidity-to-location").to(MapId::HumidityToLocation),
    ))
}

fn parse_map_rules() -> impl Parser<char, Vec<Rule>, Error = Simple<char>> {
    parse_map_rule().repeated()
}
fn parse_map_rule() -> impl Parser<char, Rule, Error = Simple<char>> {
    let sp = sp();
    let parse_number = text::int(10).map(|cs: String| Number::from_str_radix(&*cs, 10).unwrap());
    parse_number
        .then(sp.ignore_then(parse_number))
        .then(sp.ignore_then(parse_number))
        .then_ignore(just("\n"))
        .map(|((dest_start, source_start), length)| Rule {
            dest_start,
            source_start,
            length,
        })
}

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let input = std::fs::read_to_string(&*path).unwrap();
    let (seeds, maps) = match parse_file().parse(&*input) {
        Ok(cs) => cs,
        Err(errs) => {
            for err in errs {
                Report::build(ReportKind::Error, &*path, err.span().start)
                    .with_message(err.to_string())
                    .with_label(
                        Label::new((&*path, err.span()))
                            .with_message(format!("{:?}", err.reason()))
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((&*path, Source::from(&*input)))
                    .unwrap();
            }
            return;
        }
    };
    println!("seeds: {:?}", seeds);
    for map in maps.iter() {
        println!("map {:?}:", map.id);
        for rule in map.rules.iter() {
            println!("  {:?}", rule);
        }
    }

    let seed_ranges = seeds
        .iter()
        .fold(
            (Vec::new(), None),
            |(mut accum, hold_opt), n| match hold_opt {
                Some(start) => {
                    accum.push(start..start + *n);
                    (accum, None)
                }
                None => (accum, Some(*n)),
            },
        )
        .0;

    let mut res = BTreeMap::new();
    for r in seed_ranges.into_iter() {
        println!("seed range {}", DispRange(&r));
        let mut steps = Vec::new();
        let mut cross_section = vec![r];
        for map in maps.iter() {
            steps.push(cross_section.clone());
            let cardinality_before = cross_section.iter().map(|r| r.end - r.start).sum::<usize>();
            cross_section = map.apply(cross_section);
            let cardinality_after = cross_section.iter().map(|r| r.end - r.start).sum::<usize>();
            assert_eq!(cardinality_before, cardinality_after);
        }
        let location_start = cross_section[0].start;
        steps.push(cross_section);
        res.insert(location_start, steps);
    }

    for (location, path) in res.into_iter() {
        println!(
            "location {}{}",
            location,
            path.into_iter()
                .rev()
                .fold(String::new(), |mut s, cross_section| {
                    write!(s, " <- {}", DispRanges(&cross_section)).unwrap();
                    s
                })
        );
    }
}
