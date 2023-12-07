use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::error::Simple;
use chumsky::primitive::just;
use chumsky::text;
use chumsky::Parser;
use std::collections::BTreeSet;

#[derive(Clone, Debug)]
struct Card {
    no: usize,
    winning: BTreeSet<u32>,
    draw: BTreeSet<u32>,
}

impl Card {
    fn count_matches(&self) -> usize {
        self.draw.intersection(&self.winning).count()
    }

    fn score_a(&self) -> usize {
        let matches = self.count_matches();
        if matches > 0 {
            2_usize.pow(self.count_matches() as u32 - 1)
        } else {
            0
        }
    }

    fn score_b(cards: &[Card]) -> Vec<u32> {
        let mut tallies = Vec::new();
        let limit = cards.iter().map(|c| c.no).max().unwrap_or(0);
        tallies.resize(limit + 1, 0);
        for c in cards.into_iter() {
            let tally = &mut tallies[c.no];
            *tally += 1;
            let matches = c.count_matches();
            if matches > 0 {
                let tally = *tally;
                tallies[(c.no + 1).min(limit)..(c.no + 1 + matches).min(limit+1)]
                    .iter_mut()
                    .for_each(|other_tally| *other_tally += tally);
            }
        }
        tallies
    }
}

fn sp() -> impl Parser<char, (), Error = Simple<char>> + Copy {
    just(' ').repeated().at_least(1).ignored()
}

fn parse_card() -> impl Parser<char, Card, Error = Simple<char>> {
    let sp = sp();

    (just("Card")
        .ignore_then(sp)
        .ignore_then(text::int(10).map(|cs: String| usize::from_str_radix(&*cs, 10).unwrap()))
        .then_ignore(just(':')))
    .then_ignore(sp)
    .then(
        text::int(10)
            .map(|cs: String| u32::from_str_radix(&*cs, 10).unwrap())
            .separated_by(sp)
            .map(|v| v.into_iter().collect()),
    )
    .then_ignore(sp.then(just('|')).then(sp))
    .then(
        text::int(10)
            .map(|cs: String| u32::from_str_radix(&*cs, 10).unwrap())
            .separated_by(sp)
            .map(|v| v.into_iter().collect()),
    )
    .then_ignore(just(' ').repeated())
    .map(|((no, winning), draw)| Card { no, winning, draw })
}

fn parse_cards() -> impl Parser<char, Vec<Card>, Error = Simple<char>> {
    parse_card().separated_by(just("\n"))
}

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let input = std::fs::read_to_string(&*path).unwrap();
    let cards = match parse_cards().parse(&*input) {
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
    for card in cards.iter() {
        println!("{:?}", card);
    }
    println!(
        "score a: {}",
        cards.iter().map(|c| c.score_a()).sum::<usize>()
    );
    let score_b = Card::score_b(&cards);
    println!("score b: {:?}", score_b);
    println!("score b sum: {}", score_b.iter().sum::<u32>());
}
