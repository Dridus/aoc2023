use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, space1, u32};
use nom::combinator::map;
use nom::multi::separated_list1;
use nom::sequence::preceded;
use nom::IResult;
use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
struct Observation {
    red: u32,
    green: u32,
    blue: u32,
}

impl Observation {
    fn power(&self) -> u32 {
        self.red * self.green * self.blue
    }
}

impl PartialOrd<Observation> for Observation {
    fn partial_cmp(&self, rhs: &Observation) -> Option<Ordering> {
        if self.red == rhs.red && self.green == rhs.green && self.blue == rhs.blue {
            Some(Ordering::Equal)
        } else if self.red <= rhs.red && self.green <= rhs.green && self.blue <= rhs.blue {
            Some(Ordering::Less)
        } else {
            Some(Ordering::Greater)
        }
    }
}

impl Default for Observation {
    fn default() -> Self {
        Self {
            red: 0,
            green: 0,
            blue: 0,
        }
    }
}

impl std::ops::Add<&Observation> for &Observation {
    type Output = Observation;
    fn add(self, rhs: &Observation) -> Observation {
        Observation {
            red: self.red.max(rhs.red),
            green: self.green.max(rhs.green),
            blue: self.blue.max(rhs.blue),
        }
    }
}

impl std::ops::AddAssign<&Observation> for Observation {
    fn add_assign(&mut self, rhs: &Observation) {
        self.red = self.red.max(rhs.red);
        self.green = self.green.max(rhs.green);
        self.blue = self.blue.max(rhs.blue);
    }
}

impl<'a> std::iter::Sum<&'a Observation> for Observation {
    fn sum<I: Iterator<Item = &'a Self>>(iter: I) -> Self {
        iter.fold(Observation::default(), |mut obsum, ob| {
            obsum += ob;
            obsum
        })
    }
}

impl std::fmt::Display for Observation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut first = true;
        let mut w = |n, s| {
            if n <= 0 {
                return Ok(());
            }

            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }

            write!(f, "{} {}", n, s)
        };
        w(self.red, "red")?;
        w(self.green, "green")?;
        w(self.blue, "blue")?;
        Ok(())
    }
}

#[derive(Debug)]
struct Game {
    game_no: u32,
    observations: Vec<Observation>,
}

impl std::fmt::Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Game {}: ", self.game_no)?;
        let mut first = true;
        for ob in self.observations.iter() {
            if first {
                first = false;
            } else {
                write!(f, "; ")?;
            }
            write!(f, "{}", ob)?;
        }
        Ok(())
    }
}

fn main() {
    let limit = Observation {
        red: 12,
        green: 13,
        blue: 14,
    };
    let games = std::io::stdin().lines().map(|l| {
        let l = l.unwrap();
        match parse_game(&*l) {
            Ok(("", n)) => n,
            other => panic!("canne parse {l:?}: {other:x?}"),
        }
    });
    let games_summed: Vec<(Game, Observation, bool)> = games
        .into_iter()
        .map(|g| {
            let obsum: Observation = g.observations.iter().sum();
            let possible = &obsum <= &limit;
            (g, obsum, possible)
        })
        .collect();

    for (g, obsum, possible) in games_summed.iter() {
        println!(
            "{}{} => {}, {} power",
            if *possible { "Possible " } else { "" },
            g,
            obsum,
            obsum.power()
        );
    }

    println!(
        "{}",
        games_summed
            .iter()
            .filter(|(_, _, possible)| *possible)
            .map(|(g, _, _)| g.game_no)
            .sum::<u32>()
    );

    println!(
        "{}",
        games_summed
            .iter()
            .map(|(_, obsum, _)| obsum.power())
            .sum::<u32>()
    );
}

fn parse_game(inp: &str) -> IResult<&str, Game> {
    let (inp, _) = tag("Game")(inp)?;
    let (inp, _) = space1(inp)?;
    let (inp, game_no) = u32(inp)?;
    let (inp, _) = tag(":")(inp)?;
    let (inp, observations) = parse_observations(inp)?;
    Ok((
        inp,
        Game {
            game_no,
            observations,
        },
    ))
}

fn parse_observations(inp: &str) -> IResult<&str, Vec<Observation>> {
    separated_list1(tag(";"), preceded(space0, parse_observation))(inp)
}

fn parse_observation(inp: &str) -> IResult<&str, Observation> {
    map(
        separated_list1(tag(","), preceded(space0, parse_qty_color)),
        |obs| obs.iter().sum(),
    )(inp)
}

fn parse_qty_color(inp: &str) -> IResult<&str, Observation> {
    let (inp, qty) = nom::character::complete::u32(inp)?;
    let (inp, _) = space1(inp)?;
    alt((
        map(tag("red"), move |_| Observation {
            red: qty,
            ..Observation::default()
        }),
        map(tag("green"), move |_| Observation {
            green: qty,
            ..Observation::default()
        }),
        map(tag("blue"), move |_| Observation {
            blue: qty,
            ..Observation::default()
        }),
    ))(inp)
}
