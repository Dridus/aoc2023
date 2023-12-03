use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{anychar, one_of};
use nom::combinator::{map, peek, value};
use nom::multi::fold_many0;
use nom::{IResult, Parser as _};

fn main() {
    println!(
        "{:?}",
        std::io::stdin()
            .lines()
            .map(|l| {
                let l = l.unwrap();
                match p(&*l) {
                    Ok(("", n)) => {
                        println!("{l:?} -> {n:?}");
                        n
                    }
                    other => panic!("canne parse {l:?}: {other:x?}"),
                }
            })
            .sum::<u32>()
    );
}

fn p(inp: &str) -> IResult<&str, u32> {
    map(
        fold_many0(
            digit_opt,
            || (None, None),
            |s, dopt| (s.0.or(dopt), dopt.or(s.1)),
        ),
        |s| 10 * s.0.unwrap_or(0) + s.1.unwrap_or(0),
    )(inp)
}

fn digit_opt(inp: &str) -> IResult<&str, Option<u32>> {
    alt((
        map(
            alt((
                map(
                    peek(alt((
                        value(0, tag("zero")),
                        value(1, tag("one")),
                        value(2, tag("two")),
                        value(3, tag("three")),
                        value(4, tag("four")),
                        value(5, tag("five")),
                        value(6, tag("six")),
                        value(7, tag("seven")),
                        value(8, tag("eight")),
                        value(9, tag("nine")),
                    )))
                    .and(anychar),
                    |(dopt, _)| dopt,
                ),
                map(one_of("0123456789"), |c| char::to_digit(c, 10).unwrap()),
            )),
            Some,
        ),
        value(None, anychar),
    ))(inp)
}
