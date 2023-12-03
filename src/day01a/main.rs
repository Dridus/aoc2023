fn main() {
    fn digit_or_0(mut it: impl Iterator<Item = char>) -> u32 {
        it.find_map(|c| c.to_digit(10)).unwrap_or(0)
    }
    println!(
        "{:?}",
        std::io::stdin().lines().map(|l|
            l.map_or(0, |l| 10 * digit_or_0(l.chars()) + digit_or_0(l.chars().rev()))
        ).sum::<u32>()
    );
}
