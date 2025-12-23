use crate::util::naturals::factors;
use num::pow;

fn e5(n: u64) -> u64 {
    // figure out the max power for each prime in the range, then multiply it all out
    let mut x: Vec<(u64, usize)> = (2..=n).flat_map(factors).collect();
    x.sort_unstable();
    // dedup_by takes first elt, want highest power, so reverse first
    x.reverse();
    x.dedup_by(|&mut (x, _), &mut (y, _)| x == y);
    x.iter().fold(1, |tot, &(n, p)| {
        // println!("{}\t{}\t{}", tot, n, p);
        tot * pow(n, p)
    })
}

// sum:		20	232792560
// seconds:	0.000099
pub fn main() -> String {
    let result = e5(20);
    format!("sum:\t\t{}\t{}", 20, result)
}

#[test]
fn test_e5() {
    assert_eq!(2520, e5(10));
    assert_eq!(232792560, e5(20));
}
