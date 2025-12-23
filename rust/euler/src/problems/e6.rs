use crate::util::formulae::{sum_of_squares_upto, sum_upto};
use num::pow;

fn e6(n: u64) -> u64 {
    pow(sum_upto(n), 2) - sum_of_squares_upto(n)
}

// sum:		100	25164150
// seconds:	0.000001
pub fn main() -> String {
    let result = e6(100);
    format!("sum:\t\t{}\t{}", 100, result)
}

#[test]
fn test_e6() {
    assert_eq!(2640, e6(10));
    assert_eq!(25164150, e6(100));
}
