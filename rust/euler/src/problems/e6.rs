use euler::util::formulae::{sum_of_squares_upto, sum_upto};
use euler::util::timeit;
use num::pow;

fn e6(n: u64) -> u64 {
    pow(sum_upto(n), 2) - sum_of_squares_upto(n)
}

// sum:		100	25164150
// seconds:	0.000001
fn main() {
    let (result, seconds) = timeit(|| e6(100));
    println!("// sum:\t\t{}\t{}", 100, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e6() {
    assert_eq!(2640, e6(10));
    assert_eq!(25164150, e6(100));
}
