#![feature(test)]

extern crate test;

use euler::util::sieves::VecAddSieve;
use euler::util::timeit;

fn e7(n: usize) -> u64 {
    VecAddSieve::new().nth(n - 1).unwrap()
}

// nth prime:		10001	104743
// seconds:	0.49628
fn main() {
    let (result, seconds) = timeit(|| e7(10001));
    println!("// nth prime:\t\t{}\t{}", 10001, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e7() {
    assert_eq!(2, e7(1));
    assert_eq!(11, e7(5));
    assert_eq!(104743, e7(10001));
}
