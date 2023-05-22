#![feature(test)]

extern crate test;

use euler::util::{min_factors_for_num_diophantine_solutions, timeit};
use num::pow;

fn e108(solutions_exceed: u64) -> u64 {
    let x = min_factors_for_num_diophantine_solutions(solutions_exceed);
    println!("Found: {:?}", x);
    x.iter().fold(1, |tot, &(p, c)| tot * pow(p, c))
}

// n:		1000	180180
// seconds:	0.000082
fn main() {
    let (result, seconds) = timeit(|| e108(1000));
    println!("// n:\t\t{}\t{}", 1000, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e108() {
    assert_eq!(4, e108(2));
    assert_eq!(180180, e108(1000));
}
