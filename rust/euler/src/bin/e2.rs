#![feature(test)]

extern crate test;

use euler::util::{fib_iter, timeit};

fn e2(n: u64) -> u64 {
    return fib_iter()
        .take_while(|x| *x <= n)
        .filter(|x| x % 2 == 0)
        .sum();
}

// sum:		4000000	4613732
// seconds:	0.000005
fn main() {
    let (result, seconds) = timeit(|| e2(4_000_000));
    println!("// sum:\t\t{}\t{}", 4_000_000, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e2() {
    assert_eq!(2 + 8 + 34, e2(100));
    assert_eq!(4613732, e2(4_000_000));
}
