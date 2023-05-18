#![feature(test)]

extern crate test;

use euler::util::timeit;

fn e1(n: u64) -> u64 {
    let mut tot = 0;
    let mut i = 3;
    while i < n {
        tot += i;
        i += 3;
    }
    i = 5;
    while i < n {
        tot += i;
        if i + 5 < n {
            tot += i + 5;
        }
        i += 15;
    }
    return tot;
}

// sum:		1000	233168
// seconds:	0.000002
fn main() {
    let (result, seconds) = timeit(|| e1(1000));
    println!("sum:\t\t{}\t{}", 1000, result);
    println!("seconds:\t{}", seconds)
}

#[test]
fn test_e1() {
    assert_eq!(23, e1(10));
    assert_eq!(78, e1(20));
    assert_eq!(233168, e1(1000));
}
