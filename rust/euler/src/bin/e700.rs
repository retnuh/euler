#![feature(test)]
#![allow(dead_code)]

extern crate test;

use euler::util::timeit;
use num::integer::div_rem;

const MODULUS: u64 = 4503599627370517_u64;
const ADDEND: u64 = 1504170715041707_u64;

pub struct Sequence {
    cur: u64,
    // seen: HashSet<BigInt>,
}

// impl Sequence {
//     pub fn new() -> Sequence {
//         Sequence {
//             cur: 0,
//             // seen: HashSet::new(),
//         }
//     }
// }
//
// impl Iterator for Sequence {
//     type Item = u64;
//
//     #[inline]
//     fn next(&mut self) -> Option<Self::Item> {
//         let mut tmp: u64 = self.cur as u64 + ADDEND;
//         if tmp > MODULUS {
//             tmp -= MODULUS;
//         }
//         self.cur = tmp as u64;
//         // if self.seen.contains(&val) {
//         //     return None
//         // }
//         // todo not sure about seen logic tbh
//         // self.seen.insert(val);
//         Some(self.cur)
//     }
// }

fn e700(limit: usize) -> u64 {
    let mut count = 0;
    let mut prev: u64 = MODULUS;
    let mut sum = 0;
    let mut cur: u64 = ADDEND;
    loop {
        sum += cur;
        let n: u64 = prev % cur;
        println!("Found coin: {}\t{}\t{}", cur, n, cur - n);
        prev = cur;
        cur = cur - n;
        if prev <= 1 {
            break;
        }
        count += 1;
        if limit > 0 && count >= limit {
            break;
        }
    }
    sum
}

// sum all coins:		0	1517926517777556
// seconds:	0.000506
fn main() {
    let limit = 0;
    let (result, seconds) = timeit(|| e700(limit));
    println!("// sum all coins:\t\t{}\t{}", limit, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e700() {
    assert_eq!(1504170715041707_u64, e700(1));
    assert_eq!(1513083232796311_u64, e700(2));
    assert_eq!(1517017461828034_u64, e700(5));
}
