#![feature(test)]

extern crate test;

use euler::util::digit_sum;
use num::Integer;

use test::Bencher;

fn main() {}

const MODULO_FACTOR: u64 = 1_000_000_007;

fn little_s_brute(n: u64) -> u64 {
    let mut i = 0;
    loop {
        if digit_sum(i) == n {
            // println!("s:\t{}\t{}",n,i);
            return i;
        }
        i += 1;
    }
}

fn little_s_hk(n: u64) -> u64 {
    let (d, r) = n.div_rem(&9);
    let mut tot = r;
    for _ in 0..d {
        tot = 10 * tot + 9
    }
    tot
}

// jscrane had nice elegant re-write of little_s, should be same
fn little_s(n: u64) -> u64 {
    let (d, r) = n.div_rem(&9);
    (r + 1) * 10u64.pow(d as u32) - 1
}

#[bench]
fn e684_little_s_10000(b: &mut Bencher) {
    b.iter(|| little_s(10000))
}
#[bench]
fn e684_little_s_hk_10000(b: &mut Bencher) {
    b.iter(|| little_s_hk(10000))
}
#[bench]
fn e684_little_s_brute_10000(b: &mut Bencher) {
    b.iter(|| little_s_brute(10000))
}

#[test]
fn test_little_s() {
    assert_eq!(19, little_s_brute(10));
    assert_eq!(19, little_s(10));
    for i in 0..50 {
        // assert_eq!(i, little_s(i))
        assert_eq!(little_s_brute(i), little_s(i))
    }
    let x = 81;
    println!(
        "s:\t{}\t{}\t{}",
        x,
        little_s(x),
        little_s(x) % MODULO_FACTOR
    );
    println!(
        "s:\t{}\t{}\t{}",
        x + 1,
        little_s(x + 1),
        little_s(x + 1) % MODULO_FACTOR
    );
    println!(
        "s:\t{}\t{}\t{}",
        x + 2,
        little_s(x + 2),
        little_s(x + 2) % MODULO_FACTOR
    );
    let diff = little_s(x + 1) - little_s(x);
    let md = diff % MODULO_FACTOR;
    let neg_md = MODULO_FACTOR - md;
    println!(
        "s diff:\t{}\t{}\t-{}\t{}\t{}",
        diff,
        md,
        neg_md,
        MODULO_FACTOR / md,
        MODULO_FACTOR / neg_md
    );
}

fn big_s_brute(n: u64) -> u64 {
    (1..=n).map(|i| little_s(i)).sum()
}

#[test]
fn test_big_s() {
    assert_eq!(1074, big_s_brute(20));
    let _x = 90;
    for x in 80..100 {
        let bx = big_s_brute(x);
        let bxp = big_s_brute(x - 1);
        let md = (bx - bxp) % MODULO_FACTOR;
        let neg_md = MODULO_FACTOR - md;
        println!(
            "S:\t{}\t{}\t{}\t-{}\t\t{}\t{}\t-{}",
            x,
            bx,
            bx % MODULO_FACTOR,
            MODULO_FACTOR - (bx % MODULO_FACTOR),
            bx - bxp,
            md,
            neg_md
        );
    }
    // let diff = big_s_brute(x+1) - big_s_brute(x);
    // let md = diff % MODULO_FACTOR;
    // let neg_md = MODULO_FACTOR - md;
    // println!("S diff:\t{}\t{}\t-{}\t{}\t{}", diff, md, neg_md, MODULO_FACTOR / md, MODULO_FACTOR/neg_md);
}
