#![allow(dead_code)]

use euler::problems::e684::little_s;

#[cfg(test)]
use euler::debug_println;
#[cfg(test)]
use euler::problems::e684::MODULO_FACTOR;

fn main() {}

fn big_s_brute(n: u64) -> u64 {
    (1..=n).map(little_s).sum()
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
        debug_println!(
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
