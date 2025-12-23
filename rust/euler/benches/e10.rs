#![cfg_attr(not(nightly), allow(internal_features))]
#![feature(test)]

extern crate test;

use euler::util::sieves::{VecAddSieve, VecSieve, VecTightSieve};
use test::Bencher;

#[bench]
fn e10_vec_add_sieve(b: &mut Bencher) {
    b.iter(|| VecAddSieve::new().take_while(|x| *x < 2000000).sum::<u64>())
}

#[bench]
fn e10_vec_tight_sieve(b: &mut Bencher) {
    b.iter(|| {
        VecTightSieve::new()
            .take_while(|x| *x < 2000000)
            .sum::<u64>()
    })
}

#[bench]
fn e10_vec_sieve(b: &mut Bencher) {
    b.iter(|| VecSieve::new().take_while(|x| *x < 2000000).sum::<u64>())
}
