#![feature(test)]

extern crate test;

use euler::util::sieves::ChainSieve;
use euler::util::sieves::VecAddSieve;
use euler::util::sieves::{VecSieve, VecTightSieve};
use test::Bencher;

#[bench]
fn b10k_chain_sieve_10000(b: &mut Bencher) {
    b.iter(|| ChainSieve::new().take(10000).last())
}

#[bench]
fn b10k_vec_add_sieve_10000(b: &mut Bencher) {
    b.iter(|| VecAddSieve::new().take(10000).last())
}

#[bench]
fn b10k_vec_sieve_10000(b: &mut Bencher) {
    b.iter(|| VecSieve::new().take(10000).last())
}

#[bench]
fn b10k_vec_tight_sieve_10000(b: &mut Bencher) {
    b.iter(|| VecTightSieve::new().take(10000).last())
}
