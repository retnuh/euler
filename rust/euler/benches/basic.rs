#![cfg_attr(not(nightly), allow(internal_features))]
#![feature(test)]

extern crate test;

use euler::util::sieves::ChainSieve;
use euler::util::sieves::VecAddSieve;
use euler::util::sieves::VecSieve;
use euler::util::sieves::VecTightSieve;
use test::Bencher;

#[bench]
fn basic_vec_sieve_100(b: &mut Bencher) {
    b.iter(|| VecSieve::new().take(100).last())
}

#[bench]
fn basic_vec_sieve_1000(b: &mut Bencher) {
    b.iter(|| VecSieve::new().take(1000).last())
}

#[bench]
fn basic_vec_tight_sieve_100(b: &mut Bencher) {
    b.iter(|| VecTightSieve::new().take(100).last())
}

#[bench]
fn basic_vec_tight_sieve_1000(b: &mut Bencher) {
    b.iter(|| VecTightSieve::new().take(1000).last())
}

#[bench]
fn basic_vec_add_sieve_100(b: &mut Bencher) {
    b.iter(|| VecAddSieve::new().take(100).last())
}

#[bench]
fn basic_vec_add_sieve_1000(b: &mut Bencher) {
    b.iter(|| VecAddSieve::new().take(1000).last())
}

#[bench]
fn basic_chain_sieve_100(b: &mut Bencher) {
    b.iter(|| ChainSieve::new().take(100).last())
}

#[bench]
fn basic_chain_sieve_1000(b: &mut Bencher) {
    b.iter(|| ChainSieve::new().take(1000).last())
}
