#![cfg_attr(not(nightly), allow(internal_features))]
#![feature(test)]

extern crate test;

use euler::problems::e684::{little_s, little_s_brute, little_s_hk};
use test::Bencher;

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
