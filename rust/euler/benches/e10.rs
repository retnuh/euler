#![cfg_attr(not(nightly), allow(internal_features))]
#![feature(test)]

extern crate test;

use euler::util::sieves::{VecAddSieve, VecSieve, VecTightSieve};
use test::Bencher;

// Old timing info
// Debug:
// sum2:	142913828922
// seconds2:	93.878197
// sum3:	142913828922
// seconds3:	171.110359
// sum4:	142913828922
// seconds4:	159.840458
// Release: (wow big difference!)
// sum2:	142913828922
// seconds2:	11.557855
// sum3:	142913828922
// seconds3:	75.365262
// sum4:	142913828922
// seconds4:	67.068501

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
