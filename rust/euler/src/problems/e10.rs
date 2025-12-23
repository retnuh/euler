use crate::util::sieves::VecAddSieve;

pub fn main() -> String {
    // Using VecAddSieve as it's the fastest according to benchmarks
    let sum = VecAddSieve::new().take_while(|x| *x < 2000000).sum::<u64>();
    format!("sum:\t\t{}", sum)
}
