use crate::util::sieves::VecAddSieve;

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

pub fn main() -> String {
    // Using VecAddSieve as it's the fastest according to benchmarks above
    let sum = VecAddSieve::new().take_while(|x| *x < 2000000).sum::<u64>();
    format!("sum:\t\t{}", sum)
}
