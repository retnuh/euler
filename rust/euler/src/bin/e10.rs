use euler::util::sieves::{VecAddSieve, VecSieve, VecTightSieve};
use euler::util::timeit;

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

fn main() {
    // let (sum, seconds) = timeit(|| VecAddSieve::new().take_while(|x| *x < 2000000).sum::<u64>());
    let (sum2, seconds2) = timeit(|| {
        VecAddSieve::new()
            .take_while(|x| *x < 2000000)
            .fold(0, |x, y| x + y)
    });
    println!("// sum VecAddSeive:\t{}", sum2);
    println!("// seconds VecAddSeive:\t{}", seconds2);
    let (sum3, seconds3) = timeit(|| {
        VecTightSieve::new()
            .take_while(|x| *x < 2000000)
            .fold(0, |x, y| x + y)
    });
    println!("// sum VecTightSieve:\t{}", sum3);
    println!("// seconds VecTightSieve:\t{}", seconds3);
    let (sum4, seconds4) = timeit(|| {
        VecSieve::new()
            .take_while(|x| *x < 2000000)
            .fold(0, |x, y| x + y)
    });
    println!("// sum VecSieve:\t{}", sum4);
    println!("// seconds VecSieve:\t{}", seconds4);
}
