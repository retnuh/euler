use euler::util::{min_factors_for_num_diophantine_solutions, timeit};
use num::pow;

fn e110(solutions_exceed: u64) -> u64 {
    let x = min_factors_for_num_diophantine_solutions(solutions_exceed);
    println!("Found: {:?}", x);
    x.iter().fold(1, |tot, &(p, c)| tot * pow(p, c))
}

// n:		4000000	9350130049860600
// seconds:	0.049693
fn main() {
    // Same as e108 but bigger # solutions
    let (result, seconds) = timeit(|| e110(4_000_000));
    println!("// n:\t\t{}\t{}", 4_000_000, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e110() {
    assert_eq!(4, e110(2));
    assert_eq!(180180, e110(1000));
}
