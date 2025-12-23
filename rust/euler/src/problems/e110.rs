use crate::util::min_factors_for_num_diophantine_solutions;
use num::pow;

fn e110(solutions_exceed: u64) -> u64 {
    let x = min_factors_for_num_diophantine_solutions(solutions_exceed);
    println!("Found: {:?}", x);
    x.iter().fold(1, |tot, &(p, c)| tot * pow(p, c))
}

// n:		4000000	9350130049860600
// seconds:	0.049693
pub fn main() -> String {
    // Same as e108 but bigger # solutions
    let result = e110(4_000_000);
    format!("n:\t\t{}\t{}", 4_000_000, result)
}

#[test]
fn test_e110() {
    assert_eq!(4, e110(2));
    assert_eq!(180180, e110(1000));
}
