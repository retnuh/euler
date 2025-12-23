use euler::util::timeit;
use num::{pow, BigInt, ToPrimitive};

fn e48(n: usize, chop: usize) -> u64 {
    let mut x: BigInt = BigInt::from(0);
    let modulus = BigInt::from(pow(10_u64, chop));
    for i in 1..=n {
        let exp = BigInt::from(i);
        x += BigInt::from(i).modpow(&exp, &modulus)
    }
    (x % modulus).to_u64().unwrap()
}

// input:		1000
// answer:		9110846700
// seconds:	0.017014
fn main() {
    let input = 1000;
    let (result, seconds) = timeit(|| e48(input, 10));
    println!("// input:\t\t{}", input);
    println!("// answer:\t\t{}", result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e48() {
    assert_eq!(10405071317, e48(10, 12));
}
