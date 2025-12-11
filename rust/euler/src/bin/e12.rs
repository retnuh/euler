use euler::debug_println;
use euler::util::divisor_count;
use euler::util::timeit;

fn e12(n: u64) -> u64 {
    let naturals = 1_u64..;
    let mut triangle_number = 0;
    for i in naturals {
        triangle_number += i;
        let divisor_count = divisor_count(triangle_number);
        debug_println!(
            "triangle_number: {}, count: {}",
            triangle_number,
            divisor_count,
        );
        if divisor_count > n {
            return triangle_number;
        }
    }
    return 0;
}

// val:		500	76576500
// seconds:	0.59765
fn main() {
    let (result, seconds) = timeit(|| e12(500));
    println!("// val:\t\t{}\t{}", 500, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e12() {
    assert_eq!(6, e12(3));
    assert_eq!(28, e12(5));
}
