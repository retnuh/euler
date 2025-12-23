use crate::debug_println;
use crate::util::naturals::divisor_count;

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
    0
}

// val:		500	76576500
// seconds:	0.59765
pub fn main() -> String {
    let result = e12(500);
    format!("val:\t\t{}\t{}", 500, result)
}

#[test]
fn test_e12() {
    assert_eq!(6, e12(3));
    assert_eq!(28, e12(5));
}
