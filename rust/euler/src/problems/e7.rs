use crate::util::sieves::VecAddSieve;

fn e7(n: usize) -> u64 {
    VecAddSieve::new().nth(n - 1).unwrap()
}

// nth prime:		10001	104743
// seconds:	0.49628
pub fn main() -> String {
    let result = e7(10001);
    format!("nth prime:\t\t{}\t{}", 10001, result)
}

#[test]
fn test_e7() {
    assert_eq!(2, e7(1));
    assert_eq!(11, e7(5));
    assert_eq!(104743, e7(10001));
}
