use crate::util::naturals::factors;

fn e3(n: u64) -> u64 {
    factors(n)
        .iter()
        .max_by(|&&(x, _), &&(y, _)| x.cmp(&y))
        .unwrap()
        .0
}

// sum:		600851475143	6857
// seconds:	0.004301
pub fn main() -> String {
    let result = e3(600851475143);
    format!("sum:\t\t{}\t{}", 600851475143_u64, result)
}

#[test]
fn test_e3() {
    assert_eq!(29, e3(13195));
    assert_eq!(6857, e3(600851475143));
}
