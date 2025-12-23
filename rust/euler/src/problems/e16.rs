use num::{BigUint, FromPrimitive};

fn e16(p: u32) -> u64 {
    let x: BigUint = BigUint::from_u32(2).unwrap().pow(p);
    // let y = x.to_string();
    // println!("{:?}", y.chars());
    x.to_string()
        .chars()
        .fold(0, |acc, c| acc + (c as u8 - b'0') as u64)
}

// pow:         1000
// answer:      1366
// seconds:     0.000021
pub fn main() -> String {
    let input = 1000;
    let result = e16(input);
    format!("pow:\t\t{}\nanswer:\t{}", input, result)
}

#[test]
fn test_e16() {
    assert_eq!(26, e16(15));
}
