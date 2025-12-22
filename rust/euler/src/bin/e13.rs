use euler::util::timeit_duration;
use num::integer::div_rem;

static E13_DATA: &str = include_str!("resources/e13.txt");

fn e13<'a>(num_digits: usize, number_strings: impl Iterator<Item = &'a str>) -> String {
    let nums: Vec<Vec<u8>> = number_strings
        .map(|s| s.as_bytes().iter().rev().map(|b| b - b'0').collect())
        .collect();
    let mut sum: Vec<u8> = Vec::with_capacity(2 * nums[0].len());
    let mut tot: u64 = 0;
    for digit in 0..nums[0].len() {
        for v in &nums {
            // debug_println!("{digit} {i} {tot} {}", nums[i][digit]);
            tot += v[digit] as u64;
        }

        let (t, d) = div_rem(tot, 10);
        sum.push(d as u8);
        tot = t
    }
    while tot > 0 {
        let (t, d) = div_rem(tot, 10);
        sum.push(d as u8);
        tot = t
    }
    // Convert digits to String
    let s: String = sum[sum.len() - num_digits..]
        .iter()
        .rev()
        .map(|d| char::from(b'0' + *d))
        .collect();

    // debug_println!("string = {}", s);
    s
}

// val:         5537376230
// nanoseconds: 9000
fn main() {
    let (result, duration) = timeit_duration(|| e13(10, E13_DATA.split_ascii_whitespace()));
    println!("// val:\t\t{}", result);
    println!("// nanoseconds:\t{}", duration.as_nanos())
}

#[test]
fn test_e13() {
    assert_eq!(
        "123".to_string(),
        e13(3, vec!["103456", "020111"].into_iter())
    );
    assert_eq!(
        "123".to_string(),
        e13(3, vec!["83456", "40111"].into_iter())
    );
}
