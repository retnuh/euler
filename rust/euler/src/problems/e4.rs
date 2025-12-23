extern crate core;

use num::range_step;

fn is_palindrome(n: i64) -> bool {
    let sn: Vec<char> = n.to_string().chars().collect();
    let len = sn.len();
    for i in 0..(len / 2) {
        if sn[i] != sn[len - 1 - i] {
            return false;
        }
    }
    true
}

fn e4(from: i64, to: i64) -> i64 {
    let mut cur = 0;
    'outer: for i in range_step(to, from - 1 + ((to + 1 - from) / 2), -1) {
        for j in range_step(to, from - 1, -1) {
            let n = i * j;
            if is_palindrome(n) {
                // println!("Palindrome:\t{}\t{}\t{}", i, j, n);
                if n > cur {
                    cur = n;
                }
                continue 'outer;
            }
        }
    }
    cur
}

// sum:         600851475143    906609
// seconds:     0.015419
pub fn main() -> String {
    let result = e4(100, 999);
    format!("val:\t\t{}\t{}\t{}", 100, 999, result)
}

#[test]
fn test_e4() {
    assert_eq!(9009, e4(10, 99));
    assert_eq!(906609, e4(100, 999));
}
