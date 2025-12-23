use crate::debug_println;
use crate::util::{all_split_positions, all_string_splits};
use itertools::Itertools;
use num::integer::{div_rem, sqrt};
use num::iter::range_step_from;
use num::pow;

// function g(n, s)
//     n ≤ s && return n == s
//     for i in 1:ndigits(n)-1
//         g(n % 10^i, s - n ÷ 10^i) && return true
//     end
//     false
// end

// Depth first walk of permutations, from Forums https://projecteuler.net/thread=719
fn g(s: u64, n: u64) -> bool {
    if s <= n {
        return n == s;
    }
    let limit = (s.ilog10() + 1) as usize;
    // println!("s:\t{}\tn:\t{}\tlimit:{}", s, n, limit);
    for i in 1..limit {
        let (d, r) = div_rem(s, pow(10, i));
        // println!("\t{}\t{}\t{}", i, d, r);
        if r > n {
            return false;
        }
        // println!("\t\tg({},{})", d, n - r);
        if g(d, n - r) {
            return true;
        }
    }
    false
}

fn e719(upper: u64) -> u64 {
    let upto = sqrt(upper) as usize;
    let mut tot = 0;
    for n in range_step_from(9, 9)
        .interleave(range_step_from(9, 9).map(|x| x + 1))
        .take(2 * upto / 9)
    {
        // println!("start");
        if g(n * n, n) {
            // println!("S:\t{}\t{}", n * n, n);
            tot += n * n;
        }
    }
    tot
}

// T(1000000000000):		128088830547982
// seconds:	336.89556
#[allow(dead_code)]
fn e719_orig(upper: u64) -> u64 {
    let upto = sqrt(upper) as usize;
    let mut tot = 0;
    let mut cached_s_size = 2;
    let mut cached_split_posns = all_split_positions(2).collect::<Vec<Vec<usize>>>();
    // congruency hint from https://oeis.org/A104113 - all congruent 0 or 1 mod 9
    for n in range_step_from(9, 9)
        .interleave(range_step_from(9, 9).map(|x| x + 1))
        .take(2 * upto / 9)
    {
        let s = n * n;
        let s_string = s.to_string();
        if s_string.len() != cached_s_size {
            debug_println!("Realloc split pos");
            cached_split_posns = all_split_positions(s_string.len()).collect();
            cached_s_size = s_string.len();
        }
        for split_digits in all_string_splits(&s_string, &cached_split_posns) {
            if split_digits
                .iter()
                .map(|digits| digits.parse::<u64>().unwrap())
                .sum::<u64>()
                == n
            {
                // println!("S:\t{}\t{}\t{:?}\t{}\t{}", n, s, split_digits, n % 9, s % 9);
                tot += s;
                break;
            }
        }
    }
    tot
}

// T(1000000000000):		128088830547982
// seconds:	2.272181
pub fn main() -> String {
    let n = pow(10, 12);
    let result = e719(n);
    format!("T({}):\t\t{}", n, result)
}

#[test]
fn test_e719_s_number() {
    assert_eq!(41333, e719(10000));
    assert_eq!(128088830547982, e719(pow(10, 12)));
}
