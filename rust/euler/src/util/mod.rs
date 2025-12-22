use crate::util::naturals::Factors;
use crate::util::sieves::VecAddSieve;
use itertools::Itertools;
use num::{pow, Integer};
use std::time::{Duration, SystemTime};

pub mod debug;
pub mod formulae;
pub mod naturals;
pub mod sieves;

pub fn timeit<F: Fn() -> T, T>(f: F) -> (T, f64) {
    let start = SystemTime::now();
    let result = f();
    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();
    (result, duration.as_secs_f64())
}

pub fn timeit_duration<F: Fn() -> T, T>(f: F) -> (T, Duration) {
    let start = SystemTime::now();
    let result = f();
    let end = SystemTime::now();
    let duration = end.duration_since(start).unwrap();
    (result, duration)
}

pub fn fib(n: u32) -> u64 {
    let n: u64 = n as u64;
    match n {
        0 => 0,
        1 => 1,
        _ => (2u64..=n).fold((0, 1), |(a, b), _| (b, a + b)).1,
    }
}

pub struct FibIter(u64, u64);

impl Iterator for FibIter {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let r = self.0;
        self.0 = self.1;
        self.1 += r;
        Some(r)
    }
}

pub fn fib_iter() -> FibIter {
    FibIter(0, 1)
}

#[test]
fn test_fib() {
    assert_eq!(0, fib(0));
    assert_eq!(1, fib(1));
    assert_eq!(1, fib(2));
    assert_eq!(2, fib(3));
    assert_eq!(3, fib(4));
    assert_eq!(5, fib(5));
    assert_eq!(8, fib(6));
    assert_eq!(6765, fib(20));
    assert_eq!(2880067194370816120, fib(90));
    assert_eq!(4660046610375530309, fib(91));
}

#[test]
fn test_fib_iter() {
    assert_eq!(
        vec![0, 1, 1, 2, 3, 5, 8, 13, 21],
        fib_iter().take(9).collect::<Vec<u64>>()
    );
    assert_eq!((8).div_rem(&3), (2, 2));
}

pub fn digit_sum(n: u64) -> u64 {
    let mut tot = 0;
    let mut n = n;
    while n > 0 {
        let (d, r) = n.div_rem(&10);
        tot += r;
        n = d;
    }
    tot
}

#[test]
fn test_digit_sum() {
    assert_eq!(0, digit_sum(0));
    assert_eq!(3, digit_sum(111));
    assert_eq!(10, digit_sum(19));
}

fn iter_num_diophantine_solutions(
    factors: Factors,
    best_n_so_far: u64,
    left_total_solns: u64,
    left_total_product: u64,
    index: usize,
    target: u64,
) -> Option<(Factors, u64)> {
    if index >= factors.len() {
        if left_total_solns > target && left_total_product < best_n_so_far {
            // println!(
            //     "New best end: {:?} {} {} {}",
            //     factors, left_total, best_so_far, index
            // );
            return Some((factors, left_total_product));
        }
        // println!(
        //     "Not best end: {:?} {} {} {}",
        //     factors, left_total, best_so_far, index
        // );
        return None;
    }
    let (prime, start_count) = factors[index];
    let other_totals = factors.iter().skip(index + 1).fold(
        (left_total_solns, left_total_product),
        |(ts, tp), &(p, c)| (ts * (2 * c + 1) as u64, tp * p),
    );
    let current_solns = other_totals.0 * (2 * start_count + 1) as u64;
    if current_solns >= target {
        if other_totals.1 * prime < best_n_so_far {
            // println!(
            //     "New best edge: {:?} {} {} {}",
            //     factors, current, best_so_far, index
            // );
            return Some((factors, other_totals.1 * prime));
        }
        // println!(
        //     "Not best edge: {:?} {} {} {}",
        //     factors, current, best_so_far, index
        // );
        return None;
    }
    let mut count = start_count;
    let mut best: Option<(Factors, u64)> = None;
    while (count * 2 + 1) as u64 * other_totals.0 < target
        && pow(prime, count) * other_totals.1 < best_n_so_far
    {
        count += 1;
        let mut nf = factors.clone();
        nf[index] = (prime, count);
        match iter_num_diophantine_solutions(
            nf,
            best.as_ref().map_or(best_n_so_far, |(_, b)| *b),
            left_total_solns * (count * 2 + 1) as u64,
            left_total_product * pow(prime, count),
            index + 1,
            target,
        ) {
            None => {}
            Some(b) => {
                match &best {
                    None => best = Some(b),
                    Some((_, best_n)) => {
                        if b.1 < *best_n {
                            best = Some(b);
                            // println!("New best rec: {:?} {} {} {}", best.0, count, best.1, index);
                        }
                    }
                }
            }
        }
    }
    best
}

pub fn min_factors_for_num_diophantine_solutions(solutions_exceed: u64) -> Factors {
    let mut factors: Factors = Vec::new();
    let mut primes = VecAddSieve::new();
    let mut total_solns = 1;
    let mut total_product = 1;
    while total_solns < 2 * solutions_exceed {
        let p = primes.next().unwrap();
        total_solns *= 3;
        total_product *= p;
        factors.push((p, 1))
    }
    let mut best = (factors.clone(), total_product);
    while !factors.is_empty() {
        factors.pop();
        match iter_num_diophantine_solutions(factors.clone(), best.1, 1, 1, 0, 2 * solutions_exceed)
        {
            None => {}
            Some(candidate) => {
                println!("Checking {:?} vs {:?}", candidate, best);
                if candidate.1 < best.1 {
                    println!("\tBetter!");
                    best = candidate
                }
            }
        }
    }
    best.0
}

#[test]
fn test_min_factors_for_num_diophantine_solutions() {
    assert_eq!(vec![(2, 2)], min_factors_for_num_diophantine_solutions(2));
    assert_eq!(
        vec![(2, 2), (3, 2), (5, 1), (7, 1), (11, 1), (13, 1)],
        min_factors_for_num_diophantine_solutions(1000)
    );
}

pub fn all_split_positions(count: usize) -> impl Iterator<Item = Vec<usize>> {
    // All the split positions for a string/vector like object
    // i.e. for the string "abcde", all the positions needed to split
    // the string in all possible ways, "a" + "bcde", "a" + "b" + "cde", etc.
    (1..count).flat_map(move |i| (1..count).combinations(i))
}

#[test]
fn test_all_split_positions() {
    assert_eq!(
        vec![] as Vec<Vec<usize>>,
        all_split_positions(0).collect::<Vec<Vec<usize>>>()
    );
    assert_eq!(
        vec![] as Vec<Vec<usize>>,
        all_split_positions(1).collect::<Vec<Vec<usize>>>()
    );
    assert_eq!(
        vec![vec![1]],
        all_split_positions(2).collect::<Vec<Vec<usize>>>()
    );
    assert_eq!(
        vec![vec![1], vec![2], vec![1, 2]],
        all_split_positions(3).collect::<Vec<Vec<usize>>>()
    );
    assert_eq!(
        vec![
            vec![1],
            vec![2],
            vec![3],
            vec![1, 2],
            vec![1, 3],
            vec![2, 3],
            vec![1, 2, 3]
        ],
        all_split_positions(4).collect::<Vec<Vec<usize>>>()
    );
}

pub fn all_string_splits<'a>(
    s: &'a str,
    positions: &'a [Vec<usize>],
) -> impl Iterator<Item = Vec<&'a str>> {
    // Assumes ascii tbh
    positions.iter().map(move |split| {
        let mut this_split: Vec<&str> = Vec::new();
        let mut cur = 0;
        for pos in split {
            this_split.push(&s[cur..*pos]);
            cur = *pos;
        }
        this_split.push(&s[cur..]);
        this_split
    })
}

// Euclid's algorithm
pub fn euclid_gcd(mut a: u32, mut b: u32) -> u32 {
    while b != 0 {
        let temp = b;
        b = a % b;
        a = temp;
    }
    a
}

// Binary GCD (Stein's Algorithm)
pub fn binary_gcd(mut a: u32, mut b: u32) -> u32 {
    if a == 0 {
        return b;
    }
    if b == 0 {
        return a;
    }

    // Finding common factors of 2
    let mut shift = 0;
    while ((a | b) & 1) == 0 {
        a >>= 1;
        b >>= 1;
        shift += 1;
    }

    while (a & 1) == 0 {
        a >>= 1;
    }

    while b != 0 {
        while (b & 1) == 0 {
            b >>= 1;
        }
        if a > b {
            std::mem::swap(&mut a, &mut b);
        }
        b -= a;
    }

    a << shift
}

pub fn lcm(a: u32, b: u32) -> u32 {
    let gcd = binary_gcd(a, b);
    if gcd == 0 {
        0
    } else {
        (a * b) / gcd
    }
}

#[test]
fn test_gcd() {
    assert_eq!(binary_gcd(48, 18), 6);
    assert_eq!(binary_gcd(54, 24), 6);
    assert_eq!(binary_gcd(101, 103), 1);
    assert_eq!(binary_gcd(0, 5), 5);
    assert_eq!(binary_gcd(10, 0), 10);
    assert_eq!(binary_gcd(0, 0), 0);
    assert_eq!(euclid_gcd(48, 18), 6);
    assert_eq!(euclid_gcd(54, 24), 6);
    assert_eq!(euclid_gcd(101, 103), 1);
    assert_eq!(euclid_gcd(0, 5), 5);
    assert_eq!(euclid_gcd(10, 0), 10);
    assert_eq!(euclid_gcd(0, 0), 0);
}

#[test]
fn test_lcm() {
    assert_eq!(lcm(48, 18), 144);
    assert_eq!(lcm(54, 24), 216);
    assert_eq!(lcm(101, 103), 10403);
    assert_eq!(lcm(0, 5), 0); // LCM with zero is typically defined as 0
    assert_eq!(lcm(10, 0), 0); // LCM with zero is typically defined as 0
    assert_eq!(lcm(0, 0), 0); // LCM with zero is typically defined as 0
}

pub fn n_choose_k(n: u64, k: u64) -> u64 {
    if k > n {
        panic!("k must be < n");
    } else if k == 0 || k == n {
        return 1;
    } else if k == 1 || k == n - 1 {
        return n;
    }

    let k = k.min(n - k); // symmetry

    // Could theoretically start result at n and iterate from 2..=k but this
    // maintains clarity
    let mut result = 1u64;
    for i in 1..=k {
        result = result * (n + 1 - i) / i;
    }

    result
}

#[test]
fn test_n_choose_k() {
    assert_eq!(1, n_choose_k(4, 0));
    assert_eq!(4, n_choose_k(4, 1));
    assert_eq!(6, n_choose_k(4, 2));
    assert_eq!(4, n_choose_k(4, 3));
    assert_eq!(1, n_choose_k(4, 4));
    assert_eq!(10, n_choose_k(5, 2));
    assert_eq!(15, n_choose_k(6, 2));
    assert_eq!(20, n_choose_k(6, 3));
    assert_eq!(15, n_choose_k(6, 4));
}
