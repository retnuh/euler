use euler::util::timeit_duration;
use imbl::{shared_ptr, GenericVector};
use num::integer::div_rem;
use std::collections::HashMap;

// WIP still not enough
// idea was to generate a matching sequence, but test shows that first ten of even the
// test sample is not small enough, and generating 100 is taking ages

type TheMap = HashMap<u64, (u64, char)>;

type MyVector<T> = GenericVector<T, shared_ptr::RcK>;

struct ModifiedCollatzGeneratorIter {
    queue: MyVector<MyVector<char>>,
    threshold: u64,
}

impl ModifiedCollatzGeneratorIter {
    fn compute_seq(&self, acc: u64, seq: &MyVector<char>) -> Option<u64> {
        let mut tot = acc;
        for c in seq {
            let next = match c {
                'D' => tot * 3,
                'U' => {
                    let x = tot * 3 - 2;
                    let (d, r) = div_rem(x, 4);
                    if r != 0 {
                        return None;
                    }
                    d
                }
                'd' => {
                    let x = tot * 3 + 1;
                    let (d, r) = div_rem(x, 2);
                    if r != 0 {
                        return None;
                    }
                    d
                }
                _ => panic!("Unreachable: {c}"),
            };
            if next < 2 {
                return None;
            }
            tot = next
        }
        Some(tot)
    }
}

impl ModifiedCollatzGeneratorIter {
    fn new(seq: &str, threshold: u64) -> Self {
        let base_sequence = seq.chars().fold(GenericVector::new(), |mut v, c| {
            v.push_front(c);
            v
        });
        let mut queue = GenericVector::new();
        queue.push_back(base_sequence);
        ModifiedCollatzGeneratorIter { queue, threshold }
    }
}

impl Iterator for ModifiedCollatzGeneratorIter {
    type Item = (u64, String);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let seq = self.queue.pop_front()?;
            for c in ['U', 'd', 'D'] {
                let mut ns = seq.clone();
                ns.push_front(c);
                self.queue.push_back(ns);
            }
            if let Some(n) = self.compute_seq(1, &seq) {
                if n >= self.threshold {
                    let s: String = seq.iter().rev().collect();
                    return Some((n, s));
                }
            }
        }
    }
}

fn e277_reverse(_threshold: u64, seq: &str) -> u64 {
    let _seq: Vec<char> = seq.chars().rev().collect();
    for i in 1..10 {
        for c in ['D', 'U', 'd'] {
            let next = match c {
                'D' => i * 3,
                'U' => (i * 3 - 2) / 4,
                'd' => (i * 3 + 1) / 2,
                _ => panic!("Unreachable: {c}"),
            };
            println!("{i}:\t{c}\t{next}");
        }
    }

    0
}

fn e277_brute(start: u64, seq: &str) -> (u64, TheMap) {
    let seq: Vec<char> = seq.chars().collect();
    fn modified_collatz(n: u64, _map: &mut TheMap) -> (u64, char) {
        // if let Some(&v) = map.get(&n) {
        //     return v;
        // }
        let next = match n % 3 {
            0 => (n / 3, 'D'),
            1 => ((4 * n).div_ceil(3), 'U'),
            2 => ((2 * n - 1) / 3, 'd'),
            _ => panic!("Unreachable"),
        };
        // map.insert(n, next);
        next
    }
    let mut map = HashMap::new();
    map.insert(1, (1, '.'));

    for x in (start + 1).. {
        let mut n = x;
        let mut i = 0;
        loop {
            let (next, s) = modified_collatz(n, &mut map);
            // println!("{n} {next} {s}");
            if seq[i] == s {
                i += 1;
                n = next;
            } else {
                break;
            }
            if i == seq.len() {
                return (x, map);
            }
        }
    }
    // Unreachable
    (0, map)
}

fn main() {
    e277_reverse(10, "DdDddUUdDD");

    // let n = 10_u64.pow(15);
    // let (result, duration) = timeit_duration(|| e277_brute(n, "UDDDUdddDDUDDddDdDddDDUDDdUUDd"));
    // println!("// val:\t\t10^15\t{}", result.0);
    // println!("// seconds:\t{}", duration.as_secs_f32())
}

#[test]
fn test_e277() {
    let ((first_term, _map), duration) =
        timeit_duration(|| e277_brute(10_u64.pow(6), "DdDddUUdDD"));
    assert_eq!(1004064, first_term);
    println!("// val:\t\t10^6\t{}", duration.as_micros());
}

#[test]
fn test_e277_generate() {
    assert_eq!(
        (231, "DdDddUUdDD".to_string()),
        ModifiedCollatzGeneratorIter::new("DdDddUUdDD", 1)
            .next()
            .unwrap()
    );
    let x: Vec<(u64, String)> = ModifiedCollatzGeneratorIter::new("DdDddUUdDD", 10_u64.pow(6))
        .take(100)
        .collect();
    println!("{x:#?}");
    assert_eq!(
        (1004064, "DdDddUUdDDDdUDUUUdDdUUDDDUdDD".to_string()),
        *x.iter().min().unwrap(),
    );
}
