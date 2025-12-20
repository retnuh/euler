use crate::debug_println;
use crate::util::sieves::VecAddSieve;
use num::integer::div_rem;

#[derive(PartialEq, Debug)]
pub enum NaturalNumber<T> {
    Zero,
    One,
    Prime(T),
    Composite { c: T, prime_divisor: T },
}

#[derive(Debug)]
pub struct NaturalNumbers {
    current_number: u64,
    primes: Vec<(u64, u64)>,
}

impl Default for NaturalNumbers {
    fn default() -> Self {
        Self::new_skip_one()
    }
}

impl NaturalNumbers {
    pub fn new() -> NaturalNumbers {
        NaturalNumbers {
            current_number: 1,
            primes: Vec::new(),
        }
    }

    pub fn new_skip_one() -> NaturalNumbers {
        NaturalNumbers {
            current_number: 2,
            primes: Vec::new(),
        }
    }

    #[inline]
    fn check_prime(&mut self, current: u64) -> NaturalNumber<u64> {
        for (prime, next_multiple) in self.primes.iter_mut() {
            debug_println!("\tchecking prime {current} {prime} {next_multiple}");
            while current > *next_multiple {
                *next_multiple += *prime
            }
            if current == *next_multiple {
                return NaturalNumber::Composite {
                    c: current,
                    prime_divisor: *prime,
                };
            }
        }
        NaturalNumber::Prime(current)
    }
}

impl Iterator for NaturalNumbers {
    type Item = NaturalNumber<u64>;

    #[inline]
    fn next(&mut self) -> Option<NaturalNumber<u64>> {
        let current = self.current_number;
        self.current_number += 1;
        if current == 1 {
            return Some(NaturalNumber::One);
        } else if current == 2 {
            self.primes.push((2, 4));
            Some(NaturalNumber::Prime(2))
        } else {
            let r = self.check_prime(current);
            debug_println!("check_prime({current}) == {r:?}");
            match r {
                comp @ NaturalNumber::Composite { .. } => Some(comp),
                prime @ NaturalNumber::Prime(p) => {
                    self.primes.push((p, 3 * p));
                    return Some(prime);
                }
                NaturalNumber::Zero | NaturalNumber::One => {
                    panic!("Zero|One returned at impossible time")
                }
            }
        }
    }
}

#[test]
fn test_natural_numbers() {
    let mut nc = NaturalNumbers::new();
    assert_eq!(NaturalNumber::One, nc.next().unwrap());
    assert_eq!(NaturalNumber::Prime(2), nc.next().unwrap());
    assert_eq!(NaturalNumber::Prime(3), nc.next().unwrap());
    assert_eq!(
        NaturalNumber::Composite {
            c: 4,
            prime_divisor: 2
        },
        nc.next().unwrap()
    );
    assert_eq!(NaturalNumber::Prime(5), nc.next().unwrap());
    assert_eq!(
        NaturalNumber::Composite {
            c: 6,
            prime_divisor: 2
        },
        nc.next().unwrap()
    );
    assert_eq!(NaturalNumber::Prime(7), nc.next().unwrap());
    assert_eq!(
        NaturalNumber::Composite {
            c: 8,
            prime_divisor: 2
        },
        nc.next().unwrap()
    );
    assert_eq!(
        NaturalNumber::Composite {
            c: 9,
            prime_divisor: 3
        },
        nc.next().unwrap()
    );
}

pub type Factors = Vec<(u64, usize)>;

pub struct FactorsIter {
    natural_numbers: NaturalNumbers,
}

impl FactorsIter {
    pub fn default() -> FactorsIter {
        FactorsIter {
            natural_numbers: NaturalNumbers::new_skip_one(),
        }
    }

    pub fn factors(&self, n: u64, first_prime_divisor: u64) -> Factors {
        factors_using_source(
            n,
            self.natural_numbers
                .primes
                .iter()
                .map(|(prime, _)| *prime)
                .skip_while(|p| *p < first_prime_divisor),
        )
    }
}

impl Iterator for FactorsIter {
    type Item = (u64, Factors);

    fn next(&mut self) -> Option<Self::Item> {
        let nat = self.natural_numbers.next()?;
        match nat {
            NaturalNumber::Composite { c, prime_divisor } => {
                Some((c, self.factors(c, prime_divisor)))
            }
            NaturalNumber::Prime(p) => Some((p, vec![(p, 1)])),
            _ => {
                panic!("Not expecting Zero|One here")
            }
        }
    }
}

#[test]
fn test_factors_iter() {
    let mut fi = FactorsIter::default();
    assert_eq!((2, vec![(2, 1)]), fi.next().unwrap());
    assert_eq!((3, vec![(3, 1)]), fi.next().unwrap());
    assert_eq!((4, vec![(2, 2)]), fi.next().unwrap());
    assert_eq!((5, vec![(5, 1)]), fi.next().unwrap());
    assert_eq!((6, vec![(2, 1), (3, 1)]), fi.next().unwrap());
    assert_eq!((7, vec![(7, 1)]), fi.next().unwrap());
    assert_eq!((8, vec![(2, 3)]), fi.next().unwrap());
    assert_eq!((9, vec![(3, 2)]), fi.next().unwrap());
    assert_eq!((10, vec![(2, 1), (5, 1)]), fi.next().unwrap());
}

pub fn factors_using_source(n: u64, mut primes: impl Iterator<Item = u64>) -> Factors {
    let mut prime = primes.next().unwrap();
    let mut vec = Vec::new();
    let mut cur = n;
    let mut count = 0;
    while cur != 1 {
        let (div, rem) = div_rem(cur, prime);
        if rem != 0 {
            if count > 0 {
                vec.push((prime, count));
                count = 0;
            }
            prime = primes.next().unwrap();
            continue;
        }
        count += 1;
        cur = div;
    }
    if count > 0 {
        vec.push((prime, count));
    }
    vec
}

pub fn factors(n: u64) -> Factors {
    factors_using_source(n, &mut VecAddSieve::new())
}

#[test]
fn test_factors() {
    assert_eq!(vec![(3_u64, 1_usize), (5, 2)], factors(75));
    assert_eq!(vec![(2_u64, 2_usize), (7, 1)], factors(28));
    assert_eq!(vec![(53, 1)], factors(53));
}

pub fn divisor_count(n: u64) -> u64 {
    factors(n)
        .iter()
        .fold(1, |acc, (_, count)| acc * (*count as u64 + 1))
}

#[test]
fn test_divisor_count() {
    assert_eq!(4_u64, divisor_count(6));
    assert_eq!(6_u64, divisor_count(28));
}
