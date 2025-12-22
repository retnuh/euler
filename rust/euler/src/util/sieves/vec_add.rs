pub struct VecAddSieve {
    cur: u64,
    primes: Vec<(u64, u64)>,
}

impl Default for VecAddSieve {
    fn default() -> Self {
        Self::new()
    }
}

impl VecAddSieve {
    pub fn new() -> VecAddSieve {
        VecAddSieve {
            cur: 3,
            primes: Vec::new(),
        }
    }

    #[inline]
    fn check_cur_is_prime(&mut self) -> Option<u64> {
        for (number, next_multiple) in self.primes.iter_mut() {
            while self.cur > *next_multiple {
                *next_multiple += 2 * *number
            }
            if self.cur == *next_multiple {
                return None;
            }
        }
        Some(self.cur)
    }
}

impl Iterator for VecAddSieve {
    type Item = u64;

    #[inline]
    fn next(&mut self) -> Option<u64> {
        if self.primes.is_empty() {
            self.primes.push((2, 4));
            Some(self.primes[0].0)
        } else {
            loop {
                match self.check_cur_is_prime() {
                    None => {
                        self.cur += 2;
                    }
                    Some(p) => {
                        self.cur += 2;
                        self.primes.push((p, 3 * p));
                        return Some(p);
                    }
                }
            }
        }
    }
}

#[test]
fn test_vec_add_sieve() {
    assert_eq!(Some(2), VecAddSieve::new().next());
    assert_eq!(Some(3), VecAddSieve::new().nth(1));
    assert_eq!(Some(5), VecAddSieve::new().nth(2));
    assert_eq!(Some(7), VecAddSieve::new().nth(3));
    assert_eq!(Some(11), VecAddSieve::new().nth(4));
    assert_eq!(Some(29), VecAddSieve::new().nth(9));
    assert_eq!(Some(104729), VecAddSieve::new().nth(9999));
}
