pub struct VecSieve {
    cur: u64,
    primes: Vec<u64>,
}

impl VecSieve {
    pub fn new() -> VecSieve { VecSieve { cur: 3, primes: Vec::new() } }

    #[inline]
    fn check_cur_is_prime(&mut self) -> Option<u64> {
        for p in self.primes.iter() {
            if self.cur % p == 0 {
                return None;
            }
        }
        Some(self.cur)
    }
}

impl Iterator for VecSieve {
    type Item = u64;

    #[inline]
    fn next(&mut self) -> Option<u64> {
        if self.primes.len() == 0 {
            self.primes.push(2);
            Some(self.primes[0])
        } else {
            loop {
                match self.check_cur_is_prime() {
                    None => {
                        self.cur += 2;
                    }
                    Some(p) => {
                        self.cur += 2;
                        self.primes.push(p);
                        return Some(p);
                    }
                }
            }
        }
    }
}

#[test]
fn test_vec_sieve() {
    assert_eq!(Some(2), VecSieve::new().nth(0));
    assert_eq!(Some(29), VecSieve::new().nth(9));
    assert_eq!(Some(104729), VecSieve::new().nth(9999));
}
