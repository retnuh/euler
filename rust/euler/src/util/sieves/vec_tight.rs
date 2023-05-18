
pub struct VecTightSieve {
    cur: u64,
    primes: Vec<u64>,
}

impl VecTightSieve {
    pub fn new() -> VecTightSieve { VecTightSieve { cur: 3, primes: Vec::new() } }
}

impl Iterator for VecTightSieve {
    type Item = u64;

    #[inline]
    fn next(&mut self) -> Option<u64> {
        if self.primes.len() == 0 {
            self.primes.push(2);
            Some(self.primes[0])
        } else {
            loop {
                let is_prime: bool;
                'inner: loop {
                    for p in self.primes.iter() {
                        if self.cur % p == 0 {
                            is_prime = false;
                            break 'inner;
                        }
                    }
                    is_prime = true;
                    break 'inner
                }
                if !is_prime {
                    self.cur += 2;
                } else {
                    let p = self.cur;
                    self.cur += 2;
                    self.primes.push(p);
                    return Some(p);
                }
            }
        }
    }
}


#[test]
fn test_vec_tight_sieve() {
    assert_eq!(Some(2), VecTightSieve::new().nth(0));
    assert_eq!(Some(3), VecTightSieve::new().nth(1));
    assert_eq!(Some(5), VecTightSieve::new().nth(2));
    assert_eq!(Some(7), VecTightSieve::new().nth(3));
    assert_eq!(Some(11), VecTightSieve::new().nth(4));
    assert_eq!(Some(29), VecTightSieve::new().nth(9));
    assert_eq!(Some(104729), VecTightSieve::new().nth(9999));
}
