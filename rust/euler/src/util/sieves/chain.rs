use std::mem::replace;

#[derive(Debug)]
pub struct ChainSieve {
    bootstrap: bool,
    chain: ChainLink,
}

impl ChainSieve {
    pub fn new() -> ChainSieve {
        ChainSieve {
            bootstrap: true,
            chain: ChainLink::bottom(),
        }
    }
}

#[derive(Debug)]
enum ChainLink {
    Link { p: u64, nm: u64, link: Box<ChainLink> },
    Bottom { val: u64 },
    Dummy,
}

impl ChainLink {
    fn bottom() -> ChainLink {
        ChainLink::Bottom { val: 1 }
    }

    fn next(&mut self) -> Option<u64> {
        // println!("next called: {:?}", self);

        match self {
            ChainLink::Link { p, nm, link } => {
                loop {
                    let v = link.next();
                    // println!("next {}: {:?}", p, v);
                    match v {
                        Some(v) => {
                            while v > *nm {
                                *nm += 2 * *p
                            }
                            if v != *nm {
                                return Some(v);
                            }
                        }
                        None => return None
                    }
                }
            }
            ChainLink::Bottom { val } => {
                *val += 2;
                Some(*val)
            }
            ChainLink::Dummy => unreachable!()
        }
    }

    fn link(p: u64, link: ChainLink) -> ChainLink {
        ChainLink::Link { p, nm: 3 * p, link: Box::new(link) }
    }
}


impl Iterator for ChainSieve {
    type Item = u64;

    #[inline]
    fn next(&mut self) -> Option<u64> {
        // println!("sieve next called: {:?}", self);
        if self.bootstrap {
            self.bootstrap = false;
            Some(2)
        } else {
            match self.chain.next() {
                Some(p) => {
                    let old = replace(&mut self.chain, ChainLink::Dummy);
                    self.chain = ChainLink::link(p, old);
                    Some(p)
                }
                None => None
            }
        }
    }
}


#[test]
fn test_chain_sieve() {
    assert_eq!(Some(2), ChainSieve::new().nth(0));
    assert_eq!(Some(3), ChainSieve::new().nth(1));
    assert_eq!(Some(5), ChainSieve::new().nth(2));
    assert_eq!(Some(7), ChainSieve::new().nth(3));
    assert_eq!(Some(11), ChainSieve::new().nth(4));
    assert_eq!(Some(29), ChainSieve::new().nth(9));
    // huh causes stack overflow when running coverage
    // assert_eq!(Some(104729), ChainSieve::new().nth(9999));
}
