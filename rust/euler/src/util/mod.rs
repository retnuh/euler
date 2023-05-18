use num::Integer;

pub mod sieves;

pub fn fib(n: u32) -> u64 {
    let n: u64 = n as u64;
    match n {
        0 => 0,
        1 => 1,
        _ => (2u64..=n).fold((0,1), |(a,b), _| (b, a+b)).1
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
    FibIter(0,1)
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
    assert_eq!(vec![0,1,1,2,3,5,8,13,21], fib_iter().take(9).collect::<Vec<u64>>());
    assert_eq!(( 8).div_rem( &3), ( 2,  2));

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
