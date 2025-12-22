use euler::util::sieves::VecTightSieve;

fn main() {
    let mut sieve = VecTightSieve::new();
    let s = &mut sieve;
    for (i, p) in s.take(10).enumerate() {
        println!("prime {}: {}", i, p)
    }
    println!("last: {:?}", sieve.next());
    #[allow(clippy::disallowed_names)]
    let mut foo = 0..100;
    println!("foo 5: {:?}", foo.nth(5));
    println!("foo 10: {:?}", foo.nth(10));
    println!("foo 50: {:?}", foo.nth(50));
}
