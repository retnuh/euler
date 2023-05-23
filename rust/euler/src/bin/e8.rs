#![feature(test)]
extern crate test;

use euler::util::timeit;

const THE_1000_DIGIT_NUMBER: &str = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450";

fn the_1000_digits() -> Vec<i8> {
    let mut v: Vec<i8> = Vec::new();
    let bytes = THE_1000_DIGIT_NUMBER.as_bytes();
    for b in bytes {
        v.push((b - b'0') as i8);
    }
    v
}

fn e8(window_size: usize) -> i64 {
    let mut best: i64 = 0;
    let the_digits = the_1000_digits();
    let candidates = the_digits
        .split(|x| *x == 0)
        .filter(|s| s.len() >= window_size);
    for candidate in candidates {
        let mut i = 0;
        while i + window_size <= candidate.len() {
            let p = (candidate[i..(i + window_size)])
                .iter()
                .fold(1_i64, |r, &x| r * x as i64);
            if p > best {
                println!(
                    "Found better: {:?}\t{}\t{}",
                    &candidate[i..(i + window_size)],
                    p,
                    best
                );
                best = p;
            }
            i += 1
        }
    }
    best
}

// max product of adjacent of size:		13	23514624000
// seconds:	0.000267
fn main() {
    let input = 13;
    let (result, seconds) = timeit(|| e8(input));
    println!(
        "// max product of adjacent of size:\t\t{}\t{}",
        input, result
    );
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e8() {
    assert_eq!(5832, e8(4));
}
