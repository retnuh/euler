// use memoize::memoize;
// use std::cmp::min;

// use cached::proc_macro::cached;
use num::integer::cbrt;
use num::{pow, Integer};

use euler::util::timeit;

fn cubes(largest: u64) -> Vec<u64> {
    let mut numbers: Vec<u64> = Vec::with_capacity(largest as usize);
    for i in 1..=largest {
        numbers.push(i * i * i)
    }
    numbers
}

fn s(upper: u64, upto: usize, cubes: &Vec<u64>) -> u64 {
    (1..upper).fold(0, |acc, x| acc + d(x, cubes, upto))
}

fn e884(upper: u64) -> u64 {
    let highest_cube_root = cbrt(upper - 1);
    let cubes = cubes(highest_cube_root);
    let highest_cube = cubes[(highest_cube_root - 1) as usize];
    let repeated_sum = s(highest_cube + 1, highest_cube_root as usize, &cubes);
    let (div, rem) = (upper - 1).div_rem(&highest_cube);
    let partial_sum = s(rem + 1, highest_cube_root as usize, &cubes);
    let repeat_count = div * (div + 1) / 2;
    // don't over count the actual cubes!  hmm do need to account for final prime subtraction on remainder though
    let repeats = repeated_sum * repeat_count;
    let remainder_highest_cube_subtractions = div * rem;
    let cube_adjustment = repeat_count * cubes.len() as u64;
    repeats + remainder_highest_cube_subtractions + partial_sum - cube_adjustment
}

fn d(initial_n: u64, cubes: &Vec<u64>, largest: usize) -> u64 {
    assert!(initial_n > 0);
    if initial_n <= 7 {
        return initial_n;
    }
    // only do this if initial_n > cubes[largest] ?
    let (div, rem) = initial_n.div_rem(&cubes[largest - 1]);
    let mut steps = div;
    let n = rem;
    if n > 0 {
        steps += 1;
        match &cubes[0..largest].binary_search(&n) {
            Ok(_) => {}
            Err(index) => {
                assert!(*index > 0);
                let nearest_cube = &cubes[*index - 1];
                steps += d(rem - *nearest_cube, cubes, *index + 1)
            }
        }
    }
    // println!("d({}):\t{}", initial_n, steps);
    steps
}

// T(1000000000000):		128088830547982
// seconds:	2.272181
fn main() {
    let n = pow(10, 17);
    let (result, seconds) = timeit(|| e884(n));
    println!("// S({}):\t{}", n, result);
    println!("// seconds:\t{}", seconds)
}

#[test]
fn test_e884() {
    let cubes = cubes(4);
    // assert_eq!(4, d(100, &cubes, 4));
    // assert_eq!(5, d(101, &cubes, 4));
    // assert_eq!(6, d(109, &cubes, 4));
    // assert_eq!(8, d(34, &cubes, 4));
    // assert_eq!(1, d(8, &cubes, 4));
    // assert_eq!(2, d(35, &cubes, 4));
    // assert_eq!(1, d(27, &cubes, 4));
    // assert_eq!(1, d(64, &cubes, 4));
    // assert_eq!(512, s(100, 4, &cubes));
    let s_val = s(26, 4, &cubes);
    println!("s(26):\t{}", s_val);
    let e_val = e884(26);
    println!("e884(26):\t{}", e_val);
    assert_eq!(512, e884(100));
    assert_eq!(s_val, e_val);
}
