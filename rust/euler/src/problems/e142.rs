#![allow(dead_code)]

use itertools::Itertools;
// use num::ToPrimitive;

fn is_perfect_square(n: u64) -> bool {
    if n < 2 {
        return true;
    }
    let sqrt_n = (n as f64).sqrt() as u64;
    sqrt_n * sqrt_n == n
}

// Some algebra on the equations gives us this, where a-f are squares:
// 2x = a + b
// 2x = c + d
// 2z = c - d
// 2z = e - f
// a + b = c + d
// c - d = e - f
// a + b = d + e - f
fn constraints(a: u64, b: u64, c: u64, d: u64, e: u64, f: u64) -> bool {
    a + b == c + d && c - d == e - f && a + b == d + e - f
}

fn e142() -> (u64, u64, u64) {
    let mut x = 6;
    let mut squares: Vec<u64> = vec![1, 4, 9, 16, 25];
    loop {
        let a = x * x;
        let res = squares
            .iter()
            .permutations(5)
            .find(|v| constraints(a, *v[0], *v[1], *v[2], *v[3], *v[4]));
        if let Some(v) = res {
            let x = (a + v[0]) / 2;
            let z = (v[3] - v[4]) / 2;
            let y = a - x;
            return (x, y, z);
        }
        squares.push(x);
        x += 1;
    }
}

// fn e142_brute() -> u64 {
//     let mut x: u64 = 2;
//     'outer: loop {
//         x += 1;
//         for y in 2..x {
//             for z in 1..y {
//                 if !is_perfect_square(x + y) {
//                     continue 'outer;
//                 }
//                 if !is_perfect_square(x - y) {
//                     continue 'outer;
//                 }
//                 if !is_perfect_square(x + z) {
//                     continue 'outer;
//                 }
//                 if !is_perfect_square(x - z) {
//                     continue 'outer;
//                 }
//                 if !is_perfect_square(y + z) {
//                     continue 'outer;
//                 }
//                 if !is_perfect_square(y - z) {
//                     continue 'outer;
//                 }
//                 return x + y + z;
//             }
//         }
//     }
// }

pub fn main() -> String {
    let (x, y, z) = e142();
    format!("ans = {}\nx: {}\t, y: {}\tz: {}", x + y + z, x, y, z)
}
