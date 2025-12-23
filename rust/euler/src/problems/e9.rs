use num::ToPrimitive;

// Some algebra, substituting in c and solving for b yields
// b = 1000 * (500-a) / (1000-a)
// so we know a is 0 < a < 500
fn e9_find_a_b() -> (i64, i64) {
    for a in 1..500 {
        let af: f64 = a as f64;
        let b: f64 = 1000.0 * (500.0 - af) / (1000.0 - af);
        // println!("a = {}\tb = {}", a, b);
        if b.fract() == 0.0 {
            return (a, b.to_i64().unwrap());
        }
    }
    unreachable!("There should be a value found at this point")
}

fn e9() -> (i64, i64, i64) {
    let (a, b) = e9_find_a_b();
    (a, b, 1000 - a - b)
}

// a = 200,	b = 375,	c = 425
// abc = 31875000
// seconds:	0.000003
pub fn main() -> String {
    let (a, b, c) = e9();
    format!("a = {},\tb = {},\tc = {}\nabc = {}", a, b, c, a * b * c)
}
