use crate::util::n_choose_k;

// val:         20x20   137846528820
// seconds:     0.000001
pub fn main() -> String {
    // Turns out this is just n_choose_k
    // 40 "steps" on the grid, 20 across, 20 down, and you have to choose
    // 20 positions at which to take a down step instead of across step
    let result = n_choose_k(40, 20);
    format!("val:\t\t{}x{}\t{}", 20, 20, result)
}
