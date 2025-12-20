use euler::util::{n_choose_k, timeit};

// val:         20x20   137846528820
// seconds:     0.000001
fn main() {
    // Turns out this is just n_choose_k
    // 40 "steps" on the grid, 20 across, 20 down, and you have to choose
    // 20 positions at which to take a down step instead of across step
    let (result, seconds) = timeit(|| n_choose_k(40, 20));
    println!("// val:\t\t{}x{}\t{}", 20, 20, result);
    println!("// seconds:\t{}", seconds)
}
