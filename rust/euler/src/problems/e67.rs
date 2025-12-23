use euler::util::timeit_duration;

use euler::problems::e18;

static E67_DATA: &str = include_str!("resources/e67.txt");

// val:         7273
// nanoseconds: 78000
fn main() {
    let (result, duration) = timeit_duration(|| e18::maximum_path_sum(E67_DATA));
    println!("// val:\t\t{}", result);
    println!("// nanoseconds:\t{}", duration.as_nanos())
}
