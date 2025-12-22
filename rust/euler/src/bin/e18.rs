use euler::util::timeit_duration;

use euler::problems::e18;

static E18_DATA: &str = include_str!("resources/e18.txt");

// val:         1074
// nanoseconds: 52000
fn main() {
    let (result, duration) = timeit_duration(|| e18::maximum_path_sum(E18_DATA));
    println!("// val:\t\t{}", result);
    println!("// nanoseconds:\t{}", duration.as_nanos())
}
