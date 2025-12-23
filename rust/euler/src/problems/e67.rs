use super::e18;

static E67_DATA: &str = include_str!("resources/e67.txt");

// val:         7273
// nanoseconds: 78000
pub fn main() -> String {
    let result = e18::maximum_path_sum(E67_DATA);
    format!("val:\t\t{}", result)
}
