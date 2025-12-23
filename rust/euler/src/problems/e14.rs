use euler::util::timeit_duration;
use std::collections::HashMap;

type TheMap<T> = HashMap<T, T>;
fn e14(n: u64) -> (u64, TheMap<u64>) {
    fn collatz(x: u64, map: &mut TheMap<u64>) -> u64 {
        if let Some(&v) = map.get(&x) {
            return v;
        }
        let n = if x.is_multiple_of(2) {
            x / 2
        } else {
            3 * x + 1
        };
        let v = 1 + collatz(n, map);
        map.insert(x, v);
        v
    }
    let mut map = HashMap::new();
    map.insert(1, 1);
    map.insert(2, 2);

    let mut longest_term = 2;
    let mut longest_term_count = 2;
    for i in 3..n {
        let terms = collatz(i, &mut map);
        if terms > longest_term_count {
            longest_term = i;
            longest_term_count = terms;
        }
    }

    (longest_term, map)
}

// val:         1000000 837799
// seconds:     0.100887

fn main() {
    let n = 1_000_000;
    let (result, duration) = timeit_duration(|| e14(n));
    println!("// val:\t\t{}\t{}", n, result.0);
    println!("// seconds:\t{}", duration.as_secs_f32())
}

#[test]
fn test_e14() {
    let (longest_term, map) = e14(13);
    assert_eq!(9, longest_term);
    assert_eq!(20, map[&9]);
    assert_eq!(10, map[&13]);
    // This was BTreeMap but was "noticeably" slower
    // assert_eq!(52, *map.last_key_value().unwrap().0);
}
