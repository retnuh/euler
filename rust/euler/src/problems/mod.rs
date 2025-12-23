use crate::build_info::build_info_string;
use crate::util::timeit_duration;
use std::time::Duration;

macro_rules! register_problems {
    ($($module:ident),* $(,)?) => {
        $(pub mod $module;)*

        pub static PROBLEMS: &[(&str, fn() -> String)] = &[
            $((stringify!($module), $module::main)),*
        ];
    };
}

register_problems! {
    e1,
    e2,
    e3,
    e4,
    e5,
    e6,
    e7,
    e8,
    e9,
    e10,
    e12,
    e13,
    e14,
    e15,
    e16,
    e18,
    e48,
    e67,
    e108,
    e110,
    e142,
    e277,
    e684,
    e700,
    e719,
    e884,
}

fn run_problem_harness(problem_fn: fn() -> String) -> Duration {
    let build_info = build_info_string();
    let (result, duration) = timeit_duration(problem_fn);
    let duration_str = format!("duration:{:?}", duration);

    // Collect all lines (build info, result lines, duration)
    let mut all_lines = vec![build_info];
    all_lines.extend(result.lines().map(String::from));
    all_lines.push(duration_str);

    // Parse lines to find key-value pairs and compute max key length
    let mut pairs: Vec<(String, String)> = Vec::new();
    let mut max_key_len = 0;

    for line in &all_lines {
        // Remove "// " prefix if present for parsing
        let trimmed = line.strip_prefix("// ").unwrap_or(line);

        // Split on first colon and strip all whitespace between key and value
        if let Some(colon_pos) = trimmed.find(':') {
            let key = trimmed[..colon_pos].trim();
            let value = trimmed[colon_pos + 1..].trim_start();
            max_key_len = max_key_len.max(key.len());
            pairs.push((key.to_string(), value.to_string()));
        }
    }

    // Calculate target column for values (accounting for "// " prefix)
    const TAB_WIDTH: usize = 8;
    let prefix_len = 3; // "// "
    let max_key_column = prefix_len + max_key_len + 1; // +1 for the ':'
    let target_column = max_key_column.div_ceil(TAB_WIDTH) * TAB_WIDTH;

    // Print aligned output
    for (key, value) in pairs {
        let current_column = prefix_len + key.len() + 1; // "// " + key + ":"
        let tabs_needed = (target_column - current_column).div_ceil(TAB_WIDTH);
        let tabs = "\t".repeat(tabs_needed.max(1));
        println!("// {}:{}{}", key, tabs, value);
    }
    println!();
    duration
}

pub fn run_problem(name: &str) -> Result<Duration, String> {
    println!();
    if name == "all" {
        let mut total_duration = Duration::ZERO;
        for (problem_name, problem_fn) in PROBLEMS {
            println!("Running {}:", problem_name);
            println!();
            let duration = run_problem_harness(*problem_fn);
            total_duration += duration;
        }
        println!("// Total duration:\t{:?}", total_duration);
        Ok(total_duration)
    } else {
        match PROBLEMS.iter().find(|(n, _)| *n == name) {
            Some((_, problem_fn)) => {
                let duration = run_problem_harness(*problem_fn);
                Ok(duration)
            }
            None => Err(format!("Problem '{}' not found", name)),
        }
    }
}
