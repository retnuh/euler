use std::env;
use std::process;

use euler::problems::run_problem;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <problem>", args[0]);
        eprintln!("Example: {} e1  or  {} 1", args[0], args[0]);
        process::exit(1);
    }

    let mut problem = args[1].clone();

    // If the argument doesn't start with 'e', prepend it (unless it's "all")
    if !problem.starts_with('e') && problem != "all" {
        problem = format!("e{}", problem);
    }

    match run_problem(&problem) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("{}", err);
            process::exit(1);
        }
    }
}
