macro_rules! register_problems {
    ($($module:ident),* $(,)?) => {
        $(pub mod $module;)*

        pub static PROBLEMS: &[(&str, fn())] = &[
            $((stringify!($module), $module::main)),*
        ];
    };
}

register_problems! {
    e18,
    e684,
}
