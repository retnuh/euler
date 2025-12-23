// Include the generated build information
mod built_info {
    include!(concat!(env!("OUT_DIR"), "/built.rs"));
}

/// Returns a formatted string with Rust build metadata
pub fn build_info_string() -> String {
    let profile = built_info::PROFILE;
    let version = built_info::RUSTC_VERSION;

    format!("// rust info:\t|profile: {}|version: {}|", profile, version)
}
