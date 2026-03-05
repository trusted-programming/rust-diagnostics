//! Phase 0: remove crate-level `#![allow(...)]` suppressions that hide the
//! warnings we intend to rewrite.
//!
//! We strip lines that match any of:
//!   #![allow(clippy::arithmetic_side_effects)]
//!   #![allow(clippy::as_conversions)]
//!   #![allow(clippy::arithmetic_side_effects, clippy::as_conversions)]
//!   #![allow(clippy::as_conversions, clippy::arithmetic_side_effects)]
//!
//! Lines that contain other lints inside the same `allow(...)` are narrowed:
//! the two target lint identifiers are removed and the attribute is kept if
//! anything else remains.

use std::io::{self, Read, Write};
use std::path::Path;

/// The lint names we want to strip from `#![allow(...)]` attributes.
const TARGETS: &[&str] = &[
    "clippy::arithmetic_side_effects",
    "clippy::as_conversions",
];

/// Process a single file: remove or narrow any crate-level allow lines that
/// suppress our target lints.  Returns `true` if the file was modified.
fn strip_file(path: &Path) -> io::Result<bool> {
    let mut src = String::new();
    std::fs::File::open(path)?.read_to_string(&mut src)?;

    let new_src = strip_allows_from_source(&src);
    if new_src == src {
        return Ok(false);
    }

    std::fs::File::create(path)?.write_all(new_src.as_bytes())?;
    Ok(true)
}

/// Strip/narrow allow attributes from source text (pure function, easy to unit-test).
pub fn strip_allows_from_source(src: &str) -> String {
    src.lines()
        .map(|line| process_line(line))
        .filter_map(|opt| opt)
        .collect::<Vec<_>>()
        .join("\n")
        // Preserve trailing newline if original had one.
        + if src.ends_with('\n') { "\n" } else { "" }
}

/// Returns `Some(new_line)` to keep (possibly rewritten), or `None` to drop.
fn process_line(line: &str) -> Option<String> {
    let trimmed = line.trim();

    // Must start with `#![allow(` to be relevant.
    if !trimmed.starts_with("#![allow(") || !trimmed.ends_with(")]") {
        return Some(line.to_owned());
    }

    // Extract the contents between `#![allow(` and `)]`.
    let inner = &trimmed["#![allow(".len()..trimmed.len() - 2];

    // Split by comma, trim whitespace from each lint name.
    let lints: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();

    // Remove our target lints.
    let remaining: Vec<&str> = lints
        .iter()
        .copied()
        .filter(|&l| !TARGETS.contains(&l))
        .collect();

    if remaining.is_empty() {
        // The entire attribute only contained our targets — drop the line.
        return None;
    }

    if remaining.len() == lints.len() {
        // Nothing was removed.
        return Some(line.to_owned());
    }

    // Rebuild the attribute, preserving the original leading whitespace.
    let leading = &line[..line.len() - line.trim_start().len()];
    Some(format!("{}#![allow({})]", leading, remaining.join(", ")))
}

/// Walk `project_dir` recursively, strip crate-level allow lines from every
/// `.rs` file, and return the number of files modified.
pub fn strip_project(project_dir: &Path, lint: crate::LintTarget) -> usize {
    // Nothing to do if we're not touching a lint family.
    if !lint.includes_arithmetic() && !lint.includes_as_conversions() {
        return 0;
    }

    let mut modified = 0usize;
    for entry in walkdir::WalkDir::new(project_dir)
        .follow_links(false)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
    {
        let path = entry.path();

        // Only process Rust source files.
        if path.extension().and_then(|e| e.to_str()) != Some("rs") {
            continue;
        }

        // Skip generated / third-party directories.
        if crate::apply::is_third_party(path) {
            continue;
        }

        match strip_file(path) {
            Ok(true) => {
                modified += 1;
                println!("  stripped allows: {}", path.display());
            }
            Ok(false) => {}
            Err(e) => eprintln!("warn-rewrite: strip_allows: {}: {}", path.display(), e),
        }
    }
    modified
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_single_arithmetic() {
        let src = "#![allow(clippy::arithmetic_side_effects)]\nfn main() {}\n";
        let out = strip_allows_from_source(src);
        assert_eq!(out, "fn main() {}\n");
    }

    #[test]
    fn test_strip_single_as_conversions() {
        let src = "#![allow(clippy::as_conversions)]\nfn f() {}\n";
        let out = strip_allows_from_source(src);
        assert_eq!(out, "fn f() {}\n");
    }

    #[test]
    fn test_strip_combined() {
        let src =
            "#![allow(clippy::arithmetic_side_effects, clippy::as_conversions)]\nfn f() {}\n";
        let out = strip_allows_from_source(src);
        assert_eq!(out, "fn f() {}\n");
    }

    #[test]
    fn test_narrow_keeps_other_lints() {
        let src = "#![allow(dead_code, clippy::arithmetic_side_effects)]\nfn f() {}\n";
        let out = strip_allows_from_source(src);
        assert_eq!(out, "#![allow(dead_code)]\nfn f() {}\n");
    }

    #[test]
    fn test_unrelated_allow_untouched() {
        let src = "#![allow(dead_code)]\nfn f() {}\n";
        let out = strip_allows_from_source(src);
        assert_eq!(out, src);
    }

    #[test]
    fn test_no_trailing_newline() {
        let src = "#![allow(clippy::as_conversions)]";
        let out = strip_allows_from_source(src);
        assert_eq!(out, "");
    }

    #[test]
    fn test_preserves_leading_whitespace() {
        // Unusual but valid: indented inner-module attribute
        let src = "  #![allow(dead_code, clippy::as_conversions)]\nfn f() {}\n";
        let out = strip_allows_from_source(src);
        assert_eq!(out, "  #![allow(dead_code)]\nfn f() {}\n");
    }
}
