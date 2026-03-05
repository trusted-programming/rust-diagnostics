//! Apply collected rewrites to source files.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::{Rewrite, RewriteKind};

/// Apply all rewrites, grouped by file, in reverse byte-offset order.
/// Skips third-party crate files (registry, toolchain paths).
pub fn apply_rewrites(rewrites: Vec<Rewrite>) {
    let mut by_file: HashMap<PathBuf, Vec<Rewrite>> = HashMap::new();
    for r in rewrites {
        // Skip registry and toolchain files (absolute paths containing these components)
        if is_third_party(&r.file) {
            continue;
        }
        by_file.entry(r.file.clone()).or_default().push(r);
    }

    for (file, mut file_rewrites) in by_file {
        if !file.exists() {
            eprintln!("warn-rewrite: file not found: {:?}", file);
            continue;
        }

        let src = match std::fs::read(&file) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("warn-rewrite: read error {:?}: {}", file, e);
                continue;
            }
        };

        // Sort descending by full_start so reverse-order application keeps offsets valid.
        // Break ties by full_end descending (process outer expressions before inner).
        file_rewrites.sort_by(|a, b| {
            b.full_start.cmp(&a.full_start)
                .then(b.full_end.cmp(&a.full_end))
        });

        // Deduplicate overlapping rewrites: if two rewrites overlap, keep only the
        // one with the larger span (outer expression).
        let mut deduped: Vec<Rewrite> = Vec::new();
        for rw in file_rewrites {
            let overlaps = deduped.iter().any(|prev| {
                rw.full_start < prev.full_end && rw.full_end > prev.full_start
            });
            if !overlaps {
                deduped.push(rw);
            }
        }

        let mut result = src;
        for rw in deduped {
            let start = rw.full_start as usize;
            let end = rw.full_end as usize;

            // AllowConstCast is a pure insertion: start == end, no bytes replaced.
            let is_insertion = start == end;

            if !is_insertion && (end > result.len() || start > end) {
                eprintln!(
                    "warn-rewrite: offset out of range {}..{} in {:?} (len {})",
                    start, end, file, result.len()
                );
                continue;
            }
            if is_insertion && start > result.len() {
                eprintln!(
                    "warn-rewrite: insertion offset {} out of range in {:?} (len {})",
                    start, file, result.len()
                );
                continue;
            }

            let replacement = generate_replacement(&rw);
            let replacement_bytes = replacement.as_bytes();

            let mut new_result = Vec::with_capacity(result.len() + replacement_bytes.len());
            new_result.extend_from_slice(&result[..start]);
            new_result.extend_from_slice(replacement_bytes);
            new_result.extend_from_slice(&result[end..]);
            result = new_result;
        }

        if let Err(e) = std::fs::write(&file, &result) {
            eprintln!("warn-rewrite: write error {:?}: {}", file, e);
        }
    }
}

pub fn is_third_party(path: &std::path::Path) -> bool {
    path.components().any(|c| {
        matches!(c, std::path::Component::Normal(s) if
            s == ".cargo" || s == "registry" || s == "rustup" || s == "toolchains"
        )
    })
}

fn generate_replacement(rw: &Rewrite) -> String {
    match &rw.kind {
        RewriteKind::TypeFrom { dst } => {
            format!("{}::from({})", dst, rw.inner_snippet)
        }
        RewriteKind::TryFrom { dst } => {
            format!("{}::try_from({}).unwrap_or_default()", dst, rw.inner_snippet)
        }
        RewriteKind::CharFrom => {
            format!("char::from({})", rw.inner_snippet)
        }
        RewriteKind::WrappingBinop { method, lhs_ty } => {
            let rhs = rw.rhs_snippet.as_deref().unwrap_or("_rhs_");
            let lhs = &rw.inner_snippet;
            let lhs_wrapped = if needs_parens(lhs) {
                format!("({})", lhs)
            } else if is_bare_integer_literal(lhs) {
                // Integer literals need a type suffix to avoid E0689 (ambiguous type)
                format!("{}_{}", lhs, lhs_ty)
            } else {
                lhs.clone()
            };
            // Also annotate the RHS if it's a bare integer literal, to help type inference
            let rhs_annotated = if is_bare_integer_literal(rhs) {
                format!("{}_{}", rhs, lhs_ty)
            } else {
                rhs.to_string()
            };
            format!("{}.{}({})", lhs_wrapped, method, rhs_annotated)
        }
        RewriteKind::WrappingAssignOp { method, lhs_snippet } => {
            let rhs = rw.rhs_snippet.as_deref().unwrap_or("_rhs_");
            // The receiver (second occurrence of lhs) needs parens if it has a unary prefix
            let receiver = if needs_parens(lhs_snippet) {
                format!("({})", lhs_snippet)
            } else {
                lhs_snippet.clone()
            };
            format!("{} = {}.{}({})", lhs_snippet, receiver, method, rhs)
        }
        RewriteKind::WrappingNeg { operand_ty } => {
            let operand = &rw.inner_snippet;
            let operand_wrapped = if needs_parens(operand) {
                format!("({})", operand)
            } else if is_bare_integer_literal(operand) {
                format!("{}_{}", operand, operand_ty)
            } else {
                operand.clone()
            };
            format!("{}.wrapping_neg()", operand_wrapped)
        }
        RewriteKind::AllowConstCast => {
            // Prepend the allow attribute on its own line before the item.
            "#[allow(clippy::as_conversions)]\n".to_owned()
        }
    }
}

/// Heuristic: does this expression snippet need parentheses when used as a method receiver?
/// Wraps expressions that contain spaces (indicating a compound expression), start with
/// unary operators, or start with keywords, to avoid precedence issues like
/// `*ptr.wrapping_add(n)` (should be `(*ptr).wrapping_add(n)`).
fn needs_parens(s: &str) -> bool {
    let s = s.trim();
    // Bare integer literals: handled separately via is_bare_integer_literal (no parens needed)
    if is_bare_integer_literal(s) {
        return false;
    }
    // Unary prefix operators (* & ! -): need parens to avoid wrong parse
    if s.starts_with('*') || s.starts_with('&') || s.starts_with('!') || s.starts_with('-') {
        return true;
    }
    // Simple identifiers, array indexing, field access, function calls: no parens needed
    if s.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | '[' | ']' | '.' | '(' | ')')) {
        return false;
    }
    // Contains spaces (binary op, cast, or other compound): needs parens
    s.contains(' ')
}

/// Returns true if the snippet is a bare integer literal without a type suffix.
/// E.g. "1", "365", "2000" — but not "1_usize", "1u32", or "CONST_NAME".
fn is_bare_integer_literal(s: &str) -> bool {
    let s = s.trim();
    // Must be all digits (possibly with _ separators but no letters)
    if s.is_empty() {
        return false;
    }
    // Check for typed literal: e.g. "1u8", "365i64" — already has suffix
    // Pattern: digits, then optional underscore, then alpha suffix
    let bytes = s.as_bytes();
    let mut all_digits_underscores = true;
    for &b in bytes {
        if !b.is_ascii_digit() && b != b'_' {
            all_digits_underscores = false;
            break;
        }
    }
    if !all_digits_underscores {
        return false; // has letters, either already typed or an identifier
    }
    // Confirm it starts with a digit
    bytes[0].is_ascii_digit()
}
