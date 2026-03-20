//! Post-rewrite fixup pass: resolves ambiguous integer type errors (E0689) and
//! unstable-in-const errors (E0658) left behind by the rewriter.
//!
//! Algorithm:
//!   1. Run `cargo check --message-format=json` in the target project.
//!   2. Parse rustc JSON diagnostics.
//!   3. For E0689: find the "consider giving ... a type" suggestion span and
//!      insert `: TYPE` at the variable declaration.
//!   4. Repeat until no more E0689 errors (up to MAX_ITERATIONS).

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Run the fixup loop on the project at `project_dir`.
/// Passes `cargo_args` as extra arguments to `cargo check` (e.g. workspace flags).
pub fn fixup_loop(project_dir: &Path, cargo_args: &[String]) {
    const MAX_ITERATIONS: usize = 10;

    for iteration in 0..MAX_ITERATIONS {
        let diagnostics = run_cargo_check_json(project_dir, cargo_args);

        let e0689: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code.as_deref() == Some("E0689"))
            .collect();
        let e0658: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code.as_deref() == Some("E0658"))
            .collect();

        if e0689.is_empty() && e0658.is_empty() {
            if iteration > 0 {
                eprintln!("warn-rewrite fixup: clean after {} iteration(s)", iteration);
            }
            return;
        }

        // E0658 (unstable const trait) should no longer appear since we skip
        // const-context casts rather than rewriting them.  Report any remaining
        // ones as unexpected so the user can investigate.
        for diag in &e0658 {
            for span in &diag.spans {
                if span.is_primary {
                    eprintln!(
                        "warn-rewrite fixup: unexpected E0658 at {}:{}..{} — {}",
                        span.file_name, span.byte_start, span.byte_end, diag.message
                    );
                }
            }
        }

        eprintln!(
            "warn-rewrite fixup iteration {}: {} E0689, {} E0658 (reported only)",
            iteration + 1,
            e0689.len(),
            e0658.len()
        );

        let mut applied = false;
        applied |= fix_e0689(project_dir, &e0689);

        if !applied {
            eprintln!("warn-rewrite fixup: no fixes could be applied, stopping");
            return;
        }
    }

    eprintln!("warn-rewrite fixup: reached max iterations ({})", MAX_ITERATIONS);
}

// ── JSON diagnostic structures ──────────────────────────────────────────────

#[derive(Debug)]
struct Diagnostic {
    code: Option<String>,
    message: String,
    spans: Vec<DiagSpan>,
    children: Vec<Diagnostic>,
}

#[derive(Debug, Clone)]
struct DiagSpan {
    file_name: String,
    byte_start: u32,
    byte_end: u32,
    suggested_replacement: Option<String>,
    is_primary: bool,
}

// ── cargo check --message-format=json ───────────────────────────────────────

fn run_cargo_check_json(project_dir: &Path, extra_args: &[String]) -> Vec<Diagnostic> {
    let output = Command::new("cargo")
        .arg("check")
        .arg("--message-format=json")
        .args(extra_args)
        .current_dir(project_dir)
        .output()
        .expect("failed to run cargo check");

    // cargo outputs one JSON object per line on stdout
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut result = Vec::new();

    for line in stdout.lines() {
        let Ok(value) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        // Only process "compiler-message" events
        if value.get("reason").and_then(|r| r.as_str()) != Some("compiler-message") {
            continue;
        }
        let Some(msg) = value.get("message") else { continue };
        if let Some(diag) = parse_diagnostic(msg) {
            result.push(diag);
        }
    }

    result
}

fn parse_diagnostic(v: &serde_json::Value) -> Option<Diagnostic> {
    let code = v
        .get("code")
        .and_then(|c| c.get("code"))
        .and_then(|c| c.as_str())
        .map(str::to_owned);
    let message = v
        .get("message")
        .and_then(|m| m.as_str())
        .unwrap_or("")
        .to_owned();

    let spans = v
        .get("spans")
        .and_then(|s| s.as_array())
        .map(|arr| arr.iter().filter_map(parse_span).collect())
        .unwrap_or_default();

    let children = v
        .get("children")
        .and_then(|c| c.as_array())
        .map(|arr| arr.iter().filter_map(parse_diagnostic).collect())
        .unwrap_or_default();

    Some(Diagnostic { code, message, spans, children })
}

fn parse_span(v: &serde_json::Value) -> Option<DiagSpan> {
    let file_name = v.get("file_name")?.as_str()?.to_owned();
    let byte_start = v.get("byte_start")?.as_u64()? as u32;
    let byte_end = v.get("byte_end")?.as_u64()? as u32;
    let suggested_replacement = v
        .get("suggested_replacement")
        .and_then(|s| s.as_str())
        .map(str::to_owned);
    let is_primary = v.get("is_primary").and_then(|b| b.as_bool()).unwrap_or(false);

    Some(DiagSpan { file_name, byte_start, byte_end, suggested_replacement, is_primary })
}

// ── E0689 fix: insert `: TYPE` at the variable declaration ──────────────────
//
// rustc E0689 emits a child suggestion like:
//   "consider giving `x` a type: `usize`"
// with a span pointing at the pattern in the `let` statement (just the name).
// We insert `: TYPE` immediately after that span's end.

fn fix_e0689(project_dir: &Path, diags: &[&Diagnostic]) -> bool {
    // Collect all (file, byte_end, type_str) insertion points.
    // byte_end is where we insert `: TYPE`.
    let mut insertions: HashMap<PathBuf, Vec<(u32, String)>> = HashMap::new();

    for diag in diags {
        // Walk children for the "consider giving ... a type" suggestion
        if let Some((file, byte_end, ty)) = find_type_suggestion(project_dir, diag) {
            insertions.entry(file).or_default().push((byte_end, ty));
        }
    }

    if insertions.is_empty() {
        return false;
    }

    let mut applied = false;
    for (file, mut points) in insertions {
        if !file.exists() {
            continue;
        }
        let src = match std::fs::read(&file) {
            Ok(b) => b,
            Err(e) => { eprintln!("fixup: read {:?}: {}", file, e); continue; }
        };

        // Sort descending so insertions don't shift earlier offsets.
        points.sort_by(|a, b| b.0.cmp(&a.0));
        // Deduplicate same offset.
        points.dedup_by_key(|p| p.0);

        let mut result = src;
        for (offset, ty) in points {
            let pos = offset as usize;
            if pos > result.len() {
                eprintln!("fixup: offset {} out of range in {:?}", pos, file);
                continue;
            }
            // Check we're not double-annotating: look at bytes at pos
            // If there's already ': ' right after, skip.
            if result.get(pos..pos + 2) == Some(b": ") {
                continue;
            }
            let insertion = format!(": {}", ty);
            let mut new_result = Vec::with_capacity(result.len() + insertion.len());
            new_result.extend_from_slice(&result[..pos]);
            new_result.extend_from_slice(insertion.as_bytes());
            new_result.extend_from_slice(&result[pos..]);
            result = new_result;
            eprintln!("fixup E0689: {:?} +{} `{}`", file, offset, insertion);
            applied = true;
        }

        if let Err(e) = std::fs::write(&file, &result) {
            eprintln!("fixup: write {:?}: {}", file, e);
        }
    }
    applied
}

/// Search a diagnostic and its children for a "consider giving `X` a type" suggestion.
/// Returns (absolute_path, byte_end_of_pattern, type_string).
fn find_type_suggestion(
    project_dir: &Path,
    diag: &Diagnostic,
) -> Option<(PathBuf, u32, String)> {
    // rustc emits the suggestion as a child diagnostic with:
    //   message: "consider giving `X` a type"
    //   spans[0].suggested_replacement = ": TYPE"   (MachineApplicable suggestion)
    // OR as a child with:
    //   message: "consider giving `X` a type: `TYPE`"
    //   spans[0] pointing at the declaration (no suggested_replacement)
    //
    // We handle both forms.
    for child in &diag.children {
        let msg = &child.message;
        if !msg.contains("consider giving") || !msg.contains("a type") {
            continue;
        }

        // Try to extract type from suggested_replacement on a span
        for span in &child.spans {
            if let Some(repl) = &span.suggested_replacement {
                // suggested_replacement is e.g. `: usize` — strip leading `: `
                let ty = repl.trim_start_matches(": ").trim().to_owned();
                if !ty.is_empty() && is_valid_type_name(&ty) {
                    let path = resolve_path(project_dir, &span.file_name);
                    // Insert at byte_start (where the `: TYPE` should be added)
                    return Some((path, span.byte_start, ty));
                }
            }
        }

        // Fallback: parse type from message "consider giving `X` a type: `TYPE`"
        if let Some(ty) = extract_type_from_message(msg) {
            // Find the primary span in the *parent* diagnostic for the insertion point
            for span in &diag.spans {
                if span.is_primary {
                    // The primary span is at the call site; we need the declaration.
                    // Without a suggested_replacement span, we can't reliably locate it.
                    // Use child spans as fallback.
                    let _ = (span, &ty); // suppress unused warning
                }
            }
            // Try child spans
            for span in &child.spans {
                let path = resolve_path(project_dir, &span.file_name);
                return Some((path, span.byte_end, ty.clone()));
            }
        }
    }
    None
}

fn extract_type_from_message(msg: &str) -> Option<String> {
    // Pattern: "consider giving `X` a type: `TYPE`"
    let after_colon = msg.split(": `").nth(1)?;
    let ty = after_colon.trim_end_matches('`').trim().to_owned();
    if is_valid_type_name(&ty) { Some(ty) } else { None }
}

fn is_valid_type_name(s: &str) -> bool {
    !s.is_empty()
        && s.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | ':' | '<' | '>' | '&' | '[' | ']'))
}

fn resolve_path(project_dir: &Path, file_name: &str) -> PathBuf {
    let p = PathBuf::from(file_name);
    if p.is_absolute() {
        p
    } else {
        project_dir.join(p)
    }
}

