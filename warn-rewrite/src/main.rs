//! warn-rewrite: type-aware rewriter for clippy::arithmetic_side_effects
//! and clippy::as_conversions.
//!
//! Usage (single crate, acting as a rustc wrapper):
//!   RUSTC_WRAPPER=warn-rewrite cargo check
//!
//! Or directly:
//!   warn-rewrite --lint all --folder /path/to/project

#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;

mod apply;
mod callbacks;
mod classify;
mod fixup;
mod strip_allows;
mod visitor;

use std::path::PathBuf;
use std::process;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LintTarget {
    ArithmeticSideEffects,
    AsConversions,
    All,
}

impl LintTarget {
    pub fn includes_arithmetic(self) -> bool {
        matches!(self, LintTarget::ArithmeticSideEffects | LintTarget::All)
    }
    pub fn includes_as_conversions(self) -> bool {
        matches!(self, LintTarget::AsConversions | LintTarget::All)
    }
}

#[derive(Clone, Debug)]
pub struct Rewrite {
    /// Absolute path to the source file
    pub file: PathBuf,
    /// Byte offset of the start of the full expression to replace
    pub full_start: u32,
    /// Byte offset of the end of the full expression to replace (exclusive)
    pub full_end: u32,
    /// Source text snippet of the inner (lhs) expression
    pub inner_snippet: String,
    /// Source text snippet of the rhs expression (for binary ops)
    pub rhs_snippet: Option<String>,
    /// The kind of rewrite to apply
    pub kind: RewriteKind,
}

#[derive(Clone, Debug)]
pub enum RewriteKind {
    /// `x as T` → `T::from(x)` (lossless widening)
    TypeFrom { dst: String },
    /// Narrowing cast `x as T` — cannot be safely rewritten automatically.
    /// `.unwrap_or_default()` silently changes semantics (returns 0 instead of
    /// truncating).  Collected only so the driver can report for manual review.
    SkippedNarrowingCast { file_display: String, dst: String },
    /// Classification-only: narrowing cast detected by classify_cast.
    /// Converted to `SkippedNarrowingCast` (with location) by the visitor.
    TryFrom { dst: String },
    /// `x as char` → `char::from(x)` (u8 only)
    CharFrom,
    /// `a op b` → `a.method(b)` (wrapping arithmetic)
    WrappingBinop { method: &'static str, lhs_ty: String },
    /// `a op= b` → `a = a.method(b)` (wrapping compound assignment)
    WrappingAssignOp { method: &'static str, lhs_snippet: String },
    /// `-x` → `x.wrapping_neg()`
    WrappingNeg { operand_ty: String },
    /// Placeholder for a const-context `as` cast that cannot be rewritten because
    /// `From`/`TryFrom` are not const-stable.  Never applied — collected only so
    /// the driver can report these sites to the user for manual review.
    SkippedConstCast { file_display: String, reason: String },
}

fn main() {
    // Determine invocation mode:
    // 1. RUSTC_WRAPPER mode: first arg is the path to rustc
    // 2. Direct --folder mode
    let args: Vec<String> = std::env::args().collect();

    if args.len() >= 2 && (args[1].contains("rustc") || args[1].ends_with("rustc")) {
        // RUSTC_WRAPPER mode — pass through to rustc but intercept after analysis
        run_as_rustc_wrapper(&args[1..]);
    } else {
        // Direct mode: parse --lint and --folder flags
        run_direct(&args[1..]);
    }
}

fn run_as_rustc_wrapper(args: &[String]) {
    let lint = parse_lint_from_env();
    let dry_run = std::env::var("WARN_REWRITE_DRY_RUN").map(|v| v == "1").unwrap_or(false);

    let rustc_args: Vec<String> = args.to_vec();
    // Ensure --edition and other needed flags are present
    // Add --cap-lints=allow to suppress lint output from rustc itself
    // (we're not running clippy, just type-checking)

    callbacks::run_with_callbacks(rustc_args, lint, dry_run);
}

fn run_direct(args: &[String]) {
    let mut lint = LintTarget::All;
    let mut folder: Option<PathBuf> = None;
    let mut dry_run = false;
    let mut extra_cargo_args: Vec<String> = Vec::new();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "--lint" => {
                i += 1;
                lint = match args.get(i).map(String::as_str) {
                    Some("arithmetic_side_effects") => LintTarget::ArithmeticSideEffects,
                    Some("as_conversions") => LintTarget::AsConversions,
                    Some("all") => LintTarget::All,
                    other => {
                        eprintln!("Unknown lint: {:?}", other);
                        process::exit(1);
                    }
                };
            }
            "--folder" => {
                i += 1;
                folder = args.get(i).map(PathBuf::from);
            }
            "--dry-run" => dry_run = true,
            // Forward any unrecognised flags (e.g. --workspace, --package) to cargo.
            other => extra_cargo_args.push(other.to_owned()),
        }
        i += 1;
    }

    let folder = folder.unwrap_or_else(|| PathBuf::from("."));
    println!("warn-rewrite: running on {:?} for {:?}", folder, lint);

    // Phase 0: remove crate-level `#![allow(...)]` suppressions so that the
    // warnings become visible to the compiler during the rewrite pass.
    if !dry_run {
        let stripped = strip_allows::strip_project(&folder, lint);
        if stripped > 0 {
            println!("warn-rewrite: stripped crate-level allows from {} file(s)", stripped);
        }
    }

    // Phase 1: rewrite pass via RUSTC_WRAPPER.
    let self_path = std::env::current_exe().expect("can't find own path");
    let status = process::Command::new("cargo")
        .arg("check")
        .arg("--manifest-path")
        .arg(folder.join("Cargo.toml"))
        .args(&extra_cargo_args)
        .env("RUSTC_WRAPPER", &self_path)
        .env("WARN_REWRITE_LINT", format!("{:?}", lint))
        .env("WARN_REWRITE_DRY_RUN", if dry_run { "1" } else { "" })
        .status()
        .expect("failed to run cargo");

    if dry_run || !status.success() {
        // In dry-run mode, or if the build failed for non-E0689 reasons, stop here.
        process::exit(status.code().unwrap_or(1));
    }

    // Phase 2: fixup loop — resolve E0689 (ambiguous integer types) and
    // E0658 (From/TryFrom not const-stable) left by the rewriter.
    fixup::fixup_loop(&folder, &extra_cargo_args);
}

fn parse_lint_from_env() -> LintTarget {
    match std::env::var("WARN_REWRITE_LINT").as_deref() {
        Ok("ArithmeticSideEffects") => LintTarget::ArithmeticSideEffects,
        Ok("AsConversions") => LintTarget::AsConversions,
        _ => LintTarget::All,
    }
}
