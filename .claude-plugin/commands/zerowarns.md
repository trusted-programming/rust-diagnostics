---
name: zerowarns
description: >
  Compiler-guaranteed elimination of clippy::arithmetic_side_effects and
  clippy::as_conversions warnings via type-aware semantic rewriting (no LLM,
  no #[allow] suppression). The final stage of the rust-diagnostics pipeline.
  Use when the user asks to "zero warnings", "remove arithmetic warnings",
  "fix as_conversions", "run zerowarns", "apply semantic rewrites", or wants
  to produce warning-free Rust for LLM training data.
argument-hint: [folder]
allowed-tools: Bash
---

# zerowarns — Compiler-Guaranteed Warning Elimination

`zerowarns` rewrites `clippy::arithmetic_side_effects` and `clippy::as_conversions`
warnings using rustc's type information. Every rewrite is verified by the compiler.
No LLM, no `#[allow]` suppression.

## Prerequisites

```bash
# Verify zerowarns is installed
which zerowarns || cargo +nightly install zerowarns
```

If not installed, build from source:
```bash
cd /path/to/rust-diagnostics/warn-rewrite
cargo build --release
# binary is at target/release/zerowarns
```

## Steps

### 0. Determine the target folder
- If `$ARGUMENTS` is provided, use it as `FOLDER`
- Otherwise use the current working directory (`.`)
- Verify `FOLDER/Cargo.toml` exists

### 1. Record baseline
```bash
rust-diagnostics --folder FOLDER --count 2>/dev/null | \
  grep -E "arithmetic_side_effects|as_conversions|Number of warnings"
```
Note the before counts. If `rust-diagnostics` is unavailable, use:
```bash
cargo clippy --manifest-path FOLDER/Cargo.toml -- \
  -W clippy::arithmetic_side_effects -W clippy::as_conversions 2>&1 | \
  grep "warning\[" | wc -l
```

### 2. Run zerowarns
```bash
zerowarns --folder FOLDER
```

Expected output:
```
warn-rewrite: running on "FOLDER" for All
warn-rewrite: stripped crate-level allows from N file(s)   ← Phase 0
   Compiling ...                                            ← Phase 1 rewrite pass
warn-rewrite: applied N rewrites across M files
   Checking ...                                            ← Phase 2 fixup loop
warn-rewrite: fixup iteration 1: 0 E0689, 0 E0658 errors
```

If the build fails during Phase 1 (non-E0689 errors), check with:
```bash
cargo check --manifest-path FOLDER/Cargo.toml 2>&1 | head -40
```

### 3. Verify zero warnings
```bash
cargo clippy --manifest-path FOLDER/Cargo.toml -- \
  -W clippy::arithmetic_side_effects -W clippy::as_conversions 2>&1 | \
  grep "warning\[clippy::"
```
Expected: no output (zero warnings for both lint families).

If any `#[allow(clippy::as_conversions)]` remain, they are targeted allows on
`const` items where `From`/`TryFrom` are not yet const-stable — this is correct
and intentional.

### 4. Confirm the project still compiles
```bash
cargo build --manifest-path FOLDER/Cargo.toml
```

### 5. Report results
Present a summary table:

```
## zerowarns Results: FOLDER

| Metric                          | Before  | After |
|---------------------------------|---------|-------|
| arithmetic_side_effects         | N       | 0     |
| as_conversions                  | N       | 0*    |
| Rewrites applied                | —       | N     |
| Targeted #[allow] (const items) | —       | N     |

*Remaining allows are on const items (From/TryFrom not const-stable).
```

## Lint-specific targeting

```bash
# Only arithmetic (wrapping arithmetic rewrites)
zerowarns --folder FOLDER --lint arithmetic_side_effects

# Only as_conversions (From/TryFrom rewrites)
zerowarns --folder FOLDER --lint as_conversions
```

## Integration with the full pipeline

```
/warn-identify FOLDER    →  measure density and distribution
/warn-reduce FOLDER      →  eliminate easy wins down to 18/KLOC
/zerowarns FOLDER        →  semantic rewrite to 0 for arithmetic + as casts
```

## Troubleshooting

**`E0689: can't call method on ambiguous numeric type`** — Phase 2 should resolve
these automatically. If they persist, check that the fixup loop ran (look for
"fixup iteration" in output).

**`E0658: use of unstable library feature`** in const items — expected; Phase 2
reverts those specific casts and adds targeted `#[allow(clippy::as_conversions)]`.

**Build errors not related to E0689/E0658** — `zerowarns` will exit with an error.
Run `cargo check` to diagnose before re-running.

**`zerowarns: command not found`** — the binary may be named `warn-rewrite` in a
pre-rename build. Try `warn-rewrite --folder FOLDER` as a fallback.
