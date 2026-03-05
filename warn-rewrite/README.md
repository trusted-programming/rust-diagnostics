# zerowarns

[![crates.io](https://img.shields.io/crates/v/zerowarns.svg)](https://crates.io/crates/zerowarns)

**Compiler-guaranteed elimination of high-density Clippy warnings in Rust — no LLM, no `#[allow]` suppression.**

`zerowarns` is the final stage of the [`rust-diagnostics`](https://github.com/trusted-programming/rust-diagnostics) warning-elimination pipeline.
After `warn-identify` measures warning density and `warn-reduce` removes the easy wins,
`zerowarns` finishes the job by rewriting the two highest-density remaining lint types
using rustc's own type information:

| Lint | Strategy | Example |
|------|----------|---------|
| `clippy::arithmetic_side_effects` | Replace `+`, `-`, `*`, `/`, `-x` with `wrapping_*` methods | `a + b` → `a.wrapping_add(b)` |
| `clippy::as_conversions` | Replace `x as T` with `T::from(x)` / `T::try_from(x).unwrap_or_default()` | `x as u32` → `u32::from(x)` |

## Why no LLM?

The goal of the `rust-diagnostics` project is to produce **training data** for language models:
warning-free Rust code where every rewrite is semantically correct.
Using an LLM to fix the warnings would contaminate the training signal.
`zerowarns` uses only the Rust type checker — correctness is guaranteed by the compiler.

## The three-phase pipeline

```
Phase 0  strip crate-level #![allow(clippy::arithmetic_side_effects / as_conversions)]
Phase 1  RUSTC_WRAPPER pass — type-aware semantic rewrites (6 000+ rewrites in ~18 s)
Phase 2  fixup loop — resolve E0689 (ambiguous integer types) and E0658 (const-context
         From/TryFrom not yet stable) left by Phase 1; const items get targeted
         #[allow(clippy::as_conversions)] instead of crate-level suppression
```

Everything runs as a single `cargo check` invocation via `RUSTC_WRAPPER`.

## Installation

Requires a nightly toolchain (uses `rustc_private` for type information):

```bash
# install the nightly channel used during development
rustup toolchain install nightly-2026-01-20 --component rustc-dev llvm-tools rust-src

# install the binary
cargo +nightly-2026-01-20 install zerowarns
```

## Usage

```bash
# Rewrite arithmetic_side_effects and as_conversions in the current project
zerowarns --folder .

# Target only one lint family
zerowarns --folder . --lint arithmetic_side_effects
zerowarns --folder . --lint as_conversions

# Dry-run: show what would be rewritten without touching files
zerowarns --folder . --dry-run

# Forward extra flags to cargo (e.g. --workspace, --package foo)
zerowarns --folder . --workspace
```

## Integration with the full pipeline

```
warn-identify .        # measure: density, Pareto distribution
warn-reduce .          # iterative: auto-fix + manual elimination down to 18/KLOC
zerowarns .            # semantic: compiler-guaranteed rewrite of arithmetic + as casts
```

After `zerowarns`, run `cargo clippy` to confirm zero remaining warnings for the two lint families.

## How it works

1. **Phase 0** — walks every `.rs` file and removes `#![allow(clippy::arithmetic_side_effects)]`
   and `#![allow(clippy::as_conversions)]` lines so the warnings become visible.

2. **Phase 1** — registers itself as `RUSTC_WRAPPER` and runs `cargo check`.
   For each compilation unit, it hooks into `rustc_interface::after_analysis`,
   walks the HIR, classifies every arithmetic expression and `as` cast by the
   *resolved types* from the type-checker, and writes precise byte-range rewrites.

3. **Phase 2** — runs `cargo check --message-format=json` in a loop (up to 10 iterations)
   to catch any `E0689` (ambiguous integer type) errors introduced when type-inference
   lost its anchor after a rewrite, and `E0658` errors in `const` contexts where
   `From`/`TryFrom` are not yet const-stable. Both are fixed deterministically from
   rustc's own diagnostic suggestions.

## Performance (openhitls-rs, 162 KLOC)

| Metric | Value |
|--------|-------|
| Input warnings | 8 298 (51/KLOC) |
| Rewrites applied | 6 305 |
| Wall-clock time | ~18 s |
| Remaining `#[allow]` added | 4 (all in `const` items) |

## License

Apache-2.0 — same as `rust-diagnostics`.
