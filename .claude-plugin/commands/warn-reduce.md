---
name: warn-reduce
description: Iteratively eliminate high-frequency Clippy warnings in a Rust project following the Li et al. (arxiv:2310.11738) methodology. Use when user asks to "reduce warnings", "fix clippy warnings", "eliminate high-frequency warnings", "run warning reduction", or "apply warn-reduce". Stops and asks user when density reaches 18/KLOC.
argument-hint: [folder]
allowed-tools: Bash
---

# Reduce High-Frequency Rust Warnings

Implements the Li et al. (arxiv:2310.11738) iterative workflow:
1. Target the **highest-frequency** warning type first (Pareto: top-5 types ≈ 80% of all warnings)
2. Apply the paper's three strategies: auto-fix → regex script → semantic rewrite
3. Repeat until density ≤ 18/KLOC (below crates-io average of 21/KLOC)
4. **Ask user** whether to continue with long-tail warnings at 18/KLOC

## Steps

### 0. Setup
- If `$ARGUMENTS` is provided, use as `FOLDER`; otherwise use `.`
- Confirm `Cargo.toml` exists in `FOLDER`
- Record baseline density:
  ```bash
  rust-diagnostics --folder FOLDER --warning-per-KLOC
  rust-diagnostics --folder FOLDER --count
  ```

### 1. Identify the current top warning type
Parse `--count` output (sorted ascending; highest count is the last lint entry before
"Number of warnings ="). Extract `TOP_LINT` = the warning type with the most occurrences.

### 2. Apply the paper's fix strategy for `TOP_LINT`

Work through strategies in order until the warning count drops significantly.

---

#### Strategy A — `default_numeric_fallback`
**Paper result:** 3,842 → 0 via Clippy modification (tag changed to MachineApplicable).
**Action:** Try auto-fix first; if Clippy version supports it, it will be machine-applicable:
```bash
cd FOLDER && cargo clippy --fix --allow-dirty --allow-staged \
  -- -W clippy::default_numeric_fallback 2>&1
```
If auto-fix is unavailable, use a sed-based script to add type suffixes to bare integer
and float literals in contexts where inference would fall back to i32/f64:
```bash
# Find all warnings locations from rust-diagnostics output, then for each:
# Replace bare `0` with `0_i32`, `1.0` with `1.0_f64`, etc. per the inferred type
# shown in the clippy message.
```
Verify: `cargo build`

---

#### Strategy B — `arithmetic_side_effects`
**Paper result:** 1,919 → 682 using three TXL transformation rules.
**Action:** Apply these regex/sed rewrites (Li et al.'s exact patterns translated to sed):

```bash
cd FOLDER
# Rule eqAddFix: i += N  →  i = i.wrapping_add(N)
find . -name '*.rs' | xargs sed -i -E \
  's/\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\+=\s*([0-9]+)\b/\1 = \1.wrapping_add(\2)/g'

# Rule eqSubFix: i -= N  →  i = i.wrapping_sub(N)
find . -name '*.rs' | xargs sed -i -E \
  's/\b([a-zA-Z_][a-zA-Z0-9_]*)\s*-=\s*([0-9]+)\b/\1 = \1.wrapping_sub(\2)/g'

# Rule eqDivFix: i /= N  →  i = i.wrapping_div(N)
find . -name '*.rs' | xargs sed -i -E \
  's/\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\/=\s*([0-9]+)\b/\1 = \1.wrapping_div(\2)/g'

# Rule AddExpFix: varId + N  →  varId.wrapping_add(N)  (in expression context)
find . -name '*.rs' | xargs sed -i -E \
  's/\b([a-zA-Z_][a-zA-Z0-9_]*)\s*\+\s*([0-9]+)\b/\1.wrapping_add(\2)/g'
```
**Important:** Run `cargo build` immediately. If any rewrite breaks compilation, revert
that specific rule with `git diff` and apply more conservatively file-by-file using
`rust-diagnostics --count` to locate exact warning positions.

---

#### Strategy C — `undocumented_unsafe_blocks`
**Paper result:** 806 → 5 using a regex shell script.
**Action:** Insert `// SAFETY: automatically documented` before each bare `unsafe` block:
```bash
cd FOLDER
find . -name '*.rs' | while read f; do
  # Insert SAFETY comment before unsafe blocks that lack one
  perl -i -0pe \
    's/(?<!\/\/ SAFETY[^\n]*\n)(\s*unsafe\s*\{)/\n    \/\/ SAFETY: TODO - verify invariants\n\1/g' \
    "$f"
done
```
Then review inserted comments: replace generic text with specific invariant descriptions
where the code makes the reason clear.
Verify: `cargo build`

---

#### Strategy D — `missing_debug_implementations`
**Paper result:** 160 → 8 using a regex shell script.
**Action:** Add `#[derive(Debug)]` before all `pub struct` and `pub enum` declarations
that don't already have it:
```bash
cd FOLDER
find . -name '*.rs' | while read f; do
  perl -i -0pe \
    's/(\n(?!#\[derive[^\]]*Debug))(pub\s+(?:struct|enum)\s)/\1#[derive(Debug)]\2/g' \
    "$f"
done
```
Verify: `cargo build`. If any type contains a non-Debug field, add the derive manually
or use a custom `impl Debug`.

---

#### Strategy E — `missing_errors_doc`
**Action:** For each `pub fn` returning `Result<_, _>`, add a `# Errors` doc section:
```bash
cd FOLDER
# Use rust-diagnostics to locate all missing_errors_doc warnings, then for each
# function, append to its doc comment:
#   /// # Errors
#   /// Returns an error if ...
```
Work file-by-file using clippy output positions for precision.

---

#### Strategy F — `missing_panics_doc`
**Action:** For each function with explicit `panic!`/`.unwrap()`/`.expect()`, add `# Panics`:
```bash
# Append to doc comment:
#   /// # Panics
#   /// Panics if ...
```

---

#### Strategy G — `unwrap_used` / `expect_used`
**Action:** Replace `.unwrap()` with `.expect("invariant: <reason>")` where the reason is
determinable from context, or propagate with `?` if the containing function returns `Result`.

---

#### Strategy H — Any other lint (generic fallback)
1. Try `cargo clippy --fix --allow-dirty --allow-staged -- -W clippy::<TOP_LINT>`
2. If not machine-applicable, read each warning location and apply clippy's suggestion text
3. As a last resort, add `#[allow(clippy::<TOP_LINT>)] // <justification>` to the specific item

---

### 3. Verify and recount after each lint type
```bash
cargo build   # must pass before proceeding
rust-diagnostics --folder FOLDER --warning-per-KLOC
rust-diagnostics --folder FOLDER --count
```

### 4. Check threshold and loop

**If density > 18/KLOC** → go to step 1, pick the new highest-frequency lint.

**If density ≤ 18/KLOC** → **STOP and ask the user:**

> "Warning density has reached **X.XX/KLOC** (≤ 18/KLOC threshold, Li et al. 2310.11738).
> Remaining warnings are long-tail types (typically <10 occurrences each).
>
> **Continue with long-tail warning removals?**
> - **Yes** — eliminate remaining warning types one by one
> - **No** — stop here; the project is at a healthy warning density"

If **Yes**: continue iterating through remaining lint types lowest-to-highest until done.
If **No**: stop and print the final report.

### 5. Final report
```
## Warning Reduction Complete

| Metric            | Before   | After    |
|-------------------|----------|----------|
| Warnings/KLOC     | X.XX     | Y.YY     |
| Total warnings    | N        | M        |
| Warnings removed  | —        | N−M      |
| LOC               | N        | N        |

### High-frequency phase (Pareto elimination):
- default_numeric_fallback: N removed  [strategy: auto-fix]
- arithmetic_side_effects:  N removed  [strategy: wrapping_* rewrites]
- undocumented_unsafe_blocks: N removed [strategy: SAFETY comments]
- missing_debug_implementations: N removed [strategy: #[derive(Debug)]]

### Long-tail phase: [completed / skipped by user]
```

## Safety Rules
- Always `cargo build` after each batch — never proceed past a compile error
- Revert with `git checkout -- <file>` if a rewrite breaks a file
- Never use `#[allow(clippy::all)]` — only targeted allows with justification
- Prefer semantic fixes (wrapping arithmetic, `expect`, `?`) over suppression
- For `arithmetic_side_effects` rewrites: double-check that `wrapping_*` semantics
  are actually correct for the domain (e.g., don't use wrapping in security-critical code)
