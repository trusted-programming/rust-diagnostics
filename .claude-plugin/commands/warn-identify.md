---
name: warn-identify
description: Identify high-frequency Clippy warnings in a Rust project using rust-diagnostics. Use when user asks to "identify warnings", "find high-frequency warnings", "analyse warning density", "count clippy warnings", or wants to see warning distribution following the Li et al. 2310.11738 methodology.
argument-hint: [folder]
allowed-tools: Bash
---

# Identify High-Frequency Rust Warnings

Following Li et al. (arxiv:2310.11738) methodology: warnings follow a highly skewed Pareto distribution — the top-5 lint types typically account for >80% of all warnings.

## Steps

1. **Determine the target folder**
   - If `$ARGUMENTS` is provided, use it as `FOLDER`
   - Otherwise use the current working directory (`.`)
   - Verify `Cargo.toml` exists in `FOLDER`

2. **Run warning count analysis**
   ```bash
   rust-diagnostics --folder FOLDER --count
   ```
   This prints:
   - Per-type sorted counts (ascending)
   - Total warnings
   - Lines of Rust code
   - Warnings per KLOC

3. **Run quick density check**
   ```bash
   rust-diagnostics --folder FOLDER --warning-per-KLOC
   ```

4. **Present findings as a ranked table** (descending by count):
   Parse the `--count` output and re-sort descending. Show:
   ```
   Rank  Count  %Total  Warning Type
   ----  -----  ------  ------------
      1    123    56%   default_numeric_fallback
      2     45    20%   arithmetic_side_effects
      ...
   ```

5. **Compute cumulative coverage** and highlight:
   - Top-1 lint share
   - Top-5 lint share
   - Current density vs 18/KLOC threshold (target) and 21/KLOC (crates-io average)

6. **Summarise actionability**:
   - Mark lints fixable by `cargo clippy --fix` (machine-applicable) with `[auto-fixable]`
   - Suggest the `warn-reduce` skill to start elimination

## Output Format

```
## Warning Analysis: FOLDER
Density: X.XX warnings/KLOC  [TARGET: 18/KLOC]

### Distribution (top N lint types = Y% of all warnings)
Rank  Count  Cumul%  Type                          Fixable?
   1    NNN    XX%   lint_name                     [auto]
   2    NNN    XX%   lint_name
   ...

### Summary
- Total warnings: N
- LOC: N
- Top-1 coverage: XX%
- Top-5 coverage: XX%
- Status: ABOVE/BELOW 18/KLOC threshold
```
