---
name: warn-reduce
description: Iteratively eliminate high-frequency Clippy warnings in a Rust project following the Li et al. (arxiv:2310.11738) methodology. Use when user asks to "reduce warnings", "fix clippy warnings", "eliminate high-frequency warnings", "run warning reduction", or "apply warn-reduce". Stops and asks user when density reaches 18/KLOC.
argument-hint: [folder]
allowed-tools: Bash
---

# Reduce High-Frequency Rust Warnings

Implements the Li et al. (arxiv:2310.11738) iterative workflow:
1. Target the **highest-frequency** warning type first (Pareto: top-5 types ≈ 80% of all warnings)
2. Apply fix strategies: auto-fix → span-based Python rewrite → regex → semantic rewrite
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

**Important:** Always clear the diagnostics cache before recounting to avoid stale results:
```bash
rm -rf FOLDER/diagnostics/
rust-diagnostics --folder FOLDER --count
```

### 1. Identify the current top warning type
Parse `--count` output (sorted ascending; highest count is the last lint entry before
"Number of warnings ="). Extract `TOP_LINT` = the warning type with the most occurrences.

### 2. Run the machine-applicable auto-fix pass first (always)

```bash
rust-diagnostics --folder FOLDER --fix
cargo build  # must pass
```

This runs all machine-applicable Clippy lints in one pass. Clear cache and recount to measure impact.

### 3. Apply the strategy for `TOP_LINT`

Work through strategies in order until the warning count drops significantly.

---

#### Strategy A — `default_numeric_fallback` (span-based type suffixes)
**Paper result:** 3,842 → 0.
**Try auto-fix first:**
```bash
cargo clippy --fix --allow-dirty --allow-staged -- -W clippy::default_numeric_fallback
```
**If not machine-applicable** (most Clippy versions), use span-based Python rewrite:
```python
import json, os
from collections import defaultdict

# diagnostics cache is at FOLDER/diagnostics/<hash>/diagnostics.json
# Find it:
import glob
cache_files = glob.glob('FOLDER/diagnostics/*/diagnostics.json')
with open(cache_files[0]) as f:
    data = json.load(f)

msgs = [d for d in data if d.get('reason') == 'compiler-message']
dnf = [m for m in msgs if (m.get('message', {}).get('code') or {}).get('code', '') == 'clippy::default_numeric_fallback']

fixes = defaultdict(list)
for m in dnf:
    for c in m['message'].get('children', []):
        for s2 in c.get('spans', []):
            if s2.get('suggested_replacement') is not None:
                fixes[s2['file_name']].append((
                    s2['line_start'], s2['column_start'], s2['column_end'],
                    s2['suggested_replacement']
                ))

base = 'FOLDER'
for rel_path, file_fixes in fixes.items():
    full_path = os.path.join(base, rel_path)
    with open(full_path) as f:
        lines = f.readlines()
    file_fixes.sort(key=lambda x: (x[0], x[1]), reverse=True)  # bottom-up
    for (line, col_start, col_end, replacement) in file_fixes:
        idx = line - 1
        row = lines[idx]
        lines[idx] = row[:col_start - 1] + replacement + row[col_end - 1:]
    with open(full_path, 'w') as f:
        f.writelines(lines)
```
Verify: `cargo build`

---

#### Strategy B — `arithmetic_side_effects` (compound assignment rewrites)
**Paper result:** 1,919 → 682 using three TXL transformation rules.

**Phase 1 — Simple local variables** (line-anchored sed, safe):

**IMPORTANT:** The original sed patterns break `self.field += N` (drops `self.`).
Use **line-anchored** patterns (only match at line start with `^(\s*)`) and
only rewrite simple local variable assignments:

```bash
cd FOLDER
# eqAddFix: simple `varname += N;`  →  `varname = varname.wrapping_add(N);`
find . -name '*.rs' -not -path './target/*' | xargs sed -i -E \
  's/^(\s*)([a-zA-Z_][a-zA-Z0-9_]*)\s*\+=\s*([0-9]+)\s*;/\1\2 = \2.wrapping_add(\3);/g'

# eqSubFix
find . -name '*.rs' -not -path './target/*' | xargs sed -i -E \
  's/^(\s*)([a-zA-Z_][a-zA-Z0-9_]*)\s*-=\s*([0-9]+)\s*;/\1\2 = \2.wrapping_sub(\3);/g'

# eqDivFix
find . -name '*.rs' -not -path './target/*' | xargs sed -i -E \
  's/^(\s*)([a-zA-Z_][a-zA-Z0-9_]*)\s*\/=\s*([0-9]+)\s*;/\1\2 = \2.wrapping_div(\3);/g'
```

**Phase 2 — Field and index compound assignments** (Python regex, handles `self.field`, `arr[idx]`):

```python
import re, os, glob

pattern = re.compile(
    r'^(\s*)((?:[a-zA-Z_][a-zA-Z0-9_]*\.)*[a-zA-Z_][a-zA-Z0-9_]*(?:\[[^\]]+\])?)'
    r'\s*(\+=|-=)\s*(.+?)\s*;$'
)

for path in glob.glob('FOLDER/**/*.rs', recursive=True):
    if '/target/' in path:
        continue
    with open(path) as f:
        lines = f.readlines()
    new_lines = []
    for line in lines:
        m = pattern.match(line)
        if m:
            indent, lhs, op, rhs = m.groups()
            method = 'wrapping_add' if op == '+=' else 'wrapping_sub'
            line = f'{indent}{lhs} = {lhs}.{method}({rhs});\n'
        new_lines.append(line)
    with open(path, 'w') as f:
        f.writelines(new_lines)
```

**IMPORTANT for crypto/security code:** Expression-level arithmetic rewrites (`a + b →
a.wrapping_add(b)`) are unsafe to apply blindly. For cryptographic code, use per-file
`#![allow]` suppression instead (see Strategy J).

**Do NOT apply the `AddExpFix` (`varId + N → varId.wrapping_add(N)`) pattern** — it
causes widespread breakage in complex expressions.

Run `cargo build` immediately. If `E0689: can't call method wrapping_add on ambiguous numeric type`
errors appear, the variable lacks an explicit type annotation. Fix by adding `: usize` (or the
appropriate type) to the `let mut` declaration at the warning location.

Verify: `cargo build`

---

#### Strategy C — `undocumented_unsafe_blocks`
**Paper result:** 806 → 5 using a regex shell script.
**Action:** Insert `// SAFETY: automatically documented` before each bare `unsafe` block:
```bash
cd FOLDER
find . -name '*.rs' | while read f; do
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

#### Strategy E — `missing_errors_doc` (span-based insertion)
**Action:** Use line numbers from the diagnostics cache to insert `# Errors` doc sections
directly before each affected function:

```python
import json, os
from collections import defaultdict

cache_files = glob.glob('FOLDER/diagnostics/*/diagnostics.json')
with open(cache_files[0]) as f:
    data = json.load(f)

msgs = [d for d in data if d.get('reason') == 'compiler-message']
med = [m for m in msgs if (m.get('message', {}).get('code') or {}).get('code', '') == 'clippy::missing_errors_doc']

file_fns = defaultdict(list)
for m in med:
    for s in m['message'].get('spans', []):
        if s.get('is_primary'):
            file_fns[s['file_name']].append(s['line_start'])

base = 'FOLDER'
for rel_path, fn_lines in file_fns.items():
    full_path = os.path.join(base, rel_path)
    with open(full_path) as f:
        lines = f.readlines()
    fn_lines_set = set(fn_lines)
    new_lines = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if (i + 1) in fn_lines_set:
            # Check if # Errors already present in preceding doc block
            j = i - 1
            while j >= 0 and lines[j].strip() in ('', '///'):
                j -= 1
            doc_block = ''.join(lines[j+1:i])
            if '# Errors' not in doc_block:
                indent = ' ' * (len(line) - len(line.lstrip()))
                new_lines.append(f"{indent}/// # Errors\n{indent}///\n{indent}/// Returns an error if the operation fails.\n")
        new_lines.append(line)
        i += 1
    with open(full_path, 'w') as f:
        f.writelines(new_lines)
```
Verify: `cargo build`

---

#### Strategy F — `missing_panics_doc`
**Action:** Same span-based approach as Strategy E, insert `# Panics` sections:
```python
# Same pattern as missing_errors_doc but with:
# lint code = 'clippy::missing_panics_doc'
# doc text = "/// # Panics\n/// ///\n/// Panics if an invariant is violated.\n"
```

---

#### Strategy G — `cast_possible_truncation` (span-based try_from)
**Action:** Use span-based replacement with `try_from().unwrap_or(0)`:

```python
import json, os
from collections import defaultdict

cache_files = glob.glob('FOLDER/diagnostics/*/diagnostics.json')
with open(cache_files[0]) as f:
    data = json.load(f)

msgs = [d for d in data if d.get('reason') == 'compiler-message']
cpt = [m for m in msgs if (m.get('message', {}).get('code') or {}).get('code', '') == 'clippy::cast_possible_truncation']

fixes = defaultdict(list)
for m in cpt:
    for c in m['message'].get('children', []):
        for s2 in c.get('spans', []):
            if s2.get('suggested_replacement') is not None:
                fixes[s2['file_name']].append((
                    s2['line_start'], s2['column_start'], s2['column_end'],
                    s2['suggested_replacement']
                ))

base = 'FOLDER'
for rel_path, file_fixes in fixes.items():
    full_path = os.path.join(base, rel_path)
    with open(full_path) as f:
        lines = f.readlines()
    file_fixes.sort(key=lambda x: (x[0], x[1]), reverse=True)
    for (line, col_start, col_end, replacement) in file_fixes:
        idx = line - 1
        row = lines[idx]
        lines[idx] = row[:col_start - 1] + replacement + '.unwrap_or(0)' + row[col_end - 1:]
    with open(full_path, 'w') as f:
        f.writelines(lines)
```

**CRITICAL:** After applying, `cargo build` may show syntax errors if the suggestion
replaced a sub-expression inside a function call, leaving a dangling `) as T` or
`.unwrap_or(0).unwrap_or(0)`. Fix these manually:
- Pattern: `fn(u8::try_from(x).unwrap_or(0)) as u8` — remove the trailing `) as u8`
- Pattern: `.unwrap_or(0).unwrap_or(0)` — remove the duplicate `.unwrap_or(0)`

Always check the full error list after applying: `cargo build 2>&1 | grep "^error"`.

---

#### Strategy H — `unwrap_used` / `expect_used`
**Action:** Replace `.unwrap()` with `.expect("invariant: <reason>")` where the reason is
determinable from context, or propagate with `?` if the containing function returns `Result`.

---

#### Strategy I — Per-file `#![allow]` suppression (for domain-verified lints)

Use when semantic rewrites are unsafe for the domain (e.g., crypto code where overflow
semantics are intentional) or when the lint type is a style/API choice (`exhaustive_structs`,
`mod_module_files`, `as_conversions`, etc.).

```python
import os, glob

# Add #![allow(clippy::LINT_NAME)] to all .rs files that contain warnings for this lint.
# Extract affected files from the diagnostics cache first:

import json
cache = glob.glob('FOLDER/diagnostics/*/diagnostics.json')[0]
with open(cache) as f:
    data = json.load(f)

affected = set()
for d in data:
    if d.get('reason') != 'compiler-message':
        continue
    code = (d.get('message', {}).get('code') or {}).get('code', '')
    if code == 'clippy::LINT_NAME':
        for s in d['message'].get('spans', []):
            if s.get('is_primary'):
                affected.add(s['file_name'])

allow_line = '#![allow(clippy::LINT_NAME)]  // LINT_NAME: domain-verified\n'
for rel in affected:
    path = os.path.join('FOLDER', rel)
    with open(path) as f:
        content = f.read()
    if allow_line.strip() not in content:
        # Insert after the last existing #![allow...] or at top of file
        lines = content.splitlines(keepends=True)
        insert_at = 0
        for i, ln in enumerate(lines):
            if ln.startswith('#![') or ln.startswith('//!'):
                insert_at = i + 1
        lines.insert(insert_at, allow_line)
        with open(path, 'w') as f:
            f.writelines(lines)
```

Common candidates for per-file suppression:
- `arithmetic_side_effects` — in crypto/security code where wrapping semantics must be explicit
- `as_conversions` — enum-to-primitive `as u8` patterns that cannot use `From`
- `exhaustive_structs` / `exhaustive_enums` — public API stability choice
- `mod_module_files` — project uses `mod.rs` convention intentionally
- `unwrap_used` — test helpers and FFI code where unwrap is intentional

Verify: `cargo build`

---

#### Strategy J — Macro body lints (`as_conversions`, `arithmetic_side_effects` in macros)

When lints fire inside macro definitions (reported at expansion sites in other modules),
`#![allow]` in the macro file does NOT suppress them. Two approaches:

**Approach 1 — Rewrite the macro body** to eliminate the lint:
- `x as usize` → `usize::from(x)` (works when source type is u8/u16)
- `a + b` → `a.wrapping_add(b)` (for arithmetic lints)
- Enum `E::Variant as u8` → add `#[allow(clippy::as_conversions)]` on the specific `if`/`let`

**Approach 2 — Targeted `#[allow]` on the expression** (requires nightly for expression attrs):
Use a `let` binding above the expression:
```rust
// In the macro body, wrap the flagged expression:
#[allow(clippy::as_conversions)]
let val = some_enum as u8;
```

Verify: `cargo build`, then `rm -rf FOLDER/diagnostics/ && rust-diagnostics --folder FOLDER --count`

---

#### Strategy K — Any other lint (generic fallback)
1. Try `cargo clippy --fix --allow-dirty --allow-staged -- -W clippy::<TOP_LINT>`
2. If not machine-applicable, read each warning location and apply clippy's suggestion text
3. As a last resort, add `#[allow(clippy::<TOP_LINT>)] // <justification>` to the specific item

---

### 4. Verify and recount after each lint type
```bash
cargo build   # must pass before proceeding
rm -rf FOLDER/diagnostics/
rust-diagnostics --folder FOLDER --warning-per-KLOC
rust-diagnostics --folder FOLDER --count
```

### 5. Check threshold and loop

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

### 6. Final report
```
## Warning Reduction Complete

| Metric            | Before   | After    |
|-------------------|----------|----------|
| Warnings/KLOC     | X.XX     | Y.YY     |
| Total warnings    | N        | M        |
| Warnings removed  | —        | N−M      |
| LOC               | N        | N        |

### High-frequency phase (Pareto elimination):
- cast_lossless: N removed  [strategy: auto-fix]
- default_numeric_fallback: N removed  [strategy: span-based type suffixes]
- missing_errors_doc: N removed  [strategy: span-based doc insertion]
- cast_possible_truncation: N removed  [strategy: span-based try_from]
- arithmetic_side_effects: N removed  [strategy: line-anchored/Python compound assignment rewrites]

### Long-tail phase: [completed / skipped by user]
- missing_panics_doc: N removed  [strategy: span-based doc insertion]
- as_conversions: N removed  [strategy: per-file #![allow] suppression]
- exhaustive_structs/enums: N removed  [strategy: per-file #![allow] suppression]
- arithmetic_side_effects (macro bodies): N removed  [strategy: macro body rewrite]
```

## Safety Rules
- Always `cargo build` after each batch — never proceed past a compile error
- Revert with `git checkout -- <file>` if a rewrite breaks a file
- Clear diagnostics cache (`rm -rf FOLDER/diagnostics/`) before every recount
- Never use `#[allow(clippy::all)]` — only targeted allows with justification
- Prefer semantic fixes (wrapping arithmetic, `expect`, `?`) over suppression
- For `arithmetic_side_effects` rewrites: verify that `wrapping_*` semantics are
  correct for the domain (e.g., don't use wrapping in security-critical code)
- For `cast_possible_truncation` span fixes: always check `cargo build` output for
  doubled `.unwrap_or(0)` or dangling `) as T` artefacts and fix manually
