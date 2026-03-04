# rust-diagnostics

[![](https://img.shields.io/crates/v/rust-diagnostics.svg)](https://crates.io/crates/rust-diagnostics)

This is a utility to insert diagnostics of code fragments as comments in Rust
code and check how a warning/error in the diagnostics has been fixed in git
commit history.

Rust compiler displays many diagnostics to the console, using file name and
line numbers to indicate their exact locations. Without an IDE, it requires a
programmer to go back and forth between command console and the editor. 

This utility inserts the diagnostic messages in-place, which could enable
transformer-based machine learning approaches to analyse Rust diagnostic
semantics.

Through additional arguments, this utility also checks how a warning found
in revision r1 has been manually fixed by a revision r2. 

Currently we integrate the utility with `clippy` and `git2-rs`.

## Installation
```bash
cargo install rust-diagnostics
```

## Usage:

The full synoposis of the command is shown below.
```bash
rust-diagnostics [--patch <commit_id> [--confirm] [--pair] [--function] [--single] [--location] [--mixed] ]
```

### A running example

To illustrate its usage, we use a small example, let's call it an `abc` project. 

```bash
rm -rf abc
cargo init --bin abc
cat > abc/src/main.rs <<EOF
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
EOF
```

### Inserting warnings info into Rust code

The default function (i.e., without any argument) of `rust-diagnostics` will insert warning
info into the Rust code. For example,
```bash
cd abc
rust-diagnostics
```

The command invokes `clippy` to report all the warnings:
```
    Checking abc v0.1.0 (...)
    Finished dev [unoptimized + debuginfo] target(s) in 0.06s
There are 1 warnings in 1 files.
```

As a result, there is also a new folder `diagnostics` created, with a file `src/main.rs` inside:
```rust

fn main() {
    let s = /*#[Warning(clippy::unwrap_used)*/std::fs::read_to_string("Cargo.toml").unwrap()/*
#[Warning(clippy::unwrap_used)
note: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
if this value is an `Err`, it will panic
requested on the command line with `-W clippy::unwrap-used`*/;
    println!("The configuration file is: {s}");
}
```
As one can see, the warning related has been marked by two comments, before and
after the violation code. The comment before indicates the type of warning,
here `clippy::unwrap_used`. The comment after indicates also some additional
note reported by `cargo clippy`, providing details of the type of warning and
hints on how to address it. In this example, `unwrap_used` is not automatically
fixed.

### Analyse the manually fixed warnings from change history

A useful extension of the above utility checks how many warnings in the change
history have been fixed, whether it is done by `cargo clippy --fix`
automatically, or by manual patches. If the manual fixes are repetitive, it
would become useful for learning the language, either manually or by machine
learning. 

To do this, we restart the example by making a few changes to the git repository as follows.
```bash
rm -rf abc
cargo init --vcs git --bin abc
cd abc
cat > src/main.rs <<EOF
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
EOF
git commit -am "r1"
cat > src/main.rs <<EOF
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
EOF
git commit -am "r2"
```
The `--vcs git` option is used here so that the example project can contain some
change history, in order to illustrate the functionality to do with git
repository analysis.

If you inspect the code and wonder whether revision r2 has fixed the warning of revision r1, 
you can use `git log -p` to identify the revisions' commit id first. 
```
commit 839164fa28d71a9c00009c9e25bc84dce6caa286             .......... (r2)
Author: ...
Date:   ...

    update

diff --git a/src/main.rs b/src/main.rs
index 36d2d89..6175ab1 100644
--- a/src/main.rs
+++ b/src/main.rs
@@ -1,5 +1,6 @@
 
 fn main() {
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
 }

commit 6fafc98041f47155bc51c5ddc55b8e8b0b7548bf           .......... (r1)
Author: ...
Date:   ...

    init

diff --git a/src/main.rs b/src/main.rs
new file mode 100644
index 0000000..36d2d89
--- /dev/null
+++ b/src/main.rs
@@ -0,0 +1,5 @@
+
+fn main() {
+    let s = std::fs::read_to_string("Cargo.toml").unwrap();
+    println!("{s}");
+}


```

Then run the following two commands, we can check whether the warning in r1 has been fixed by r2.
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm
```
The output `diagnostics.log` includes the count of warnings of $r1 and the hunks between $r1..$r2
that matters to fix the warnings listed in front of the hunks.

For example, the output will be the same as those in the `git diff` format:
```
There are 1 warnings in 1 files.
##[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
```
Note that here we have removed all the context lines, just like the `-U0` option of
`git-diff` command, so that it is possible to get a more precise function context 
of the patch.

#### Generate into a pair using the `--pair` option
Using the `--pair` option changes the patch into a pair of code before and after the change:
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm --pair
```

For example, the `diagnostics.log` will contain
```
There are 1 warnings in 1 files.
##[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
```
Note. To avoid possible clash with existing code, in the separator of the pair we use the hash key `19a3477889393ea2cdd0edcb5e6ab30c`, which has been created from the command
```bash
echo rust-diagnostics | md5sum 
```

#### Generate function context using the `--function` option
The pair may be too terse to learn, we use the `--function` option to
print the function surrounding the patch as its context:
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm --pair --function
```

For example, it will print the following instead:
```
There are 1 warnings in 1 files.
##[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
@@ -3,2 +3,3 @@ fn main() {
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
```

#### Generate marked up context using the `--location` option
This option could insert the location of warning and its hints of fixing 
according to `clippy` into the original context.
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm --pair --function --location
```
For example, it will print the following instead:
```
There are 1 warnings in 1 files.
##[Warning(clippy::unwrap_used)
fn main() {
    let s = /*#[Warning(clippy::unwrap_used)*/std::fs::read_to_string("Cargo.toml").unwrap()/*
#[Warning(clippy::unwrap_used)
note: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
if this value is an `Err`, it will panic
requested on the command line with `-W clippy::unwrap-used`*/;
    println!("{s}");
}
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
```
#### Generate mixed context and patch using the `--mixed` option
This option could pair up the context with the actual patch.
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm --pair --function --location --mixed
```
For example, it will print the following instead:
```
There are 1 warnings in 1 files.
##[Warning(clippy::unwrap_used)
fn main() {
    let s = /*#[Warning(clippy::unwrap_used)*/std::fs::read_to_string("Cargo.toml").unwrap()/*
#[Warning(clippy::unwrap_used)
note: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
if this value is an `Err`, it will panic
requested on the command line with `-W clippy::unwrap-used`*/;
    println!("{s}");
}
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
```
Note that we don't keep the header because the line numbers are no longer important if we already know where the
warning is and the inserted markup hints already shifted the original line numbers.

## Counting warnings

To count the number of warnings and calculate the warnings per KLOC (thousand lines of code), use the `--count` (or `-c`) option:
```bash
rust-diagnostics --count
```
This will run clippy, group warnings by type, and display the total count alongside the lines of code in the `src` directory.

An alternative to count warnings (probably quicker) is to use "cargo lintcheck".

### Warnings per KLOC

To print just the warnings-per-KLOC ratio as a single number, use the `--warning-per-KLOC` (or `-q`) option:
```bash
rust-diagnostics -q
```
This uses the [`tokei`](https://crates.io/crates/tokei) library to count lines of Rust code accurately (excluding comments and blank lines), then outputs the ratio as a floating-point number, e.g.:
```
12.34
```

## Warning Reduction Skills (Claude Code)

Two Claude Code skills automate the Li et al. ([arXiv:2310.11738](https://arxiv.org/abs/2310.11738)) methodology for iteratively reducing Clippy warning density. Warnings follow a Pareto distribution — the top-5 lint types typically account for >80% of all warnings. The target is **18 warnings/KLOC** (below the 21/KLOC crates-io average).

### Installation

```bash
# Clone the marketplace
git clone https://github.com/yijunyu/claude-skills-marketplace /tmp/claude-skills-marketplace

# Install the skill
mkdir -p ~/.claude/skills
cp -r /tmp/claude-skills-marketplace/skills/rust-warning-reduction ~/.claude/skills/
```

Alternatively, install as Claude Code commands (usable with `/warn-identify` and `/warn-reduce`):

```bash
mkdir -p ~/.claude/commands
# warn-identify
curl -o ~/.claude/commands/warn-identify.md \
  https://raw.githubusercontent.com/yijunyu/claude-skills-marketplace/main/skills/rust-warning-reduction/SKILL.md

# Or copy from this repo's .claude/commands/ after cloning
```

### `/warn-identify` — Analyse warning distribution

Invokes `rust-diagnostics --count` and `--warning-per-KLOC` to produce a ranked Pareto table showing which lint types dominate, their cumulative coverage, and whether auto-fix is available.

**Usage** (inside Claude Code):
```
/warn-identify
/warn-identify /path/to/project
```

**Example output:**
```
## Warning Analysis: .
Density: 7.30 warnings/KLOC  [TARGET: ≤18/KLOC | crates-io avg: 21/KLOC]

Rank  Count  Cumul%  Type                          Fixable?
   1      4    25%   arithmetic_side_effects
   2      3    44%   unwrap_used
   3      2    56%   as_conversions
   4      2    69%   cast_precision_loss
   5      2    81%   manual_checked_ops
   6      1    88%   default_numeric_fallback        [auto]
   7      1    94%   float_arithmetic
   8      1   100%   struct_excessive_bools

Top-1 coverage: 25% | Top-5 coverage: 81%
Status: BELOW 18/KLOC threshold
```

### `/warn-reduce` — Iterative elimination

Targets the highest-frequency lint type per pass, applies the paper's fix strategy, verifies with `cargo build`, recounts, and loops. At **18/KLOC** it pauses and asks whether to continue with long-tail removals.

**Usage** (inside Claude Code):
```
/warn-reduce
/warn-reduce /path/to/project
```

**Fix strategies** (from Li et al.):

| Warning Type | Strategy | Paper result |
|---|---|---|
| `default_numeric_fallback` | `cargo clippy --fix` / add type suffixes | 3,842 → 0 |
| `arithmetic_side_effects` | `wrapping_add/sub/div` rewrites (3 TXL rules) | 1,919 → 682 |
| `undocumented_unsafe_blocks` | Insert `// SAFETY:` comments via perl | 806 → 5 |
| `missing_debug_implementations` | Add `#[derive(Debug)]` via perl | 160 → 8 |
| `unwrap_used` / `expect_used` | `.expect("reason")` or `?` propagation | varies |
| `manual_checked_ops` | Replace guarded division with `.checked_div()` | varies |
| `cast_precision_loss` / `as_conversions` | Fixed-point integer arithmetic instead of float casts | varies |
| Any other lint | `cargo clippy --fix` → apply suggestion → targeted `#[allow]` | varies |

### Results on this project

Running `/warn-reduce` on `rust-diagnostics` itself (all changes in `src/main.rs`):

| Metric | Before | After |
|---|---|---|
| Warnings/KLOC | 7.30 | 0.00 |
| Total warnings | 16 | 0 |
| LOC | 2,192 | 2,195 |

Fixes applied:

| Warning | Count | Fix |
|---|---|---|
| `arithmetic_side_effects` | 4 | `saturating_add`, `saturating_mul`, `checked_div` |
| `manual_checked_ops` | 2 | `if let Some(x) = a.checked_div(b)` |
| `unwrap_used` | 3 | `.expect("ARGS mutex poisoned")` + `#[allow(clippy::expect_used)]` |
| `cast_precision_loss` + `as_conversions` + `float_arithmetic` + `default_numeric_fallback` | 5 | Replaced `total as f64 * 1000.0 / loc as f64` with fixed-point integer arithmetic |
| `struct_excessive_bools` | 1 | `#[allow]` with justification (CLI struct driven by `structopt`) |

Since the project started below the 18/KLOC threshold, all 16 warnings were in the long-tail phase and were eliminated in a single pass.

## Acknowledgement

- [David Wood](https://davidtw.co) offered the idea that we can use the `--message-format=json` option to get diagnostic information from the Rust compiler, which saves tremendous effort in modifying the Rust compiler. Now our solution is kind of independent from the Rust compiler implementations;
- [Mara Bos](https://github.com/m-ou-se) provided some hints on how to fix `unwrap()` warnings using `if-let` statements;
- [Amanieu d'Antras](https://github.com/Amanieu) provided some explanation for the necessity of certain clippy rules in practice, he also improves the performance of the underlying BTreeMap.
- [Guillaume Gomez](https://github.com/GuillaumeGoemz) explained the current role of `cargo clippy --fix` rules.
- Dr Chunmiao Li implemented the refactoring rule `unwrapped_used.txl` to fix the corresponding warnings automatically.
- Haitao Wu made some improvement on the `unwrapped_used.txl` rule.
- [Dr Nghi Bui](https://github.com/bdqnghi) suggested an idea to create mixed pairs.
- [Josh Triplett](https://github.com/joshtriplett) implemented the underlying `git2-rs` which wraps the `libgit2` library in Rust.
- Dr Dong Qiu requested the `--warning-per-KLOC` (`-q`) feature to measure warning density using the `tokei` library.
