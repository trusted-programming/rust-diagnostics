# rust-diagnostics

This is a utility to insert diagnostics of code fragments as comments in Rust
code and checks whether a warning/error in the diagnostics has been fixed in
git commit history.

Rust compiler displays many diagnostics to the console, using file name and
line numbers to indicate their exact locations. Without an IDE, it requires a
programmer to go back and forth between command console and the editor. 

This utility inserts the diagnostic messages in-place, which could enable
transformer-based machine learning approaches to analyse Rust diagnostic
semantics.

Through additional arguments, this utility also checks whether a warning found
in revision r1 has been manually fixed by a revision r2. 

Currently we integrate the utility with `clippy` and `git-rs`.

## optional feature: `fix`
Automated fix of warnings by `clippy` could be recorded as transformations,
including the programs before and after of fixes. Furthermore, scope of such
transformations are narrowed down to the individual items, making it easier to
spot whether the exact warnings get fixed or not. The remaining unfixed
warnings are still kept in the transformed results.

## Installation
```bash
cargo install rust-diagnostics
```

## Usage:
```bash
rust-diagnostics [--patch <commit_id> [--confirm] [--pair] [--function] [--single] ]
```

### Inserting warnings info into Rust code

The [commented
code](https://github.com/yijunyu/rust-diagnostics/blob/main/diagnostics/src/main.rs)
is generated from the [Rust
code](https://github.com/yijunyu/rust-diagnostics/blob/main/src/main.rs).

Note that this is a result of applying the utilility on its own implementation,
i.e., eating our own dog food. We have manually resolved all the clippy
warnings according to the specified clippy rules, except for the one on
`dbg_macro` to show the results as an example:

```rust
                                    /*#[Warning(clippy::dbg_macro)*/dbg!(&r)/*
#[Warning(clippy::dbg_macro)
note: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#dbg_macro
the lint level is defined here
ensure to avoid having uses of it in version control*/;
```
contains a `Warning` as the diagnostic code, and `clippy::dbg_macro` as the name of the lint rule violated by the code `dbg!(&msg)`. 

### Analyse the manually fixed warnings from change history

If you inspect the code and wonder whether revision r2 has fixed the warning of revision r1, 
you can use `git log -p` to identify the revisions' commit id first. Then run
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm
```
The output includes the count of warnings of $r1 and the hunks between $r1..$r2
that matters to fix the warnings listed in front of the hunks.

For example, the output will be in the `git diff` format:

```
There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
```

#### Generate pair format using the `--pair` option
Furthermore, using the `--pair` option changes the patch into a pair of 
code before and after the change:
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm --pair
```

For example, it will print
```
There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
```
Note. To avoid possible clash with existing code, in the separator we use the hash key `19a3477889393ea2cdd0edcb5e6ab30c`, which is created from the command
```bash
echo rust-diagnostics | md5sum 
```

#### Generate function context using the `--function` option
The pair may be too terse to learn, we use the `--function` option to
print the function surrounding the patch as its context:
```bash
git checkout $r1
rust-diagnostics --patch $r2 --confirm --pair [--function | -W]
```

For example, it will print the following instead:
```
There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
fn main() {


    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
```

### (optional) Generating inputs and outputs of warning fixes by `cargo clippy --fix`
This requires that the 'fix’ feature being enabled when building the tool.

The code snippets before fix are listed as `*.2.rs`, and after fix are listed
as `*.3.rs` under the `transform/foo/` folder, where `foo.rs` is the Rust code
that contains the fixed warnings.

### (optional) Inherit Rustc flags to analyse diagnostics 
This requires that the 'rustc_flags’ feature being enabled when building the tool.

Rustc flags used in `.cargo/config` are typically inherited by the cargo
clippy. In this way one can avoid typing multiple `-Wclippy::...` options from
the command line. Using `rustc_flags` feature it is possible to inherit them
from the compiler options.

## Acknowledgement

- Thanks for [David Wood](https://davidtw.co), who offered the idea that we can use the `--message-format=json` option to get diagnostic information from the Rust compiler, which saves tremendous effort in modifying the Rust compiler. Now our solution is kind of independent from the Rust compiler implementations;
- Thanks for [Mara Bos](https://github.com/m-ou-se), who provided some hints on how to fix `unwrap()` warnings using `if-let` statements;
- Thanks for [Amanieu d'Antras](https://github.com/Amanieu), who provided some explanation for the necessity of certain clippy rules in practice.
