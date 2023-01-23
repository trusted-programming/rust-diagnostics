# rust-diagnostics

This is a utility to insert diagnostics of code fragments as comments in Rust
code and checks how a warning/error in the diagnostics has been fixed in git
commit history.

Rust compiler displays many diagnostics to the console, using file name and
line numbers to indicate their exact locations. Without an IDE, it requires a
programmer to go back and forth between command console and the editor. 

This utility inserts the diagnostic messages in-place, which could enable
transformer-based machine learning approaches to analyse Rust diagnostic
semantics.

Through additional arguments, this utility also checks how a warning found
in revision r1 has been manually fixed by a revision r2. 

Currently we integrate the utility with `clippy` and `git-rs`.

## Installation
```bash
cargo install rust-diagnostics
```

## Usage:

The full synoposis of the command is shown below.
```bash
rust-diagnostics [--patch <commit_id> [--confirm] [--pair] [--function] [--single] ]
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
note: if this value is an `Err`, it will panic
for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
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
#[Warning(clippy::unwrap_used)
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
#[Warning(clippy::unwrap_used)
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

## Acknowledgement

- Thanks for [David Wood](https://davidtw.co), who offered the idea that we can use the `--message-format=json` option to get diagnostic information from the Rust compiler, which saves tremendous effort in modifying the Rust compiler. Now our solution is kind of independent from the Rust compiler implementations;
- Thanks for [Mara Bos](https://github.com/m-ou-se), who provided some hints on how to fix `unwrap()` warnings using `if-let` statements;
- Thanks for [Amanieu d'Antras](https://github.com/Amanieu), who provided some explanation for the necessity of certain clippy rules in practice.
- Thanks for [Josh Triplett](https://github.com/joshtriplett), who implemented `git-rs` which wraps the `libgit2` library to use in Rust.
- Thanks for Dr Chunmiao Li, who implemented `unwrapped_used.txl` refactoring rule to tackle fix the corresponding warning.
