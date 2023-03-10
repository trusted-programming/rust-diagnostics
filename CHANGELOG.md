## Change Log
- [ ] Infer return type of functions and map them to if-let test: Result => Ok(), Option => Some()
================= Release 0.1.11
- [x] Use cargo-release and cargo-dist to create the new release
================= Release 0.1.10
- [x] Put header ahead of before and after contexts of the hunk pairs
- [x] Updated the TXL refactoring transformation and Rust grammaar
- [x] Add '--fix' option to invoke flags related to all machine applicable fix rules from rust-clippy project
- [x] Count clippy fixes
- [x] Cache computed data by serialized/deserialize diff hunks
- [x] Print pair pragma with ## so it is easier to differentiate it with the pragma inside warning hints
- [x] Swap the order of diagnostic folder name from `$id/diagnostics` to `diagnostics/$id`
- [x] Fix the ordering of diagnostic hints so that the regression test will be repeatable 
- [x] Add an option `--location` to markup the old_context with the warnings
- [x] Add an option `--mixed` to save the messages into a file in case the information can be useful context
- [x] Save the messages into `[$hash]/diagnostics.json` to avoid rerun the compilation for changes of the tool features
      where $hash is the hash id of the current work copy of a git repository
================= Release 0.1.9
- [x] Remove dependencies requiring nightly, now only depends on stable rust
- [x] Generate test results in random folders so there is less interference between tests
- [x] Removed the optional features
================= Release 0.1.8
- [x] Refactor the code to simplify the logic by introduce a Hunk structure as unified state of various analyses
- [x] Add an option `--single` to select only those diff records that patch exactly one warning
- [x] Test rust-diagnostics on its own history
- [x] Add an option `-W` to generate diff records with the surrounding function contexts (which was a feature of `git diff` but not supported by `libgit2`
- [x] Add an option `--pair` to generate diff records into code pairs
================= Release 0.1.7
- [x] Add a `--patch <id> --commit` option to print out the hunks only when they have been fixed by the revision <id>
- [x] Print out the hunks only when they are relevant to the spans of warning locations
- [x] Make the `--patch <id>` feature to print out the patch of HEAD..<id>
- [x] Add a `--patch <id>` option to print out the patch of HEAD..<id> where <id> is a commit id and HEAD is the current work tree
- [x] Move the implementation of optional functionalities into rustc_flags, fix features to reduce the dependencies
- [x] Perform `rustfmt` to output of TXL transformations
================= Release 0.1.6
- [x] Integrate with transformation systems to fix some of the warnings not yet fixed by clippy
================= Release 0.1.5
- [x] Call fix only when the number of warnings is larger than 0
- [x] Get RustCFlags from `cargo`
- [x] Integrate with `txl` through `txl-rs`
- [x] List the fixed warnings and keep the remaining warnings in the output 
- [x] Select only the relevant marked rules
- [x] list the marked rules applied to the transformations
- [x] Store the transformation results before and after `clippy --fix` into the `transform` folder 
- [x] Measure the number of warnings per KLOC through `count_diagnostics.sh`
================= Release 0.1.4
- [x] Separate the output files into a different folder, so as to keep using the same ".rs" file extension
- [x] Insert rendered diagnostic messages into the second comment.
================= Release 0.1.3
================= Release 0.1.2
================= Release 0.1.1
================= Release 0.1.0
- [x] Name the comments by the lint rules, and insert the rendered diagnostics into the second comment
- [x] Insert two comments around the diagnositic spans
