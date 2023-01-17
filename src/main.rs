#![feature(internal_output_capture)]
use cargo_metadata::{diagnostic::Diagnostic, Message};
use once_cell::sync::Lazy;
use serde::Serialize;
use std::{
    collections::BTreeMap,
    fs::read_to_string,
    path::PathBuf,
    process::{Command, Stdio},
    sync::Mutex,
};

#[cfg(patch)]
mod patch {
    use git2::{Commit, DiffOptions, ObjectType, Repository, Signature, Time};
    use git2::{DiffFormat, Error, Pathspec};
}

mod language;
use tree_sitter::QueryCursor;
use tree_sitter_parsers::parse;

#[cfg(rustc_flags)]
mod rustc_flags {
    use cargo::util::command_prelude::{ArgMatchesExt, Config};
    use cargo::{
        core::compiler::{CompileKind, RustcTargetData},
        util::command_prelude::{CompileMode, ProfileChecking},
    };
    use clap::Arg;
}

use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
struct Args {
    #[structopt(name = "flags", long)]
    /// warnings concerning the warning flags
    flags: Vec<String>,
    #[structopt(name = "patch", long)]
    /// reduce patch id to hunks that may be relevant to the warnings
    patch: Option<String>,
    #[structopt(name = "confirm", long)]
    /// confirm whether the related warnings of current revision are indeed fixed by the patch
    confirm: bool,
    #[structopt(name = "pair", long)]
    /// display diff hunks as a pair separated by a special marker `=== 19a3477889393ea2cdd0edcb5e6ab30c ===`
    /// which is the checksum by `echo rust-diagnostics | md5sum`
    pair: bool,
    #[structopt(name = "function", long)]
    #[structopt(name = "W", short)]
    /// generate diff records with the surrounding function contexts (which was a feature of `git diff` but not supported by `libgit2`
    function: bool,
    #[structopt(name = "single", long)]
    /// select only those diff records that patch exactly one warning
    single: bool,
}

static ARGS: Lazy<Mutex<Vec<Args>>> = Lazy::new(|| Mutex::new(vec![]));

fn get_args() -> Vec<Args> {
    set_args();
    ARGS.lock().unwrap().to_vec()
}

fn set_args() {
    if ARGS.lock().unwrap().len() == 0 {
        let params = Args::from_args();
        ARGS.lock().unwrap().push(params);
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Warning {
    name: String,
    start: usize,
    end: usize,
    suggestion: String,
    note: String,
    start_line: usize,
    end_line: usize,
    // start_column: usize,
    // end_column: usize,
    fixed: bool,
}

// insert diagnostic code as an markup element around the code causing the diagnostic message
fn markup(source: &[u8], map: Vec<Warning>) -> Vec<u8> {
    let mut output = Vec::new();
    for (i, c) in source.iter().enumerate() {
        for m in &map {
            // deal with the element
            if m.start <= i && i < m.end && i == m.start {
                output.extend(format!("/*{}*/", m.name).as_bytes());
            }
            if m.end == i {
                output.extend(
                    format!(
                        "/*\n{}{}{}*/",
                        m.name,
                        if m.suggestion == "None" {
                            EMPTY_STRING.to_string()
                        } else {
                            format!(
                                "\nsuggestion: {}",
                                m.suggestion.replace("\\n", "\n").replace('\"', "")
                            )
                        },
                        if m.note == "None" {
                            EMPTY_STRING.to_string()
                        } else {
                            format!("\nnote: {}", m.note.replace("\\n", "\n").replace('\"', ""))
                        }
                    )
                    .as_bytes(),
                )
            }
        }
        output.push(*c);
    }
    output
}

#[cfg(fix)]
use std::num::Wrapping;

#[cfg(fix)]
// list the relevant rules as comments
fn markup_rules(start: Wrapping<usize>, end: Wrapping<usize>, map: Vec<Warning>) -> Vec<u8> {
    let mut output = Vec::new();
    for m in &map {
        if start <= Wrapping(m.start) && Wrapping(m.end) <= end {
            output.extend(format!("/*{}*/\n", m.name).as_bytes());
        }
    }
    output
}

#[derive(Debug, Serialize, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExtractedNode<'query> {
    name: &'query str,
    start_byte: usize,
    start_line: usize,
    end_byte: usize,
    end_line: usize,
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LineRange {
    start_byte: usize,
    start_line: usize,
    end_line: usize,
}

// Split up the Rust source_file into individual items, indiced by their start_byte offsets
fn splitup(source: String) -> anyhow::Result<BTreeMap<LineRange, String>> {
    let mut output: BTreeMap<LineRange, String> = BTreeMap::new();
    if let Ok(s) = std::str::from_utf8(source.as_bytes()) {
        let tree = parse(s, "rust");
        if let Ok(query) = language::Language::Rust.parse_query(
            "([
  (const_item) @fn
  (attribute_item) @fn
  (inner_attribute_item) @fn
  (mod_item) @fn
  (foreign_mod_item) @fn
  (struct_item) @fn
  (union_item) @fn
  (enum_item) @fn
  (type_item) @fn
  (function_item) @fn
  (function_signature_item) @fn
  (impl_item) @fn
  (trait_item) @fn
  (static_item) @fn
        ])",
        ) {
            let captures = query.capture_names().to_vec();
            let mut cursor = QueryCursor::new();
            let extracted = cursor
                .matches(&query, tree.root_node(), source.as_bytes())
                .flat_map(|query_match| query_match.captures)
                .map(|capture| {
                    if let Ok(idx) = usize::try_from(capture.index) {
                        let name = &captures[idx];
                        let node = capture.node;
                        Ok(ExtractedNode {
                            name,
                            start_byte: node.start_byte(),
                            start_line: node.start_position().row,
                            end_byte: node.end_byte(),
                            end_line: node.end_position().row,
                        })
                    } else {
                        Ok(ExtractedNode {
                            name: "",
                            start_byte: 0,
                            start_line: 0,
                            end_byte: 0,
                            end_line: 0,
                        })
                    }
                })
                .collect::<anyhow::Result<Vec<ExtractedNode>>>()?;
            for m in extracted {
                if m.name == "fn" {
                    if let Ok(code) =
                        std::str::from_utf8(&source.as_bytes()[m.start_byte..m.end_byte])
                    {
                        let lr = LineRange {
                            start_byte: m.start_byte,
                            start_line: m.start_line,
                            end_line: m.end_line,
                        };
                        output.insert(lr, code.to_string());
                    }
                }
            }
        }
    }
    Ok(output)
}

#[cfg(fix)]
mod fix {
    use anyhow::Result;
    // restore the original file
    fn restore_original(file_name: &String, content: &String) {
        std::fs::write(file_name, content).ok();
    }
}
fn to_diagnostic(map: &mut BTreeMap<String, Vec<Warning>>, args: Vec<String>) {
    if let Ok(mut command) = Command::new("cargo")
        .args(args)
        .stdout(Stdio::piped())
        .spawn()
    {
        if let Some(take) = command.stdout.take() {
            let reader = std::io::BufReader::new(take);
            for message in cargo_metadata::Message::parse_stream(reader).flatten() {
                if let Message::CompilerMessage(msg) = message {
                    for s in msg.message.spans {
                        if let Ok(x) = usize::try_from(s.byte_start) {
                            if let Ok(y) = usize::try_from(s.byte_end) {
                                if let Some(message_code) = &msg.message.code {
                                    let r = Warning {
                                        name: format!(
                                            "#[{:?}({})",
                                            msg.message.level,
                                            message_code.clone().code
                                        ),
                                        start: x,
                                        start_line: s.line_start,
                                        // start_column: s.column_start,
                                        end: y,
                                        end_line: s.line_end,
                                        // end_column: s.column_end,
                                        suggestion: format!("{:?}", s.suggested_replacement),
                                        note: format!("{:?}", sub_messages(&msg.message.children)),
                                        fixed: false,
                                    };
                                    let filename = s.file_name;
                                    match map.get_mut(&filename) {
                                        Some(v) => v.push(r),
                                        None => {
                                            let v = vec![r];
                                            map.insert(filename, v);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        command.wait().ok();
    }
}

#[cfg(rustc_flags)]
// Find all the RUSTC_FLAGS enabled by `cargo`
// Adapted from https://github.com/rust-lang/cargo/blob/master/src/bin/cargo/commands/build.rs
fn rustflags() -> Vec<String> {
    let args = clap::Command::new("rust-diagnostics")
        .arg(Arg::new("cfg").short('c').takes_value(true))
        .get_matches(); // builds the instance of ArgMatches
    let config = Option::unwrap(Config::default().ok());
    let ws = Option::unwrap(Result::ok(args.workspace(&config)));
    let compile_opts = Option::unwrap(Result::ok(args.compile_options(
        &config,
        CompileMode::Build,
        Some(&ws),
        ProfileChecking::Custom,
    )));
    // if compile_opts.build_config.export_dir.is_some() { config.cli_unstable(); }
    let target_data = Option::unwrap(Result::ok(RustcTargetData::new(
        &ws,
        &compile_opts.build_config.requested_kinds,
    )));
    let target_info = target_data.info(CompileKind::Host);
    target_info.rustflags.clone()
}

// markup all warnings into diagnostics
fn diagnose_all_warnings(flags: Vec<String>) -> BTreeMap<String, Vec<Warning>> {
    let mut args = vec![
        "clippy".to_string(),
        "--message-format=json".to_string(),
        "--".to_string(),
    ];
    for flag in flags {
        args.push(format!("-Wclippy::{}", flag));
    }
    let mut map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
    to_diagnostic(&mut map, args);
    if !map.is_empty() {
        let mut markup_map: BTreeMap<String, String> = BTreeMap::new();
        for file in map.keys() {
            if let Ok(source) = read_to_string(file) {
                if let Some(v) = map.get(file) {
                    let markedup = &markup(source.as_bytes(), v.to_vec());
                    if let Ok(s) = std::str::from_utf8(markedup) {
                        markup_map.insert(file.to_string(), s.to_string());
                    }
                }
            }
        }
        for file in map.keys() {
            if markup_map.contains_key(file) {
                let markedup = &markup_map[file];
                let file_name = PathBuf::from("diagnostics").join(file);
                // println!("Marked warning(s) into {:?}", &file_name);
                if let Some(p) = file_name.parent() {
                    if !p.exists() {
                        std::fs::create_dir_all(p).ok();
                    }
                }
                std::fs::write(&file_name, markedup).ok();
            }
        }
    }
    map
}

#[cfg(fix)]
// process warnings from one RUSTC_FLAG at a time
fn fix_warnings(flags: Vec<String>, map: &BTreeMap<String, Vec<Warning>>) {
    for flag in &flags {
        let mut flagged_map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
        for file in map.keys() {
            if let Some(v) = map.get(file) {
                let mut new_v = Vec::new();
                for r in v {
                    let rule = &flag[2..];
                    if r.name == format!("#[Warning({})", &rule) {
                        let r1 = Warning {
                            name: r.name.clone(),
                            start: r.start,
                            end: r.end,
                            suggestion: r.suggestion.clone(),
                            note: r.note.clone(),
                        };
                        new_v.push(r1);
                    }
                }
                if !new_v.is_empty() {
                    flagged_map.insert(file.to_string(), new_v);
                }
            }
        }
        if !flagged_map.is_empty() {
            let mut origin_map: BTreeMap<String, String> = BTreeMap::new();
            let mut markup_map: BTreeMap<String, String> = BTreeMap::new();
            for file in flagged_map.keys() {
                if let Ok(source) = read_to_string(file) {
                    if let Some(v) = flagged_map.get(file) {
                        let markedup = &markup(source.as_bytes(), v.to_vec());
                        origin_map.insert(file.to_string(), source);
                        if let Ok(s) = std::str::from_utf8(markedup) {
                            markup_map.insert(file.to_string(), s.to_string());
                        }
                    }
                }
                if flag == "-Wclippy::unwrap_used" {
                    fix_unwrap_used(file);
                }
            }
            let mut args = vec![
                "clippy".to_string(),
                "--message-format=json".to_string(),
                "--fix".to_string(),
                "--allow-dirty".to_string(),
                "--allow-no-vcs".to_string(),
                "--broken-code".to_string(),
                "--".to_string(),
            ];
            for flag in &flags {
                args.push(flag.to_string());
            }
            let mut fixed_map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
            to_diagnostic(&mut fixed_map, args);
            for file in flagged_map.keys() {
                if let Ok(source) = read_to_string(file) {
                    let input = &origin_map[file];
                    let output = source.as_bytes();
                    if let Some(warnings) = flagged_map.get(file) {
                        if let Some(fixes) = fixed_map.get(file) {
                            let mut fixed_warnings = Vec::new();
                            let mut remaining_warnings = Vec::new();
                            for w in warnings {
                                let mut found = false;
                                for f in fixes {
                                    if w.name == f.name {
                                        found = true;
                                        remaining_warnings.push(f.clone());
                                        break;
                                    }
                                }
                                if !found {
                                    fixed_warnings.push(w.clone());
                                }
                            }
                            to_fix(
                                &flag,
                                file,
                                warnings.to_vec(),
                                fixed_warnings.clone(),
                                remaining_warnings.clone(),
                                input,
                                output,
                            );
                        }
                    }
                }
            }
            for file in flagged_map.keys() {
                let input = &origin_map[file];
                restore_original(file, input);
            }
        }
    }
}

// run the following bash commands
// ```bash
// git checkout $commit_id
// ```
fn checkout(commit_id: git2::Oid) {
    let repo = git2::Repository::open(".").unwrap();
    let commit = repo.find_commit(commit_id);
    repo.reset(
        commit.unwrap().as_object(),
        git2::ResetType::Hard,
        Some(
            git2::build::CheckoutBuilder::new()
                .force()
                .remove_untracked(true),
        ),
    )
    .ok();
}

#[cfg(feature = "patch")]
fn get_flags() -> Vec<String> {
    let v = get_args();
    let args = &v[0];
    let mut flags = args.flags.clone();
    if flags.is_empty() {
        flags = vec![
            "ptr_arg".to_string(),
            "too_many_arguments".to_string(),
            "missing_errors_doc".to_string(),
            "missing_panics_doc".to_string(),
            "await_holding_lock".to_string(),
            "await_holding_refcell_ref".to_string(),
            "assertions_on_constants".to_string(),
            "large_stack_arrays".to_string(),
            "match_bool".to_string(),
            "needless_bitwise_bool".to_string(),
            "empty_enum".to_string(),
            "empty_enum".to_string(),
            "enum_clike_unportable_variant".to_string(),
            "enum_glob_use".to_string(),
            "enum_glob_use".to_string(),
            "exhaustive_enums".to_string(),
            "cast_precision_loss".to_string(),
            "float_arithmetic".to_string(),
            "float_cmp".to_string(),
            "float_cmp_const".to_string(),
            "imprecise_flops".to_string(),
            "suboptimal_flops".to_string(),
            "as_conversions".to_string(),
            "cast_lossless".to_string(),
            "cast_possible_truncation".to_string(),
            "cast_possible_wrap".to_string(),
            "cast_precision_loss".to_string(),
            "ptr_as_ptr".to_string(),
            "default_numeric_fallback".to_string(),
            "checked_conversions".to_string(),
            "integer_arithmetic".to_string(),
            "cast_sign_loss".to_string(),
            "modulo_arithmetic".to_string(),
            "exhaustive_structs".to_string(),
            "struct_excessive_bools".to_string(),
            "unwrap_used".to_string(),
            "expect_used".to_string(),
            "expect_fun_call".to_string(),
            "large_types_passed_by_value".to_string(),
            "fn_params_excessive_bools".to_string(),
            "trivially_copy_pass_by_ref".to_string(),
            "inline_always".to_string(),
            "inefficient_to_string".to_string(),
            "dbg_macro".to_string(),
            "wildcard_imports".to_string(),
            "self_named_module_files".to_string(),
            "mod_module_files".to_string(),
            "disallowed_methods".to_string(),
            "disallowed_script_idents".to_string(),
            "disallowed_types".to_string(),
        ];
    }
    flags
}

#[cfg(feature = "patch")]
fn print_warning_count(all_warnings: BTreeMap<String, Vec<Warning>>) {
    let mut count = 0;
    all_warnings.iter().for_each(|(_k, v)| {
        count += v.len();
    });
    println!(
        "There are {} warnings in {} files.",
        count,
        all_warnings.len()
    );
}

#[cfg(feature = "patch")]
fn get_current_id() -> Option<git2::Oid> {
    if let Ok(repo) = git2::Repository::open(".") {
        if let Ok(x) = repo.head() {
            if let Some(y) = x.target() {
                Some(y)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

#[cfg(feature = "patch")]
fn get_diff(repo: &git2::Repository, id: String) -> Option<git2::Diff> {
    let c1 = repo
        .find_commit(repo.head().unwrap().target().unwrap())
        .unwrap();
    let c2 = repo.find_commit(git2::Oid::from_str(&id).unwrap()).unwrap();
    let a = Some(c1.tree().unwrap());
    let b = Some(c2.tree().unwrap());
    let mut diffopts2 = git2::DiffOptions::new();
    diffopts2.context_lines(0);
    if let Ok(diff) = repo.diff_tree_to_tree(a.as_ref(), b.as_ref(), Some(&mut diffopts2)) {
        Some(diff)
    } else {
        None
    }
}

#[derive(Clone)]
struct Hunk {
    patch_text: String, // c.f. git-diff patch
    header: String,    // c.f. header
    old_text: String, // the old version of the patch
    new_text: String, // the new version of the patch
    old_start_line: u32, // start line number of the old version
    old_end_line: u32, // end line number of the old version
    new_start_line: u32, // start line number of the new version
    new_end_line: u32, // end line number of the new version
    _context: String,    // c.f. git-diff -W the enclosing function
    old_context: String, // the old version with surrounding function
    new_context: String, // the new version with surrounding function
    warnings: String, // the related warning(s) in currenet version
    n_warnings: u32, // the number of related warning(s) in currenet version
    fixed: bool, // whether the related hunk will be fixed by the new version
}
static EMPTY_STRING: Lazy<String> = Lazy::new(|| "".to_string());

impl std::fmt::Display for Hunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = get_args();
        let args = &v[0];
        if ! args.confirm || self.fixed {
            if ! args.single && self.n_warnings > 0 || self.n_warnings == 1 {
                if args.function {
                    write!(f, "{}{}{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
                        self.warnings, self.header, self.old_context, self.new_context)
                } else if args.pair {
                    write!(f, "{}{}{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
                        self.warnings, self.header, self.old_text, self.new_text)
                } else {
                    write!(f, "{}{}{}", self.warnings, self.header, self.patch_text)
                }
            } else { // no warning or more than one warnings
                write!(f, "")
            }
        } else { // not fixed
            write!(f, "")
        }
    }
}

#[cfg(feature = "patch")]
fn print_hunks(map: BTreeMap<String, Vec<Hunk>>) 
{
    map.iter().for_each(|(k, v)| {
        println!("{k}");
        v.iter().for_each(|h| {
            println!("{h}");
        });
    });
}

#[cfg(feature = "patch")]
fn get_hunks(diff: git2::Diff) -> BTreeMap<String, Vec<Hunk>> {
    let mut map = BTreeMap::new();
    let mut hunks: Vec<Hunk> = Vec::new();
    let mut cur_filename = EMPTY_STRING.to_string();
    let mut cur_hunk = Hunk {
        patch_text: EMPTY_STRING.to_string(),
        old_text: EMPTY_STRING.to_string(),
        old_start_line: 0,
        old_end_line: 0,
        new_text: EMPTY_STRING.to_string(),
        new_start_line: 0,
        new_end_line: 0,
        header: EMPTY_STRING.to_string(),
        _context: EMPTY_STRING.to_string(),
        old_context: EMPTY_STRING.to_string(),
        new_context: EMPTY_STRING.to_string(),
        warnings: EMPTY_STRING.to_string(),
        n_warnings: 0,
        fixed: false,
    }; 
    diff.print(git2::DiffFormat::Patch, |delta, hunk, line| {
        let path = delta.old_file().path().unwrap();
        let filename = path.to_str().unwrap().to_string();
        if let Some(hunk) = hunk {
            if cur_filename != filename || cur_hunk.old_start_line != hunk.old_start() {
                // reinitialize
                let hh = cur_hunk.clone();
                hunks.push(hh);
                let mut cur_hunk = Hunk {
                    patch_text: EMPTY_STRING.to_string(),
                    old_text: EMPTY_STRING.to_string(),
                    old_start_line: 0,
                    old_end_line: 0,
                    new_text: EMPTY_STRING.to_string(),
                    new_start_line: 0,
                    new_end_line: 0,
                    header: EMPTY_STRING.to_string(),
                    _context: EMPTY_STRING.to_string(),
                    old_context: EMPTY_STRING.to_string(),
                    new_context: EMPTY_STRING.to_string(),
                    warnings: EMPTY_STRING.to_string(),
                    n_warnings: 0,
                    fixed: false,
                }; 
                cur_hunk.old_start_line = hunk.old_start();
                cur_hunk.old_end_line = hunk.old_start() + hunk.old_lines();
                cur_hunk.new_start_line = hunk.new_start();
                cur_hunk.new_end_line = hunk.new_start() + hunk.new_lines();
                if cur_filename != filename {
                    map.insert(filename.clone(), hunks.clone());
                    cur_filename = filename;
                    hunks = Vec::new();
                }
            }
            let content = std::str::from_utf8(line.content()).unwrap();
            cur_hunk
                .patch_text
                .push_str(format!("{}{content}", line.origin()).as_str());
            match line.origin() {
                ' ' => {
                    cur_hunk.old_text.push_str(content);
                    cur_hunk.new_text.push_str(content);
                }
                '+' => {
                    cur_hunk.new_text.push_str(content);
                }
                '-' => {
                    cur_hunk.old_text.push_str(content);
                }
                _ => {
                    // cur_hunk.old_text.push_str(content);
                }
            }
        }
        true
    })
    .ok();
    map
}

#[cfg(feature = "patch")]
fn reset_hunk(
    h: &git2::DiffHunk,
    p: &std::path::Path,
    mut prev_l: i32,
    prefix: &mut String,
    suffix: &mut String,
    pair: &mut Vec<String>,
    related_warnings: &mut std::collections::HashSet<Warning>,
) {
    let v = get_args();
    let args = &v[0];
    let function_items = get_function_items(p).unwrap();
    if args.pair {
        let mut prev_f = EMPTY_STRING.to_string();
        for k in function_items.keys() {
            let v = function_items.get(k).unwrap();
            prev_f = v.clone();
            prev_l = i32::try_from(h.old_start() - 1).unwrap();
            let prev_r = i32::try_from(h.old_start() + h.old_lines() - 1).unwrap();
            if k.start_line <= usize::try_from(prev_l).unwrap()
                && usize::try_from(prev_r).unwrap() <= k.end_line
            {
                break;
            }
        }
        let lines: Vec<&str> = prev_f.split('\n').collect();
        let lines_deleted: Vec<&str> = pair[0].split('\n').collect();
        *prefix = EMPTY_STRING.to_string();
        *suffix = EMPTY_STRING.to_string();
        let ll = i32::try_from(lines_deleted.len()).unwrap();
        let n = usize::try_from(i32::try_from(h.old_start()).unwrap() - prev_l).unwrap();
        let m =
            usize::try_from(i32::try_from(h.old_start() + h.old_lines()).unwrap() - prev_l + ll)
                .unwrap();
        for i in 0..n {
            *prefix = format!("{}{}\n", *prefix, lines[i]);
        }
        for i in (m - 1)..lines.len() {
            *suffix = format!("{}{}\n", *suffix, lines[i]);
        }
        if (!pair[0].is_empty() || !pair[1].is_empty())
            && (!args.single || related_warnings.len() == 1)
        {
            print_pair(pair.clone(), prefix.clone(), suffix.clone());
        }
    }
    *pair = vec![EMPTY_STRING.to_string(), EMPTY_STRING.to_string()];
    if !args.single || related_warnings.len() == 1 {
        related_warnings.iter().for_each(|m| {
            println!("{}", m.name);
        });
    }
    *related_warnings = std::collections::HashSet::new();
}

#[cfg(feature = "patch")]
fn handle_patch(mut all_warnings: BTreeMap<String, Vec<Warning>>, flags: Vec<String>) {
    let v = get_args();
    let args = &v[0];
    if let Some(id) = &args.patch {
        let mut prev_hunk = 0;
        let mut related_warnings = std::collections::HashSet::new();
        let repo = git2::Repository::open(".").unwrap();
        let old_id = get_current_id().unwrap();
        let diff = get_diff(&repo, id.clone()).unwrap();
        // let _hunks = get_hunks(diff);
        diff.print(git2::DiffFormat::Patch, |delta, hunk, line| {
            let p = delta.old_file().path().unwrap();
            let mut overlap = false;
            if let Some(h) = hunk {
                all_warnings.iter_mut().for_each(|(k, v)| {
                    v.iter_mut().for_each(|m| {
                        if std::path::Path::new(k) == p
                            && usize::try_from(h.old_start()).unwrap() <= m.end_line
                            && usize::try_from(h.old_start() + h.old_lines()).unwrap()
                                >= m.start_line
                        {
                            overlap = true;
                            m.fixed = true;
                            related_warnings.insert(m.clone());
                        }
                    });
                });
                if overlap {
                    if prev_hunk == 0 || prev_hunk != h.old_start() {
                        related_warnings.iter().for_each(|m| {
                            if !args.confirm {
                                println!("{}", m.name);
                            }
                        });
                        related_warnings = std::collections::HashSet::new();
                    }
                    if !args.confirm {
                        match line.origin() {
                            ' ' | '+' | '-' => print!("{}", line.origin()),
                            _ => {}
                        }
                        print!("{}", std::str::from_utf8(line.content()).unwrap());
                    }
                    prev_hunk = h.old_start();
                    true
                } else {
                    related_warnings = std::collections::HashSet::new();
                    prev_hunk = h.old_start();
                    true
                }
            } else {
                true
            }
        })
        .ok();
        if args.confirm {
            // We go through the 2nd pass, to output only those confirmed fixes
            let oid = git2::Oid::from_str(&id).unwrap();
            checkout(oid);
            let all_new_warnings = diagnose_all_warnings(flags);
            all_warnings.iter_mut().for_each(|(k1, v1)| {
                v1.iter_mut().for_each(|m1| {
                    if m1.fixed {
                        let mut confirmed = true;
                        all_new_warnings.iter().for_each(|(k2, v2)| {
                            v2.iter().for_each(|m2| {
                                if k1 == k2
                                    && m1.start_line <= m2.end_line
                                    && m1.end_line >= m2.start_line
                                {
                                    confirmed = false;
                                }
                            });
                        });
                        m1.fixed = confirmed;
                    }
                });
            });
            checkout(old_id);
            prev_hunk = 0;
            related_warnings = std::collections::HashSet::new();
            let mut pair = vec![EMPTY_STRING.to_string(), EMPTY_STRING.to_string()];
            let prev_l: i32 = 0;
            let mut prefix = EMPTY_STRING.to_string();
            let mut suffix = EMPTY_STRING.to_string();
            diff.print(git2::DiffFormat::Patch, |delta, hunk, line| -> bool {
                let p = delta.old_file().path().unwrap();
                let mut overlap = false;
                if let Some(h) = hunk {
                    all_warnings.iter_mut().for_each(|(k, v)| {
                        v.iter_mut().for_each(|m| {
                            if m.fixed
                                && std::path::Path::new(k) == p
                                && usize::try_from(h.old_start()).unwrap() <= m.end_line
                                && usize::try_from(h.old_start() + h.old_lines()).unwrap()
                                    >= m.start_line
                            {
                                // println!("@@{}:{}@@ overlaps with {}:{}", h.old_start(), h.old_lines(), m.start_line, m.end_line);
                                overlap = true;
                                related_warnings.insert(m.clone());
                            }
                        });
                    });
                    if overlap {
                        if prev_hunk == 0 || prev_hunk != h.old_start() {
                            reset_hunk(
                                &h,
                                &p,
                                prev_l,
                                &mut prefix,
                                &mut suffix,
                                &mut pair,
                                &mut related_warnings,
                            );
                        }
                        let content = std::str::from_utf8(line.content()).unwrap();
                        match line.origin() {
                            ' ' => {
                                if args.pair {
                                    if !args.single || related_warnings.len() == 1 {
                                        //remainder
                                        if !args.function {
                                            pair[0] = format!("{}{}", pair[0], content);
                                            pair[1] = format!("{}{}", pair[1], content);
                                        }
                                    }
                                } else if !args.single || related_warnings.len() == 1 {
                                    //remainder
                                    print!("{}", line.origin());
                                    print!("{content}");
                                }
                            }
                            '+' => {
                                if args.pair {
                                    if !args.single || related_warnings.len() == 1 {
                                        //remainder
                                        pair[1] = format!("{}{}", pair[1], content);
                                    }
                                } else if !args.single || related_warnings.len() == 1 {
                                    //remainder
                                    print!("{}", line.origin());
                                    print!("{content}");
                                }
                            }
                            '-' => {
                                if args.pair {
                                    if !args.single || related_warnings.len() == 1 {
                                        //remainder
                                        pair[0] = format!("{}{}", pair[0], content);
                                    }
                                } else if !args.single || related_warnings.len() == 1 {
                                    //remainder
                                    print!("{}", line.origin());
                                    print!("{content}");
                                }
                            }
                            _ => {
                                // @@
                                let p = delta.old_file().path().unwrap();
                                reset_hunk(
                                    &h,
                                    &p,
                                    prev_l,
                                    &mut prefix,
                                    &mut suffix,
                                    &mut pair,
                                    &mut related_warnings,
                                );
                                if args.pair {
                                    if !args.single || related_warnings.len() == 1 {
                                        //remainder
                                        if !args.function {
                                            pair[0] = format!("{}{}", pair[0], content);
                                        }
                                    }
                                } else {
                                    if !args.single || related_warnings.len() == 1 {
                                        //remainder
                                        print!("{content}");
                                    }
                                    related_warnings = std::collections::HashSet::new();
                                }
                            }
                        }
                        prev_hunk = h.old_start();
                        true
                    } else {
                        related_warnings = std::collections::HashSet::new();
                        prev_hunk = h.old_start();
                        true
                    }
                } else {
                    true
                }
            })
            .ok();
            if args.pair
                && (!pair[0].is_empty() || !pair[1].is_empty())
                && (!args.single || related_warnings.len() == 1)
            {
                //remainder
                print_pair(pair.clone(), prefix.clone(), suffix.clone());
            }
        }
    }
}

fn run() {
    remove_previously_generated_files("./diagnostics", "*.rs"); // marked up
    #[cfg(fix)]
    {
        remove_previously_generated_files("./original", "*.rs"); // before fix
        remove_previously_generated_files(".", "*.2.rs"); // transformed from
        remove_previously_generated_files(".", "*.3.rs"); // transformed to
    }
    let flags = get_flags();
    let all_warnings = diagnose_all_warnings(flags.clone());
    print_warning_count(all_warnings.clone());
    #[cfg(feature = "patch")]
    handle_patch(all_warnings, flags);
    #[cfg(fix)]
    fix_warnings(flags, &all_warnings);
}

fn get_function_items(p: &std::path::Path) -> Result<BTreeMap<LineRange, String>, anyhow::Error> {
    splitup(read_to_string(p).unwrap())
}

fn print_pair(pair: Vec<String>, prefix: String, suffix: String) {
    let v = get_args();
    let args = &v[0];
    if args.function {
        print!(
            "{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
            format!("{}{}{}", prefix, pair[0], suffix),
            format!("{}{}{}", prefix, pair[1], suffix)
        );
    } else {
        print!(
            "{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
            pair[0], pair[1]
        );
    }
}

// Run cargo clippy to generate warnings from "foo.rs" into temporary "foo.rs.1" files
fn main() {
    run();
}

#[cfg(fix)]
mod fix {
    extern crate reqwest;
    const URL: &str = "http://bertrust.s3.amazonaws.com/unwrap_used.txl";
    #[cfg(fix)]
    fn fix_unwrap_used(file: &str) {
        if !std::path::Path::new("unwrap_used.txl").exists() {
            if let Ok(resp) = reqwest::blocking::get(URL) {
                if let Ok(bytes) = resp.bytes() {
                    std::fs::write("unwrap_used.txl", bytes).ok();
                }
            }
        }
        let args = vec![
            "-q".to_string(),
            "-s".to_string(),
            "3000".to_string(),
            file.to_string(),
            "unwrap_used.txl".to_string(),
        ];
        if let Ok(output) = txl_rs::txl(ARGS) {
            std::fs::write(file, output).ok();
            if let Ok(command) = Command::new("rustfmt")
                .args([file])
                .stdout(Stdio::piped())
                .spawn()
            {
                if let Ok(_output) = command.wait_with_output() {
                    if let Ok(s) = std::fs::read_to_string(file) {
                        println!("{s}");
                    }
                }
            }
        }
    }
    fn to_fix(
        flag: &str,
        file: &String,
        warnings: Vec<Warning>,
        fixed_warnings: Vec<Warning>,
        remaining_warnings: Vec<Warning>,
        input: &String,
        output: &[u8],
    ) {
        let trans_name = PathBuf::from("transform")
            .join(flag.replace("-Wclippy::", ""))
            .join(file);
        let input_markedup = &markup(input.as_bytes(), warnings);
        let output_markedup = &markup(output, remaining_warnings);
        if let Ok(orig_items) = splitup(input_markedup) {
            if let Ok(output_items) = splitup(output_markedup) {
                if let Some(t) = trans_name.parent() {
                    let path = PathBuf::from(&file);
                    if let Some(p) = path.file_stem() {
                        let mut found = false;
                        let mut offset = Wrapping(0);
                        for k1 in orig_items.keys().sorted() {
                            if let Some(v1) = orig_items.get(k1) {
                                for k2 in output_items.keys().sorted() {
                                    if let Some(v2) = output_items.get(k2) {
                                        if (Wrapping(*k1) + offset) == Wrapping(*k2) && *v1 != *v2 {
                                            let pp = t.join(p);
                                            if !pp.exists() {
                                                std::fs::create_dir_all(&pp).ok();
                                            }
                                            let trans_filename1 = pp.join(format!("{}.2.rs", &k1));
                                            let trans_filename2 = pp.join(format!("{}.3.rs", &k1));
                                            if let Ok(vv1) = std::str::from_utf8(v1) {
                                                if let Ok(vv2) = std::str::from_utf8(v2) {
                                                    if let Ok(markedrules) =
                                                        String::from_utf8(markup_rules(
                                                            Wrapping(*k1),
                                                            Wrapping(*k1) + Wrapping(vv1.len()),
                                                            fixed_warnings.to_vec(),
                                                        ))
                                                    {
                                                        let _ = &trans_filename1;
                                                        std::fs::write(
                                                            &trans_filename1,
                                                            format!("{}{}", markedrules, vv1),
                                                        )
                                                        .ok();
                                                        std::fs::write(
                                                            &trans_filename2,
                                                            format!("{}{}", markedrules, vv2),
                                                        )
                                                        .ok();
                                                        found = true;
                                                        offset +=
                                                            Wrapping(v2.len()) - Wrapping(v1.len());
                                                    }
                                                }
                                            }
                                            if !found && pp.exists() {
                                                std::fs::remove_dir_all(&pp).ok();
                                            }
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn sub_messages(children: &[Diagnostic]) -> String {
    children
        .iter()
        .map(|x| {
            if let Some(rendered) = &x.rendered {
                format!("{}: {}", &x.message, &rendered)
            } else {
                x.message.to_owned()
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}

// remove the previously generated files under folder, matching with the pattern
fn remove_previously_generated_files(folder: &str, pattern: &str) {
    if !std::path::Path::new(folder).exists() {
        return;
    }
    if let Ok(command) = Command::new("find")
        .args([folder, "-name", pattern])
        .stdout(Stdio::piped())
        .spawn()
    {
        if let Ok(output) = command.wait_with_output() {
            #[cfg(verbose)]
            if !output.stdout.is_empty() {
                println!("Removed previously generated warning files in {folder} matching with {pattern}")
            }
            if let Ok(s) = String::from_utf8(output.stdout) {
                s.split('\n').for_each(|tmp| {
                    if let Ok(mut command) = Command::new("rm")
                        .args(["-f", tmp])
                        .stdout(Stdio::piped())
                        .spawn()
                    {
                        if let Ok(w) = command.wait() {
                            if !w.success() {
                                println!("wait not successful");
                            }
                        }
                    }
                });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;
    #[test]
    #[serial]
    fn diagnostics() {
        let args = Args {
            flags: vec![],
            patch: None,
            confirm: false,
            pair: false,
            function: false,
            single: false,
        };
        let dir = std::path::Path::new("abc");
        if dir.exists() {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(command) = Command::new("cargo")
            .args(["init", "--bin", "--vcs", "git", "abc"])
            .spawn()
        {
            if let Ok(_output) = command.wait_with_output() {
                let code = r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#;
                let _ = std::fs::write("abc/src/main.rs", code);
                let cd = std::env::current_dir().unwrap();
                std::env::set_current_dir(dir).ok();
                my_args(args);
                run();
                assert!(std::path::Path::new("diagnostics/src/main.rs").exists());
                if let Ok(s) = std::fs::read_to_string("diagnostics/src/main.rs") {
                    assert_eq!(
                        s,
                        r###"
fn main() {
    let s = /*#[Warning(clippy::unwrap_used)*/std::fs::read_to_string("Cargo.toml").unwrap()/*
#[Warning(clippy::unwrap_used)
note: if this value is an `Err`, it will panic
for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
requested on the command line with `-W clippy::unwrap-used`*/;
    println!("{s}");
}
"###
                    );
                }
                std::env::set_current_dir(cd).ok();
            }
        }
    }

    fn my_args(args: Args) {
        ARGS.lock().unwrap().pop();
        ARGS.lock().unwrap().push(args);
    }

    // run the following bash commands
    // ```bash
    // cat $code > $filename
    // git add $filename
    // git commit -am $message
    // ```
    fn commit_file(
        message: &str,
        filename: &str,
        code: &str,
    ) -> core::result::Result<git2::Oid, git2::Error> {
        std::fs::write(filename, code).ok();
        let repo = git2::Repository::open(std::path::Path::new(".")).unwrap();
        let author = git2::Signature::now("Yijun Yu", "y.yu@open.ac.uk").unwrap();
        let mut index = repo.index().unwrap();
        index.add_path(std::path::Path::new(filename)).ok();
        let index_oid = index.write_tree_to(&repo).unwrap();
        let tree = repo.find_tree(index_oid).unwrap();
        let h = repo.head();
        if let Ok(head) = h {
            let oid = head.target().unwrap();
            let parent = repo.find_commit(oid).unwrap();
            let result = repo.commit(Some("HEAD"), &author, &author, message, &tree, &[&parent]);
            result.and_then(|oid| {
                repo.find_object(oid, None).and_then(|object| {
                    repo.reset(&object, git2::ResetType::Soft, None)
                        .map(|_| oid)
                })
            })
        } else {
            let result = repo.commit(Some("HEAD"), &author, &author, message, &tree, &[]);
            result.and_then(|oid| {
                repo.find_object(oid, None).and_then(|object| {
                    repo.reset(&object, git2::ResetType::Soft, None)
                        .map(|_| oid)
                })
            })
        }
    }

    fn setup(
        code: &str,
        fix: &str,
    ) -> anyhow::Result<(std::path::PathBuf, git2::Oid), std::io::Error> {
        let dir = std::path::Path::new("abc");
        if dir.exists() {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(command) = Command::new("cargo")
            .args(["init", "--vcs", "git", "--bin", "abc"])
            .spawn()
        {
            if let Ok(_output) = command.wait_with_output() {
                let cd = std::env::current_dir().unwrap();
                std::env::set_current_dir(dir).ok();
                let init_commit = commit_file("init", "src/main.rs", code).ok().unwrap();
                let update_commit = commit_file("update", "src/main.rs", fix).ok().unwrap();
                checkout(init_commit);
                Ok((cd, update_commit))
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "Cannot initiate the cargo project",
                ))
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Cannot checkout",
            ))
        }
    }

    fn teardown(cd: std::path::PathBuf, update_commit: git2::Oid) {
        checkout(update_commit);
        std::env::set_current_dir(cd).ok();
    }

    use std::sync::Arc;
    #[test]
    #[serial]
    // run the following bash commands
    // ```bash
    // rm -rf abc
    // cargo init --vcs git --bin abc
    // cd abc
    // cat $code1 > src/main.rs
    // git add src/main.rs
    // git commit -am init
    // init_commit=$(git rev-parse HEAD)
    // cat $code2 > src/main.rs
    // git commit -am update
    // update_commit=$(git rev-parse HEAD)
    // git checkout $init_commit
    // rust-diagnostics --patch $update_commit
    // cd -
    // ```
    fn fixed() {
        if let Ok((cd, update_commit)) = setup(
            r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,
            r#"
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#,
        ) {
            let debug_confirm = true;
            let args = Args {
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: debug_confirm,
                pair: false,
                function: false,
                single: false,
            };
            std::io::set_output_capture(Some(Default::default()));
            my_args(args);
            run();
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(
                captured,
                r###"There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
"###
            );
            teardown(cd, update_commit);
        }
    }

    #[test]
    #[serial]
    fn pair() {
        if let Ok((cd, update_commit)) = setup(
            r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,
            r#"
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#,
        ) {
            let debug_confirm = true;
            let args = Args {
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: debug_confirm,
                pair: true,
                function: false,
                single: false,
            };
            std::io::set_output_capture(Some(Default::default()));
            my_args(args);
            run();
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(
                captured,
                r###"There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
"###
            );
            teardown(cd, update_commit);
        }
    }

    fn function_setup(code1: &str, code2: &str, code3: &str) {
        if let Ok((cd, update_commit)) = setup(code1, code2) {
            let debug_confirm = true;
            dbg!(&update_commit);
            let args = Args {
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: debug_confirm,
                pair: true,
                function: true,
                single: false,
            };
            std::io::set_output_capture(Some(Default::default()));
            my_args(args);
            run();
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(captured, code3);
            teardown(cd, update_commit);
        }
    }

    #[test]
    #[serial]
    fn function1() {
        function_setup(
            r#"
fn main() {


    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,
            r#"
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#,
            r###"There are 1 warnings in 1 files.
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
"###,
        );
    }

    #[test]
    #[serial]
    fn function2() {
        function_setup(
            r#"
fn _test() {
}
fn main() {


    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,
            r#"
fn _test() {
}
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#,
            r###"There are 1 warnings in 1 files.
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
"###,
        );
    }

    #[test]
    #[serial]
    fn main() {
        let dir = std::path::Path::new("abc");
        if dir.exists() {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(command) = Command::new("cargo").args(["init", "--bin", "abc"]).spawn() {
            if let Ok(_output) = command.wait_with_output() {
                let code = r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#;
                let cd = std::env::current_dir().unwrap();
                std::env::set_current_dir(dir).ok();
                std::fs::write("src/main.rs", code).ok();
                let args = Args {
                    flags: vec![],
                    patch: None,
                    confirm: false,
                    pair: false,
                    function: false,
                    single: false,
                };
                my_args(args);
                run();
                assert!(
                    !std::path::Path::new("test/transform/Wclippy::unwrap_used/0.2.rs").exists()
                );
                std::env::set_current_dir(cd).ok();
            }
        }
    }

    #[test]
    #[serial]
    fn unfixed() {
        if let Ok((cd, update_commit)) = setup(
            r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,
            r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("The configuration file is: {s}");
}
"#,
        ) {
            let args = Args {
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: true,
                pair: false,
                function: false,
                single: false,
            };
            std::io::set_output_capture(Some(Default::default()));
            my_args(args);
            run();
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(
                captured,
                r###"There are 1 warnings in 1 files.
"###
            );
            teardown(cd, update_commit);
        }
    }

    // ```bash
    // git clone .git rd
    // cd cd
    // git checkout $rev1
    // rust-diagnostics --patch $rev2
    // ```
    fn rd_setup<F>(args: Args, rev1: &str, run: F) -> String 
    where F: Fn(&str) -> (),
    {
        let dir = std::path::Path::new("rd");
        let git_dir = std::path::Path::new("rd/.git");
        if !git_dir.exists() {
            let fo = git2::FetchOptions::new();
            let co = git2::build::CheckoutBuilder::new();
            git2::build::RepoBuilder::new()
                .fetch_options(fo)
                .with_checkout(co)
                .clone(".git", std::path::Path::new("rd"))
                .ok();
            println!();
        }
        let cd = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir).ok();
        let oid = git2::Oid::from_str(rev1).unwrap();
        checkout(oid);
        std::io::set_output_capture(Some(Default::default()));
        my_args(args.clone());
        let rev2 = args.patch.clone().unwrap();
        run(rev2.as_str());
        let captured = std::io::set_output_capture(None).unwrap();
        let captured = Arc::try_unwrap(captured).unwrap();
        let captured = captured.into_inner().unwrap();
        let captured = String::from_utf8(captured).unwrap();
        std::env::set_current_dir(cd).ok();
        captured
    }

    fn rd_run(_rev2: &str) {
        run();
    }

    fn diff_run(rev2: &str) {
        let repo = git2::Repository::open(".").unwrap();
        let diff = get_diff(&repo, rev2.to_string());
        let hunks = get_hunks(diff.unwrap());
        print_hunks(hunks);
    }

    #[test]
    #[serial]
    fn rd1() {
        assert_eq!(
            rd_setup(
                Args {
                    patch: Some("512236bac29f09ca798c93020ce377c30a4ed2a5".to_string()),
                    flags: vec![],
                    confirm: true,
                    pair: true,
                    function: true,
                    single: true,
                },
                "2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1",
                rd_run
            ),
            "There are 30 warnings in 1 files.\n"
        );
    }

    #[test]
    #[serial]
    fn rd2() {
        insta::assert_snapshot!(rd_setup(Args { patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: false, function: false, single: true, },
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files.
        #[Warning(clippy::len_zero)
        -    if output.len() != 0 {
        +    if !output.is_empty() {
        "###);
        insta::assert_snapshot!(rd_setup(Args { patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: true, function: false, single: true, },
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files.
        #[Warning(clippy::len_zero)
        "###);
        insta::assert_snapshot!(rd_setup(Args { patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: true, function: true, single: true, },
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files.
        #[Warning(clippy::len_zero)
        "###);
    }

    #[test]
    #[serial]
    fn rd3() {
        insta::assert_snapshot!(rd_setup(Args { patch: Some("035ef892fa57fe644ef76065849ebd025869614d".to_string()),
                flags: vec![], confirm: true, pair: false, function: false, single: true, },
                "375981bb06cf819332c202cdd09d5a8c48e296db", rd_run), @r###"
        There are 27 warnings in 1 files.
        #[Warning(clippy::collapsible_if)
        -            if m.start <= i && i < m.end {
        -                if i == m.start {
        -                    output.extend(format!("<{}>", m.name).as_bytes());
        -                }
        +            if m.start <= i && i < m.end && i == m.start {
        +                output.extend(format!("<{}>", m.name).as_bytes());
        #[Warning(clippy::unwrap_used)
        -        let file_name = path
        -            .parent()
        -            .unwrap()
        -            .join(format!("{}.rs.1",path.file_stem().unwrap().to_string_lossy()));
        +        let file_name = path.parent().unwrap().join(format!(
        +            "{}.rs.1",
        +            path.file_stem().unwrap().to_string_lossy()
        +        ));
        -            std::fs::create_dir(&file_name.parent().unwrap()).ok();
        -        }            
        +            std::fs::create_dir(file_name.parent().unwrap()).ok();
        +        }
        #[Warning(clippy::expect_used)
        -        .expect("failed to aquire programm output").stdout;
        +        .expect("failed to aquire programm output")
        +        .stdout;
        -    String::from_utf8(output).expect("programm output was not valid utf-8").split('\n').for_each(|tmp| {
        -        let mut command = Command::new("rm")
        -        .args(["-f", tmp])
        -        .stdout(Stdio::piped())
        -        .spawn()
        -        .unwrap();
        -        command.wait().expect("problem with file deletion");
        -    });
        +    String::from_utf8(output)
        +        .expect("programm output was not valid utf-8")
        +        .split('\n')
        +        .for_each(|tmp| {
        +            let mut command = Command::new("rm")
        +                .args(["-f", tmp])
        +                .stdout(Stdio::piped())
        +                .spawn()
        +                .unwrap();
        +            command.wait().expect("problem with file deletion");
        +        });
        "###);
        insta::assert_snapshot!(rd_setup(Args { patch: Some("035ef892fa57fe644ef76065849ebd025869614d".to_string()),
                flags: vec![], confirm: true, pair: true, function: true, single: true, },
                "375981bb06cf819332c202cdd09d5a8c48e296db", rd_run), @r###"
        There are 27 warnings in 1 files.
        #[Warning(clippy::collapsible_if)
        #[Warning(clippy::unwrap_used)
        fn remove_previously_generated_files() {
                    std::fs::create_dir(&file_name.parent().unwrap()).ok();
                }            
                .spawn()
                .unwrap();
            let output = command
                .wait_with_output()
                .expect("failed to aquire programm output").stdout;
            if !output.is_empty() {
                println!("Removed previously generated warning files")
            }
            String::from_utf8(output).expect("programm output was not valid utf-8").split('\n').for_each(|tmp| {
                let mut command = Command::new("rm")
                .args(["-f", tmp])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
                command.wait().expect("problem with file deletion");
            });
        }
        === 19a3477889393ea2cdd0edcb5e6ab30c ===
        fn remove_previously_generated_files() {
                    std::fs::create_dir(file_name.parent().unwrap()).ok();
                }
                .spawn()
                .unwrap();
            let output = command
                .wait_with_output()
                .expect("failed to aquire programm output").stdout;
            if !output.is_empty() {
                println!("Removed previously generated warning files")
            }
            String::from_utf8(output).expect("programm output was not valid utf-8").split('\n').for_each(|tmp| {
                let mut command = Command::new("rm")
                .args(["-f", tmp])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
                command.wait().expect("problem with file deletion");
            });
        }
        #[Warning(clippy::expect_used)
        "###);
    }

    #[test]
    #[serial]
    fn hunks_patch() {
        insta::assert_snapshot!(rd_setup(Args {
            flags: vec![],
            patch: Some("512236bac29f09ca798c93020ce377c30a4ed2a5".to_string()),
            confirm: true,
            pair: false,
            function: false,
            single: false,
        }, "2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1", diff_run), 
        @r###"
        src/main.rs

        src/main.rs.1











        "###);
     }

    #[test]
    #[serial]
    fn hunks_pairs() {
         insta::assert_snapshot!(rd_setup(Args {
            flags: vec![],
            patch: Some("512236bac29f09ca798c93020ce377c30a4ed2a5".to_string()),
            confirm: true,
            pair: true,
            function: false,
            single: false,
        }, "2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1", diff_run), 
        @r###"
         src/main.rs

         src/main.rs.1











         "###);
    }
}
