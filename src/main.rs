#![feature(internal_output_capture)]
use std::{sync::Arc};
use cargo_metadata::{diagnostic::Diagnostic, Message};
use serde::Serialize;
use std::{
    collections::HashMap,
    fs::read_to_string,
    path::PathBuf,
    process::{Command, Stdio},
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

#[derive(StructOpt)]
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Ran {
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
fn markup(source: &[u8], map: Vec<Ran>) -> Vec<u8> {
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
                            "".to_string()
                        } else {
                            format!(
                                "\nsuggestion: {}",
                                m.suggestion.replace("\\n", "\n").replace('\"', "")
                            )
                        },
                        if m.note == "None" {
                            "".to_string()
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
fn markup_rules(start: Wrapping<usize>, end: Wrapping<usize>, map: Vec<Ran>) -> Vec<u8> {
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

#[derive(PartialEq, Eq, Hash)]
pub struct LineRange {
    start_byte: usize,
    start_line: usize,
    end_line: usize,
}

// Split up the Rust source_file into individual items, indiced by their start_byte offsets
fn splitup(source: &[u8]) -> anyhow::Result<HashMap<LineRange, &[u8]>> {
    let mut output: HashMap<LineRange, &[u8]> = HashMap::new();
    if let Ok(s) = std::str::from_utf8(source) {
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
                .matches(&query, tree.root_node(), source)
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
                    if let Ok(code) = std::str::from_utf8(&source[m.start_byte..m.end_byte]) {
                        let lr = LineRange {start_byte: m.start_byte, start_line: m.start_line, end_line: m.end_line};
                        output.insert(lr, code.as_bytes());
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
fn to_diagnostic(map: &mut HashMap<String, Vec<Ran>>, args: Vec<String>) {
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
                                    let r = Ran {
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
fn diagnose_all_warnings(flags: Vec<String>) -> HashMap<String, Vec<Ran>> {
    let mut args = vec![
        "clippy".to_string(),
        "--message-format=json".to_string(),
        "--".to_string(),
    ];
    for flag in flags {
        args.push(format!("-Wclippy::{}", flag));
    }
    let mut map: HashMap<String, Vec<Ran>> = HashMap::new();
    to_diagnostic(&mut map, args);
    if !map.is_empty() {
        let mut markup_map: HashMap<String, String> = HashMap::new();
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
    map
}

#[cfg(fix)]
// process warnings from one RUSTC_FLAG at a time
fn fix_warnings(flags: Vec<String>, map: &HashMap<String, Vec<Ran>>) {
    for flag in &flags {
        let mut flagged_map: HashMap<String, Vec<Ran>> = HashMap::new();
        for file in map.keys() {
            if let Some(v) = map.get(file) {
                let mut new_v = Vec::new();
                for r in v {
                    let rule = &flag[2..];
                    if r.name == format!("#[Warning({})", &rule) {
                        let r1 = Ran {
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
            let mut origin_map: HashMap<String, String> = HashMap::new();
            let mut markup_map: HashMap<String, String> = HashMap::new();
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
            let mut fixed_map: HashMap<String, Vec<Ran>> = HashMap::new();
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
    repo.reset(commit.unwrap().as_object(),
               git2::ResetType::Hard,
               Some(git2::build::CheckoutBuilder::new()
                .force()
                .remove_untracked(true)),
               ).ok();
}


fn run(args: Args) {
    remove_previously_generated_files("./diagnostics", "*.rs"); // marked up
    #[cfg(fix)]
    {
        remove_previously_generated_files("./original", "*.rs"); // before fix
        remove_previously_generated_files(".", "*.2.rs"); // transformed from
        remove_previously_generated_files(".", "*.3.rs"); // transformed to
    }
    let mut flags = args.flags;
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
    let mut all_warnings = diagnose_all_warnings(flags.clone());
    let mut count = 0;
    all_warnings.iter().for_each(|(_k, v)| {
        count += v.len();
    });
    println!(
        "There are {} warnings in {} files.",
        count,
        all_warnings.len()
    );
    let patch = args.patch;
    if patch.is_some() {
        #[cfg(feature = "patch")]
        {
            if let Some(id) = patch {
                let repo = git2::Repository::open(".").unwrap();
                let old_id = repo.head().unwrap().target().unwrap();
                let c1 = repo
                    .find_commit(repo.head().unwrap().target().unwrap())
                    .unwrap();
                let c2 = repo.find_commit(git2::Oid::from_str(&id).unwrap()).unwrap();
                let a = Some(c1.tree().unwrap());
                let b = Some(c2.tree().unwrap());
                let mut diffopts2 = git2::DiffOptions::new();
                diffopts2.context_lines(0);
                let diff = repo
                    .diff_tree_to_tree(a.as_ref(), b.as_ref(), Some(&mut diffopts2))
                    .unwrap();
                let mut prev_hunk = 0;
                let mut related_warnings = std::collections::HashSet::new();
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
                                    if ! args.confirm {
                                        println!("{}", m.name);
                                    }
                                });
                                related_warnings = std::collections::HashSet::new();
                            }
                            if ! args.confirm {
                                match line.origin() {
                                    ' ' | '+' | '-' => print!("{}", line.origin()),
                                    _ => {}
                                }
                                print!("{}", std::str::from_utf8(line.content()).unwrap());
                            }
                            prev_hunk = h.old_start();
                            true
                        } else {
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
                                        if k1 == k2 && m1.start_line <= m2.end_line && m1.end_line >= m2.start_line {
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
                    let mut pair = vec!["".to_string(), "".to_string()];
                    let mut prev_l: i32 = 0;
                    let mut prev_r: i32 = 0;
                    let mut prefix = "".to_string();
                    let mut suffix = "".to_string();
                    diff.print(git2::DiffFormat::Patch, |delta, hunk, line| -> bool {
                        let p = delta.old_file().path().unwrap();
                        let mut function_items = HashMap::new();
                        let source = read_to_string(p).unwrap(); 
                        if let Ok(items) = splitup(source.as_bytes()) {
                            function_items = items;
                        }
                        let mut overlap = false;
                        if let Some(h) = hunk {
                            all_warnings.iter_mut().for_each(|(k, v)| {
                                v.iter_mut().for_each(|m| {
                                    if m.fixed && std::path::Path::new(k) == p
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
                                    if args.pair { // && (!pair[0].is_empty() || !pair[1].is_empty() || prev_hunk == 0) { //remainder
                                        let mut prev_f = "".to_string();
                                        for k in function_items.keys() {
                                            let v = function_items.get(k).unwrap();
                                            prev_f = format!("{}", std::str::from_utf8(v).unwrap());
                                            prev_l = i32::try_from(h.old_start() - 1).unwrap(); 
                                            // dbg!(&h);
                                            prev_r = i32::try_from(h.old_start() + h.old_lines() - 1).unwrap(); 
                                            if k.start_line <= usize::try_from(prev_l).unwrap() && usize::try_from(prev_r).unwrap() <= k.end_line {
                                                break;
                                            }
                                        }
                                        let lines: Vec<&str> = prev_f.split('\n').collect();
                                        let lines_deleted: Vec<&str> = pair[0].split('\n').collect();
                                        // let lines_added: Vec<&str> = pair[1].split('\n').collect();
                                        // dbg!(&prev_f); dbg!(&prev_l); dbg!(&prev_r); dbg!(&lines); dbg!(&lines_deleted); dbg!(&lines_added);
                                        prefix = "".to_string();
                                        suffix = "".to_string();
                                        let ll = i32::try_from(lines_deleted.len()).unwrap();
                                        let n = usize::try_from(i32::try_from(h.old_start()).unwrap() - prev_l).unwrap();
                                        let m = usize::try_from(i32::try_from(h.old_start()+ h.old_lines()).unwrap() - prev_l + ll).unwrap();
                                        // dbg!(&prev_l); dbg!(&prev_r); dbg!(&ll); dbg!(&n); dbg!(&m);
                                        for i in 0..n {
                                            prefix = format!("{}{}\n", prefix, lines[i]);
                                        }
                                        for i in (m-1)..lines.len() {
                                            suffix = format!("{}{}\n", suffix, lines[i]);
                                        }
                                        // dbg!(&prefix); dbg!(&suffix);
                                        if !pair[0].is_empty() || !pair[1].is_empty() {
                                            print_pair(args.function, pair.clone(), prefix.clone(), suffix.clone());
                                        }
                                        pair = vec!["".to_string(), "".to_string()];
                                    }
                                    related_warnings.iter().for_each(|m| {
                                        println!("{}", m.name);
                                    });
                                    related_warnings = std::collections::HashSet::new();
                                }
                                let content = std::str::from_utf8(line.content()).unwrap();
                                match line.origin() {
                                    ' ' => {
                                        if args.pair {
                                            if ! args.function {
                                                pair[0] = format!("{}{}", pair[0], content);
                                                pair[1] = format!("{}{}", pair[1], content);
                                            }
                                        } else {
                                            print!("{}", line.origin());
                                            print!("{}", content);
                                        }
                                    }, 
                                    '+' => {
                                        if args.pair {
                                            pair[1] = format!("{}{}", pair[1], content);
                                        } else {
                                            print!("{}", line.origin());
                                            print!("{}", content);
                                        }
                                    },
                                    '-' => { 
                                        if args.pair {
                                            pair[0] = format!("{}{}", pair[0], content);
                                        } else {
                                            print!("{}", line.origin());
                                            print!("{}", content);
                                        }
                                    },
                                    _ => { // @@
                                        if args.pair {
                                            if !args.function {
                                                pair[0] = format!("{}{}", pair[0], content);
                                            }
                                        } else {
                                            print!("{}", content);
                                        }
                                    }
                                }
                                prev_hunk = h.old_start();
                                true
                            } else {
                                prev_hunk = h.old_start();
                                true
                            }
                        } else {
                            true
                        }
                    })
                    .ok();
                    if args.pair && (!pair[0].is_empty() || !pair[1].is_empty()) { //remainder
                        print_pair(args.function, pair.clone(), prefix.clone(), suffix.clone());
                    }
                }
            }
        }
        #[cfg(not(feature = "patch"))]
        {
            println!("To use the `--patch` option, please enable the `patch` feature");
        }
    }

    #[cfg(fix)]
    fix_warnings(flags, &all_warnings);
}

fn print_pair(function: bool, pair: Vec<String>, prefix: String, suffix: String) {
    // dbg!(&prefix); dbg!(&suffix);
    if function {
        print!("{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}", 
           format!("{}{}{}", prefix, pair[0], suffix),format!("{}{}{}", prefix, pair[1], suffix));
    } else {
        print!("{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}", pair[0], pair[1]);
    }
}

// Run cargo clippy to generate warnings from "foo.rs" into temporary "foo.rs.1" files
fn main() {
    let args = Args::from_args();
    run(args);
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
        if let Ok(output) = txl_rs::txl(args) {
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
        warnings: Vec<Ran>,
        fixed_warnings: Vec<Ran>,
        remaining_warnings: Vec<Ran>,
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
    use serial_test::serial;
    use super::*;
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
        if let Ok(command) = Command::new("cargo").args(["init", "--bin", "--vcs", "git", "abc"]).spawn() {
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
                run(args);
                assert!(std::path::Path::new("diagnostics/src/main.rs").exists());
                if let Ok(s) = std::fs::read_to_string("diagnostics/src/main.rs") {
                    assert_eq! (s, r###"
fn main() {
    let s = /*#[Warning(clippy::unwrap_used)*/std::fs::read_to_string("Cargo.toml").unwrap()/*
#[Warning(clippy::unwrap_used)
note: if this value is an `Err`, it will panic
for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
requested on the command line with `-W clippy::unwrap-used`*/;
    println!("{s}");
}
"###);
                }
                std::env::set_current_dir(cd).ok();
            }
        }
    }

    // run the following bash commands
    // ```bash
    // cat $code > $filename
    // git add $filename
    // git commit -am $message
    // ```
    fn commit_file(message: &str, filename: &str, code: &str) -> core::result::Result<git2::Oid, git2::Error> {
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
                    repo.reset(&object, git2::ResetType::Soft, None).map(|_| oid)
                })
            })
        } else {
            let result = repo.commit(Some("HEAD"), &author, &author, message, &tree, &[]);
            result.and_then(|oid| {
                repo.find_object(oid, None).and_then(|object| {
                    repo.reset(&object, git2::ResetType::Soft, None).map(|_| oid)
                })
            })
        }
    }

    fn setup(code: &str, fix: &str) -> anyhow::Result<(std::path::PathBuf, git2::Oid), std::io::Error> {
        let dir = std::path::Path::new("abc");
        if dir.exists() {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(command) = Command::new("cargo").args(["init", "--vcs", "git", "--bin", "abc"]).spawn() {
            if let Ok(_output) = command.wait_with_output() {
                let cd = std::env::current_dir().unwrap();
                std::env::set_current_dir(dir).ok();
                let init_commit = commit_file("init", "src/main.rs", code).ok().unwrap();
                let update_commit = commit_file("update", "src/main.rs", fix).ok().unwrap();
                checkout(init_commit);
                Ok((cd, update_commit))
            } else {
                Err(std::io::Error::new(std::io::ErrorKind::NotFound, "Cannot initiate the cargo project"))
            }
        } else {
            Err(std::io::Error::new(std::io::ErrorKind::NotFound, "Cannot checkout"))
        }
    }

    fn teardown(cd: std::path::PathBuf, update_commit: git2::Oid) {
        checkout(update_commit);
        std::env::set_current_dir(cd).ok();
    }

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
       if let Ok((cd, update_commit)) = setup(r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,r#"
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#) 
        {
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
            run(args);
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(captured, r###"There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
"###);
            teardown(cd, update_commit);
        }
    }

    #[test]
    #[serial]
    fn pair() {
       if let Ok((cd, update_commit)) = setup(r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,r#"
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#) 
        {
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
            run(args);
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(captured, r###"There are 1 warnings in 1 files.
#[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
"###);
            teardown(cd, update_commit);
        }
    }

    use std::io::*;
    // ```bash
    // git clone .git rd
    // cd cd
    // git checkout $rev1
    // rust-diagnostics --patch $rev2
    // ```
    fn rd_setup(rev1: &str, rev2: &str) {
        let dir = std::path::Path::new("rd/.git");
        if ! dir.exists() {
            let fo = git2::FetchOptions::new();
            let co = git2::build::CheckoutBuilder::new();
            git2::build::RepoBuilder::new()
                .fetch_options(fo)
                .with_checkout(co)
                .clone(".git", std::path::Path::new("rd")).ok();
            println!();
        } 
        let cd = std::env::current_dir().unwrap();
        std::env::set_current_dir(dir).ok();
        let oid = git2::Oid::from_str(rev1).unwrap();
        checkout(oid);
        let args = Args {
            flags: vec![],
            patch: Some(rev2.to_string()),
            confirm: true,
            pair: true,
            function: true,
            single: false,
        };
        run(args);
        std::env::set_current_dir(cd).ok();
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
            run(args);
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
    fn rd1() {
        rd_setup("2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1",
                 "512236bac29f09ca798c93020ce377c30a4ed2a5");
    }

    #[test]
    #[serial]
    fn function1() {
         function_setup(r#"
fn main() {


    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,r#"
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#, r###"There are 1 warnings in 1 files.
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
"###
        );
    }

    #[test]
    #[serial]
    fn function2() {
         function_setup(r#"
fn _test() {
}
fn main() {


    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,r#"
fn _test() {
}
fn main() {
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
}
"#, r###"There are 1 warnings in 1 files.
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
"###
        );
    }

    #[test]
    #[serial]
    fn unfixed() {
       if let Ok((cd, update_commit)) = setup(r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#,r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("The configuration file is: {s}");
}
"#) 
        {
            let args = Args {
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: true,
                pair: false,
                function: false,
                single: false,
            };
            std::io::set_output_capture(Some(Default::default()));
            run(args);
            let captured = std::io::set_output_capture(None).unwrap();
            let captured = Arc::try_unwrap(captured).unwrap();
            let captured = captured.into_inner().unwrap();
            let captured = String::from_utf8(captured).unwrap();
            assert_eq!(captured, r###"There are 1 warnings in 1 files.
"###);
            teardown(cd, update_commit);
        }
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
                run(args);
                assert!(!std::path::Path::new("test/transform/Wclippy::unwrap_used/0.2.rs").exists());
                std::env::set_current_dir(cd).ok();
            }
        }
    }
}
