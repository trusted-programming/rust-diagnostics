use cargo_metadata::{diagnostic::Diagnostic, Message};
use once_cell::sync::Lazy;
use serde::{Serialize, Deserialize};
use std::{
    collections::BTreeMap,
    fs::read_to_string,
    path::PathBuf,
    process::{Command, Stdio},
    sync::Mutex,
};
use std::io::prelude::*;

mod language;
use tree_sitter::QueryCursor;
use tree_sitter_parsers::parse;

use structopt::StructOpt;

#[derive(StructOpt, Debug, Clone)]
struct Args {
    #[structopt(name = "folder", long)]
    /// the folder of the repository, default to "."
    folder: Option<String>,
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
    #[structopt(name = "location", long)]
    /// markup the function with exact warning
    location: bool,
    #[structopt(name = "mixed", long)]
    /// markup the function with exact warning, and print context => patch_text
    mixed: bool,
}

static ARGS: Mutex<Vec<Args>> = Mutex::new(vec![]);

fn get_args() -> Vec<Args> {
    set_args();
    Result::unwrap(ARGS.lock()).to_vec()
}

fn set_args() {
    if Result::unwrap(Mutex::lock(&ARGS)).len() == 0 {
        let params = Args::from_args();
        Result::unwrap(ARGS.lock()).push(params);
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
                    let code = &source[m.start_byte..m.end_byte];
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
    Ok(output)
}

fn get_diagnostics_folder() -> String
{
    let folder = get_folder();
    let mut diagnostics_folder = format!("{folder}/diagnostics");
    if let Some(id) = get_current_id() {
        diagnostics_folder = format!("{folder}/diagnostics/{id}");
    }
    let p = std::path::Path::new(diagnostics_folder.as_str());
    if ! p.exists() {
        std::fs::create_dir_all(p).ok();
    }
    diagnostics_folder
}

fn to_diagnostic(map: &mut BTreeMap<String, Vec<Warning>>, args: Vec<String>) {
    let diagnostics_folder = get_diagnostics_folder();
    let p = std::path::Path::new(diagnostics_folder.as_str());
    if !p.exists() {
        std::fs::create_dir_all(p).ok();
    }
    let json_filename = format!("{diagnostics_folder}/diagnostics.json");
    let p = std::path::Path::new(json_filename.as_str());
    if !p.exists() {
        let cargo = get_cargo();
        if let Ok(mut command) = Command::new(cargo)
            .args(args)
            .stdout(Stdio::piped())
            .spawn()
        {
            if let Some(take) = command.stdout.take() {
                if open_file_to_write(json_filename.clone()).is_ok() {
                    if let Ok(mut file) = open_file_to_append(json_filename.clone()) {
                        let reader = std::io::BufReader::new(take);
                        let mut message_vector = Vec::new();
                        for message in cargo_metadata::Message::parse_stream(reader).flatten() {
                            message_vector.push(message.clone());
                        }
                        if let Ok(msg) = serde_json::to_string(&message_vector) {
                            file.write_all(msg.as_bytes()).ok();
                        }
                    }
                }
            }
            command.wait().ok();
        }
    }
    if let Ok(msg) = read_to_string(json_filename.as_str()) {
        if let Ok(messages) = serde_json::from_str::<Vec<cargo_metadata::Message>>(&msg) {
            for message in messages {
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
    }
}

fn get_cargo() -> &'static str {
    let mut cargo = "cargo";
    if std::path::Path::new("x.py").exists() {
        cargo = "./x.py";
    } else if std::path::Path::new("miri").exists() {
        cargo = "./miri";
    }
    cargo
}

fn get_folder() -> String {
    let mut folder = ".".to_string();
    let v = get_args();
    let args = &v[0];
    if let Some(f) = args.folder.clone() {
        folder = f;
    }
    folder
}

// mark up the given code of $filename within a line range $lr with the warnings $ws
fn markup_code(source: &[u8], lr: &LineRange, ws: Vec<Warning>) -> Vec<u8> 
{
    let mut output = Vec::new();
    let mut num_lines = 0;
    for (i, c) in source.iter().enumerate() {
        if lr.start_line <= num_lines && num_lines <= lr.end_line {
            for w in &ws {
                // deal with the element
                if w.start <= i && i < w.end && i == w.start {
                    output.extend(format!("/*{}*/", w.name).as_bytes());
                }
                if w.end == i {
                    output.extend(
                        format!(
                            "/*\n{}{}{}*/",
                            w.name,
                            if w.suggestion == "None" {
                                EMPTY_STRING.to_string()
                            } else {
                                format!(
                                    "\nsuggestion: {}",
                                    w.suggestion.replace("\\n", "\n").replace('\"', "")
                                )
                            },
                            if w.note == "None" {
                                EMPTY_STRING.to_string()
                            } else {
                                format!("\nnote: {}", w.note.replace("\\n", "\n").replace('\"', ""))
                            }
                        )
                        .as_bytes(),
                    )
                }
            }
            output.push(*c);
        }
        if *c == b'\n' {
            num_lines = num_lines.saturating_add(1);
        }
    }
    output
}

// markup all warnings into diagnostics
fn diagnose_all_warnings(flags: Vec<String>) -> BTreeMap<String, Vec<Warning>> {
    let folder = get_folder();
    let diagnostics_folder = get_diagnostics_folder();
    let manifest = format!("{folder}/Cargo.toml");
    let mut args = vec![
        "clippy".to_string(),
        "--manifest-path".to_string(), 
        manifest,
        "--message-format=json".to_string(),
        "--".to_string(),
    ];
    for flag in flags {
        args.push(format!("-Wclippy::{flag}"));
    }
    let mut map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
    to_diagnostic(&mut map, args);
    if !map.is_empty() {
        let mut markup_map: BTreeMap<String, String> = BTreeMap::new();
        for file in map.keys() {
            if let Ok(source) = read_to_string(format!("{folder}/{file}")) {
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
                let file_name = PathBuf::from(&diagnostics_folder).join(file);
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

fn count(map: BTreeMap<String, Vec<Warning>>) -> usize
{
    let mut sum: usize = 0;
    map.iter().for_each(|(_,v)|{
        sum = sum.saturating_add(v.len());
    });
    sum
}

fn clippy_fix() -> usize
{
    std::env::set_var("__CARGO_FIX_YOLO", "1");
    let folder = get_folder();
    let manifest = format!("{folder}/Cargo.toml");
    let args = vec![
        "clippy".to_string(),
        "--message-format=json".to_string(),
        "--manifest-path".to_string(), 
        manifest,
         "--fix".to_string(),
        "--allow-dirty".to_string(),
        "--allow-no-vcs".to_string(),
        "--broken-code".to_string(),
        "--".to_string(),
    ];
    let mut map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
    to_diagnostic(&mut map, args);
    count(map)
}

// run the following bash commands
// ```bash
// git checkout $commit_id
// ```
fn checkout(commit_id: git2::Oid) {
    let folder = get_folder();
    if let Ok(repo) = git2::Repository::open(folder) {
        if let Ok(commit) = repo.find_commit(commit_id) {
            repo.reset(
                commit.as_object(),
                git2::ResetType::Hard,
                Some(
                    git2::build::CheckoutBuilder::new()
                        .force()
                        .remove_untracked(true),
                ),
            )
            .ok();
        } 
    }
}

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
            /*
            // all clippy rules that are either inplaceholder or machine applicable 
            "assertions_on_result_states".to_string(),
            "bind_instead_of_map".to_string(),
            "blocks_in_if_conditions".to_string(),
            "bool_to_int_with_if".to_string(),
            "borrow_as_ptr".to_string(),
            "borrow_deref_ref".to_string(),
            "borrowed_box".to_string(),
            "box_default".to_string(),
            "bytes_count_to_len".to_string(),
            "bytes_nth".to_string(),
            "cast_abs_to_unsigned".to_string(),
            "cast_lossless".to_string(),
            "cast_slice_different_sizes".to_string(),
            "cast_slice_from_raw_parts".to_string(),
            "char_lit_as_u8".to_string(),
            "cloned_instead_of_copied".to_string(),
            "clone_on_copy".to_string(),
            "collapsible_if".to_string(),
            "collapsible_str_replace".to_string(),
            "crate_in_macro_def".to_string(),
            "dbg_macro".to_string(),
            "default_instead_of_iter_empty".to_string(),
            "derivable_impls".to_string(),
            "equatable_if_let".to_string(),
            "err_expect".to_string(),
            "expect_fun_call".to_string(),
            "explicit_into_iter_loop".to_string(),
            "explicit_iter_loop".to_string(),
            "explicit_write".to_string(),
            "extend_with_drain".to_string(),
            "filter_map_identity".to_string(),
            "filter_map_next".to_string(),
            "filter_next".to_string(),
            "flat_map_identity".to_string(),
            "flat_map_option".to_string(),
            "from_over_into".to_string(),
            "from_str_radix_10".to_string(),
            "get_last_with_len".to_string(),
            "get_unwrap".to_string(),
            "implicit_clone".to_string(),
            "implicit_return".to_string(),
            "implicit_saturating_add".to_string(),
            "implicit_saturating_sub".to_string(),
            "inconsistent_struct_constructor".to_string(),
            "inefficient_to_string".to_string(),
            "infallible_destructuring_match".to_string(),
            "init_numbered_fields".to_string(),
            "inline_fn_without_body".to_string(),
            "into_iter_on_ref".to_string(),
            "int_plus_one".to_string(),
            "is_digit_ascii_radix".to_string(),
            "iter_cloned_collect".to_string(),
            "iter_count".to_string(),
            "iter_kv_map".to_string(),
            "iter_next_slice".to_string(),
            "iter_nth_zero".to_string(),
            "iter_overeager_cloned".to_string(),
            "iter_skip_next".to_string(),
            "large_const_arrays".to_string(),
            "len_zero".to_string(),
            "let_unit_value".to_string(),
            "manual_assert".to_string(),
            "manual_async_fn".to_string(),
            "manual_bits".to_string(),
            "manual_find".to_string(),
            "manual_is_ascii_check".to_string(),
            "manual_let_else".to_string(),
            "manual_ok_or".to_string(),
            "manual_rem_euclid".to_string(),
            "manual_retain".to_string(),
            "manual_saturating_arithmetic".to_string(),
            "manual_string_new".to_string(),
            "manual_str_repeat".to_string(),
            "manual_unwrap_or".to_string(),
            "map_clone".to_string(),
            "map_collect_result_unit".to_string(),
            "map_flatten".to_string(),
            "map_identity".to_string(),
            "map_unwrap_or".to_string(),
            "match_as_ref".to_string(),
            "match_bool".to_string(),
            "match_result_ok".to_string(),
            "match_single_binding".to_string(),
            "match_str_case_mismatch".to_string(),
            "missing_spin_loop".to_string(),
            "needless_arbitrary_self_type".to_string(),
            "needless_bool".to_string(),
            "needless_collect".to_string(),
            "needless_for_each".to_string(),
            "needless_late_init".to_string(),
            "needless_match".to_string(),
            "needless_option_as_deref".to_string(),
            "needless_option_take".to_string(),
            "needless_parens_on_range_literals".to_string(),
            "needless_question_mark".to_string(),
            "neg_multiply".to_string(),
            "no_effect".to_string(),
            "non_octal_unix_permissions".to_string(),
            "nonstandard_macro_braces".to_string(),
            "obfuscated_if_else".to_string(),
            "option_as_ref_deref".to_string(),
            "option_map_or_none".to_string(),
            "map_unwrap_or".to_string(),
            "or_fun_call".to_string(),
            "or_then_unwrap".to_string(),
            "partialeq_to_none".to_string(),
            "path_buf_push_overwrite".to_string(),
            "precedence".to_string(),
            "ptr_as_ptr".to_string(),
            "ptr_offset_with_cast".to_string(),
            "question_mark".to_string(),
            "rc_clone_in_vec_init".to_string(),
            "redundant_clone".to_string(),
            "redundant_closure_call".to_string(),
            "redundant_field_names".to_string(),
            "redundant_pattern".to_string(),
            "redundant_pub_crate".to_string(),
            "redundant_slicing".to_string(),
            "redundant_static_lifetimes".to_string(),
            "repeat_once".to_string(),
            "search_is_some".to_string(),
            "seek_from_current".to_string(),
            "seek_to_start_instead_of_rewind".to_string(),
            "single_char_pattern".to_string(),
            "single_component_path_imports".to_string(),
            "single_element_loop".to_string(),
            "single_match".to_string(),
            "stable_sort_primitive".to_string(),
            "strlen_on_c_strings".to_string(),
            "suspicious_operation_groupings".to_string(),
            "swap_ptr_to_ref".to_string(),
            "to_digit_is_some".to_string(),
            "transmute_ptr_to_ref".to_string(),
            "transmutes_expressible_as_ptr_casts".to_string(),
            "try_err".to_string(),
            "unit_arg".to_string(),
            "unnecessary_cast".to_string(),
            "unnecessary_fold".to_string(),
            "unnecessary_join".to_string(),
            "unnecessary_owned_empty_strings".to_string(),
            "unnecessary_sort_by".to_string(),
            "unnecessary_to_owned".to_string(),
            "unneeded_wildcard_pattern".to_string(),
            "unnested_or_patterns".to_string(),
            "unused_rounding".to_string(),
            "unused_unit".to_string(),
            "unwrap_or_else_default".to_string(),
            "useless_asref".to_string(),
            "use_self".to_string(),
            "vec_box".to_string(),
            "vec_init_then_push".to_string(),
            "while_let_loop".to_string(),
            "while_let_on_iterator".to_string(),
            "wildcard_imports".to_string(),
            */
        ];
    }
    flags
}

fn open_file_to_append(file: String) -> Result<std::fs::File, String> {
    if ! std::path::Path::new(&file).exists() {
        open_file_to_write(file)
    } else {
        let mut binding = std::fs::OpenOptions::new();
        let option = binding.create_new(false).write(true).append(true);
        match option.open(file) {
            Err(e) => Err(format!("{e}")),
            Ok(option) => Ok(option),
        }
    }
}

fn open_file_to_write(file: String) -> Result<std::fs::File, String> {
    if std::path::Path::new(&file).exists() {
        remove_a_file(file.as_str());
    }
    let mut binding = std::fs::OpenOptions::new();
    let option = binding.create_new(true).write(true).append(false);
    match option.open(file) {
            Err(e) => Err(format!("{e}")),
            Ok(option) => Ok(option),
    }
 }

fn fprint_warning_count(file: String, all_warnings: BTreeMap<String, Vec<Warning>>) {
    let mut count: usize = 0;
    all_warnings.iter().for_each(|(_k, v)| {
        count = count.saturating_add(v.len());
    });
    let remained = clippy_fix();
    if let Ok(mut file) = open_file_to_write(file) {
        file.write_all(format!("There are {} warnings in {} files, {} has been fixed.\n", 
                               count, all_warnings.len(), count.saturating_sub(remained)).as_bytes()).ok();
    } 
}

fn get_current_id() -> Option<git2::Oid> {
    let folder = get_folder();
    if let Ok(repo) = git2::Repository::open(folder) {
        if let Ok(x) = repo.head() {
            x.target()
        } else {
            None
        }
    } else {
        None
    }
}

fn get_diff(repo: &git2::Repository, id: String) -> Option<git2::Diff> {
    if let Ok(head) = repo.head() {
        if let Some(target) = head.target() {
            if let Ok(c1) = repo.find_commit(target) {
                if let Ok(oid) = git2::Oid::from_str(&id) {
                    if let Ok(c2) = repo.find_commit(oid) {
                        let a = if let Ok(a) = c1.tree() { Some(a) } else { None };
                        let b = if let Ok(b) = c2.tree() { Some(b) } else { None };
                        let mut diffopts2 = git2::DiffOptions::new();
                        diffopts2.context_lines(0);
                        if let Ok(diff) = repo.diff_tree_to_tree(a.as_ref(), b.as_ref(), Some(&mut diffopts2)) {
                            Some(diff)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Hunk {
    patch_text: String, // c.f. git-diff patch
    header: String,    // c.f. header
    old_text: String, // the old version of the patch
    new_text: String, // the new version of the patch
    old_start_line: usize, // start line number of the old version
    old_end_line: usize, // end line number of the old version
    new_start_line: usize, // start line number of the new version
    new_end_line: usize, // end line number of the new version
    context: String,    // c.f. git-diff -W the enclosing function
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
                    if !args.mixed {
                        write!(f, "{}{}\n=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
                            self.warnings, self.context, self.new_context)
                    } else if args.location {
                        write!(f, "{}{}=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
                            self.warnings, self.context, self.patch_text)
                    } else {
                        write!(f, "{}{}\n=== 19a3477889393ea2cdd0edcb5e6ab30c ===\n{}",
                            self.warnings, self.context, self.patch_text)
                    }
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

fn fprint_hunks(file: String, map: BTreeMap<String, Vec<Hunk>>) 
{
    if let Ok(mut file) = open_file_to_append(file) {
        map.iter().for_each(|(_, v)| {
            v.iter().for_each(|h| {
                file.write_all(format!("{h}").as_bytes()).ok();
            });
        });
    }
}

fn u32_to_usize(x: u32) -> usize
{
    if let Ok(y) = usize::try_from(x) {
        y
    } else {
        0
    }
}

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
        context: EMPTY_STRING.to_string(),
        new_context: EMPTY_STRING.to_string(),
        warnings: EMPTY_STRING.to_string(),
        n_warnings: 0,
        fixed: false,
    }; 
    let mut prev_lineno = 0;
    let mut filename = EMPTY_STRING.to_string();
    diff.print(git2::DiffFormat::Patch, |delta, hunk, line| {
        if let Some(path) = delta.old_file().path() {
            if let Some(path) = path.to_str() {
                filename = path.to_string();
                if let Some(hunk) = hunk {
                    // reinitialize
                    if prev_lineno != hunk.old_start() {
                        if prev_lineno > 0 {
                            hunks.push(cur_hunk.clone());
                        }
                        if cur_filename != filename {
                            map.insert(filename.clone(), hunks.clone());
                            cur_filename = filename.clone();
                            hunks = Vec::new();
                        }
                        prev_lineno = hunk.old_start();
                        cur_hunk = Hunk {
                            patch_text: EMPTY_STRING.to_string(),
                            old_text: EMPTY_STRING.to_string(),
                            old_start_line: 0,
                            old_end_line: 0,
                            new_text: EMPTY_STRING.to_string(),
                            new_start_line: 0,
                            new_end_line: 0,
                            header: EMPTY_STRING.to_string(),
                            context: EMPTY_STRING.to_string(),
                            new_context: EMPTY_STRING.to_string(),
                            warnings: EMPTY_STRING.to_string(),
                            n_warnings: 0,
                            fixed: false,
                        }; 
                        cur_hunk.old_start_line = u32_to_usize(hunk.old_start());
                        cur_hunk.old_end_line = u32_to_usize(hunk.old_start().saturating_add(hunk.old_lines()));
                        cur_hunk.new_start_line = u32_to_usize(hunk.new_start());
                        cur_hunk.new_end_line = u32_to_usize(hunk.new_start().saturating_add(hunk.new_lines()));
                    }
                    if let Ok(content) = std::str::from_utf8(line.content()) {
                        match line.origin() {
                            ' ' => {
                                cur_hunk
                                    .patch_text
                                    .push_str(format!("{}{content}", line.origin()).as_str());
                                cur_hunk.old_text.push_str(content);
                                cur_hunk.new_text.push_str(content);
                            }
                            '+' => {
                                cur_hunk
                                    .patch_text
                                    .push_str(format!("{}{content}", line.origin()).as_str());
                                cur_hunk.new_text.push_str(content);
                            }
                            '-' => {
                                cur_hunk
                                    .patch_text
                                    .push_str(format!("{}{content}", line.origin()).as_str());
                                cur_hunk.old_text.push_str(content);
                            }
                            _ => {
                                cur_hunk.header.push_str(content);
                            }
                        }
                    }
                }
            }
        } 
        true
    })
    .ok();
    hunks.push(cur_hunk);
    map.insert(filename, hunks.clone());
    map
}

// This function associate the warnings to the hunks when they overlap.
fn add_warnings_to_hunks(hunks: &mut BTreeMap<String, Vec<Hunk>>, warnings: BTreeMap<String, Vec<Warning>>) 
{
   hunks.iter_mut().for_each(|(k1, v1)| {
       warnings.iter().for_each(|(k2, v2)| {
          if k1 == k2 {
            v1.iter_mut().for_each(|h| {
                v2.iter().for_each(|w| {
                    if h.old_start_line <= w.end_line
                       && h.old_end_line >= w.start_line
                    {
                        h.n_warnings = h.n_warnings.saturating_add(1);
                        h.warnings = format!("{}#{}\n", h.warnings, w.name);
                    }
               });
            });
          }
       });
    });
}

fn get_all_new_warnings() -> BTreeMap<String, Vec<Warning>>
{
    let v = get_args();
    let args = &v[0];
    let map = BTreeMap::new();
    if args.confirm {
        let flags = get_flags();
        if let Some(id) = &args.patch {
            if let Some(old_id) = get_current_id() {
                if let Ok(oid) = git2::Oid::from_str(id) {
                    checkout(oid);
                    let all_new_warnings = diagnose_all_warnings(flags);
                    checkout(old_id);
                    all_new_warnings
                } else {
                    map
                }
            } else {
                map
            }
        } else {
            map
        }
    } else {
        map
    }
}

// This function associates the warnings to the hunks when they will be fixed in next revision.
fn check_fixed(hunks: &mut BTreeMap<String, Vec<Hunk>>, warnings: BTreeMap<String, Vec<Warning>>) 
{
   let v = get_args();
   let args = &v[0];
   hunks.iter_mut().for_each(|(k1, v1)| {
        v1.iter_mut().for_each(|h| {
           if !args.single && h.n_warnings>0 || h.n_warnings == 1 {
                let mut fixed = true;
                warnings.iter().for_each(|(k2, v2)| {
                    if k1 == k2 {
                        v2.iter().for_each(|w| {
                            if h.new_start_line <= w.end_line 
                                && h.new_end_line >= w.start_line
                            {
                                fixed = false;
                            }
                        });
                    }
                });
                if fixed {
                   // print!(">>>>>{h}");
                   h.fixed = true;
                }
           }
       });
    });
}

fn handle_patch(all_warnings: BTreeMap<String, Vec<Warning>>) {
    let v = get_args();
    let args = &v[0];
    let folder = get_folder();
    let diagnostics_folder = get_diagnostics_folder();
    if let Some(id) = &args.patch {
        let mut hunks = BTreeMap::new();
        let json_filename = format!("{diagnostics_folder}/{id}.json");
        if let Ok(json_hunks) = read_to_string(json_filename.as_str()) {
           hunks = if let Ok(hunks) = serde_json::from_str::<BTreeMap<String,Vec<Hunk>>>(&json_hunks) { hunks } else { hunks };
           fprint_hunks(format!("{diagnostics_folder}/diagnostics.log"), hunks);
        } else if let Ok(repo) = git2::Repository::open(&folder) {
            if let Some(diff) = get_diff(&repo, id.to_string()) {
                let mut hunks = get_hunks(diff);
                add_warnings_to_hunks(&mut hunks, all_warnings.clone());
                let new_warnings = get_all_new_warnings();
                check_fixed(&mut hunks, new_warnings);
                hunks.iter_mut().for_each(|(k0,v)| {
                    let filename = format!("{folder}/{k0}");
                    if let Ok(source) = read_to_string(filename.as_str()) {
                        let source = source.as_bytes();
                        let p = std::path::Path::new(filename.as_str());
                        if let Ok(function_items) = get_function_items(p) {
                            v.iter_mut().for_each(|h| {
                                function_items.iter().for_each(|(k, f)| {
                                    let prev_l = h.old_start_line.saturating_sub(1);
                                    let prev_r = h.old_end_line.saturating_sub(1);
                                    if k.start_line <= prev_l
                                        && prev_r <= k.end_line
                                    {
                                        if args.location {
                                            if let Some(ws) = &all_warnings.get(k0) {
                                                let markedup = &markup_code(source, k, ws.to_vec());
                                                if let Ok(s) = std::str::from_utf8(markedup) {
                                                    h.context = s.to_string();
                                                } else {
                                                    h.context = f.to_string();
                                                }
                                            } else {
                                                h.context = f.to_string();
                                            }
                                        } else {
                                            h.context = f.to_string();
                                        }
                                        let n = h.old_start_line.saturating_sub(k.start_line).saturating_sub(1);
                                        let m = h.old_end_line.saturating_sub(k.start_line);
                                        let lines: Vec<&str> = f.split('\n').collect();
                                        h.new_context = "".to_string();
                                        (0..n).for_each(|i| {
                                            h.new_context.push_str(lines[i]);
                                            h.new_context.push('\n');
                                        });
                                        h.new_context.push_str(h.new_text.as_str());
                                        (m.saturating_sub(1)..lines.len()).for_each(|i| {
                                            h.new_context.push_str(lines[i]);
                                            h.new_context.push('\n');
                                        });
                                    }
                                });
                            });
                        }
                    } 
                });
                if open_file_to_write(json_filename.clone()).is_ok() {
                    if let Ok(mut file) = open_file_to_append(json_filename) {
                        if let Ok(msg) = serde_json::to_string(&hunks) {
                            file.write_all(msg.as_bytes()).ok();
                        }
                        fprint_hunks(format!("{diagnostics_folder}/diagnostics.log"), hunks);
                    }
                }
            }
        } else {
            println!("========================cannot find {json_filename} nor .git repository");
        }
    }
}

fn run() {
    let diagnostics_folder = get_diagnostics_folder();
    remove_previously_generated_files(&diagnostics_folder, "*.rs"); // marked up
    let flags = get_flags();
    let all_warnings = diagnose_all_warnings(flags);
    fprint_warning_count(format!("{diagnostics_folder}/diagnostics.log"), all_warnings.clone());
    handle_patch(all_warnings);
}

fn get_function_items(p: &std::path::Path) -> Result<BTreeMap<LineRange, String>, anyhow::Error> {
    if let Ok(s) = read_to_string(p) {
        splitup(s)
    } else {
        let map: BTreeMap<LineRange, String> = BTreeMap::new();
        Ok(map)
    }
}

// Run cargo clippy to generate warnings from "foo.rs" into temporary "foo.rs.1" files
fn main() {
    run();
}

fn sub_messages(children: &[Diagnostic]) -> String {
    let mut v = children
        .iter()
        .map(|x| {
            if let Some(rendered) = &x.rendered {
                format!("{}: {}", &x.message, &rendered)
            } else {
                x.message.to_owned()
            }
        })
        .collect::<Vec<String>>();
    v.sort();
    v.join("\n")
}

fn remove_a_file(tmp: &str) {
    if let Ok(mut command) = Command::new("rm")
        .args(["-f", tmp])
        .stdout(Stdio::piped())
        .spawn()
    {
        if let Ok(w) = command.wait() {
            if !w.success() {
                eprintln!("wait not successful");
            }
        }
    }
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
            if let Ok(s) = String::from_utf8(output.stdout) {
                s.split('\n').for_each(|tmp| {
                    remove_a_file(tmp);
                });
            }
        }
    }
    remove_a_file(format!("{folder}/diagnostics.log").as_str());
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    fn get_temp_dir() -> String 
    {
        format!("tmp_{}", uuid::Uuid::new_v4()).replace('-', "_")
    }

    #[test]
    #[serial]
    fn diagnostics() {
        let temp_dir = get_temp_dir();
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: false,
            pair: false,
            function: false,
            single: true,
            location: false,
            mixed: false,
        };
        let dir = std::path::Path::new(&temp_dir);
        if dir.exists() {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(command) = Command::new("cargo")
            .args(["init", "--bin", "--vcs", "git", temp_dir.as_str()])
            .spawn()
        {
            if let Ok(_output) = command.wait_with_output() {
                let code = r#"
fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
"#;
            my_args(args);
            let diagnostics_folder = get_diagnostics_folder();
            let filename = format!("{}/src/main.rs", temp_dir.clone());
                let _ = std::fs::write(filename, code);
                run();
                let filename = format!("{}/src/main.rs", diagnostics_folder);
                let filename = filename.as_str();
                assert!(std::path::Path::new(filename).exists());
                if let Ok(s) = std::fs::read_to_string(filename) { 
                    assert_eq!(s, r###"
fn main() {
    let s = /*#[Warning(clippy::unwrap_used)*/std::fs::read_to_string("Cargo.toml").unwrap()/*
#[Warning(clippy::unwrap_used)
note: for further information visit https://rust-lang.github.io/rust-clippy/master/index.html#unwrap_used
if this value is an `Err`, it will panic
requested on the command line with `-W clippy::unwrap-used`*/;
    println!("{s}");
}
"###); }
            }
        }
    }

    fn my_args(args: Args) {
        if let Ok(mut lock) = ARGS.lock() {
            lock.pop();
            lock.push(args);
        }
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
        let folder = get_folder();
        std::fs::write(format!("{folder}/{filename}"), code).ok();
        if let Ok(repo) = git2::Repository::open(std::path::Path::new(folder.as_str())) {
            if let Ok(author) = git2::Signature::now("Yijun Yu", "y.yu@open.ac.uk") {
                if let Ok(mut index) = repo.index() {
                    index.add_path(std::path::Path::new(&filename)).ok();
                    if let Ok(index_oid) = index.write_tree_to(&repo) {
                        if let Ok(tree) = repo.find_tree(index_oid) {
                            let h = repo.head();
                            if let Ok(head) = h {
                                if let Some(oid) = head.target() {
                                    if let Ok(parent) = repo.find_commit(oid) {
                                        let result = repo.commit(Some("HEAD"), &author, &author, message, &tree, &[&parent]);
                                        result.and_then(|oid| {
                                            repo.find_object(oid, None).and_then(|object| {
                                                repo.reset(&object, git2::ResetType::Soft, None)
                                                    .map(|_| oid)
                                            })
                                        })
                                    } else {
                                        Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot find the commit"))
                                    }
                                } else {
                                    Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot find the target"))
                                }
                            } else {
                                let result = repo.commit(Some("HEAD"), &author, &author, message, &tree, &[]);
                                result.and_then(|oid| {
                                    repo.find_object(oid, None).and_then(|object| {
                                        repo.reset(&object, git2::ResetType::Soft, None)
                                            .map(|_| oid)
                                    })
                                })
                            }
                        } else {
                            Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot find the tree"))
                        } 
                    } else {
                        Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot create the work tree"))
                    }
                } else {
                    Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot create the index"))
                }
            } else {
                Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot create the author"))
            }
        } else {
            Err(git2::Error::new(git2::ErrorCode::GenericError, git2::ErrorClass::None, "Cannot open the repository"))
        }
    }

    fn setup(temp_dir: String,
        code: &str,
        fix: &str,
    ) -> anyhow::Result<git2::Oid, std::io::Error> {
        let dir = std::path::Path::new(temp_dir.as_str());
        if dir.exists() {
            let _ = std::fs::remove_dir_all(dir);
        }
        if let Ok(command) = Command::new("cargo")
            .args(["init", "--vcs", "git", "--bin", temp_dir.as_str()])
            .spawn()
        {
            if let Ok(_output) = command.wait_with_output() {
                if let Ok(init_commit) = commit_file("init", "src/main.rs", code) {
                    if let Ok(update_commit) = commit_file("update", "src/main.rs", fix) {
                        checkout(init_commit);
                        Ok(update_commit)
                    } else {
                        Err(std::io::Error::new(std::io::ErrorKind::NotFound, "Cannot create the update commit",))
                    }
                } else {
                    Err(std::io::Error::new(std::io::ErrorKind::NotFound, "Cannot create the init commit",))
                }
            } else {
                Err(std::io::Error::new(std::io::ErrorKind::NotFound, "Cannot initiate the cargo project",))
            }
        } else {
            Err(std::io::Error::new(std::io::ErrorKind::NotFound, "Cannot checkout",))
        }
    }

    fn teardown(update_commit: git2::Oid) {
        checkout(update_commit);
    }

    #[test]
    #[serial]
    // run the following bash commands
    // ```bash
    // temp=$(mktemp)
    // rm -rf $temp
    // cargo init --vcs git --bin $temp
    // cd $temp
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
        let temp_dir = get_temp_dir();
        let debug_confirm = true;
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: debug_confirm,
            pair: false,
            function: false,
            single:true,
            location: false,
            mixed: false,
        };
        my_args(args);
        if let Ok(update_commit) = setup(temp_dir.clone(),
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
            let args = Args {
                folder: Some(temp_dir.clone()),
                flags: vec![],
                patch: Some(update_commit.to_string()),
                confirm: debug_confirm,
                pair: false,
                function: false,
                single: true,
                location: false,
                mixed: false,
            };
            my_args(args);
            run();
            let diagnostics_folder = get_diagnostics_folder();
            if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                assert_eq!( s,
                r###"There are 1 warnings in 1 files, 0 has been fixed.
##[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
"###);
            }
            teardown(update_commit);
        }
    }

    #[test]
    #[serial]
    fn pair() {
        let temp_dir = get_temp_dir();
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: false,
            pair: true,
            function: false,
            single: true,
            location: false,
            mixed: false,
        };
        my_args(args);
        if let Ok(update_commit) = setup(temp_dir.clone(),
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
            let debug_confirm = false;
            let args = Args {
                folder: Some(temp_dir),
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: debug_confirm,
                pair: true,
                function: false,
                single: true,
                location: false,
                mixed: false,
            };
            my_args(args);
            run();
            let diagnostics_folder = get_diagnostics_folder();
            if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                assert_eq!(
                s,
                r###"There are 1 warnings in 1 files, 0 has been fixed.
##[Warning(clippy::unwrap_used)
@@ -3,2 +3,3 @@ fn main() {
    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
        println!("{s}");
    }
"###
            );
            }
            teardown(update_commit);
        }
    }

    fn mixed_function_setup(code1: &str, code2: &str, code3: &str) {
        let temp_dir = get_temp_dir();
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: true,
            pair: true,
            function: true,
            single: true,
            location: false,
            mixed: true,
        };
        my_args(args);

        if let Ok(update_commit) = setup(temp_dir.clone(), code1, code2) {
            let args = Args {
                folder: Some(temp_dir),
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: true,
                pair: true,
                function: true,
                single: true,
                location: false,
                mixed: true,
            };
            my_args(args);
            let diagnostics_folder = get_diagnostics_folder();
            run();
            if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                assert_eq!(s, code3);
            }
            teardown(update_commit);
        }
    }

    fn location_mixed_function_setup(code1: &str, code2: &str, code3: &str) {
        let temp_dir = get_temp_dir();
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: true,
            pair: true,
            function: true,
            single: true,
            location: true,
            mixed: true,
        };
        my_args(args);

        if let Ok(update_commit) = setup(temp_dir.clone(), code1, code2) {
            let args = Args {
                folder: Some(temp_dir),
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: true,
                pair: true,
                function: true,
                single: true,
                location: true,
                mixed: true,
            };
            my_args(args);
            let diagnostics_folder = get_diagnostics_folder();
            run();
            if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                assert_eq!(s, code3);
            }
            teardown(update_commit);
        }
    }

    fn function_setup(code1: &str, code2: &str, code3: &str) {
        let temp_dir = get_temp_dir();
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: true,
            pair: true,
            function: true,
            single: true,
            location: false,
            mixed: false,
        };
        my_args(args);

        if let Ok(update_commit) = setup(temp_dir.clone(), code1, code2) {
            let args = Args {
                folder: Some(temp_dir),
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: true,
                pair: true,
                function: true,
                single: true,
                location: false,
                mixed: false,
            };
            my_args(args);
            let diagnostics_folder = get_diagnostics_folder();
            run();
            if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                assert_eq!(s, code3);
            }
            teardown(update_commit);
        }
    }

    #[test]
    #[serial]
    fn function_mixed_location() {
        location_mixed_function_setup(
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
            r###"There are 1 warnings in 1 files, 0 has been fixed.
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
-
-
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
"###,
        );
    }


    #[test]
    #[serial]
    fn function_mixed() {
        mixed_function_setup(
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
            r###"There are 1 warnings in 1 files, 0 has been fixed.
##[Warning(clippy::unwrap_used)
fn main() {


    let s = std::fs::read_to_string("Cargo.toml").unwrap();
    println!("{s}");
}
=== 19a3477889393ea2cdd0edcb5e6ab30c ===
-
-
-    let s = std::fs::read_to_string("Cargo.toml").unwrap();
-    println!("{s}");
+    if let Ok(s) = std::fs::read_to_string("Cargo.toml") {
+        println!("{s}");
+    }
"###,
        );
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
            r###"There are 1 warnings in 1 files, 0 has been fixed.
##[Warning(clippy::unwrap_used)
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
            r###"There are 1 warnings in 1 files, 0 has been fixed.
##[Warning(clippy::unwrap_used)
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
    fn unfixed() {
        let temp_dir = get_temp_dir();
        let args = Args {
            folder: Some(temp_dir.clone()),
            flags: vec![],
            patch: None,
            confirm: true,
            pair: false,
            function: false,
            single: true,
            location: false,
            mixed: false,
        };
        my_args(args);
        if let Ok(update_commit) = setup(temp_dir.clone(),
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
                folder: Some(temp_dir),
                flags: vec![],
                patch: Some(format!("{update_commit}")),
                confirm: true,
                pair: false,
                function: false,
                single: true,
                location: false,
                mixed: false,
            };
            my_args(args);
            run();
            let diagnostics_folder = get_diagnostics_folder();
            if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                assert_eq!(
                    s,    
                    r###"There are 1 warnings in 1 files, 0 has been fixed.
"###
                );
            }
            teardown(update_commit);
        }
    }

    // ```bash
    // git clone .git rd
    // cd cd
    // git checkout $rev1
    // rust-diagnostics --patch $rev2
    // ```
    fn rd_setup_twice<F>(temp_dir: String, args: Args, rev1: &str, run: F) -> String 
    where F: Fn(&str),
    {
        my_args(args.clone());
        let git_dir = std::path::Path::new("{temp_dir}/.git");
        if !git_dir.exists() {
            let fo = git2::FetchOptions::new();
            let co = git2::build::CheckoutBuilder::new();
            git2::build::RepoBuilder::new()
                .fetch_options(fo)
                .with_checkout(co)
                .clone(".git", std::path::Path::new(temp_dir.as_str()))
                .ok();
            println!();
        }
        if let Ok(oid) = git2::Oid::from_str(rev1) {
            checkout(oid);
            if let Some(rev2) = args.patch {
                let diagnostics_folder = get_diagnostics_folder();
                run(rev2.as_str());
                run(rev2.as_str()); // second time run
                if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                    s
                } else {
                    "".to_string()
                }
            } else {
                "".to_string()
            }
        } else {
            "".to_string()
        }
    }


    // ```bash
    // git clone .git rd
    // cd cd
    // git checkout $rev1
    // rust-diagnostics --patch $rev2
    // ```
    fn rd_setup<F>(temp_dir: String, args: Args, rev1: &str, run: F) -> String 
    where F: Fn(&str),
    {
        my_args(args.clone());
        let git_dir = std::path::Path::new("{temp_dir}/.git");
        if !git_dir.exists() {
            let fo = git2::FetchOptions::new();
            let co = git2::build::CheckoutBuilder::new();
            git2::build::RepoBuilder::new()
                .fetch_options(fo)
                .with_checkout(co)
                .clone(".git", std::path::Path::new(temp_dir.as_str()))
                .ok();
            println!();
        }
        if let Ok(oid) = git2::Oid::from_str(rev1) { 
            checkout(oid);
            if let Some(rev2) = args.patch {
                let diagnostics_folder = get_diagnostics_folder();
                run(rev2.as_str());
                if let Ok(s) = read_to_string(format!("{diagnostics_folder}/diagnostics.log")) {
                    s
                } else {
                    "".to_string()
                }
            } else {
                "".to_string()
            }
        } else {
            "".to_string()
        }
    }

    fn rd_run(_rev2: &str) {
        run();
    }

    fn diff_run(rev2: &str) {
        let folder = get_folder();
        if let Ok(repo) = git2::Repository::open(folder) {
            let diff = get_diff(&repo, rev2.to_string());
            if let Some(diff) = diff {
                let hunks = get_hunks(diff);
                let diagnostics_folder = get_diagnostics_folder();
                fprint_hunks(format!("{diagnostics_folder}/diagnostics.log"), hunks);
            }
        }
    }

    #[test]
    #[serial]
    fn rd1() {
        let temp_dir = get_temp_dir();
        assert_eq!(
            rd_setup(temp_dir.clone(),
                Args {folder: Some(temp_dir),
                    patch: Some("512236bac29f09ca798c93020ce377c30a4ed2a5".to_string()),
                    flags: vec![],
                    confirm: true,
                    pair: true,
                    function: true,
                    single: true,
                    location: false,
                    mixed: false,
                },
                "2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1",
                rd_run
            ),
            "There are 30 warnings in 1 files, 0 has been fixed.\n"
        );
    }

    #[test]
    #[serial]
    fn rd2() {
        let temp_dir = get_temp_dir();
        insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args { folder: Some(temp_dir.clone()),
                patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: false, function: false, single: true,  location: false, mixed: false},
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files, 0 has been fixed.
        ##[Warning(clippy::len_zero)
        @@ -107 +107 @@ fn remove_previously_generated_files() {
        -    if output.len() != 0 {
        +    if !output.is_empty() {
        "###);
        insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args { folder: Some(temp_dir.clone()),
                patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: true, function: false, single: true,  location: false, mixed: false},
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files, 0 has been fixed.
        ##[Warning(clippy::len_zero)
        @@ -107 +107 @@ fn remove_previously_generated_files() {
            if output.len() != 0 {
        === 19a3477889393ea2cdd0edcb5e6ab30c ===
            if !output.is_empty() {
        "###);
        insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args { folder: Some(temp_dir),
                patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: true, function: true, single: true,  location: false, mixed: false},
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files, 0 has been fixed.
        ##[Warning(clippy::len_zero)
        fn remove_previously_generated_files() {
            let command = Command::new("find")
                .args(&[".", "-name", "*.rs.1"])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
            let output = command
                .wait_with_output()
                .expect("failed to aquire programm output").stdout;
            if output.len() != 0 {
                println!("Removed previously generated warning files")
            }
            String::from_utf8(output).expect("programm output was not valid utf-8").split("\n").for_each(|tmp| {
                let mut command = Command::new("rm")
                .args(&["-f", tmp])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
                command.wait().expect("problem with file deletion");
            });
        }
        === 19a3477889393ea2cdd0edcb5e6ab30c ===
        fn remove_previously_generated_files() {
            let command = Command::new("find")
                .args(&[".", "-name", "*.rs.1"])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
            let output = command
                .wait_with_output()
                .expect("failed to aquire programm output").stdout;
            if !output.is_empty() {
                println!("Removed previously generated warning files")
            }
            String::from_utf8(output).expect("programm output was not valid utf-8").split("\n").for_each(|tmp| {
                let mut command = Command::new("rm")
                .args(&["-f", tmp])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
                command.wait().expect("problem with file deletion");
            });
        }
        "###);
    }

    #[test]
    #[serial]
    fn twice() {
        let temp_dir = get_temp_dir();
        insta::assert_snapshot!(rd_setup_twice(temp_dir.clone(), Args { folder: Some(temp_dir.clone()),
                patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: false, function: false, single: true,  location: false, mixed: false},
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files, 0 has been fixed.
        ##[Warning(clippy::len_zero)
        @@ -107 +107 @@ fn remove_previously_generated_files() {
        -    if output.len() != 0 {
        +    if !output.is_empty() {
        "###);
        insta::assert_snapshot!(rd_setup_twice(temp_dir.clone(), Args { folder: Some(temp_dir.clone()),
                patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: true, function: false, single: true,  location: false, mixed: false},
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files, 0 has been fixed.
        ##[Warning(clippy::len_zero)
        @@ -107 +107 @@ fn remove_previously_generated_files() {
            if output.len() != 0 {
        === 19a3477889393ea2cdd0edcb5e6ab30c ===
            if !output.is_empty() {
        "###);
        insta::assert_snapshot!(rd_setup_twice(temp_dir.clone(), Args { folder: Some(temp_dir),
                patch: Some("375981bb06cf819332c202cdd09d5a8c48e296db".to_string()),
                flags: vec![], confirm: true, pair: true, function: true, single: true,  location: false, mixed: false},
                "512236bac29f09ca798c93020ce377c30a4ed2a5", rd_run), @r###"
        There are 30 warnings in 1 files, 0 has been fixed.
        ##[Warning(clippy::len_zero)
        fn remove_previously_generated_files() {
            let command = Command::new("find")
                .args(&[".", "-name", "*.rs.1"])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
            let output = command
                .wait_with_output()
                .expect("failed to aquire programm output").stdout;
            if output.len() != 0 {
                println!("Removed previously generated warning files")
            }
            String::from_utf8(output).expect("programm output was not valid utf-8").split("\n").for_each(|tmp| {
                let mut command = Command::new("rm")
                .args(&["-f", tmp])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
                command.wait().expect("problem with file deletion");
            });
        }
        === 19a3477889393ea2cdd0edcb5e6ab30c ===
        fn remove_previously_generated_files() {
            let command = Command::new("find")
                .args(&[".", "-name", "*.rs.1"])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
            let output = command
                .wait_with_output()
                .expect("failed to aquire programm output").stdout;
            if !output.is_empty() {
                println!("Removed previously generated warning files")
            }
            String::from_utf8(output).expect("programm output was not valid utf-8").split("\n").for_each(|tmp| {
                let mut command = Command::new("rm")
                .args(&["-f", tmp])
                .stdout(Stdio::piped())
                .spawn()
                .unwrap();
                command.wait().expect("problem with file deletion");
            });
        }
        "###);
    }


    #[test]
    #[serial]
    fn rd3() {
        let temp_dir = get_temp_dir();
        insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args { folder: Some(temp_dir.clone()),
                patch: Some("035ef892fa57fe644ef76065849ebd025869614d".to_string()),
                flags: vec![], confirm: false, pair: false, function: false, single: true,  location: false, mixed: false},
                "375981bb06cf819332c202cdd09d5a8c48e296db", rd_run), @r###"
        There are 27 warnings in 1 files, 0 has been fixed.
        "###);
        insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args { folder: Some(temp_dir), 
                patch: Some("035ef892fa57fe644ef76065849ebd025869614d".to_string()),
                flags: vec![], confirm: true, pair: true, function: true, single: true,  location: false, mixed: false},
                "375981bb06cf819332c202cdd09d5a8c48e296db", rd_run), @r###"
        There are 27 warnings in 1 files, 0 has been fixed.
        "###);
    }

    #[test]
    #[serial]
    fn hunks_patch() {
        let temp_dir = get_temp_dir();
        insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args {folder: Some(temp_dir),
            flags: vec![],
            patch: Some("512236bac29f09ca798c93020ce377c30a4ed2a5".to_string()),
            confirm: true,
            pair: false,
            function: false,
            single: true,
            location: false,
            mixed: false,
        }, "2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1", diff_run), 
        @"");
     }

    #[test]
    #[serial]
    fn hunks_pairs() {
        let temp_dir = get_temp_dir();
         insta::assert_snapshot!(rd_setup(temp_dir.clone(), Args {folder: Some(temp_dir),
            flags: vec![],
            patch: Some("512236bac29f09ca798c93020ce377c30a4ed2a5".to_string()),
            confirm: true,
            pair: true,
            function: false,
            single: true,
            location: false,
            mixed: false,
        }, "2468ad1e3c0183f4a94859bcc5cea04ee3fc4ab1", diff_run), 
        @"");
    }
}
