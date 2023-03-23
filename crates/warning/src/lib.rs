use std::{collections::BTreeMap, process::{Command, Stdio}};
use cargo_metadata::{Message, diagnostic::Diagnostic};
use serde::{Serialize, Deserialize};

#[cfg(test)]
pub mod test;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Warning {
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

fn sub_messages(children: &[Diagnostic]) -> String {
    let mut v = children
        .iter()
        .map(|x| {
            if let Some(rendered) = &x.rendered {
                format!("{}: {}", &x.message, &rendered)
            } else {
                x.message.clone()
            }
        })
        .collect::<Vec<String>>();
    v.sort();
    v.join("\n")
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

fn get_flags() -> Vec<String> {
    let mut flags = Vec::new();
    if flags.is_empty() {
        if false { // clippy rules that are machine applicable 
            flags = vec![
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
                // "implicit_return".to_string(),
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
                // "needless_for_each".to_string(),
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
            ];
        } else {
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
    }
    flags
}

/// get all warnings from diagnostics
pub fn warnings(folder: &str) -> BTreeMap<String, Vec<Warning>> {
    let mut map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
    let cargo = get_cargo();
    let manifest = format!("{folder}/Cargo.toml");
    if std::path::Path::new(&manifest).exists() {
        let mut args = vec![
            "clippy".to_string(),
            "--manifest-path".to_string(), 
            manifest,
            "--message-format=json".to_string(),
            "--".to_string(),
        ];
        let flags = get_flags();
        for flag in flags {
            args.push(format!("-Wclippy::{flag}"));
        }
        if let Ok(mut command) = Command::new(cargo)
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
    map
}

use tokei::{Config, Languages, LanguageType};

/// return the LOC of Rust code
pub fn loc(folder: &str) -> usize {
    let paths = &[folder];
    // Exclude any path that contains any of these strings.
    let target = "target".to_string();
    let excluded = &[target.as_str()];
    // `Config` allows you to configure what is searched and counted.
    let config = Config::default();
    let mut languages = Languages::new();
    languages.get_statistics(paths, excluded, &config);
    if languages.contains_key(&LanguageType::Rust) {
        let rust = &languages[&LanguageType::Rust];
        println!("Lines of code: {}", rust.code);
        rust.code
    } else {
        0
    }
}
