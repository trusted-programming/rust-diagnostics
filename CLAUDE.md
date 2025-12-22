# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build
cargo build

# Run tests (tests must run serially due to shared global state)
cargo test -- --test-threads=1

# Run a specific test
cargo test <test_name> -- --test-threads=1

# Run clippy
cargo clippy

# Install locally
cargo install --path .
```

## Project Overview

rust-diagnostics is a CLI utility that embeds Clippy diagnostic messages as inline comments in Rust source code. It also analyzes git commit history to track how warnings are manually fixed between revisions.

## Architecture

The project is a single-binary CLI tool with two main modules:

- **src/main.rs**: Core logic including:
  - CLI argument parsing via `structopt` (`Args` struct at line 19)
  - Clippy invocation and diagnostic message parsing via `cargo_metadata`
  - Warning markup insertion into source files (`markup` function at line 84)
  - Git diff analysis using `git2-rs` to find patches that fix warnings
  - Function-level context extraction using tree-sitter queries (`splitup` function at line 137)
  - Hunk processing and output formatting (lines 754-802)

- **src/language.rs**: Tree-sitter language binding for Rust parsing

## Key Data Structures

- `Warning`: Represents a clippy diagnostic with byte offsets, line numbers, and suggestion text
- `Hunk`: Represents a git diff hunk with old/new text, surrounding function context, and associated warnings
- `LineRange`: Used to index function items by their source location

## Output

The tool creates a `diagnostics/` folder containing:
- Marked-up source files with embedded warning comments
- `diagnostics.json`: Cached clippy output
- `diagnostics.log`: Summary and patch analysis results

## CLI Options

Key flags for the `--patch` workflow:
- `--patch <commit_id>`: Compare current revision against specified commit
- `--confirm`: Only show hunks that actually fix warnings
- `--pair`: Output before/after code separated by `=== 19a3477889393ea2cdd0edcb5e6ab30c ===`
- `--function` / `-W`: Include surrounding function context
- `--location`: Insert warning markers in the function context
- `--mixed`: Combine context view with patch format
- `--single`: Only show hunks that fix exactly one warning
- `--fix`: Use machine-applicable clippy rules only

## Testing Notes

Tests use `serial_test` crate because they share global state (`ARGS` static mutex). Each test creates a temporary directory (e.g., `tmp_<uuid>`) and performs git operations to set up test scenarios.

Snapshot tests use `insta` crate for comparing expected outputs.
