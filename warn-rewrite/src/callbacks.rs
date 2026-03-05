//! rustc_driver Callbacks implementation.

#![allow(clippy::wildcard_imports)]

use crate::LintTarget;
use crate::apply::is_third_party;
use crate::visitor::collect_rewrites;
use crate::apply::apply_rewrites;

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;

use rustc_driver::{Callbacks, Compilation};
use rustc_interface::interface::Compiler;
use rustc_middle::ty::TyCtxt;

pub struct RewriteCallbacks {
    lint: LintTarget,
    dry_run: bool,
}

impl Callbacks for RewriteCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        let rewrites = collect_rewrites(tcx, self.lint);

        if rewrites.is_empty() {
            return Compilation::Continue;
        }

        if self.dry_run {
            for r in &rewrites {
                if !is_third_party(&r.file) {
                    println!(
                        "REWRITE {:?} {}..{} kind={:?}",
                        r.file, r.full_start, r.full_end, r.kind
                    );
                }
            }
        } else {
            let n = rewrites.len();
            apply_rewrites(rewrites);
            eprintln!("warn-rewrite: applied {} rewrites", n);
        }

        Compilation::Continue
    }
}

pub fn run_with_callbacks(rustc_args: Vec<String>, lint: LintTarget, dry_run: bool) {
    let mut callbacks = RewriteCallbacks { lint, dry_run };
    rustc_driver::run_compiler(&rustc_args, &mut callbacks);
}
