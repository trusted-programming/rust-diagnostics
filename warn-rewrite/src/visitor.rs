//! HIR visitor that collects rewrite candidates.

extern crate rustc_hir;
extern crate rustc_middle;
extern crate rustc_span;

use rustc_hir::{
    intravisit::{self, Visitor},
    AssignOpKind, BinOpKind, Body, Expr, ExprKind, UnOp,
};
use rustc_middle::ty::{TyCtxt, TypeckResults};
use rustc_span::source_map::SourceMap;

use crate::{LintTarget, Rewrite, RewriteKind};
use crate::classify::classify_cast;

/// Walk all HIR bodies in the crate and collect rewrites.
pub fn collect_rewrites<'tcx>(tcx: TyCtxt<'tcx>, lint: LintTarget) -> Vec<Rewrite> {
    let mut all = Vec::new();
    // hir_body_owners returns DefIds of functions/closures/statics with bodies
    for owner in tcx.hir_body_owners() {
        let typeck = tcx.typeck(owner);
        let body = tcx.hir_maybe_body_owned_by(owner);
        if let Some(body) = body {
            let mut visitor = RewriteVisitor {
                tcx,
                typeck,
                source_map: tcx.sess.source_map(),
                lint,
                rewrites: Vec::new(),
            };
            visitor.visit_body(body);
            all.extend(visitor.rewrites);
        }
    }
    all
}

struct RewriteVisitor<'tcx, 'sm> {
    #[allow(dead_code)]
    tcx: TyCtxt<'tcx>,
    typeck: &'tcx TypeckResults<'tcx>,
    source_map: &'sm SourceMap,
    lint: LintTarget,
    rewrites: Vec<Rewrite>,
}

impl<'tcx> RewriteVisitor<'tcx, '_> {
    fn snippet(&self, span: rustc_span::Span) -> Option<String> {
        self.source_map.span_to_snippet(span).ok()
    }

    /// Convert a resolved integral type to its name for type suffix annotation.
    /// Returns `None` if the type is still an inference variable (ambiguous).
    fn ty_name(ty: rustc_middle::ty::Ty<'tcx>) -> Option<String> {
        use rustc_middle::ty::{IntTy, TyKind, UintTy};
        let name = match ty.kind() {
            TyKind::Int(IntTy::I8) => "i8",
            TyKind::Int(IntTy::I16) => "i16",
            TyKind::Int(IntTy::I32) => "i32",
            TyKind::Int(IntTy::I64) => "i64",
            TyKind::Int(IntTy::I128) => "i128",
            TyKind::Int(IntTy::Isize) => "isize",
            TyKind::Uint(UintTy::U8) => "u8",
            TyKind::Uint(UintTy::U16) => "u16",
            TyKind::Uint(UintTy::U32) => "u32",
            TyKind::Uint(UintTy::U64) => "u64",
            TyKind::Uint(UintTy::U128) => "u128",
            TyKind::Uint(UintTy::Usize) => "usize",
            _ => return None, // Infer(IntVar(_)) or other: type not yet resolved
        };
        Some(name.to_string())
    }

    fn span_to_file_and_offsets(
        &self,
        span: rustc_span::Span,
    ) -> Option<(std::path::PathBuf, u32, u32)> {
        let lo = self.source_map.lookup_byte_offset(span.lo());
        let hi = self.source_map.lookup_byte_offset(span.hi());
        let file = lo.sf.name.prefer_local_unconditionally().to_string();
        let path = std::path::PathBuf::from(file);
        Some((path, lo.pos.0, hi.pos.0))
    }

    fn try_cast_rewrite(&mut self, expr: &Expr<'tcx>) {
        let ExprKind::Cast(inner, _hir_ty) = expr.kind else { return };

        // Skip macro-generated expressions
        if expr.span.from_expansion() || inner.span.from_expansion() {
            return;
        }

        let src_ty = self.typeck.expr_ty(inner);
        let dst_ty = self.typeck.expr_ty(expr);

        let Some(kind) = classify_cast(src_ty, dst_ty) else { return };

        let Some(inner_snippet) = self.snippet(inner.span) else { return };
        let Some((file, full_start, full_end)) = self.span_to_file_and_offsets(expr.span) else { return };

        self.rewrites.push(Rewrite {
            file,
            full_start,
            full_end,
            inner_snippet,
            rhs_snippet: None,
            kind,
        });
    }

    fn try_binop_rewrite(&mut self, expr: &Expr<'tcx>) {
        let ExprKind::Binary(op, lhs, rhs) = expr.kind else { return };

        if expr.span.from_expansion() {
            return;
        }

        let method = match op.node {
            BinOpKind::Add => "wrapping_add",
            BinOpKind::Sub => "wrapping_sub",
            BinOpKind::Mul => "wrapping_mul",
            BinOpKind::Div => "wrapping_div",
            BinOpKind::Rem => "wrapping_rem",
            _ => return,
        };

        let lhs_ty = self.typeck.expr_ty(lhs);
        if !lhs_ty.is_integral() {
            return;
        }
        let Some(lhs_ty_name) = Self::ty_name(lhs_ty) else { return }; // skip if type ambiguous

        let Some(inner_snippet) = self.snippet(lhs.span) else { return };
        let Some(rhs_snippet) = self.snippet(rhs.span) else { return };
        let Some((file, full_start, full_end)) = self.span_to_file_and_offsets(expr.span) else { return };

        self.rewrites.push(Rewrite {
            file,
            full_start,
            full_end,
            inner_snippet,
            rhs_snippet: Some(rhs_snippet),
            kind: RewriteKind::WrappingBinop { method, lhs_ty: lhs_ty_name },
        });
    }

    fn try_assign_op_rewrite(&mut self, expr: &Expr<'tcx>) {
        let ExprKind::AssignOp(op, lhs, rhs) = expr.kind else { return };

        if expr.span.from_expansion() {
            return;
        }

        let method = match op.node {
            AssignOpKind::AddAssign => "wrapping_add",
            AssignOpKind::SubAssign => "wrapping_sub",
            AssignOpKind::MulAssign => "wrapping_mul",
            AssignOpKind::DivAssign => "wrapping_div",
            AssignOpKind::RemAssign => "wrapping_rem",
            _ => return,
        };

        let lhs_ty = self.typeck.expr_ty(lhs);
        if !lhs_ty.is_integral() {
            return;
        }
        if Self::ty_name(lhs_ty).is_none() { return; } // skip if type ambiguous

        let Some(lhs_snippet) = self.snippet(lhs.span) else { return };
        let Some(rhs_snippet) = self.snippet(rhs.span) else { return };
        let Some((file, full_start, full_end)) = self.span_to_file_and_offsets(expr.span) else { return };

        self.rewrites.push(Rewrite {
            file,
            full_start,
            full_end,
            inner_snippet: lhs_snippet.clone(),
            rhs_snippet: Some(rhs_snippet),
            kind: RewriteKind::WrappingAssignOp {
                method,
                lhs_snippet,
            },
        });
    }

    fn try_neg_rewrite(&mut self, expr: &Expr<'tcx>) {
        let ExprKind::Unary(UnOp::Neg, operand) = expr.kind else { return };

        if expr.span.from_expansion() {
            return;
        }

        let ty = self.typeck.expr_ty(operand);
        if !ty.is_integral() {
            return;
        }
        let Some(operand_ty_name) = Self::ty_name(ty) else { return }; // skip if ambiguous

        let Some(inner_snippet) = self.snippet(operand.span) else { return };
        let Some((file, full_start, full_end)) = self.span_to_file_and_offsets(expr.span) else { return };

        self.rewrites.push(Rewrite {
            file,
            full_start,
            full_end,
            inner_snippet,
            rhs_snippet: None,
            kind: RewriteKind::WrappingNeg { operand_ty: operand_ty_name },
        });
    }
}

impl<'tcx> Visitor<'tcx> for RewriteVisitor<'tcx, '_> {
    fn visit_expr(&mut self, expr: &'tcx Expr<'tcx>) {
        if self.lint.includes_as_conversions() {
            if matches!(expr.kind, ExprKind::Cast(..)) {
                self.try_cast_rewrite(expr);
            }
        }
        if self.lint.includes_arithmetic() {
            match expr.kind {
                ExprKind::Binary(..) => self.try_binop_rewrite(expr),
                ExprKind::AssignOp(..) => self.try_assign_op_rewrite(expr),
                ExprKind::Unary(UnOp::Neg, _) => self.try_neg_rewrite(expr),
                _ => {}
            }
        }
        intravisit::walk_expr(self, expr);
    }

    fn visit_body(&mut self, body: &Body<'tcx>) {
        intravisit::walk_body(self, body);
    }
}
