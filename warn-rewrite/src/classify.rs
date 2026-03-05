//! Type classification for cast rewrites.

extern crate rustc_middle;

use rustc_middle::ty::{IntTy, Ty, TyKind, UintTy};

use crate::RewriteKind;

/// Determine the correct rewrite kind for `src as dst`.
/// Returns `None` if no safe rewrite is known (e.g., enum→primitive, float, pointer).
pub fn classify_cast<'tcx>(src: Ty<'tcx>, dst: Ty<'tcx>) -> Option<RewriteKind> {
    match (src.kind(), dst.kind()) {
        // u8 as char → char::from(x)  (always safe, u8 is valid char)
        (TyKind::Uint(UintTy::U8), TyKind::Char) => Some(RewriteKind::CharFrom),

        // Float conversions: skip (semantics are lossy by nature, not an overflow issue)
        (TyKind::Float(_), _) | (_, TyKind::Float(_)) => None,

        // Pointer / raw pointer / ref: skip
        (TyKind::RawPtr(..) | TyKind::Ref(..) | TyKind::FnPtr(..), _)
        | (_, TyKind::RawPtr(..) | TyKind::Ref(..) | TyKind::FnPtr(..)) => None,

        // Enum → primitive: skip (need From impl or repr-specific handling)
        (TyKind::Adt(def, _), _) if def.is_enum() => None,
        (_, TyKind::Adt(def, _)) if def.is_enum() => None,

        // Integer → integer: classify widening vs narrowing
        (TyKind::Int(src_i), TyKind::Int(dst_i)) => {
            let dst_name = int_name(*dst_i);
            if is_widening_signed(*src_i, *dst_i) {
                Some(RewriteKind::TypeFrom { dst: dst_name })
            } else {
                Some(RewriteKind::TryFrom { dst: dst_name })
            }
        }
        (TyKind::Uint(src_u), TyKind::Uint(dst_u)) => {
            let dst_name = uint_name(*dst_u);
            if is_widening_unsigned(*src_u, *dst_u) {
                Some(RewriteKind::TypeFrom { dst: dst_name })
            } else {
                Some(RewriteKind::TryFrom { dst: dst_name })
            }
        }
        // Unsigned → signed: widening only if src fits in dst
        (TyKind::Uint(src_u), TyKind::Int(dst_i)) => {
            let dst_name = int_name(*dst_i);
            if is_widening_uint_to_int(*src_u, *dst_i) {
                Some(RewriteKind::TypeFrom { dst: dst_name })
            } else {
                Some(RewriteKind::TryFrom { dst: dst_name })
            }
        }
        // Signed → unsigned: always potentially lossy
        (TyKind::Int(_), TyKind::Uint(dst_u)) => Some(RewriteKind::TryFrom {
            dst: uint_name(*dst_u),
        }),

        // Everything else: skip
        _ => None,
    }
}

fn int_name(t: IntTy) -> String {
    match t {
        IntTy::I8 => "i8",
        IntTy::I16 => "i16",
        IntTy::I32 => "i32",
        IntTy::I64 => "i64",
        IntTy::I128 => "i128",
        IntTy::Isize => "isize",
    }
    .to_string()
}

fn uint_name(t: UintTy) -> String {
    match t {
        UintTy::U8 => "u8",
        UintTy::U16 => "u16",
        UintTy::U32 => "u32",
        UintTy::U64 => "u64",
        UintTy::U128 => "u128",
        UintTy::Usize => "usize",
    }
    .to_string()
}

/// Returns `true` if casting `src` signed int to `dst` signed int is lossless.
fn is_widening_signed(src: IntTy, dst: IntTy) -> bool {
    bits_signed(src) <= bits_signed(dst)
}

/// Returns `true` if casting `src` unsigned int to `dst` unsigned int is lossless.
fn is_widening_unsigned(src: UintTy, dst: UintTy) -> bool {
    // Usize bits are platform-dependent; only treat as widening if src is small enough
    // that it's always representable (u8/u16 → u32/u64/u128)
    match (bits_unsigned(src), bits_unsigned(dst)) {
        (Some(s), Some(d)) => d >= s,
        (Some(s), None) => s <= 16, // u8/u16 → usize: always safe
        _ => false,
    }
}

/// Returns `true` if casting `src` unsigned int to `dst` signed int is lossless.
fn is_widening_uint_to_int(src: UintTy, dst: IntTy) -> bool {
    match (bits_unsigned(src), bits_signed(dst)) {
        (Some(s), d) => d > s as i32, // dst must have strictly more bits than src
        (None, _) => false, // usize → any signed: never guaranteed lossless
    }
}

fn bits_signed(t: IntTy) -> i32 {
    match t {
        IntTy::I8 => 8,
        IntTy::I16 => 16,
        IntTy::I32 => 32,
        IntTy::I64 => 64,
        IntTy::I128 => 128,
        IntTy::Isize => 64, // conservative: assume 64-bit
    }
}

fn bits_unsigned(t: UintTy) -> Option<u32> {
    match t {
        UintTy::U8 => Some(8),
        UintTy::U16 => Some(16),
        UintTy::U32 => Some(32),
        UintTy::U64 => Some(64),
        UintTy::U128 => Some(128),
        UintTy::Usize => None, // platform-dependent
    }
}
