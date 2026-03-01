//! Astro-specific formatting logic.
//!
//! This module mirrors the structure of `crates/oxc_parser/src/astro/` — all
//! formatting behaviour that is exclusive to `.astro` files lives here, keeping
//! the core JS/JSX formatters free of Astro-specific concerns.
//!
//! ## Entry points (called from JSX formatters)
//!
//! | Helper | Where called | Purpose |
//! |---|---|---|
//! | [`is_astro_top_level_parent`] | `jsx/element.rs` `get_wrap_state` | Suppress parentheses for top-level Astro JSX |
//! | [`astro_should_self_close`] | `jsx/element.rs` `fmt` | Force self-closing tags in Astro |
//! | [`is_dummy_ancestor`] | `jsx/element.rs` `format_trailing_comments` | Guard Dummy node traversal |

use oxc_allocator::StringBuilder;
use oxc_ast::ast::*;
use oxc_span::GetSpan;

use crate::{
    ast_nodes::{AstNode, AstNodes},
    formatter::{Formatter, prelude::*},
    utils::jsx::is_meaningful_jsx_text,
    write,
};

use super::FormatWrite;

// ── Astro node formatters ─────────────────────────────────────────────────────

impl<'a> FormatWrite<'a> for AstNode<'a, AstroRoot<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let has_frontmatter = self.frontmatter().is_some_and(|fm| fm.span().end > 0);

        // Print frontmatter if present and has actual content (non-zero span end)
        if has_frontmatter {
            write!(f, self.frontmatter().unwrap());
        }

        // Print the body children.
        //
        // At the top level of an Astro document the body is block-level: each
        // element/doctype/comment is separated by at most one blank line.
        // Whitespace-only JSXText nodes are used as separators – we normalise
        // them instead of outputting their raw content verbatim, which would
        // carry over indentation and multiple blank lines from the source.
        let body = self.body();
        let mut first_non_ws = true;
        // pending_separator tracks the separator to emit before the *next*
        // non-whitespace child, collected from whitespace-only text nodes.
        // None  = no separator seen yet (used before the first child)
        // Some(empty_line)  = a blank line was present in the whitespace text
        // Some(hard_line)   = at least one newline, but no blank line
        //
        // When two non-whitespace children are adjacent with NO whitespace
        // text node between them, we still emit a hard_line_break.
        let mut pending_blank_line = false; // true => emit empty_line, false => hard_line_break
        let mut pending_separator = false; // true => we need to emit a separator

        for child in body {
            // Is this child a whitespace-only JSXText?
            if let JSXChild::Text(text) = child.as_ref() {
                let raw = text.value.as_str();
                if !is_meaningful_jsx_text(raw) {
                    // Whitespace-only text: record what separator to use.
                    if !first_non_ws {
                        pending_separator = true;
                        if raw.contains("\n\n") {
                            pending_blank_line = true;
                        }
                    }
                    continue;
                }
            }

            // Non-whitespace child.
            if first_non_ws {
                // If there was frontmatter, emit a blank line before the first
                // template element (Prettier always puts one blank line there).
                // If there was no frontmatter, no leading separator is needed.
                if has_frontmatter {
                    write!(f, empty_line());
                }
                first_non_ws = false;
            } else {
                // Emit separator between non-whitespace children.
                // If we saw a whitespace text with a blank line, emit empty_line;
                // otherwise (including the case of no whitespace text at all
                // between two adjacent non-ws children), emit hard_line_break.
                if pending_separator && pending_blank_line {
                    write!(f, empty_line());
                } else {
                    write!(f, hard_line_break());
                }
            }
            // Reset separator state.
            pending_separator = false;
            pending_blank_line = false;

            write!(f, child);
        }

        write!(f, hard_line_break());
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, AstroFrontmatter<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        // Print: ---\n<TypeScript/JavaScript code>\n---\n
        // The program's FormatWrite impl already ends with hard_line_break(),
        // giving us: ---\n<code>\n---\n
        let program = self.program();
        write!(f, ["---", hard_line_break(), program, "---", hard_line_break()]);
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, AstroScript<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        // AstroScript is the parsed inner program of a <script> tag.
        // The <script> element itself is a JSXElement in the body.
        // We just format the program content here.
        write!(f, self.program());
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, AstroDoctype<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        // Always emit lowercase `<!doctype html>` per HTML spec
        write!(f, "<!doctype html>");
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, AstroComment<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        // Preserve the original HTML comment content verbatim.
        // `self` derefs to `AstroComment`, so `self.value` accesses the public field.
        let value = self.value.as_str();
        let s = StringBuilder::from_strs_array_in(["<!--", value, "-->"], f.context().allocator());
        write!(f, text(s.into_str()));
    }
}

// ── Helpers called by JSX formatters ─────────────────────────────────────────
//
// These tiny functions are the *only* integration points with `print/jsx/`.
// They are `#[inline]` so there is zero overhead in the non-Astro path.

/// Returns `true` when `parent` is an [`AstNodes::AstroRoot`].
///
/// Used in `get_wrap_state` to suppress parentheses around top-level Astro JSX:
/// elements that are direct children of the template body should never be
/// wrapped, just like elements inside `JSXExpressionContainer`.
#[inline]
pub fn is_astro_top_level_parent(parent: &AstNodes<'_>) -> bool {
    matches!(parent, AstNodes::AstroRoot(_))
}

/// Returns `true` when an element with `NoChildren` layout should be rendered
/// as a self-closing tag (`<Tag />`) because we are formatting an Astro file.
///
/// In regular JSX/TSX an element without children is still written with explicit
/// open/close tags if the source had them (`<div></div>`). Astro normalises
/// these to self-closing form, matching the `prettier-plugin-astro` behaviour.
#[inline]
pub fn astro_should_self_close(f: &Formatter<'_, '_>) -> bool {
    f.context().source_type().is_astro()
}

/// Returns `true` when a `JSXSpreadAttribute` in Astro source was written
/// *without* `...` (i.e. it is an Astro shorthand `{expr}` rather than a real
/// spread `{...expr}`).
///
/// We detect this by checking whether the three bytes after the opening `{` in
/// the source text are `...`.  Real spreads always have `...`; Astro shorthands
/// never do.
///
/// This is used in the `JSXSpreadAttribute` formatter to suppress the `...`
/// prefix for Astro shorthand attributes.
///
/// `source_slice` should be the source text of the attribute span itself
/// (e.g. `f.source_text().slice_range(span.start, span.end)`).
#[inline]
pub fn astro_spread_is_shorthand(_span: oxc_span::Span, source_slice: &str) -> bool {
    // A real spread starts with `{...` (possibly with whitespace after `{`);
    // a shorthand starts with `{expr` (no dots).
    // We trim interior whitespace to handle `{  ...meta }` correctly.
    let inner = source_slice.strip_prefix('{').unwrap_or(source_slice).trim_start();
    !inner.starts_with("...")
}

/// Returns `true` when a JSX text node contains only ASCII whitespace characters.
///
/// In Astro, elements whose only child is whitespace-only text (e.g.
/// `<slot>   </slot>` or `<custom-element> </custom-element>`) are treated as
/// having no meaningful children and are formatted as self-closing tags.
///
/// Note: the core `is_meaningful_jsx_text` considers space-only strings
/// (without newlines) as *meaningful*, which is correct for JSX.  For Astro
/// we need a stricter check.
#[inline]
pub fn astro_is_whitespace_only_text(text: &str) -> bool {
    text.bytes().all(|b| matches!(b, b' ' | b'\t' | b'\n' | b'\r'))
}

/// Returns `true` if any of `p1`, `p2`, or `p3` is the sentinel `Dummy` node.
///
/// The triple-parent walk inside `format_trailing_comments` is only safe when
/// none of the ancestors is `Dummy`.  Top-level JSX in an Astro body has
/// `AstroRoot` as grandparent and `Dummy` as great-grandparent, so we must
/// guard the walk to avoid a panic.
#[inline]
pub fn is_dummy_ancestor(p1: &AstNodes<'_>, p2: &AstNodes<'_>, p3: &AstNodes<'_>) -> bool {
    matches!(p1, AstNodes::Dummy())
        || matches!(p2, AstNodes::Dummy())
        || matches!(p3, AstNodes::Dummy())
}
