//! Astro-specific lexer functionality
//!
//! Handles the frontmatter delimiter `---` and HTML body with JSX expressions.
//! Contains Astro-specific JSX child reading logic that was previously
//! gated behind `#[cfg(feature = "astro")]` in `lexer/jsx.rs`.

// All Astro-specific lexer functionality is gated behind the `astro` feature.
#[cfg(feature = "astro")]
use super::Lexer;

#[cfg(feature = "astro")]
impl Lexer<'_> {
    /// Set the lexer position for Astro parsing.
    /// This is used to skip to a specific offset in the source.
    pub(crate) fn set_position_for_astro(&mut self, offset: u32) {
        let source_start = self.source.whole().as_ptr();
        // SAFETY: offset is within bounds of the source text
        let new_ptr = unsafe { source_start.add(offset as usize) };
        // SAFETY: Creating a SourcePosition from a valid offset within the source
        let new_pos = unsafe { super::source::SourcePosition::new(new_ptr) };
        self.source.set_position(new_pos);
    }
}

// Astro-specific JSX child reading and attribute name lexing.
#[cfg(feature = "astro")]
mod astro_jsx {
    use oxc_span::Span;

    use crate::diagnostics;

    use super::super::{
        Kind, Lexer, Token, cold_branch,
        search::{SafeByteMatchTable, byte_search, safe_byte_match_table},
    };

    /// Astro/HTML attribute names end at these characters.
    /// Everything else is valid in an attribute name.
    ///
    /// Note: Unlike JSX, HTML attribute names CAN contain quotes (`'` and `"`).
    /// Quotes are only special when delimiting attribute values, not in names.
    /// Example: `<div '"attr />` has attribute named `'"attr`.
    /// Per HTML spec, attribute names end at: whitespace, `=`, `>`, `/`.
    /// We also stop at `{`, `}`, `<` for Astro expression syntax.
    static ASTRO_ATTR_NAME_END_TABLE: SafeByteMatchTable = safe_byte_match_table!(|b| matches!(
        b,
        b'=' | b'>' | b'/' | b'{' | b'}' | b' ' | b'\t' | b'\n' | b'\r' | b'<'
    ));

    /// Astro/HTML text content can include `>` as literal text (unlike JSX).
    /// We stop at `<` (potential tag), `{` (expression start), or `}` (expression end).
    /// Note: `}` must still be included because it ends expression containers.
    static ASTRO_TEXT_END_TABLE: SafeByteMatchTable =
        safe_byte_match_table!(|b| b == b'{' || b == b'}' || b == b'<');

    /// Text content inside foreign content elements like `<math>`.
    /// In foreign content, `{` and `}` are literal text, not expression delimiters.
    /// Only `<` stops text scanning (for child tags like `<mi>`, `<mo>`, etc.).
    static ASTRO_FOREIGN_TEXT_END_TABLE: SafeByteMatchTable = safe_byte_match_table!(|b| b == b'<');

    impl Lexer<'_> {
        /// Read a JSX child token in Astro mode.
        ///
        /// This is the Astro-specific version of `read_jsx_child` that handles:
        /// - `<` not followed by valid tag-start characters as text
        /// - `{` and `}` as literal text in foreign content (e.g. `<math>`)
        /// - `>` as valid text content (unlike standard JSX)
        pub(in super::super) fn read_astro_jsx_child(&mut self) -> Kind {
            match self.peek_byte() {
                Some(b'<') => {
                    // In Astro mode, check if this is a valid HTML tag start.
                    // Per HTML spec, a tag can only start with `<` followed by:
                    // - ASCII letter (a-z, A-Z) for tag names
                    // - `/` for closing tags
                    // - `>` for fragments `<>`
                    // - `!` for comments/DOCTYPE
                    // Anything else (space, number, punctuation) means `<` is just text.
                    if let Some([_, next]) = self.peek_2_bytes() {
                        let is_valid_tag_start = next.is_ascii_alphabetic()
                            || next == b'/'
                            || next == b'>'
                            || next == b'!';
                        if !is_valid_tag_start {
                            // Not a valid tag start - read as JSXText including the `<`
                            return self.read_jsx_child_text_starting_with_lt();
                        }
                    }
                    self.consume_char();
                    Kind::LAngle
                }
                Some(b'{') => {
                    // In foreign content (e.g., <math>), `{` is literal text
                    if self.no_expression_in_jsx_children {
                        return self.read_jsx_child_foreign_text();
                    }
                    self.consume_char();
                    Kind::LCurly
                }
                Some(b'}') if self.no_expression_in_jsx_children => {
                    // In foreign content, `}` is also literal text
                    self.read_jsx_child_foreign_text()
                }
                Some(_) => {
                    // Inside foreign content (<math>), use the foreign text table
                    // that only stops at `<` and treats `{`/`}` as text.
                    if self.no_expression_in_jsx_children {
                        return self.read_jsx_child_foreign_text();
                    }

                    // In Astro mode, `>` is valid text content (unlike JSX where it's an error).
                    // Use a more permissive table that doesn't stop at `>`.
                    let text_start = self.offset();
                    let next_byte = byte_search! {
                        lexer: self,
                        table: ASTRO_TEXT_END_TABLE,
                        handle_eof: {
                            // In Astro, reaching EOF while scanning text means we have text content
                            // Return JSXText if we actually scanned any text, otherwise Eof
                            return if self.offset() > text_start {
                                Kind::JSXText
                            } else {
                                Kind::Eof
                            };
                        },
                    };
                    // `<` and `{` are valid stopping points (start of tag/expression)
                    // `}` is still an error (unexpected closing brace)
                    if matches!(next_byte, b'<' | b'{') {
                        return Kind::JSXText;
                    }
                    // `}` - unexpected closing brace
                    cold_branch(|| {
                        let start = self.offset();
                        self.error(diagnostics::unexpected_jsx_end(
                            Span::empty(start),
                            next_byte as char,
                            "rbrace",
                        ));
                        Kind::Eof
                    })
                }
                None => Kind::Eof,
            }
        }

        /// In Astro mode, read JSX text that starts with `<` when the `<` is not a valid tag start.
        /// This happens when `<` is followed by whitespace, numbers, or other non-tag-start characters.
        /// Per HTML spec, `<` only starts a tag when followed by ASCII letter, `/`, `>`, or `!`.
        fn read_jsx_child_text_starting_with_lt(&mut self) -> Kind {
            // Consume the `<` that we already peeked
            self.consume_char();

            // In Astro/HTML, `>` is valid text content (unlike JSX where it's an error).
            // We only stop at `<` (potential tag) or `{` (expression start).
            let next_byte = byte_search! {
                lexer: self,
                table: ASTRO_TEXT_END_TABLE,
                handle_eof: {
                    return Kind::JSXText;
                },
            };

            // We found `<` or `{`.
            // For `<`, we need to check if it's a valid tag start.
            if next_byte == b'<' {
                // Check if this new `<` is a valid tag start
                if let Some([_, next]) = self.peek_2_bytes() {
                    // Per HTML spec: only ASCII letters start tag names
                    let is_valid_tag_start =
                        next.is_ascii_alphabetic() || next == b'/' || next == b'>' || next == b'!';
                    if !is_valid_tag_start {
                        // Still not a valid tag - recursively continue reading text
                        return self.read_jsx_child_text_starting_with_lt();
                    }
                }
            }
            // Either `{` or a valid tag start `<` - return JSXText
            Kind::JSXText
        }

        /// Read JSX child text inside foreign content (e.g., `<math>`).
        /// `{` and `}` are treated as literal text. Only `<` stops scanning.
        fn read_jsx_child_foreign_text(&mut self) -> Kind {
            let text_start = self.offset();
            let _next_byte = byte_search! {
                lexer: self,
                table: ASTRO_FOREIGN_TEXT_END_TABLE,
                handle_eof: {
                    return if self.offset() > text_start {
                        Kind::JSXText
                    } else {
                        Kind::Eof
                    };
                },
            };
            // Only `<` can stop us. Check if we scanned any text.
            if self.offset() > text_start {
                Kind::JSXText
            } else {
                // We're right at a `<`, let the caller handle it
                Kind::Eof
            }
        }

        /// Lex an Astro/HTML attribute name starting from the current position.
        ///
        /// HTML attribute names are very permissive - they can contain almost any character
        /// except whitespace, `=`, `>`, `/`, quotes, and a few others.
        ///
        /// This method reads from the current position until an attribute-name-ending character.
        pub(crate) fn read_astro_attribute_name(&mut self) -> Token {
            self.token.set_start(self.offset());

            // Consume all valid attribute name characters (everything except terminators)
            let _next_byte = byte_search! {
                lexer: self,
                table: ASTRO_ATTR_NAME_END_TABLE,
                handle_eof: {
                    return self.finish_next(Kind::Ident);
                },
            };

            // We found an ending character, stop here
            self.finish_next(Kind::Ident)
        }
    }
}
