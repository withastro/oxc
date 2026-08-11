//! Astro-specific lexer functionality
//!
//! Handles the frontmatter delimiter `---` and HTML body with JSX expressions.
//! Contains Astro-specific JSX child reading logic that was previously
//! gated behind `#[cfg(feature = "astro")]` in `lexer/jsx.rs`.

// All Astro-specific lexer functionality is gated behind the `astro` feature.
#[cfg(feature = "astro")]
use super::Lexer;

#[cfg(feature = "astro")]
impl<C: crate::config::LexerConfig> Lexer<'_, C> {
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
    use super::super::{
        Kind, Lexer, Token,
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

    /// An unquoted attribute value ends only at HTML5's terminators — whitespace
    /// or `>` — plus the Astro-structural characters `{`/`}` (an expression
    /// starts/ends) and `<` (a following tag). Everything else is an ordinary
    /// value character, matching the HTML tokenizer, which flags `"`, `'`, `=`,
    /// and backtick as a parse error but still appends them. So `href=a=b` and
    /// `href=https://example.com/x` parse whole; the printer escapes any quotes
    /// or backticks when emitting the value. The `/` self-close form
    /// (`<img src=x />`) still works because the space ends the value first.
    static ASTRO_UNQUOTED_ATTR_VALUE_END_TABLE: SafeByteMatchTable = safe_byte_match_table!(|b| matches!(
        b,
        b'>' | b'{' | b'}' | b' ' | b'\t' | b'\n' | b'\r' | b'<'
    ));

    /// Astro/HTML text content can include `>` as literal text (unlike JSX).
    /// We stop at `<` (potential tag), `{` (expression start), or `}` (expression end).
    /// Note: `}` must still be included because it ends expression containers.
    static ASTRO_TEXT_END_TABLE: SafeByteMatchTable =
        safe_byte_match_table!(|b| b == b'{' || b == b'}' || b == b'<');

    /// As `ASTRO_TEXT_END_TABLE`, but for text with no expression container open
    /// around it, where `}` closes nothing and is therefore ordinary text.
    static ASTRO_TEXT_NO_OPEN_EXPRESSION_END_TABLE: SafeByteMatchTable =
        safe_byte_match_table!(|b| b == b'{' || b == b'<');

    /// Text content inside foreign content elements like `<math>`.
    /// In foreign content, `{` and `}` are literal text, not expression delimiters.
    /// Only `<` stops text scanning (for child tags like `<mi>`, `<mo>`, etc.).
    static ASTRO_FOREIGN_TEXT_END_TABLE: SafeByteMatchTable = safe_byte_match_table!(|b| b == b'<');

    impl<C: crate::config::LexerConfig> Lexer<'_, C> {
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
                Some(b'}') => {
                    if self.no_expression_in_jsx_children {
                        // In foreign content, `}` is literal text
                        self.read_jsx_child_foreign_text()
                    } else if self.astro_jsx_expression_depth == 0 {
                        // No container to close, so this `}` is the first byte of a text run.
                        self.read_jsx_child_text()
                    } else {
                        // Inside an expression container, `}` ends it
                        // (e.g. `{ <!-- comment --> text }` — the `}` closes `{`).
                        self.consume_char();
                        Kind::RCurly
                    }
                }
                Some(_) => {
                    // Inside foreign content (<math>), use the foreign text table
                    // that only stops at `<` and treats `{`/`}` as text.
                    if self.no_expression_in_jsx_children {
                        return self.read_jsx_child_foreign_text();
                    }

                    self.read_jsx_child_text()
                }
                None => Kind::Eof,
            }
        }

        /// Scan to the byte that ends a run of Astro child text, or `None` at EOF.
        ///
        /// In Astro mode, `>` is valid text content (unlike JSX where it's an error), so
        /// neither table stops at it. `}` ends the run only when there is an expression
        /// container open for it to close; it then stays unconsumed and the next
        /// `next_jsx_child()` call returns `RCurly`.
        fn scan_astro_text_end(&mut self) -> Option<u8> {
            if self.astro_jsx_expression_depth == 0 {
                Some(byte_search! {
                    lexer: self,
                    table: ASTRO_TEXT_NO_OPEN_EXPRESSION_END_TABLE,
                    handle_eof: { return None },
                })
            } else {
                Some(byte_search! {
                    lexer: self,
                    table: ASTRO_TEXT_END_TABLE,
                    handle_eof: { return None },
                })
            }
        }

        /// Read a run of Astro JSX child text from the current position.
        fn read_jsx_child_text(&mut self) -> Kind {
            let text_start = self.offset();
            match self.scan_astro_text_end() {
                None if self.offset() == text_start => Kind::Eof,
                _ => Kind::JSXText,
            }
        }

        /// In Astro mode, read JSX text that starts with `<` when the `<` is not a valid tag start.
        /// This happens when `<` is followed by whitespace, numbers, or other non-tag-start characters.
        /// Per HTML spec, `<` only starts a tag when followed by ASCII letter, `/`, `>`, or `!`.
        fn read_jsx_child_text_starting_with_lt(&mut self) -> Kind {
            // Consume the `<` that we already peeked
            self.consume_char();

            loop {
                let Some(next_byte) = self.scan_astro_text_end() else {
                    return Kind::JSXText;
                };
                if next_byte != b'<' {
                    return Kind::JSXText;
                }
                let Some([_, next]) = self.peek_2_bytes() else {
                    return Kind::JSXText;
                };
                // Per HTML spec: only ASCII letters start tag names
                let is_valid_tag_start =
                    next.is_ascii_alphabetic() || next == b'/' || next == b'>' || next == b'!';
                if is_valid_tag_start {
                    return Kind::JSXText;
                }
                // Still not a tag — the `<` is more text
                self.consume_char();
            }
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
        ///
        /// If the very first byte is a terminator (e.g. a stray `}` or `=`), we consume
        /// that single byte so the parser always makes forward progress and cannot
        /// enter an infinite loop.
        pub(crate) fn read_astro_attribute_name(&mut self) -> Token {
            let start = self.offset();
            self.token.set_start(start);

            // Consume all valid attribute name characters (everything except terminators)
            let _next_byte = byte_search! {
                lexer: self,
                table: ASTRO_ATTR_NAME_END_TABLE,
                handle_eof: {
                    return self.finish_next(Kind::Ident);
                },
            };

            // If the scan stopped immediately (first byte was a terminator), consume
            // that one byte so we always make progress.
            if self.offset() == start {
                self.consume_char();
            }

            // We found an ending character, stop here
            self.finish_next(Kind::Ident)
        }

        /// Lex an unquoted HTML-style Astro attribute value at the current position.
        ///
        /// Always emits `Kind::Ident` so unquoted numbers (`maxlength=255`) and
        /// non-identifier values (`color=#abc123`) flow through the existing
        /// string-valued attribute path.
        pub(crate) fn read_astro_unquoted_attribute_value(&mut self) -> Token {
            let start = self.offset();
            self.token.set_start(start);

            let _next_byte = byte_search! {
                lexer: self,
                table: ASTRO_UNQUOTED_ATTR_VALUE_END_TABLE,
                handle_eof: {
                    return self.finish_next(Kind::Ident);
                },
            };

            // Consume one byte to avoid an infinite loop if called on a bare terminator.
            if self.offset() == start {
                self.consume_char();
            }

            self.finish_next(Kind::Ident)
        }
    }
}
