use memchr::memchr;

use oxc_span::Span;
use oxc_syntax::identifier::is_identifier_part;

use crate::diagnostics;

use super::{
    Kind, Lexer, Token, cold_branch,
    search::{SafeByteMatchTable, byte_search, safe_byte_match_table},
};

static NOT_ASCII_JSX_ID_CONTINUE_TABLE: SafeByteMatchTable =
    safe_byte_match_table!(|b| !(b.is_ascii_alphanumeric() || matches!(b, b'_' | b'$' | b'-')));

/// Astro/HTML attribute names end at these characters.
/// Everything else is valid in an attribute name.
///
/// Note: Unlike JSX, HTML attribute names CAN contain quotes (`'` and `"`).
/// Quotes are only special when delimiting attribute values, not in names.
/// Example: `<div '"attr />` has attribute named `'"attr`.
/// Per HTML spec, attribute names end at: whitespace, `=`, `>`, `/`.
/// We also stop at `{`, `}`, `<` for Astro expression syntax.
#[cfg(feature = "astro")]
static ASTRO_ATTR_NAME_END_TABLE: SafeByteMatchTable = safe_byte_match_table!(|b| matches!(
    b,
    b'=' | b'>' | b'/' | b'{' | b'}' | b' ' | b'\t' | b'\n' | b'\r' | b'<'
));

static JSX_CHILD_END_TABLE: SafeByteMatchTable =
    safe_byte_match_table!(|b| b == b'{' || b == b'}' || b == b'>' || b == b'<');

/// Astro/HTML text content can include `>` as literal text (unlike JSX).
/// We stop at `<` (potential tag), `{` (expression start), or `}` (expression end).
/// Note: `}` must still be included because it ends expression containers.
#[cfg(feature = "astro")]
static ASTRO_TEXT_END_TABLE: SafeByteMatchTable =
    safe_byte_match_table!(|b| b == b'{' || b == b'}' || b == b'<');

/// Text content inside foreign content elements like `<math>`.
/// In foreign content, `{` and `}` are literal text, not expression delimiters.
/// Only `<` stops text scanning (for child tags like `<mi>`, `<mo>`, etc.).
#[cfg(feature = "astro")]
static ASTRO_FOREIGN_TEXT_END_TABLE: SafeByteMatchTable =
    safe_byte_match_table!(|b| b == b'<');

/// `JSXDoubleStringCharacters` ::
///   `JSXDoubleStringCharacter` `JSXDoubleStringCharactersopt`
/// `JSXDoubleStringCharacter` ::
///   `JSXStringCharacter` but not "
/// `JSXSingleStringCharacters` ::
///   `JSXSingleStringCharacter` `JSXSingleStringCharactersopt`
/// `JSXSingleStringCharacter` ::
///   `JSXStringCharacter` but not '
/// `JSXStringCharacter` ::
///   `SourceCharacter` but not one of `HTMLCharacterReference`
impl Lexer<'_> {
    /// Read JSX string literal.
    /// # SAFETY
    /// * `delimiter` must be an ASCII character.
    /// * Next char in `lexer.source` must be ASCII.
    pub(super) unsafe fn read_jsx_string_literal(&mut self, delimiter: u8) -> Kind {
        debug_assert!(delimiter.is_ascii());

        // Skip opening quote
        // SAFETY: Caller guarantees next byte is ASCII, so `.add(1)` is a UTF-8 char boundary
        let after_opening_quote = unsafe { self.source.position().add(1) };
        let remaining = self.source.str_from_pos_to_end(after_opening_quote);

        let len = memchr(delimiter, remaining.as_bytes());
        if let Some(len) = len {
            // SAFETY: `after_opening_quote` + `len` is position of delimiter.
            // Caller guarantees delimiter is ASCII, so 1 byte after it is a UTF-8 char boundary.
            let after_closing_quote = unsafe { after_opening_quote.add(len + 1) };
            self.source.set_position(after_closing_quote);
            Kind::Str
        } else {
            self.source.advance_to_end();
            self.error(diagnostics::unterminated_string(self.unterminated_range()));
            Kind::Eof
        }
    }

    pub(crate) fn next_jsx_child(&mut self) -> Token {
        self.token.set_start(self.offset());
        let kind = self.read_jsx_child();
        self.finish_next(kind)
    }

    /// [`JSXChild`](https://facebook.github.io/jsx/#prod-JSXChild)
    /// `JSXChild` :
    /// `JSXText`
    /// `JSXElement`
    /// `JSXFragment`
    /// { `JSXChildExpressionopt` }
    fn read_jsx_child(&mut self) -> Kind {
        match self.peek_byte() {
            Some(b'<') => {
                // In Astro mode, check if this is a valid HTML tag start.
                // Per HTML spec, a tag can only start with `<` followed by:
                // - ASCII letter (a-z, A-Z) for tag names
                // - `/` for closing tags
                // - `>` for fragments `<>`
                // - `!` for comments/DOCTYPE
                // Anything else (space, number, punctuation) means `<` is just text.
                #[cfg(feature = "astro")]
                if self.source_type.is_astro()
                    && let Some([_, next]) = self.peek_2_bytes()
                {
                    // Valid tag starters after `<` per HTML spec:
                    // - ASCII letters (a-z, A-Z) for tag names
                    // - `/` for closing tags
                    // - `>` for fragments `<>`
                    // - `!` for comments/DOCTYPE
                    // Anything else (space, number, punctuation, non-ASCII) means `<` is text.
                    // Note: Unlike JSX, Astro follows HTML and does NOT support Unicode tag names.
                    let is_valid_tag_start =
                        next.is_ascii_alphabetic() || next == b'/' || next == b'>' || next == b'!';
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
                #[cfg(feature = "astro")]
                if self.no_expression_in_jsx_children {
                    return self.read_jsx_child_foreign_text();
                }
                self.consume_char();
                Kind::LCurly
            }
            Some(b'}') if cfg!(feature = "astro") && self.no_expression_in_jsx_children => {
                // In foreign content, `}` is also literal text
                self.read_jsx_child_foreign_text()
            }
            Some(_) => {
                // In Astro mode inside foreign content (<math>), use the foreign text table
                // that only stops at `<` and treats `{`/`}` as text.
                #[cfg(feature = "astro")]
                if self.source_type.is_astro() && self.no_expression_in_jsx_children {
                    return self.read_jsx_child_foreign_text();
                }

                // In Astro mode, `>` is valid text content (unlike JSX where it's an error).
                // Use a more permissive table that doesn't stop at `>`.
                #[cfg(feature = "astro")]
                if self.source_type.is_astro() {
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
                    return cold_branch(|| {
                        let start = self.offset();
                        self.error(diagnostics::unexpected_jsx_end(
                            Span::empty(start),
                            next_byte as char,
                            "rbrace",
                        ));
                        Kind::Eof
                    });
                }

                let next_byte = byte_search! {
                    lexer: self,
                    table: JSX_CHILD_END_TABLE,
                    handle_eof: {
                        return Kind::Eof;
                    },
                };

                if matches!(next_byte, b'<' | b'{') {
                    Kind::JSXText
                } else {
                    cold_branch(|| {
                        let start = self.offset();
                        self.error(diagnostics::unexpected_jsx_end(
                            Span::empty(start),
                            next_byte as char,
                            if next_byte == b'}' { "rbrace" } else { "gt" },
                        ));
                        Kind::Eof
                    })
                }
            }
            None => Kind::Eof,
        }
    }

    /// In Astro mode, read JSX text that starts with `<` when the `<` is not a valid tag start.
    /// This happens when `<` is followed by whitespace, numbers, or other non-tag-start characters.
    /// Per HTML spec, `<` only starts a tag when followed by ASCII letter, `/`, `>`, or `!`.
    #[cfg(feature = "astro")]
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
    #[cfg(feature = "astro")]
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

    /// Expand the current `Ident` token for `JSXIdentifier`
    ///
    /// The current character is at `Ident`, continue reading for `JSXIdentifier` if it has a `-`
    ///
    /// `JSXIdentifier` :
    ///   `IdentifierStart`
    ///   `JSXIdentifier` `IdentifierPart`
    ///   `JSXIdentifier` [no `WhiteSpace` or Comment here] -
    pub(crate) fn continue_lex_jsx_identifier(&mut self) -> Option<Token> {
        if self.peek_byte() != Some(b'-') {
            return None;
        }
        self.consume_char();

        // Consume bytes which are part of identifier tail
        let next_byte = byte_search! {
            lexer: self,
            table: NOT_ASCII_JSX_ID_CONTINUE_TABLE,
            handle_eof: {
                return Some(self.finish_next(Kind::Ident));
            },
        };

        // Found a matching byte.
        // Either end of identifier found, or a Unicode char.
        if !next_byte.is_ascii() {
            // Unicode chars are rare in identifiers, so cold branch to keep common path for ASCII
            // as fast as possible
            cold_branch(|| {
                while let Some(c) = self.peek_char() {
                    if c == '-' || is_identifier_part(c) {
                        self.consume_char();
                    } else {
                        break;
                    }
                }
            });
        }

        Some(self.finish_next(Kind::Ident))
    }

    /// Lex an Astro/HTML attribute name starting from the current position.
    ///
    /// HTML attribute names are very permissive - they can contain almost any character
    /// except whitespace, `=`, `>`, `/`, quotes, and a few others.
    ///
    /// This method reads from the current position until an attribute-name-ending character.
    #[cfg(feature = "astro")]
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
