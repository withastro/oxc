//! Astro-specific JSX parsing.
//!
//! This module contains Astro-specific overrides of JSX parsing functions.
//! These duplicate the logic from `jsx/mod.rs` with Astro-specific behavior
//! (void elements, raw text elements, foreign content, HTML comments, etc.)
//! so that the standard JSX code remains free of `#[cfg(feature = "astro")]` blocks.
//!
//! Functions are named `parse_astro_*` to avoid conflicts with the standard JSX
//! functions. The Astro entry points call these instead of the standard versions.

use oxc_allocator::{Allocator, Box, Dummy, Vec};
use oxc_ast::ast::*;
use oxc_span::{Atom, GetSpan, Span};

use crate::{ParserImpl, config::ParserConfig, diagnostics, lexer::Kind};

/// Represents either a closing JSX element or fragment (Astro copy).
enum JSXClosing<'a> {
    /// [`JSXClosingElement`]
    Element(Box<'a, JSXClosingElement<'a>>),
    /// [`JSXClosingFragment`]
    Fragment(JSXClosingFragment),
}

impl<'a> Dummy<'a> for JSXClosing<'a> {
    fn dummy(allocator: &'a Allocator) -> Self {
        JSXClosing::Fragment(Dummy::dummy(allocator))
    }
}

/// How an element's children are read.
#[derive(Clone, Copy, PartialEq)]
enum AstroElementContent {
    /// Ordinary JSX children.
    Jsx,
    /// Everything up to the closing tag is one text node (`<style>`, `is:raw`).
    RawText,
    /// Tags are text but `{…}` expressions are still parsed (`<title>`, `<textarea>`).
    RcData,
}

impl<'a, C: ParserConfig> ParserImpl<'a, C> {
    // ==================== Astro-specific JSX entry points ====================
    //
    // These are Astro-specific versions of the core JSX parsing functions.
    // They handle void elements, raw text elements, foreign content, HTML comments,
    // `<script>` special handling, shorthand attributes, etc.
    //
    // They are named `parse_astro_*` to avoid conflicts with the standard
    // `parse_jsx_*` functions in `jsx/mod.rs`.

    /// Astro-specific version of `parse_jsx_expression`.
    /// Handles `<script>` special handling in expression context.
    pub(crate) fn parse_astro_jsx_expression(&mut self) -> Expression<'a> {
        let span = self.start_span();
        self.bump_any(); // bump `<`
        let kind = self.cur_kind();
        if kind == Kind::RAngle {
            Expression::JSXFragment(self.parse_astro_jsx_fragment(span, false))
        } else if kind.is_identifier_or_keyword() {
            // In Astro, check for <script> which needs special handling
            if self.at_astro_script_tag() {
                // parse_astro_script_in_jsx returns JSXChild, convert to Expression
                return match self.parse_astro_script_in_jsx(span, false) {
                    JSXChild::Element(el) => Expression::JSXElement(el),
                    JSXChild::AstroScript(script) => {
                        // Wrap AstroScript in a synthetic JSX element for expression context
                        let script_span = script.span;
                        let name = self
                            .ast
                            .jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
                        let elem_name = JSXElementName::Identifier(self.alloc(name));
                        let opening = self.ast.alloc_jsx_opening_element(
                            script_span,
                            elem_name,
                            Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                            self.ast.vec(),
                        );
                        Expression::JSXElement(self.ast.alloc_jsx_element(
                            script_span,
                            opening,
                            self.ast.vec1(JSXChild::AstroScript(script)),
                            Option::<Box<'a, JSXClosingElement<'a>>>::None,
                        ))
                    }
                    _ => unreachable!("parse_astro_script_in_jsx returns Element or AstroScript"),
                };
            }
            Expression::JSXElement(self.parse_astro_jsx_element(span, false))
        } else {
            self.unexpected()
        }
    }

    /// Astro-specific version of `parse_jsx_fragment`.
    /// Delegates to the standard `parse_jsx_fragment` for most logic but uses
    /// Astro-specific children parsing.
    pub(crate) fn parse_astro_jsx_fragment(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXFragment<'a>> {
        let outer_depth = self.lexer.astro_jsx_expression_depth;
        // Expression containers are counted per element: see `parse_astro_jsx_element`.
        self.lexer.astro_jsx_expression_depth = 0;
        self.expect_jsx_child(Kind::RAngle);
        let opening_fragment = self.ast.jsx_opening_fragment(self.end_span(span));
        let (children, closing) =
            self.parse_astro_jsx_children_and_closing(in_jsx_child, outer_depth);
        self.lexer.astro_jsx_expression_depth = outer_depth;
        let closing_fragment = match closing {
            Some(JSXClosing::Fragment(f)) => f,
            Some(JSXClosing::Element(e)) => {
                self.error(diagnostics::jsx_fragment_no_match(
                    opening_fragment.span,
                    e.name.span(),
                ));
                self.ast.jsx_closing_fragment(e.span)
            }
            None => {
                let eof = self.astro_eof_span();
                self.error(diagnostics::jsx_fragment_no_match(opening_fragment.span, eof));
                self.ast.jsx_closing_fragment(eof)
            }
        };
        let end = closing_fragment.span.end.max(self.prev_token_end);
        self.ast.alloc_jsx_fragment(
            Span::new(span, end),
            opening_fragment,
            children,
            closing_fragment,
        )
    }

    /// Astro-specific version of `parse_jsx_element`.
    /// Handles raw text elements, void elements, and foreign content.
    pub(crate) fn parse_astro_jsx_element(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXElement<'a>> {
        // Expression containers are counted per element: a `}` in this element's
        // children never closes a container opened outside it, so the depth restarts
        // at 0 for the children and is restored for the token after the closing tag.
        let outer_depth = self.lexer.astro_jsx_expression_depth;
        let (opening_element, self_closing, content_kind) =
            self.parse_astro_jsx_opening_element(span, in_jsx_child);

        let (children, closing_element) = if self_closing {
            (self.ast.vec(), None)
        } else {
            // Track this element as open so nested children can detect stray
            // closing tags (e.g. an extra `</div>`) that match no open element.
            self.astro_open_elements
                .push(opening_element.name.span().source_text(self.source_text));

            // Raw text elements (script/style/is:raw): content is raw text, not JSX children.
            let (children, closing) = if content_kind == AstroElementContent::Jsx {
                self.parse_astro_jsx_children_and_closing(in_jsx_child, outer_depth)
            } else {
                let children = if content_kind == AstroElementContent::RawText {
                    self.skip_astro_raw_text_element_content(&opening_element.name, in_jsx_child)
                } else {
                    self.parse_astro_rcdata_element_content(&opening_element.name, in_jsx_child)
                };
                // Without a closing tag, the content ran to the end of the input.
                let closing = if self.at(Kind::Eof) {
                    None
                } else {
                    // Parse `</name>` closing tag
                    let closing_span = self.start_span();
                    self.bump_any(); // bump `<`
                    self.bump_any(); // bump `/`
                    Some(self.parse_astro_jsx_closing_inline(
                        closing_span,
                        in_jsx_child,
                        outer_depth,
                    ))
                };
                (children, closing)
            };

            // This element is no longer open.
            let _ = self.astro_open_elements.pop();
            self.sync_astro_no_expression();
            self.lexer.astro_jsx_expression_depth = outer_depth;

            let Some(closing) = closing else {
                // Unclosed at the end of the input: report it, and end the element there.
                let name_span = opening_element.name.span();
                self.error(diagnostics::jsx_element_no_match(
                    name_span,
                    self.astro_eof_span(),
                    name_span.source_text(self.source_text),
                ));
                return self.ast.alloc_jsx_element(
                    Span::new(span, self.astro_eof_span().end),
                    opening_element,
                    children,
                    None::<Box<'a, JSXClosingElement<'a>>>,
                );
            };
            let closing_element = match closing {
                JSXClosing::Element(e) => {
                    if !Self::astro_jsx_element_name_eq(&opening_element.name, &e.name) {
                        self.error(diagnostics::jsx_element_no_match(
                            opening_element.name.span(),
                            e.name.span(),
                            opening_element.name.span().source_text(self.source_text),
                        ));
                    }
                    e
                }
                JSXClosing::Fragment(f) => {
                    return self.fatal_error(diagnostics::jsx_element_no_match(
                        opening_element.name.span(),
                        f.span,
                        opening_element.name.span().source_text(self.source_text),
                    ));
                }
            };
            (children, Some(closing_element))
        };
        self.ast.alloc_jsx_element(self.end_span(span), opening_element, children, closing_element)
    }

    /// Astro-specific version of `parse_jsx_opening_element`.
    /// Returns (opening_element, self_closing, content_kind).
    fn parse_astro_jsx_opening_element(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> (
        Box<'a, JSXOpeningElement<'a>>,
        bool,                // `true` if self-closing
        AstroElementContent, // how the children are read
    ) {
        let name = self.parse_astro_jsx_element_name();
        let type_arguments = if self.is_ts { self.try_parse_type_arguments() } else { None };
        let attributes = self.parse_astro_jsx_attributes();
        let explicit_self_closing = self.eat(Kind::Slash);

        // HTML void elements are implicitly self-closing even without `/`
        let self_closing = explicit_self_closing || Self::is_astro_void_element(&name);

        // `is:raw` wins over RCDATA, so `<textarea is:raw>` suppresses expressions too.
        let content_kind =
            if Self::is_astro_raw_text_element(&name) || Self::has_is_raw_attribute(&attributes) {
                AstroElementContent::RawText
            } else if Self::is_astro_rcdata_element(&name) {
                AstroElementContent::RcData
            } else {
                AstroElementContent::Jsx
            };
        let is_raw_text = content_kind != AstroElementContent::Jsx;

        // For foreign content elements like <math>, set the no-expression flag
        if Self::is_foreign_content_element(&name) && !self_closing {
            self.lexer.no_expression_in_jsx_children = true;
        }

        if !self_closing {
            // The token after `>` is the first child; see `parse_astro_jsx_element`.
            self.lexer.astro_jsx_expression_depth = 0;
        }

        // For raw text elements, don't advance past `>` — the caller will reposition the lexer.
        if is_raw_text && !explicit_self_closing {
            self.expect_without_advance(Kind::RAngle);
            self.prev_token_end = self.cur_token().span().end;
        } else if !self_closing || in_jsx_child {
            self.expect_jsx_child(Kind::RAngle);
        } else {
            self.expect(Kind::RAngle);
        }
        let elem = self.ast.alloc_jsx_opening_element(
            self.end_span(span),
            name,
            type_arguments,
            attributes,
        );
        (elem, self_closing, content_kind)
    }

    /// Astro-specific version of `parse_jsx_element_name`.
    /// In Astro, namespaced element names are not supported.
    pub(crate) fn parse_astro_jsx_element_name(&mut self) -> JSXElementName<'a> {
        let span = self.start_span();
        let identifier = self.parse_jsx_identifier();

        // In Astro, namespaced element names are not supported — don't parse `<ns:tag>`.

        // <member.foo.bar />
        if self.at(Kind::Dot) {
            return JSXElementName::MemberExpression(
                self.parse_jsx_member_expression(span, &identifier),
            );
        }

        if self.fatal_error.is_some() {
            return JSXElementName::dummy(self.ast.allocator);
        }

        let name = identifier.name.as_str();
        let is_reference = match name.as_bytes()[0] {
            b if b.is_ascii() => !b.is_ascii_lowercase(),
            _ => true,
        } && !name.contains('-');

        if is_reference {
            let identifier = self.ast.alloc_identifier_reference(identifier.span, identifier.name);
            JSXElementName::IdentifierReference(identifier)
        } else if name == "this" {
            JSXElementName::ThisExpression(self.ast.alloc_this_expression(identifier.span))
        } else {
            JSXElementName::Identifier(self.alloc(identifier))
        }
    }

    /// Astro-specific version of `parse_jsx_children_and_closing`.
    /// Handles `<script>` tags, HTML comments, and `<!` constructs.
    ///
    /// Returns `None` for the closing tag when the input ends first. `outer_depth` is
    /// the expression depth to restore for the token after the closing tag.
    fn parse_astro_jsx_children_and_closing(
        &mut self,
        in_jsx_child: bool,
        outer_depth: u32,
    ) -> (Vec<'a, JSXChild<'a>>, Option<JSXClosing<'a>>) {
        let mut children = self.ast.vec();
        loop {
            if self.fatal_error.is_some() {
                let closing = self.ast.jsx_closing_fragment(self.cur_token().span());
                return (children, Some(JSXClosing::Fragment(closing)));
            }

            match self.cur_kind() {
                Kind::LAngle => {
                    let span = self.start_span();
                    self.bump_any(); // bump `<`
                    let kind = self.cur_kind();

                    // <> open nested fragment
                    if kind == Kind::RAngle {
                        children
                            .push(JSXChild::Fragment(self.parse_astro_jsx_fragment(span, true)));
                        continue;
                    }

                    // <ident open nested element
                    if kind == Kind::Ident || kind.is_any_keyword() {
                        // Check for <script> which needs special handling
                        if self.at_astro_script_tag() {
                            children.push(self.parse_astro_script_in_jsx(span, true));
                            continue;
                        }
                        children.push(JSXChild::Element(self.parse_astro_jsx_element(span, true)));
                        continue;
                    }

                    // <! in Astro - HTML comment or doctype
                    if kind == Kind::Bang {
                        if let Some(child) = self.parse_html_comment_in_jsx(span) {
                            children.push(child);
                            continue;
                        }
                        return (children, self.unexpected());
                    }

                    // </ closing tag
                    if kind == Kind::Slash {
                        if self.at_astro_bogus_closing_tag() {
                            self.skip_astro_bogus_closing_tag();
                            continue;
                        }
                        self.bump_any(); // bump `/`
                        let closing =
                            self.parse_astro_jsx_closing_inline(span, in_jsx_child, outer_depth);
                        // A named closing tag that matches no currently-open element is
                        // stray (e.g. an extra `</div>`). Report it at its own location
                        // and skip it, so the surrounding elements still pair up correctly
                        // instead of blaming an unrelated ancestor.
                        if let JSXClosing::Element(e) = &closing {
                            let closing_name = e.name.span().source_text(self.source_text);
                            if !self
                                .astro_open_elements
                                .iter()
                                .any(|open| Self::astro_closing_name_matches(open, closing_name))
                            {
                                self.error(diagnostics::jsx_unexpected_closing_tag(
                                    e.name.span(),
                                    closing_name,
                                ));
                                continue;
                            }
                        }
                        return (children, Some(closing));
                    }

                    return (children, self.unexpected());
                }
                Kind::Eof => return (children, None),
                Kind::LCurly => {
                    children.push(self.parse_astro_jsx_child_expression());
                }
                Kind::JSXText => {
                    children.push(JSXChild::Text(self.parse_jsx_text()));
                }
                _ => {
                    return (children, self.unexpected());
                }
            }
        }
    }

    /// Astro-specific version of `parse_jsx_closing_inline`.
    ///
    /// Unless the tag is stray (it names no open element), it ends the current
    /// element, so the token after it is lexed with the parent's `outer_depth`.
    fn parse_astro_jsx_closing_inline(
        &mut self,
        open_angle_span: u32,
        in_jsx_child: bool,
        outer_depth: u32,
    ) -> JSXClosing<'a> {
        if self.at(Kind::RAngle) {
            self.lexer.astro_jsx_expression_depth = outer_depth;
            if in_jsx_child {
                self.expect_jsx_child(Kind::RAngle);
            } else {
                self.expect(Kind::RAngle);
            }
            JSXClosing::Fragment(self.ast.jsx_closing_fragment(self.end_span(open_angle_span)))
        } else {
            let name = self.parse_astro_jsx_element_name();
            let closing_name = name.span().source_text(self.source_text);
            // Consuming `>` below already lexes the token after it.
            self.sync_astro_no_expression_for_closing(closing_name);
            if self
                .astro_open_elements
                .iter()
                .any(|open| Self::astro_closing_name_matches(open, closing_name))
            {
                self.lexer.astro_jsx_expression_depth = outer_depth;
            }
            if in_jsx_child {
                self.expect_jsx_child(Kind::RAngle);
            } else {
                self.expect(Kind::RAngle);
            }
            JSXClosing::Element(
                self.ast.alloc_jsx_closing_element(self.end_span(open_angle_span), name),
            )
        }
    }

    /// The empty span at the end of the input, where unclosed elements end.
    #[expect(clippy::cast_possible_truncation)]
    fn astro_eof_span(&self) -> Span {
        Span::empty(self.source_text.len() as u32)
    }

    /// `true` when the current token is the `/` of a `</` that doesn't start a closing
    /// tag (`</ 3>`): neither a tag name nor `>` follows. HTML reads it as a bogus
    /// comment up to the next `>`.
    pub(crate) fn at_astro_bogus_closing_tag(&self) -> bool {
        let after_slash = self.cur_token().span().start as usize + 1;
        self.source_text[after_slash..].bytes().find(|b| !b.is_ascii_whitespace()).is_some_and(
            |b| !(b.is_ascii_alphabetic() || matches!(b, b'>' | b'_' | b'$') || !b.is_ascii()),
        )
    }

    /// Report and skip a bogus `</…>` (see [`Self::at_astro_bogus_closing_tag`]), up to
    /// the next `>` or the end of the input. The current token is the `/` after `<`.
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn skip_astro_bogus_closing_tag(&mut self) {
        let slash = self.cur_token().span().start as usize;
        let end =
            self.source_text[slash..].find('>').map_or(self.source_text.len(), |gt| slash + gt + 1);
        self.error(diagnostics::unexpected_token(Span::new(slash as u32 - 1, end as u32)));
        self.resume_astro_lexing_at(end as u32, /* in_jsx_child */ true);
        self.prev_token_end = end as u32;
    }

    /// Parse a `{expr}` or `{...expr}` template child. The current token is `{`.
    pub(crate) fn parse_astro_jsx_child_expression(&mut self) -> JSXChild<'a> {
        let span_start = self.start_span();
        // While the container is open, a `}` in nested template text closes it.
        self.lexer.astro_jsx_expression_depth += 1;
        self.bump_any(); // bump `{`
        if self.eat(Kind::Dot3) {
            let expr = self.parse_expr();
            self.expect_astro_jsx_child_expression_end();
            JSXChild::Spread(self.ast.alloc_jsx_spread_child(self.end_span(span_start), expr))
        } else {
            JSXChild::ExpressionContainer(
                self.parse_astro_jsx_expression_container(span_start, /* in_jsx_child */ true),
            )
        }
    }

    /// Consume the `}` that ends a template-child expression container opened by
    /// [`Self::parse_astro_jsx_child_expression`], then lex the following child.
    ///
    /// The container is closed *before* lexing what follows, so an adjacent `}`
    /// (`{x}} text`) closes nothing and is read as text.
    fn expect_astro_jsx_child_expression_end(&mut self) {
        self.lexer.astro_jsx_expression_depth -= 1;
        self.expect_jsx_child(Kind::RCurly);
    }

    /// Astro-specific version of `parse_jsx_expression_container`.
    /// Handles Astro JSX children in expressions and allows empty expressions.
    ///
    /// With `in_jsx_child`, the container must have been opened by
    /// [`Self::parse_astro_jsx_child_expression`], which this closes.
    pub(crate) fn parse_astro_jsx_expression_container(
        &mut self,
        span_start: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXExpressionContainer<'a>> {
        // In Astro mode, `{<div/>}` and `{<!-- comment -->}` are treated symmetrically:
        // both route through the JSX-children-in-expression path.
        // `<!-- -->` produces an `AstroComment` JSXChild, just as it does in a JSX
        // children position.  Trailing content after the comment (e.g. `{<!-- --> x}`)
        // is an error, exactly like `{<div/> x}` would be.
        if in_jsx_child && self.at(Kind::LAngle) {
            let expr = self.parse_astro_jsx_children_in_expression(span_start);
            return self.ast.alloc_jsx_expression_container(self.end_span(span_start), expr);
        }

        // In Astro mode, empty expressions are allowed — don't emit an error.
        let is_empty = self.at(Kind::RCurly);
        let expr = if is_empty { None } else { Some(JSXExpression::from(self.parse_expr())) };
        if in_jsx_child {
            self.expect_astro_jsx_child_expression_end();
        } else {
            self.expect(Kind::RCurly);
        }
        let expr = expr.unwrap_or_else(|| {
            let span = self.end_span(span_start);
            let expr = self.ast.jsx_empty_expression(Span::new(span.start + 1, span.end - 1));
            JSXExpression::EmptyExpression(expr)
        });

        self.ast.alloc_jsx_expression_container(self.end_span(span_start), expr)
    }

    /// Astro-specific version of `parse_jsx_attributes`.
    /// Handles shorthand attributes, special attribute names, and quotes in attribute names.
    pub(crate) fn parse_astro_jsx_attributes(&mut self) -> Vec<'a, JSXAttributeItem<'a>> {
        let mut attributes = self.ast.vec();
        loop {
            let kind = self.cur_kind();
            if matches!(kind, Kind::Eof | Kind::LAngle | Kind::RAngle | Kind::Slash)
                || self.fatal_error.is_some()
            {
                break;
            }
            let attribute = match kind {
                Kind::LCurly => {
                    // Check for empty expression container: {} or {/* comment */}
                    if self.lexer.peek_token().kind() == Kind::RCurly {
                        self.bump_any(); // bump `{`
                        self.bump_any(); // bump `}`
                        continue;
                    }
                    // `{...spread}`
                    if self.lexer.peek_token().kind() == Kind::Dot3 {
                        JSXAttributeItem::SpreadAttribute(self.parse_jsx_spread_attribute())
                    } else if let Some(attr) = self.try_parse_astro_shorthand_attribute() {
                        // `{prop}` shorthand for `prop={prop}`
                        attributes.push(JSXAttributeItem::Attribute(attr));
                        continue;
                    } else {
                        // Any other `{expr}` (e.g. `{{ answer: 1 }}`) is shorthand
                        JSXAttributeItem::Attribute(
                            self.parse_astro_expression_shorthand_attribute(),
                        )
                    }
                }
                // Quotes can appear in Astro attribute names
                Kind::Str | Kind::Undetermined => {
                    // Pop the "unterminated string" error that the lexer added
                    self.lexer.errors.pop();
                    JSXAttributeItem::Attribute(self.parse_astro_attribute())
                }
                _ => {
                    // Use permissive attribute name parsing
                    JSXAttributeItem::Attribute(self.parse_astro_attribute())
                }
            };
            attributes.push(attribute);
        }
        attributes
    }

    /// Astro-specific: Peek ahead past `<` to check if the next token starts a JSX element
    /// or fragment. Used in binary expression context to distinguish `<div>` from `< comparison`.
    ///
    /// Returns `true` if `<` is followed by an identifier, keyword, `>` (fragment), or a
    /// complete `<!-- -->` comment. Does not consume any tokens.
    pub(crate) fn is_astro_jsx_after_lt(&mut self) -> bool {
        let checkpoint = self.checkpoint();
        self.bump_any(); // bump `<`
        let next_kind = self.cur_kind();
        let is_jsx = next_kind == Kind::RAngle
            || next_kind == Kind::Ident
            || next_kind.is_any_keyword()
            || self.at_astro_html_comment();
        self.rewind(checkpoint);
        is_jsx
    }

    /// `true` when the bytes right after the just-bumped `<` open a complete
    /// `<!-- … -->` comment, letting it continue a run of sibling JSX.
    ///
    /// Requiring a real `-->` keeps the JS `< !--x` (negated pre-decrement) a
    /// comparison, and stops the binary parser looping on a `<!--` it can't consume.
    fn at_astro_html_comment(&self) -> bool {
        // `prev_token_end` is the byte right after `<` (no whitespace skipped),
        // so a `< !--` comparison is excluded; only `<!--` matches.
        self.source_text
            .get(self.prev_token_end as usize..)
            .is_some_and(|rest| rest.starts_with("!--") && rest[3..].contains("-->"))
    }

    /// Astro-specific: Parse multiple JSX elements in binary expression context.
    pub(crate) fn parse_astro_multiple_jsx_in_expression(
        &mut self,
        span_start: u32,
        first_element: Expression<'a>,
    ) -> Expression<'a> {
        let mut children = self.ast.vec();

        match first_element {
            Expression::JSXElement(el) => children.push(JSXChild::Element(el)),
            Expression::JSXFragment(frag) => children.push(JSXChild::Fragment(frag)),
            _ => unreachable!(
                "parse_astro_multiple_jsx_in_expression called with non-JSX expression"
            ),
        }

        while self.at(Kind::LAngle) {
            // The JS lexer skips whitespace between siblings; record where the
            // gap starts so it can be preserved (see the helper call below).
            let ws_start = self.prev_token_end;
            let checkpoint = self.checkpoint();
            let child_span = self.start_span();
            self.bump_any(); // bump `<`

            let kind = self.cur_kind();
            if kind == Kind::RAngle {
                self.push_astro_inter_sibling_whitespace(&mut children, ws_start, child_span);
                let fragment = self.parse_astro_jsx_fragment(child_span, false);
                children.push(JSXChild::Fragment(fragment));
            } else if kind == Kind::Ident || kind.is_any_keyword() {
                self.push_astro_inter_sibling_whitespace(&mut children, ws_start, child_span);
                // `<script>` needs the dedicated raw-text path like every other
                // JSX-children site; otherwise its body parses as JSX and a close
                // tag in a template literal (`` `</article>` ``) becomes a stray error.
                if self.at_astro_script_tag() {
                    children.push(self.parse_astro_script_in_jsx(child_span, false));
                } else {
                    let element = self.parse_astro_jsx_element(child_span, false);
                    children.push(JSXChild::Element(element));
                }
            } else if kind == Kind::Bang
                && let Some(comment) = self.parse_html_comment_in_jsx(child_span)
            {
                // HTML comment in a sibling run. The comment parser leaves the lexer
                // in JSX-child mode, but this loop is JS-expression context. Resync to
                // JS tokens so the closing `)` ends the list instead of being read as text.
                self.push_astro_inter_sibling_whitespace(&mut children, ws_start, child_span);
                let comment_end = comment.span().end;
                self.lexer.set_position_for_astro(comment_end);
                self.token = self.lexer.next_token();
                // Resync bypasses the parser's bump bookkeeping; point `prev_token_end`
                // at the comment end so the next iteration captures the whitespace
                // between the comment and the following sibling.
                self.prev_token_end = comment_end;
                children.push(comment);
            } else {
                self.rewind(checkpoint);
                break;
            }
        }

        if children.len() == 1 {
            return match children.pop().unwrap() {
                JSXChild::Element(el) => Expression::JSXElement(el),
                JSXChild::Fragment(frag) => Expression::JSXFragment(frag),
                _ => unreachable!(),
            };
        }

        let fragment_span = Span::new(span_start, self.prev_token_end);
        let opening = self.ast.jsx_opening_fragment(Span::empty(span_start));
        let closing = self.ast.jsx_closing_fragment(Span::empty(self.prev_token_end));
        let fragment = self.ast.alloc_jsx_fragment(fragment_span, opening, children, closing);
        Expression::JSXFragment(fragment)
    }

    /// Push the source text in `[start, end)` as a `JSXText` child when it is
    /// non-empty whitespace. Used to preserve whitespace between bare JSX
    /// siblings in an expression, which the JS lexer would otherwise drop.
    fn push_astro_inter_sibling_whitespace(
        &self,
        children: &mut Vec<'a, JSXChild<'a>>,
        start: u32,
        end: u32,
    ) {
        if end <= start {
            return;
        }
        let span = Span::new(start, end);
        let ws = span.source_text(self.source_text);
        if !ws.bytes().all(|b| b.is_ascii_whitespace()) {
            return;
        }
        let text = self.ast.alloc_jsx_text(span, Atom::from(ws), Some(Atom::from(ws)));
        children.push(JSXChild::Text(text));
    }

    // ==================== Astro-specific helpers ====================

    /// Try to parse Astro shorthand attribute `{prop}` -> `prop={prop}`
    fn try_parse_astro_shorthand_attribute(&mut self) -> Option<Box<'a, JSXAttribute<'a>>> {
        let checkpoint = self.checkpoint();
        let attr_span = self.start_span();
        self.bump_any(); // bump `{`

        if self.at(Kind::Ident) || self.cur_kind().is_any_keyword() {
            let ident_span = self.start_span();
            let name = self.cur_src();
            self.bump_any();
            let ident_span = self.end_span(ident_span);

            if self.at(Kind::RCurly) {
                self.bump_any(); // bump `}`
                let attr_span = self.end_span(attr_span);
                let name = Atom::from(name);
                let identifier = self.ast.jsx_identifier(ident_span, name);
                let attr_name = JSXAttributeName::Identifier(self.alloc(identifier));

                let ident_ref = self.ast.identifier_reference(ident_span, name);
                let expr = Expression::Identifier(self.alloc(ident_ref));
                let expr_container =
                    self.ast.alloc_jsx_expression_container(attr_span, JSXExpression::from(expr));
                let value = JSXAttributeValue::ExpressionContainer(expr_container);

                return Some(self.ast.alloc_jsx_attribute(attr_span, attr_name, Some(value)));
            }
        }

        self.rewind(checkpoint);
        None
    }

    /// Parse an Astro shorthand attribute whose expression is not a bare
    /// identifier, e.g. `{{ answer: sum(2, 4) }}`. The attribute name is the
    /// source text of the inner expression (matching the Go compiler).
    fn parse_astro_expression_shorthand_attribute(&mut self) -> Box<'a, JSXAttribute<'a>> {
        let span = self.start_span();
        self.bump_any(); // bump `{`

        let expr_start = self.cur_token().span().start;
        let expr = self.parse_expr();
        // `parse_expr` can fail without advancing (e.g. the next token is `)`),
        // leaving `prev_token_end < expr_start`. Clamp so the name span is never
        // inverted; `source_text()` panics on an inverted span.
        let name_span = Span::new(expr_start, self.prev_token_end.max(expr_start));
        self.expect(Kind::RCurly);

        let name = Atom::from(name_span.source_text(self.source_text).trim());
        let identifier = self.ast.jsx_identifier(name_span, name);
        let attr_name = JSXAttributeName::Identifier(self.alloc(identifier));

        let expr_container =
            self.ast.alloc_jsx_expression_container(name_span, JSXExpression::from(expr));
        let value = JSXAttributeValue::ExpressionContainer(expr_container);

        self.ast.alloc_jsx_attribute(self.end_span(span), attr_name, Some(value))
    }

    /// Parse an Astro attribute which can have special characters in the name.
    fn parse_astro_attribute(&mut self) -> Box<'a, JSXAttribute<'a>> {
        let span = self.start_span();

        let token_start = self.cur_token().span().start;
        self.lexer.set_position_for_astro(token_start);

        self.read_astro_attribute_name();
        let name_span = self.cur_token().span();
        let name = name_span.source_text(self.source_text);
        self.bump_any();

        let identifier = self.ast.jsx_identifier(name_span, name);
        let attr_name = JSXAttributeName::Identifier(self.alloc(identifier));

        let value = if self.at(Kind::Eq) { Some(self.parse_astro_attribute_value()) } else { None };

        self.ast.alloc_jsx_attribute(self.end_span(span), attr_name, value)
    }

    /// Parse an Astro attribute value. Called while positioned at `=`.
    ///
    /// The lexing mode is chosen from the raw bytes after `=` *before* the JS
    /// lexer runs, so unquoted HTML values reach the Astro reader instead of
    /// being rejected as malformed JS tokens (e.g. `color=#18b218`). Quoted
    /// strings, `{expr}` and templates still lex as JS/JSX.
    fn parse_astro_attribute_value(&mut self) -> JSXAttributeValue<'a> {
        let bytes = self.source_text.as_bytes();
        let mut value_start = self.cur_token().end() as usize;
        while matches!(bytes.get(value_start), Some(b' ' | b'\t' | b'\r' | b'\n')) {
            value_start += 1;
        }
        // A structural terminator (`>`, `}`, EOF) means the value is missing
        // (`attr=` before `>`); fall through to the JS path to report it there.
        // `/` is a valid first value char (HTML5): `href=/about` is value `/about`,
        // and even `attr=/>` is value `/`, not a self-close.
        let is_unquoted = bytes
            .get(value_start)
            .is_some_and(|b| !matches!(b, b'"' | b'\'' | b'{' | b'`' | b'<' | b'>' | b'}'));

        if is_unquoted {
            self.prev_token_end = self.cur_token().end(); // consume `=`
            self.lexer.set_position_for_astro(value_start as u32);
            self.read_astro_unquoted_attribute_value();
            let value_span = self.cur_token().span();
            let value = Atom::from(value_span.source_text(self.source_text));
            self.bump_any();
            let str_lit = self.ast.string_literal(value_span, value, None);
            return JSXAttributeValue::StringLiteral(self.alloc(str_lit));
        }

        self.expect_jsx_attribute_value(Kind::Eq);
        match self.cur_kind() {
            Kind::NoSubstitutionTemplate | Kind::TemplateHead => {
                let span = self.start_span();
                let template_lit = self.parse_template_literal(false);
                let expr = Expression::TemplateLiteral(self.alloc(template_lit));
                let expr_container = self
                    .ast
                    .alloc_jsx_expression_container(self.end_span(span), JSXExpression::from(expr));
                JSXAttributeValue::ExpressionContainer(expr_container)
            }
            Kind::LCurly => {
                let span_start = self.start_span();
                self.bump_any(); // bump `{`
                let expr = self.parse_astro_jsx_expression_container(
                    span_start, /* in_jsx_child */ false,
                );
                JSXAttributeValue::ExpressionContainer(expr)
            }
            Kind::Str | Kind::LAngle => self.parse_jsx_attribute_value(),
            // Reached only via the JS path for a token that isn't a recognized
            // value start (e.g. a comment before the value). Bail on a structural
            // terminator, otherwise re-read under HTML rules.
            _ => {
                let token_start = self.cur_token().span().start as usize;
                let starts_with_terminator = self
                    .source_text
                    .as_bytes()
                    .get(token_start)
                    .is_none_or(|b| matches!(b, b'/' | b'>' | b'<' | b'}'));
                if starts_with_terminator {
                    return self.unexpected();
                }
                self.lexer.set_position_for_astro(token_start as u32);
                self.read_astro_unquoted_attribute_value();
                let value_span = self.cur_token().span();
                let value = Atom::from(value_span.source_text(self.source_text));
                self.bump_any();
                let str_lit = self.ast.string_literal(value_span, value, None);
                JSXAttributeValue::StringLiteral(self.alloc(str_lit))
            }
        }
    }

    /// Check if the element name is an HTML void element.
    pub(crate) fn is_astro_void_element(name: &JSXElementName<'a>) -> bool {
        match name {
            JSXElementName::Identifier(ident) => {
                matches!(
                    ident.name.as_str(),
                    "area"
                        | "base"
                        | "br"
                        | "col"
                        | "embed"
                        | "hr"
                        | "img"
                        | "input"
                        | "link"
                        | "meta"
                        | "param"
                        | "source"
                        | "track"
                        | "wbr"
                )
            }
            _ => false,
        }
    }

    /// Check if the element name is a raw text element (style only in Astro).
    fn is_astro_raw_text_element(name: &JSXElementName<'a>) -> bool {
        match name {
            JSXElementName::Identifier(ident) => matches!(ident.name.as_str(), "style"),
            _ => false,
        }
    }

    /// Elements whose tags are literal text but whose `{…}` expressions still evaluate.
    fn is_astro_rcdata_element(name: &JSXElementName<'a>) -> bool {
        match name {
            JSXElementName::Identifier(ident) => {
                matches!(ident.name.as_str(), "title" | "textarea")
            }
            _ => false,
        }
    }

    /// Determine whether a `<script>` tag's content should be treated as raw text
    /// (i.e. NOT parsed as JavaScript/TypeScript).
    ///
    /// Rules:
    /// - If there is **no `type` attribute** → parse as TS/JS (return `false`)
    /// - If `type` is a JS/module MIME type → parse as TS/JS (return `false`)
    ///   - Recognised JS MIME types: `text/javascript`, `text/ecmascript`,
    ///     `application/javascript`, `application/ecmascript`,
    ///     `application/x-javascript`, `module`
    /// - If `type` is anything else (e.g. `text/css`, `application/json`) →
    ///   treat as raw text (return `true`)
    ///
    /// Other attributes (e.g. `defer`, `src`, `is:inline`, `asdf`) do not affect
    /// parsing — only the presence and value of `type` matters.
    pub(crate) fn is_raw_text_script(attributes: &[JSXAttributeItem<'a>]) -> bool {
        for attr in attributes {
            let JSXAttributeItem::Attribute(attr) = attr else { continue };
            let attr_name = match &attr.name {
                JSXAttributeName::Identifier(ident) => ident.name.as_str(),
                JSXAttributeName::NamespacedName(_) => continue,
            };
            if attr_name != "type" {
                continue;
            }
            // Found a `type` attribute — check its value.
            let type_value = match &attr.value {
                None => {
                    // `<script type>` with no value — treat as raw text
                    return true;
                }
                Some(JSXAttributeValue::StringLiteral(s)) => s.value.as_str(),
                Some(JSXAttributeValue::ExpressionContainer(container)) => {
                    // Dynamic type attribute — we can't determine it statically,
                    // so conservatively treat as raw text.
                    let _ = container;
                    return true;
                }
                Some(_) => return true,
            };
            // Check against known JS MIME types.
            return !matches!(
                type_value,
                "text/javascript"
                    | "text/ecmascript"
                    | "application/javascript"
                    | "application/ecmascript"
                    | "application/x-javascript"
                    | "module"
            );
        }
        // No `type` attribute found — parse as TS/JS.
        false
    }

    /// Check if the element is a foreign content element where `{` is literal text.
    fn is_foreign_content_element(name: &JSXElementName<'a>) -> bool {
        match name {
            JSXElementName::Identifier(ident) => Self::is_foreign_content_name(ident.name.as_str()),
            _ => false,
        }
    }

    fn is_foreign_content_name(name: &str) -> bool {
        name == "math"
    }

    /// Put the lexer back in the mode the currently open elements call for:
    /// `{` is literal text only while a foreign content element is open.
    fn sync_astro_no_expression(&mut self) {
        let in_foreign_content =
            self.astro_open_elements.iter().any(|name| Self::is_foreign_content_name(name));
        self.lexer.no_expression_in_jsx_children = in_foreign_content;
    }

    /// [`Self::sync_astro_no_expression`] for the tag being closed, which ends every
    /// open element down to the innermost one it names — a `</math>` inside `<mi>`
    /// closes both. A name that matches nothing open is stray and closes nothing.
    fn sync_astro_no_expression_for_closing(&mut self, closing_name: &str) {
        let still_open = match self
            .astro_open_elements
            .iter()
            .rposition(|name| Self::astro_closing_name_matches(name, closing_name))
        {
            Some(index) => &self.astro_open_elements[..index],
            None => &self.astro_open_elements[..],
        };
        let in_foreign_content = still_open.iter().any(|name| Self::is_foreign_content_name(name));
        self.lexer.no_expression_in_jsx_children = in_foreign_content;
    }

    /// Check if attributes contain `is:raw` directive.
    fn has_is_raw_attribute(attributes: &[JSXAttributeItem<'a>]) -> bool {
        attributes.iter().any(|attr| {
            if let JSXAttributeItem::Attribute(attr) = attr {
                match &attr.name {
                    JSXAttributeName::Identifier(ident) => ident.name.as_str() == "is:raw",
                    JSXAttributeName::NamespacedName(ns) => {
                        ns.namespace.name.as_str() == "is" && ns.name.name.as_str() == "raw"
                    }
                }
            } else {
                false
            }
        })
    }

    /// Whether a closing tag named `closing` closes an element opened as `opening`
    /// (both source text). Native HTML tag names match ASCII case-insensitively
    /// (`<div>…</DIV>`); component names, which start uppercase, match exactly.
    fn astro_closing_name_matches(opening: &str, closing: &str) -> bool {
        opening == closing
            || (Self::is_astro_native_tag_name(opening) && opening.eq_ignore_ascii_case(closing))
    }

    /// Whether `name` is an HTML element name rather than a component reference,
    /// mirroring the classification in [`Self::parse_astro_jsx_element_name`].
    fn is_astro_native_tag_name(name: &str) -> bool {
        name.as_bytes().first().is_some_and(u8::is_ascii_lowercase) || name.contains('-')
    }

    /// Astro-specific element name equality check.
    fn astro_jsx_element_name_eq(lhs: &JSXElementName<'a>, rhs: &JSXElementName<'a>) -> bool {
        match (lhs, rhs) {
            // A native tag closed with different casing parses its closing name as a
            // reference (`</DIV>`), so compare the names themselves.
            (JSXElementName::Identifier(lhs), JSXElementName::IdentifierReference(rhs)) => {
                Self::astro_closing_name_matches(&lhs.name, &rhs.name)
            }
            (JSXElementName::Identifier(lhs), JSXElementName::Identifier(rhs)) => {
                Self::astro_closing_name_matches(&lhs.name, &rhs.name)
            }
            (
                JSXElementName::IdentifierReference(lhs),
                JSXElementName::IdentifierReference(rhs),
            ) => lhs.name == rhs.name,
            (JSXElementName::NamespacedName(lhs), JSXElementName::NamespacedName(rhs)) => {
                lhs.namespace.name == rhs.namespace.name && lhs.name.name == rhs.name.name
            }
            (JSXElementName::MemberExpression(lhs), JSXElementName::MemberExpression(rhs)) => {
                Self::astro_jsx_member_expression_eq(lhs, rhs)
            }
            (JSXElementName::ThisExpression(_), JSXElementName::ThisExpression(_)) => true,
            _ => false,
        }
    }

    fn astro_jsx_member_expression_eq(
        lhs: &JSXMemberExpression<'a>,
        rhs: &JSXMemberExpression<'a>,
    ) -> bool {
        if lhs.property.name != rhs.property.name {
            return false;
        }
        match (&lhs.object, &rhs.object) {
            (
                JSXMemberExpressionObject::IdentifierReference(lhs),
                JSXMemberExpressionObject::IdentifierReference(rhs),
            ) => lhs.name == rhs.name,
            (
                JSXMemberExpressionObject::MemberExpression(lhs),
                JSXMemberExpressionObject::MemberExpression(rhs),
            ) => Self::astro_jsx_member_expression_eq(lhs, rhs),
            (
                JSXMemberExpressionObject::ThisExpression(_),
                JSXMemberExpressionObject::ThisExpression(_),
            ) => true,
            _ => false,
        }
    }

    /// `true` when the current token, just after `<`, names a `<script>` tag
    /// (and not e.g. `<script-element>` or `<script.Item>`).
    pub(crate) fn at_astro_script_tag(&self) -> bool {
        self.cur_src() == "script"
            && self
                .source_text
                .as_bytes()
                .get(self.cur_token().span().end as usize)
                .is_none_or(|b| b.is_ascii_whitespace() || matches!(b, b'>' | b'/'))
    }

    /// Parse a `<script>` element (Astro-specific). The current token is `script`,
    /// right after `<` at `span`.
    ///
    /// The content is never lexed as part of the template: it is either parsed later as
    /// TypeScript (an `AstroScript` placeholder, see [`super::parse_astro_scripts`]) or
    /// kept as raw text. See [`Self::is_raw_text_script`] for which applies.
    ///
    /// Like HTML, a `<script>` without a closing tag runs to the end of the input.
    ///
    /// `in_jsx_child` selects how the token after the element is lexed: as template
    /// text, or as JavaScript (when the element is an operand in an expression).
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn parse_astro_script_in_jsx(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> JSXChild<'a> {
        self.bump_any(); // skip `script`

        let attributes = self.parse_astro_jsx_attributes();
        let is_raw_text = Self::is_raw_text_script(&attributes);
        let is_self_closing = self.eat(Kind::Slash);

        // Consume `>` without lexing what follows: that is script content (or, after
        // `/>`, template text), not a JavaScript token.
        self.expect_without_advance(Kind::RAngle);
        if self.at(Kind::RAngle) {
            self.prev_token_end = self.cur_token().span().end;
        }

        let name_span = Span::new(span + 1, span + 7);
        let no_type_args: Option<Box<'a, TSTypeParameterInstantiation<'a>>> = None;

        if is_self_closing {
            let end = self.prev_token_end;
            self.resume_astro_lexing_at(end, in_jsx_child);

            let script_span = Span::new(span, end);
            let children = if is_raw_text {
                self.ast.vec()
            } else {
                let program = self.astro_script_placeholder_program(Span::empty(end));
                self.ast
                    .vec1(JSXChild::AstroScript(self.ast.alloc_astro_script(script_span, program)))
            };
            let name = self.ast.jsx_identifier(name_span, "script");
            let opening = self.ast.alloc_jsx_opening_element(
                script_span,
                JSXElementName::Identifier(self.alloc(name)),
                no_type_args,
                attributes,
            );
            return JSXChild::Element(self.ast.alloc_jsx_element(
                script_span,
                opening,
                children,
                None::<Box<'a, JSXClosingElement<'a>>>,
            ));
        }

        // Content starts right after `>`, so leading trivia (e.g. an
        // `// eslint-disable-next-line` comment) is part of the script.
        let content_start = self.prev_token_end as usize;
        let closing_tag = "</script";
        let rest = &self.source_text[content_start..];
        let close = find_raw_text_content_end(rest, closing_tag, /* ignore_case */ true);
        let (content_end, end, closing) = if let Some(offset) = close {
            let content_end = content_start + offset;
            let name_end = content_end + closing_tag.len();
            // `</script` is followed by whitespace, `/`, `>` or EOF; the tag ends at the
            // next `>`.
            let end = self.source_text[name_end..]
                .find('>')
                .map_or(self.source_text.len(), |gt| name_end + gt + 1);
            let closing_name = self
                .ast
                .jsx_identifier(Span::new(content_end as u32 + 2, name_end as u32), "script");
            let closing = self.ast.alloc_jsx_closing_element(
                Span::new(content_end as u32, end as u32),
                JSXElementName::Identifier(self.alloc(closing_name)),
            );
            (content_end as u32, end as u32, Some(closing))
        } else {
            let len = self.source_text.len() as u32;
            (len, len, None)
        };
        let content_start = content_start as u32;

        self.resume_astro_lexing_at(end, in_jsx_child);
        self.prev_token_end = end;

        let full_span = Span::new(span, end);
        let content_span = Span::new(content_start, content_end);
        let child = if is_raw_text {
            let raw_text = content_span.source_text(self.source_text);
            JSXChild::Text(self.ast.alloc_jsx_text(
                content_span,
                raw_text,
                Some(Atom::from(raw_text)),
            ))
        } else {
            let program = self.astro_script_placeholder_program(content_span);
            JSXChild::AstroScript(self.ast.alloc_astro_script(full_span, program))
        };

        let opening_name = self.ast.jsx_identifier(name_span, "script");
        let opening = self.ast.alloc_jsx_opening_element(
            Span::new(span, content_start),
            JSXElementName::Identifier(self.alloc(opening_name)),
            no_type_args,
            attributes,
        );
        JSXChild::Element(self.ast.alloc_jsx_element(
            full_span,
            opening,
            self.ast.vec1(child),
            closing,
        ))
    }

    /// An empty program standing in for `<script>` content at `content_span`, until
    /// [`super::parse_astro_scripts`] parses it.
    fn astro_script_placeholder_program(&self, content_span: Span) -> Program<'a> {
        self.ast.program(
            content_span,
            self.source_type,
            "",
            self.ast.vec(),
            None,
            self.ast.vec(),
            self.ast.vec(),
        )
    }

    /// The `</name` an element's raw text or RCDATA content ends at, and whether it
    /// matches case-insensitively (native HTML tag names do; component names don't).
    fn astro_raw_text_closing_tag(&self, name: &JSXElementName<'a>) -> (String, bool) {
        let tag_name = match name {
            JSXElementName::Identifier(ident) => ident.name.as_str(),
            JSXElementName::IdentifierReference(ident_ref) => ident_ref.name.as_str(),
            _ => name.span().source_text(self.source_text),
        };
        (format!("</{tag_name}"), matches!(name, JSXElementName::Identifier(_)))
    }

    /// Reposition the lexer at `pos` and lex the next token for the given context.
    pub(crate) fn resume_astro_lexing_at(&mut self, pos: u32, in_jsx_child: bool) {
        self.lexer.set_position_for_astro(pos);
        self.token =
            if in_jsx_child { self.lexer.next_jsx_child() } else { self.lexer.next_token() };
    }

    /// Skip the raw text content of a raw text element in Astro.
    pub(crate) fn skip_astro_raw_text_element_content(
        &mut self,
        name: &JSXElementName<'a>,
        in_jsx_child: bool,
    ) -> Vec<'a, JSXChild<'a>> {
        let (closing_tag, ignore_case) = self.astro_raw_text_closing_tag(name);

        let start_pos = self.prev_token_end as usize;
        // Like HTML, raw text without a closing tag runs to the end of the input.
        let end_pos =
            find_raw_text_content_end(&self.source_text[start_pos..], &closing_tag, ignore_case)
                .map_or(self.source_text.len(), |rel| start_pos + rel);
        #[expect(clippy::cast_possible_truncation)]
        let content_end = end_pos as u32;
        let content_start = self.prev_token_end;

        self.resume_astro_lexing_at(content_end, in_jsx_child);

        if content_end > content_start {
            let span = Span::new(content_start, content_end);
            let raw_text = span.source_text(self.source_text);
            let jsx_text = self.ast.alloc_jsx_text(span, raw_text, Some(Atom::from(raw_text)));
            return self.ast.vec1(JSXChild::Text(jsx_text));
        }

        self.ast.vec()
    }

    /// Parse the content of an RCDATA element (`<title>`, `<textarea>`): tags inside are
    /// literal text, but `{…}` expressions still interpolate.
    ///
    /// A closing tag inside an expression (e.g. in a string) does not end the element:
    /// the search resumes after the expression. Outside expressions, the first real
    /// closing tag ends it.
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn parse_astro_rcdata_element_content(
        &mut self,
        name: &JSXElementName<'a>,
        in_jsx_child: bool,
    ) -> Vec<'a, JSXChild<'a>> {
        // Inside foreign content (`<math>`), `{` is literal text, so there is
        // nothing to interpolate: the content is plain raw text.
        if self.lexer.no_expression_in_jsx_children {
            return self.skip_astro_raw_text_element_content(name, in_jsx_child);
        }

        let (closing_tag, ignore_case) = self.astro_raw_text_closing_tag(name);
        let source_text = self.source_text;
        let find_close = |from: usize| {
            source_text
                .get(from..)
                .and_then(|rest| find_raw_text_content_end(rest, &closing_tag, ignore_case))
                .map(|rel| from + rel)
        };

        let start_pos = self.prev_token_end as usize;
        // Like HTML, content without a closing tag runs to the end of the input.
        let mut close_at = find_close(start_pos).unwrap_or(source_text.len());

        let mut children = self.ast.vec();
        let mut pos = start_pos;

        loop {
            // An expression may have consumed the closing tag we found (it was inside a
            // string, say); only then search again, from where the expression ended.
            // Each search starts past the previous match, so the total work is linear.
            if close_at < pos {
                close_at = find_close(pos).unwrap_or(source_text.len());
            }
            let Some(brace_rel) = self.source_text[pos..close_at].find('{') else { break };

            let brace_at = pos + brace_rel;
            if brace_at > pos {
                let span = Span::new(pos as u32, brace_at as u32);
                let text = span.source_text(self.source_text);
                children.push(JSXChild::Text(self.ast.alloc_jsx_text(
                    span,
                    text,
                    Some(Atom::from(text)),
                )));
            }

            self.lexer.set_position_for_astro(brace_at as u32);
            self.token = self.lexer.next_jsx_child();
            let closing = if close_at < source_text.len() {
                Span::sized(close_at as u32, closing_tag.len() as u32)
            } else {
                Span::empty(close_at as u32)
            };
            let Some(expression) = self.try_parse_astro_rcdata_expression(closing) else {
                // An invalid expression (often unterminated: `{x</title>`). The closing
                // tag is known, so recover there: keep the rest as text, joined with
                // the text before the `{`.
                pos = brace_at;
                if let Some(JSXChild::Text(text)) = children.last()
                    && text.span.end as usize == brace_at
                {
                    pos = text.span.start as usize;
                    children.pop();
                }
                break;
            };
            children.push(expression);

            let after = self.prev_token_end as usize;
            pos = after.max(brace_at);
            // Guard against an expression that consumed nothing, which would spin forever.
            if after <= brace_at || self.fatal_error.is_some() {
                break;
            }
        }
        let content_end = close_at.max(pos);

        if content_end > pos {
            let span = Span::new(pos as u32, content_end as u32);
            let text = span.source_text(self.source_text);
            children.push(JSXChild::Text(self.ast.alloc_jsx_text(
                span,
                text,
                Some(Atom::from(text)),
            )));
        }

        self.resume_astro_lexing_at(content_end as u32, in_jsx_child);

        children
    }

    /// Parse a `{…}` child in RCDATA content. On a fatal error, rewind to the `{`,
    /// report the error without aborting the parse, and return `None`.
    ///
    /// `closing` is the element's `</name` (empty at the end of the input); an
    /// expression that fails after reaching it is reported as unterminated there.
    fn try_parse_astro_rcdata_expression(&mut self, closing: Span) -> Option<JSXChild<'a>> {
        // A plain checkpoint truncates errors by count, keeping each attempt O(1).
        let checkpoint = self.checkpoint();
        let depth = self.lexer.astro_jsx_expression_depth;
        let no_expression = self.lexer.no_expression_in_jsx_children;
        let open_elements = self.astro_open_elements.len();
        let ctx = self.ctx;

        let expression = self.parse_astro_jsx_child_expression();
        let Some(fatal) = self.fatal_error.take() else { return Some(expression) };
        let error = if !closing.is_empty() && self.prev_token_end >= closing.start {
            let found = closing.source_text(self.source_text);
            diagnostics::expect_token("}", found, closing)
        } else {
            fatal.error
        };

        self.rewind(checkpoint);
        self.ctx = ctx;
        self.lexer.astro_jsx_expression_depth = depth;
        self.lexer.no_expression_in_jsx_children = no_expression;
        self.astro_open_elements.truncate(open_elements);
        self.error(error);
        None
    }

    /// Parse HTML comment in JSX (Astro-specific).
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn parse_html_comment_in_jsx(&mut self, span: u32) -> Option<JSXChild<'a>> {
        let start_pos = self.prev_token_end as usize;

        // Search for the closing `-->` *after* the opening `!--` (offset >= 3).
        // Otherwise an overlapping marker like `<!-->` would match `-->` at
        // offset 1 and slice `rest[3..1]`, panicking the parser on malformed input.
        if let Some(rest) = self.source_text.get(start_pos..)
            && rest.starts_with("!--")
            && let Some(rel) = rest[3..].find("-->")
        {
            let end_offset = rel + 3;
            let comment_end = (start_pos + end_offset + 3) as u32;
            let comment_start = span;

            let content = &rest[3..end_offset];
            let value = oxc_span::Atom::from(content);

            let comment_span = oxc_span::Span::new(comment_start, comment_end);
            let comment = self.ast.alloc_astro_comment(comment_span, value);

            self.lexer.set_position_for_astro(comment_end);
            self.token = self.lexer.next_jsx_child();

            return Some(JSXChild::AstroComment(comment));
        }

        None
    }

    /// Parse JSX children in an expression container (Astro-specific).
    fn parse_astro_jsx_children_in_expression(&mut self, span_start: u32) -> JSXExpression<'a> {
        let fragment_span_start = span_start + 1;
        let mut children = self.ast.vec();

        loop {
            if self.at(Kind::Eof) {
                break;
            }

            match self.cur_kind() {
                Kind::LAngle => {
                    let child_span = self.start_span();
                    self.bump_any();

                    let kind = self.cur_kind();

                    if kind == Kind::RAngle {
                        let fragment = self.parse_astro_jsx_fragment(child_span, true);
                        children.push(JSXChild::Fragment(fragment));
                        if self.at(Kind::Eof) {
                            self.lexer.errors.pop();
                            self.token = self.lexer.next_token();
                        }
                    } else if kind == Kind::Ident || kind.is_any_keyword() {
                        if self.at_astro_script_tag() {
                            children.push(self.parse_astro_script_in_jsx(child_span, true));
                        } else {
                            let element = self.parse_astro_jsx_element(child_span, true);
                            children.push(JSXChild::Element(element));
                        }
                        if self.at(Kind::Eof) {
                            self.lexer.errors.pop();
                            self.token = self.lexer.next_token();
                        }
                    } else if kind == Kind::Slash {
                        let _: () = self.unexpected();
                        break;
                    } else if kind == Kind::Bang {
                        // `<!--` HTML comment — produce an AstroComment child.
                        // `parse_html_comment_in_jsx` repositions the lexer and
                        // calls `next_jsx_child()` internally, so after returning
                        // the loop continues in JSX-child mode.
                        if let Some(comment) = self.parse_html_comment_in_jsx(child_span) {
                            children.push(comment);
                        } else {
                            // Malformed `<!…>` — skip and continue.
                            self.token = self.lexer.next_jsx_child();
                        }
                    } else {
                        let _: () = self.unexpected();
                        break;
                    }
                }
                Kind::JSXText => {
                    let text = self.parse_jsx_text();
                    children.push(JSXChild::Text(text));
                }
                Kind::LCurly => {
                    children.push(self.parse_astro_jsx_child_expression());
                }
                Kind::RCurly => {
                    break;
                }
                _ => {
                    self.token = self.lexer.next_jsx_child();
                    if self.at(Kind::Eof) {
                        break;
                    }
                }
            }
        }

        // The synthesized fragment ends before the container's `}`.
        let fragment_end =
            if self.at(Kind::RCurly) { self.cur_token().span().start } else { self.prev_token_end };
        self.expect_astro_jsx_child_expression_end();

        // Strip trailing whitespace-only text node produced by the JSX child
        // lexer scanning whitespace between the last real child and `}`.
        if let Some(JSXChild::Text(t)) = children.last() {
            if t.value.as_str().chars().all(|ch| ch.is_ascii_whitespace()) {
                children.pop();
            }
        }

        if children.len() == 1 {
            match children.pop().unwrap() {
                JSXChild::Element(el) => return JSXExpression::JSXElement(el),
                JSXChild::Fragment(frag) => return JSXExpression::JSXFragment(frag),
                other => children.push(other),
            }
        }

        let fragment_span = Span::new(fragment_span_start, fragment_end);
        let opening = self.ast.jsx_opening_fragment(Span::empty(fragment_span_start));
        let closing = self.ast.jsx_closing_fragment(Span::empty(fragment_end));
        let fragment = self.ast.alloc_jsx_fragment(fragment_span, opening, children, closing);
        JSXExpression::JSXFragment(fragment)
    }
}

/// Find where raw text content ends: the first `closing_tag` (`</name`) followed by
/// whitespace, `/`, `>` or the end of input, as HTML5 requires.
///
/// With `ignore_case`, the tag name matches ASCII case-insensitively, as native HTML
/// tag names do (`<textarea>…</TEXTAREA>`).
pub(super) fn find_raw_text_content_end(
    rest: &str,
    closing_tag: &str,
    ignore_case: bool,
) -> Option<usize> {
    let bytes = rest.as_bytes();
    let tag = closing_tag.as_bytes();
    let mut searched = 0;
    while let Some(found) = memchr::memmem::find(&bytes[searched..], b"</") {
        let at = searched + found;
        let after = at + tag.len();
        let is_tag = bytes.get(at..after).is_some_and(|candidate| {
            if ignore_case { candidate.eq_ignore_ascii_case(tag) } else { candidate == tag }
        });
        if is_tag
            && bytes.get(after).is_none_or(|b| b.is_ascii_whitespace() || matches!(b, b'/' | b'>'))
        {
            return Some(at);
        }
        searched = at + 2;
    }
    None
}
