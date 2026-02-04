//! Astro-specific JSX parsing extensions.
//!
//! This module contains JSX parsing methods that are only used when parsing Astro files.
//! These are called from the main JSX parser when `source_type.is_astro()` is true.

use oxc_allocator::{Box, Vec};
use oxc_ast::ast::*;
use oxc_span::{Atom, Span};

use crate::{ParserImpl, lexer::Kind};

impl<'a> ParserImpl<'a> {
    /// Parse JSX children in an expression container (Astro-specific).
    /// Called when we see `{` followed by `<` in Astro mode.
    /// This allows: `{ <div>1</div> <div>2</div> }` without explicit fragments.
    ///
    /// We parse JSX children similarly to parse_jsx_children, but we stop at `}`
    /// instead of `</`. After each child, we re-lex as JSX child to get the next token.
    pub(crate) fn parse_astro_jsx_children_in_expression(
        &mut self,
        span_start: u32,
    ) -> JSXExpression<'a> {
        let fragment_span_start = span_start + 1; // After the `{`
        let mut children = self.ast.vec();

        // We're currently at `<` (LAngle). Parse JSX children until we hit `}`.
        loop {
            if self.at(Kind::Eof) {
                break;
            }

            match self.cur_kind() {
                Kind::LAngle => {
                    let child_span = self.start_span();
                    // Use regular bump_any() after `<` - we expect identifier/keyword/slash/etc
                    self.bump_any();

                    let kind = self.cur_kind();

                    if kind == Kind::RAngle {
                        // `<>` - fragment
                        let fragment = self.parse_jsx_fragment(child_span, true);
                        children.push(JSXChild::Fragment(fragment));
                        // parse_jsx_fragment with in_jsx_child=true calls expect_jsx_child(RAngle)
                        // Same Eof handling as elements (see below).
                        if self.at(Kind::Eof) {
                            self.lexer.errors.pop();
                            self.token = self.lexer.next_token();
                        }
                    } else if kind == Kind::Ident || kind.is_any_keyword() {
                        // Check for <script> which needs special handling
                        if self.cur_src() == "script" {
                            children.push(self.parse_astro_script_in_jsx(child_span));
                        } else {
                            // `<ident` - element
                            let element = self.parse_jsx_element(child_span, true);
                            children.push(JSXChild::Element(element));
                        }
                        // parse_jsx_element with in_jsx_child=true calls expect_jsx_child(RAngle)
                        // which calls next_jsx_child(). But next_jsx_child() returns Eof with error
                        // when text is followed by `}` (since `}` is unexpected in normal JSX).
                        // In our Astro context, `}` is the valid terminator, so if we got Eof,
                        // re-lex with regular tokenization to check for `}`.
                        if self.at(Kind::Eof) {
                            // Pop the spurious "unexpected `}`" error from lexer
                            self.lexer.errors.pop();
                            self.token = self.lexer.next_token();
                        }
                    } else if kind == Kind::Slash {
                        // `</` - unexpected closing tag, error
                        let _: () = self.unexpected();
                        break;
                    } else if kind == Kind::Bang {
                        // `<!` - HTML comment, skip
                        self.skip_jsx_html_comment();
                        // Re-lex for next child
                        self.token = self.lexer.next_jsx_child();
                    } else {
                        let _: () = self.unexpected();
                        break;
                    }
                    // Note: Don't call next_jsx_child() here - parse_jsx_element/fragment
                    // with in_jsx_child=true already advances to the next JSX child token
                }
                Kind::JSXText => {
                    let text = self.parse_jsx_text();
                    children.push(JSXChild::Text(text));
                    // parse_jsx_text uses bump_any() which calls next_token().
                    // The token is already set to the next token (LAngle, LCurly, RCurly, etc.)
                    // Don't call next_jsx_child() - we're already positioned correctly.
                }
                Kind::LCurly => {
                    // Nested expression container
                    let nested_span = self.start_span();
                    self.bump_any(); // bump `{`

                    if self.eat(Kind::Dot3) {
                        let spread = self.parse_jsx_spread_child(nested_span);
                        children.push(JSXChild::Spread(spread));
                    } else {
                        let expr = JSXExpression::from(self.parse_expr());
                        self.expect_jsx_child(Kind::RCurly);
                        let container = self
                            .ast
                            .alloc_jsx_expression_container(self.end_span(nested_span), expr);
                        children.push(JSXChild::ExpressionContainer(container));
                    }
                    // Re-lex for next child
                    self.token = self.lexer.next_jsx_child();
                }
                Kind::RCurly => {
                    // End of expression container - don't consume here, let caller handle it
                    break;
                }
                _ => {
                    // For other tokens (like Eof from next_jsx_child hitting `}`),
                    // try to re-lex and check again
                    self.token = self.lexer.next_jsx_child();
                    if self.at(Kind::Eof) {
                        break;
                    }
                }
            }
        }

        self.expect_jsx_child(Kind::RCurly);

        // If only one child and it's an element or fragment, return it directly
        // (no need for implicit fragment wrapper)
        if children.len() == 1 {
            match children.pop().unwrap() {
                JSXChild::Element(el) => return JSXExpression::JSXElement(el),
                JSXChild::Fragment(frag) => return JSXExpression::JSXFragment(frag),
                other => children.push(other),
            }
        }

        // Wrap all children in an implicit fragment
        let fragment_span = Span::new(fragment_span_start, self.prev_token_end);
        let opening = self.ast.jsx_opening_fragment(Span::empty(fragment_span_start));
        let closing = self.ast.jsx_closing_fragment(Span::empty(self.prev_token_end));
        let fragment = self.ast.alloc_jsx_fragment(fragment_span, opening, children, closing);
        JSXExpression::JSXFragment(fragment)
    }

    /// Parse multiple JSX elements in expression context (Astro-specific).
    /// Called from binary expression parsing when we have a JSX element followed by `<`.
    /// This allows: `<div>1</div><div>2</div>` without explicit fragments in any expression.
    pub(crate) fn parse_astro_multiple_jsx_in_expression(
        &mut self,
        span_start: u32,
        first_element: Expression<'a>,
    ) -> Expression<'a> {
        let mut children = self.ast.vec();

        // Add first element as a child
        match first_element {
            Expression::JSXElement(el) => children.push(JSXChild::Element(el)),
            Expression::JSXFragment(frag) => children.push(JSXChild::Fragment(frag)),
            _ => unreachable!(
                "parse_astro_multiple_jsx_in_expression called with non-JSX expression"
            ),
        }

        // Parse additional JSX elements while we see `<` followed by JSX content
        while self.at(Kind::LAngle) {
            let checkpoint = self.checkpoint();
            let child_span = self.start_span();
            self.bump_any(); // bump `<`

            let kind = self.cur_kind();
            if kind == Kind::RAngle {
                // `<>` - fragment
                let fragment = self.parse_jsx_fragment(child_span, false);
                children.push(JSXChild::Fragment(fragment));
            } else if kind == Kind::Ident || kind.is_any_keyword() {
                // `<ident` - element
                let element = self.parse_jsx_element(child_span, false);
                children.push(JSXChild::Element(element));
            } else {
                // Not a JSX element/fragment, rewind and stop
                self.rewind(checkpoint);
                break;
            }
        }

        // If only one child (shouldn't happen but handle gracefully), return it directly
        if children.len() == 1 {
            return match children.pop().unwrap() {
                JSXChild::Element(el) => Expression::JSXElement(el),
                JSXChild::Fragment(frag) => Expression::JSXFragment(frag),
                _ => unreachable!(),
            };
        }

        // Wrap all children in an implicit fragment
        let fragment_span = Span::new(span_start, self.prev_token_end);
        let opening = self.ast.jsx_opening_fragment(Span::empty(span_start));
        let closing = self.ast.jsx_closing_fragment(Span::empty(self.prev_token_end));
        let fragment = self.ast.alloc_jsx_fragment(fragment_span, opening, children, closing);
        Expression::JSXFragment(fragment)
    }

    /// Try to parse Astro shorthand attribute `{prop}` -> `prop={prop}`
    /// Returns None if this is not a shorthand (e.g., it's a spread `{...expr}`)
    pub(crate) fn try_parse_astro_shorthand_attribute(
        &mut self,
    ) -> Option<Box<'a, JSXAttribute<'a>>> {
        let checkpoint = self.checkpoint();
        self.bump_any(); // bump `{`

        // Check if this is a shorthand: `{identifier}`
        if self.at(Kind::Ident) || self.cur_kind().is_any_keyword() {
            let ident_span = self.start_span();
            let name = self.cur_src();
            self.bump_any();

            if self.at(Kind::RCurly) {
                // This is shorthand: `{prop}` -> `prop={prop}`
                self.bump_any(); // bump `}`
                let ident_span = self.end_span(ident_span);
                let name = Atom::from(name);
                let identifier = self.ast.jsx_identifier(ident_span, name);
                let attr_name = JSXAttributeName::Identifier(self.alloc(identifier));

                // Create expression for value: the identifier reference
                let ident_ref = self.ast.identifier_reference(ident_span, name);
                let expr = Expression::Identifier(self.alloc(ident_ref));
                let expr_container =
                    self.ast.alloc_jsx_expression_container(ident_span, JSXExpression::from(expr));
                let value = JSXAttributeValue::ExpressionContainer(expr_container);

                return Some(self.ast.alloc_jsx_attribute(ident_span, attr_name, Some(value)));
            }
        }

        // Not shorthand, rewind and return None to fall through to spread parsing
        self.rewind(checkpoint);
        None
    }

    /// Parse an Astro attribute which can have special characters in the name.
    /// Examples: `@click="handler"`, `x.data="value"`, `:any`, `class:list`
    pub(crate) fn parse_astro_attribute(&mut self) -> Box<'a, JSXAttribute<'a>> {
        let span = self.start_span();

        // The current token might be a special character like `:` or `@` that was
        // tokenized separately. We need to reposition the lexer to the start of
        // the current token and re-read the full attribute name.
        let token_start = self.cur_token().span().start;
        self.lexer.set_position_for_astro(token_start);

        // Read the full attribute name including special chars
        self.read_astro_attribute_name();
        let name_span = self.cur_token().span();
        let name = name_span.source_text(self.source_text);
        self.bump_any(); // consume the identifier token

        let identifier = self.ast.jsx_identifier(name_span, name);
        let attr_name = JSXAttributeName::Identifier(self.alloc(identifier));

        let value = if self.at(Kind::Eq) {
            self.expect_jsx_attribute_value(Kind::Eq);
            Some(self.parse_astro_attribute_value())
        } else {
            None
        };

        self.ast.alloc_jsx_attribute(self.end_span(span), attr_name, value)
    }

    /// Parse an Astro attribute value, which can include template literals.
    /// e.g., `<Component attr=`hello ${value}` />`
    pub(crate) fn parse_astro_attribute_value(&mut self) -> JSXAttributeValue<'a> {
        match self.cur_kind() {
            // Template literals can be used directly as attribute values in Astro
            Kind::NoSubstitutionTemplate | Kind::TemplateHead => {
                let span = self.start_span();
                let template_lit = self.parse_template_literal(false);
                let expr = Expression::TemplateLiteral(self.alloc(template_lit));
                let expr_container = self
                    .ast
                    .alloc_jsx_expression_container(self.end_span(span), JSXExpression::from(expr));
                JSXAttributeValue::ExpressionContainer(expr_container)
            }
            // Fall back to standard JSX attribute value parsing
            _ => self.parse_jsx_attribute_value(),
        }
    }

    /// Skip an HTML comment `<!-- ... -->` inside JSX (Astro-specific).
    ///
    /// HTML comments are valid in Astro files.
    /// We skip them entirely rather than producing an AST node.
    pub(crate) fn skip_jsx_html_comment(&mut self) {
        // We're at `!` after `<`
        self.bump_any(); // skip `!`

        // Look for `--` to start comment
        if self.at(Kind::Minus2) {
            self.bump_any(); // skip `--`
        } else if self.at(Kind::Minus) {
            self.bump_any(); // skip first `-`
            if self.at(Kind::Minus) {
                self.bump_any(); // skip second `-`
            }
        }

        // Skip until we find `-->`
        // The lexer is in JSX mode, so we need to scan the source directly
        let start_pos = self.cur_token().span().start as usize;
        if let Some(rest) = self.source_text.get(start_pos..)
            && let Some(end_pos) = rest.find("-->")
        {
            // Move past the `-->` by setting the lexer position
            #[expect(clippy::cast_possible_truncation)]
            let new_pos = (start_pos + end_pos + 3) as u32;
            self.lexer.set_position_for_astro(new_pos);
            self.token = self.lexer.next_jsx_child();
            return;
        }

        // If no closing `-->` found, skip tokens until EOF (error recovery)
        while !self.at(Kind::Eof) {
            if self.at(Kind::Minus2) || self.at(Kind::Minus) {
                self.bump_any();
                if self.at(Kind::Minus) {
                    self.bump_any();
                }
                if self.at(Kind::RAngle) {
                    self.bump_any();
                    break;
                }
            } else {
                self.bump_any();
            }
        }
    }

    /// Check if the element name is an HTML void element.
    /// Void elements cannot have content and don't require a closing tag.
    /// <https://html.spec.whatwg.org/multipage/syntax.html#void-elements>
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
    /// In Astro, `<style>` elements contain raw CSS text, not JSX children.
    ///
    /// Note: `<script>` is NOT included here because Astro has special handling:
    /// - Bare `<script>` (no attributes) is parsed as TypeScript
    /// - `<script>` with attributes is handled specially by `parse_astro_script`
    pub(crate) fn is_astro_raw_text_element(name: &JSXElementName<'a>) -> bool {
        match name {
            JSXElementName::Identifier(ident) => {
                matches!(ident.name.as_str(), "style")
            }
            _ => false,
        }
    }

    /// Parse a `<script>` element encountered inside JSX children (Astro-specific).
    ///
    /// This is similar to `parse_astro_script` in mod.rs but adapted for use within
    /// JSX children context (with in_jsx_child=true semantics).
    ///
    /// According to Astro spec:
    /// - Bare `<script>` (no attributes) = TypeScript, parsed as AstroScript
    /// - `<script>` with any attributes = follows HTML rules, treated as raw text
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn parse_astro_script_in_jsx(&mut self, span: u32) -> JSXChild<'a> {
        // We're at `script` identifier after `<`
        self.bump_any(); // skip `script`

        // Check if there are any attributes (anything before `>` or `/>`)
        let mut has_attributes = false;
        let mut is_self_closing = false;

        // Look for attributes or closing
        while !self.at(Kind::Eof) && !self.at(Kind::RAngle) {
            if self.at(Kind::Slash) {
                self.bump_any(); // skip `/`
                if self.at(Kind::RAngle) {
                    is_self_closing = true;
                    break;
                }
            } else if self.at(Kind::Ident) || self.cur_kind().is_any_keyword() {
                // Found an attribute
                has_attributes = true;
            }
            self.bump_any();
        }

        if self.at(Kind::RAngle) {
            self.bump_any(); // skip `>`
        }

        // Self-closing script tag
        if is_self_closing {
            let end = self.prev_token_end;
            let script_span = oxc_span::Span::new(span, end);

            if has_attributes {
                // Script with attributes - return as regular JSX element with no children
                let name =
                    self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
                let elem_name = JSXElementName::Identifier(self.alloc(name));
                let opening = self.ast.alloc_jsx_opening_element(
                    script_span,
                    elem_name,
                    Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                    self.ast.vec(),
                );
                return JSXChild::Element(self.ast.alloc_jsx_element(
                    script_span,
                    opening,
                    self.ast.vec(),
                    Option::<Box<'a, JSXClosingElement<'a>>>::None,
                ));
            }

            // Bare self-closing script - wrap empty AstroScript in JSX element
            let program = self.ast.program(
                oxc_span::Span::empty(end),
                self.source_type,
                "",
                self.ast.vec(),
                None,
                self.ast.vec(),
                self.ast.vec(),
            );
            let astro_script =
                JSXChild::AstroScript(self.ast.alloc_astro_script(script_span, program));

            // Wrap in a <script> element
            let name = self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
            let elem_name = JSXElementName::Identifier(self.alloc(name));
            let opening = self.ast.alloc_jsx_opening_element(
                script_span,
                elem_name,
                Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                self.ast.vec(),
            );
            return JSXChild::Element(self.ast.alloc_jsx_element(
                script_span,
                opening,
                self.ast.vec1(astro_script),
                Option::<Box<'a, JSXClosingElement<'a>>>::None,
            ));
        }

        // Find the closing </script> tag
        let content_start = self.cur_token().span().start as usize;
        let closing_tag = "</script";

        if let Some(rest) = self.source_text.get(content_start..)
            && let Some(end_offset) = rest.find(closing_tag)
        {
            let content_end = content_start + end_offset;

            // Move lexer to the closing tag
            self.lexer.set_position_for_astro(content_end as u32);
            self.token = self.lexer.next_jsx_child();

            // Skip the closing tag </script>
            self.bump_any(); // skip `<`
            self.bump_any(); // skip `/`
            // Skip `script`
            self.bump_any();
            // Consume the `>`
            let end = if self.at(Kind::RAngle) {
                self.bump_any();
                self.prev_token_end
            } else {
                self.cur_token().span().end
            };

            let full_span = oxc_span::Span::new(span, end);

            if has_attributes {
                // Script with attributes - return as JSX element with raw text child
                let opening_name =
                    self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
                let opening_elem_name = JSXElementName::Identifier(self.alloc(opening_name));
                let opening = self.ast.alloc_jsx_opening_element(
                    oxc_span::Span::new(span, content_start as u32),
                    opening_elem_name,
                    Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                    self.ast.vec(),
                );

                // Closing element
                let closing_name = self
                    .ast
                    .jsx_identifier(oxc_span::Span::new(content_end as u32 + 2, end - 1), "script");
                let closing_elem_name = JSXElementName::Identifier(self.alloc(closing_name));
                let closing = self.ast.alloc_jsx_closing_element(
                    oxc_span::Span::new(content_end as u32, end),
                    closing_elem_name,
                );

                // Raw text child
                let raw_text = &self.source_text[content_start..content_end];
                let text_span = oxc_span::Span::new(content_start as u32, content_end as u32);
                let text = self.ast.alloc_jsx_text(
                    text_span,
                    oxc_span::Atom::from(raw_text),
                    Some(oxc_span::Atom::from(raw_text)),
                );

                return JSXChild::Element(self.ast.alloc_jsx_element(
                    full_span,
                    opening,
                    self.ast.vec1(JSXChild::Text(text)),
                    Some(closing),
                ));
            }

            // Bare script - wrap AstroScript in <script> element
            let script_content_span = oxc_span::Span::new(content_start as u32, content_end as u32);

            let program = self.ast.program(
                script_content_span,
                self.source_type,
                "",
                self.ast.vec(),
                None,
                self.ast.vec(),
                self.ast.vec(),
            );
            let astro_script =
                JSXChild::AstroScript(self.ast.alloc_astro_script(full_span, program));

            // Create <script> opening element
            let opening_name =
                self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
            let opening_elem_name = JSXElementName::Identifier(self.alloc(opening_name));
            let opening = self.ast.alloc_jsx_opening_element(
                oxc_span::Span::new(span, content_start as u32),
                opening_elem_name,
                Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                self.ast.vec(),
            );

            // Create </script> closing element
            let closing_name = self
                .ast
                .jsx_identifier(oxc_span::Span::new(content_end as u32 + 2, end - 1), "script");
            let closing_elem_name = JSXElementName::Identifier(self.alloc(closing_name));
            let closing = self.ast.alloc_jsx_closing_element(
                oxc_span::Span::new(content_end as u32, end),
                closing_elem_name,
            );

            return JSXChild::Element(self.ast.alloc_jsx_element(
                full_span,
                opening,
                self.ast.vec1(astro_script),
                Some(closing),
            ));
        }

        // No closing tag found - error recovery, wrap in element
        let end = self.prev_token_end;
        let script_span = oxc_span::Span::new(span, end);
        let program = self.ast.program(
            oxc_span::Span::empty(end),
            self.source_type,
            "",
            self.ast.vec(),
            None,
            self.ast.vec(),
            self.ast.vec(),
        );
        let astro_script = JSXChild::AstroScript(self.ast.alloc_astro_script(script_span, program));

        // Wrap in a <script> element for consistency
        let name = self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
        let elem_name = JSXElementName::Identifier(self.alloc(name));
        let opening = self.ast.alloc_jsx_opening_element(
            script_span,
            elem_name,
            Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
            self.ast.vec(),
        );
        JSXChild::Element(self.ast.alloc_jsx_element(
            script_span,
            opening,
            self.ast.vec1(astro_script),
            Option::<Box<'a, JSXClosingElement<'a>>>::None,
        ))
    }

    /// Skip the raw text content of a script or style element in Astro.
    /// Returns an empty children vector since we don't parse the content as JSX.
    ///
    /// Note: This is called AFTER the opening element's `>` has been consumed,
    /// but BEFORE the lexer has advanced to parse JSX children.
    /// The `prev_token_end` points to right after the `>`.
    pub(crate) fn skip_astro_raw_text_element_content(
        &mut self,
        name: &JSXElementName<'a>,
        in_jsx_child: bool,
    ) -> Vec<'a, JSXChild<'a>> {
        let tag_name = match name {
            JSXElementName::Identifier(ident) => ident.name.as_str(),
            _ => return self.ast.vec(),
        };

        // Build the closing tag pattern: </script> or </style>
        let closing_tag = format!("</{tag_name}");

        // Start searching from after the `>` we just consumed
        let start_pos = self.prev_token_end as usize;
        if let Some(rest) = self.source_text.get(start_pos..)
            && let Some(end_pos) = rest.find(&closing_tag)
        {
            // Move the lexer position to the closing tag
            #[expect(clippy::cast_possible_truncation)]
            let new_pos = (start_pos + end_pos) as u32;
            self.lexer.set_position_for_astro(new_pos);
            // Read the `<` token to prepare for parsing the closing element
            // Use next_jsx_child if in_jsx_child to maintain consistent lexer state
            if in_jsx_child {
                self.token = self.lexer.next_jsx_child();
            } else {
                self.token = self.lexer.next_token();
            }
        }

        self.ast.vec()
    }
}
