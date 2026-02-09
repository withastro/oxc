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

use crate::{ParserImpl, diagnostics, lexer::Kind};

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

impl<'a> ParserImpl<'a> {
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
            if self.cur_src() == "script" {
                // parse_astro_script_in_jsx returns JSXChild, convert to Expression
                return match self.parse_astro_script_in_jsx(span) {
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
        self.expect_jsx_child(Kind::RAngle);
        let opening_fragment = self.ast.jsx_opening_fragment(self.end_span(span));
        let (children, closing) = self.parse_astro_jsx_children_and_closing(in_jsx_child);
        let closing_fragment = match closing {
            JSXClosing::Fragment(f) => f,
            JSXClosing::Element(e) => {
                self.error(diagnostics::jsx_fragment_no_match(
                    opening_fragment.span,
                    e.name.span(),
                ));
                self.ast.jsx_closing_fragment(e.span)
            }
        };
        self.ast.alloc_jsx_fragment(
            self.end_span(span),
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
        let (opening_element, self_closing, is_raw_text_element, prev_no_expression) =
            self.parse_astro_jsx_opening_element(span, in_jsx_child);

        let (children, closing_element) = if self_closing {
            (self.ast.vec(), None)
        } else {
            // Raw text elements (script/style/is:raw): content is raw text, not JSX children.
            let (children, closing) = if is_raw_text_element {
                let children =
                    self.skip_astro_raw_text_element_content(&opening_element.name, in_jsx_child);
                // Restore the no-expression flag
                self.lexer.no_expression_in_jsx_children = prev_no_expression;
                // Parse `</name>` closing tag
                let closing_span = self.start_span();
                self.bump_any(); // bump `<`
                self.bump_any(); // bump `/`
                let closing = self.parse_astro_jsx_closing_inline(closing_span, in_jsx_child);
                (children, closing)
            } else {
                let result = self.parse_astro_jsx_children_and_closing(in_jsx_child);
                self.lexer.no_expression_in_jsx_children = prev_no_expression;
                result
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
    /// Returns (opening_element, self_closing, is_raw_text_element, prev_no_expression).
    fn parse_astro_jsx_opening_element(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> (
        Box<'a, JSXOpeningElement<'a>>,
        bool, // `true` if self-closing
        bool, // `true` if raw text element (script/style)
        bool, // previous value of no_expression_in_jsx_children (to restore on close)
    ) {
        let name = self.parse_astro_jsx_element_name();
        let type_arguments = if self.is_ts { self.try_parse_type_arguments() } else { None };
        let attributes = self.parse_astro_jsx_attributes();
        let explicit_self_closing = self.eat(Kind::Slash);

        // HTML void elements are implicitly self-closing even without `/`
        let self_closing = explicit_self_closing || Self::is_astro_void_element(&name);

        // Check if this is a raw text element
        let is_raw_text =
            Self::is_astro_raw_text_element(&name) || Self::has_is_raw_attribute(&attributes);

        // For foreign content elements like <math>, set the no-expression flag
        let is_foreign = Self::is_foreign_content_element(&name);
        let prev_no_expression = self.lexer.no_expression_in_jsx_children;
        if is_foreign && !self_closing {
            self.lexer.no_expression_in_jsx_children = true;
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
        (elem, self_closing, is_raw_text, prev_no_expression)
    }

    /// Astro-specific version of `parse_jsx_element_name`.
    /// In Astro, namespaced element names are not supported.
    fn parse_astro_jsx_element_name(&mut self) -> JSXElementName<'a> {
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
    fn parse_astro_jsx_children_and_closing(
        &mut self,
        in_jsx_child: bool,
    ) -> (Vec<'a, JSXChild<'a>>, JSXClosing<'a>) {
        let mut children = self.ast.vec();
        loop {
            if self.fatal_error.is_some() {
                let closing = self.ast.jsx_closing_fragment(self.cur_token().span());
                return (children, JSXClosing::Fragment(closing));
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
                        if self.cur_src() == "script" {
                            children.push(self.parse_astro_script_in_jsx(span));
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
                        self.bump_any(); // bump `/`
                        let closing = self.parse_astro_jsx_closing_inline(span, in_jsx_child);
                        return (children, closing);
                    }

                    return (children, self.unexpected());
                }
                Kind::LCurly => {
                    let span_start = self.start_span();
                    self.bump_any(); // bump `{`

                    if self.eat(Kind::Dot3) {
                        children.push(JSXChild::Spread(self.parse_jsx_spread_child(span_start)));
                        continue;
                    }
                    children.push(JSXChild::ExpressionContainer(
                        self.parse_astro_jsx_expression_container(
                            span_start, /* in_jsx_child */ true,
                        ),
                    ));
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
    fn parse_astro_jsx_closing_inline(
        &mut self,
        open_angle_span: u32,
        in_jsx_child: bool,
    ) -> JSXClosing<'a> {
        if self.at(Kind::RAngle) {
            if in_jsx_child {
                self.expect_jsx_child(Kind::RAngle);
            } else {
                self.expect(Kind::RAngle);
            }
            JSXClosing::Fragment(self.ast.jsx_closing_fragment(self.end_span(open_angle_span)))
        } else {
            let name = self.parse_astro_jsx_element_name();
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

    /// Astro-specific version of `parse_jsx_expression_container`.
    /// Handles Astro JSX children in expressions and allows empty expressions.
    pub(crate) fn parse_astro_jsx_expression_container(
        &mut self,
        span_start: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXExpressionContainer<'a>> {
        // In Astro mode with JSX children starting with `<`, parse as potential JSX children.
        if in_jsx_child && self.at(Kind::LAngle) {
            let expr = self.parse_astro_jsx_children_in_expression(span_start);
            return self.ast.alloc_jsx_expression_container(self.end_span(span_start), expr);
        }

        let expr = if self.at(Kind::RCurly) {
            if in_jsx_child {
                self.expect_jsx_child(Kind::RCurly);
            } else {
                self.expect(Kind::RCurly);
            }
            let span = self.end_span(span_start);

            // In Astro mode, empty expressions are allowed — don't emit error
            if !in_jsx_child {
                // Still allowed in Astro, skip the error
            }

            let expr = self.ast.jsx_empty_expression(Span::new(span.start + 1, span.end - 1));
            JSXExpression::EmptyExpression(expr)
        } else {
            let expr = JSXExpression::from(self.parse_expr());
            if in_jsx_child {
                self.expect_jsx_child(Kind::RCurly);
            } else {
                self.expect(Kind::RCurly);
            }
            expr
        };

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
                    // `{prop}` can be shorthand for `prop={prop}`
                    if let Some(attr) = self.try_parse_astro_shorthand_attribute() {
                        attributes.push(JSXAttributeItem::Attribute(attr));
                        continue;
                    }
                    JSXAttributeItem::SpreadAttribute(self.parse_jsx_spread_attribute())
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
    /// Returns `true` if `<` is followed by an identifier, keyword, or `>` (fragment).
    /// Does not consume any tokens.
    pub(crate) fn is_astro_jsx_after_lt(&mut self) -> bool {
        let checkpoint = self.checkpoint();
        self.bump_any(); // bump `<`
        let next_kind = self.cur_kind();
        let is_jsx =
            next_kind == Kind::RAngle || next_kind == Kind::Ident || next_kind.is_any_keyword();
        self.rewind(checkpoint);
        is_jsx
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
            let checkpoint = self.checkpoint();
            let child_span = self.start_span();
            self.bump_any(); // bump `<`

            let kind = self.cur_kind();
            if kind == Kind::RAngle {
                let fragment = self.parse_astro_jsx_fragment(child_span, false);
                children.push(JSXChild::Fragment(fragment));
            } else if kind == Kind::Ident || kind.is_any_keyword() {
                let element = self.parse_astro_jsx_element(child_span, false);
                children.push(JSXChild::Element(element));
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

    // ==================== Astro-specific helpers ====================

    /// Try to parse Astro shorthand attribute `{prop}` -> `prop={prop}`
    fn try_parse_astro_shorthand_attribute(&mut self) -> Option<Box<'a, JSXAttribute<'a>>> {
        let checkpoint = self.checkpoint();
        self.bump_any(); // bump `{`

        if self.at(Kind::Ident) || self.cur_kind().is_any_keyword() {
            let ident_span = self.start_span();
            let name = self.cur_src();
            self.bump_any();

            if self.at(Kind::RCurly) {
                self.bump_any(); // bump `}`
                let ident_span = self.end_span(ident_span);
                let name = Atom::from(name);
                let identifier = self.ast.jsx_identifier(ident_span, name);
                let attr_name = JSXAttributeName::Identifier(self.alloc(identifier));

                let ident_ref = self.ast.identifier_reference(ident_span, name);
                let expr = Expression::Identifier(self.alloc(ident_ref));
                let expr_container =
                    self.ast.alloc_jsx_expression_container(ident_span, JSXExpression::from(expr));
                let value = JSXAttributeValue::ExpressionContainer(expr_container);

                return Some(self.ast.alloc_jsx_attribute(ident_span, attr_name, Some(value)));
            }
        }

        self.rewind(checkpoint);
        None
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

        let value = if self.at(Kind::Eq) {
            self.expect_jsx_attribute_value(Kind::Eq);
            Some(self.parse_astro_attribute_value())
        } else {
            None
        };

        self.ast.alloc_jsx_attribute(self.end_span(span), attr_name, value)
    }

    /// Parse an Astro attribute value, which can include template literals and unquoted values.
    fn parse_astro_attribute_value(&mut self) -> JSXAttributeValue<'a> {
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
            Kind::Ident => {
                let span = self.start_span();
                let value = Atom::from(self.cur_src());
                self.bump_any();
                let str_lit = self.ast.string_literal(self.end_span(span), value, None);
                JSXAttributeValue::StringLiteral(self.alloc(str_lit))
            }
            Kind::LCurly => {
                let span_start = self.start_span();
                self.bump_any(); // bump `{`
                let expr = self.parse_astro_jsx_expression_container(
                    span_start,
                    /* in_jsx_child */ false,
                );
                JSXAttributeValue::ExpressionContainer(expr)
            }
            _ => self.parse_jsx_attribute_value(),
        }
    }

    /// Skip an HTML comment `<!-- ... -->` inside JSX (Astro-specific).
    pub(crate) fn skip_jsx_html_comment(&mut self) {
        self.bump_any(); // skip `!`

        if self.at(Kind::Minus2) {
            self.bump_any();
        } else if self.at(Kind::Minus) {
            self.bump_any();
            if self.at(Kind::Minus) {
                self.bump_any();
            }
        }

        let start_pos = self.cur_token().span().start as usize;
        if let Some(rest) = self.source_text.get(start_pos..)
            && let Some(end_pos) = rest.find("-->")
        {
            #[expect(clippy::cast_possible_truncation)]
            let new_pos = (start_pos + end_pos + 3) as u32;
            self.lexer.set_position_for_astro(new_pos);
            self.token = self.lexer.next_jsx_child();
            return;
        }

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

    /// Check if the element is a foreign content element where `{` is literal text.
    fn is_foreign_content_element(name: &JSXElementName<'a>) -> bool {
        match name {
            JSXElementName::Identifier(ident) => ident.name.as_str() == "math",
            _ => false,
        }
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

    /// Astro-specific element name equality check.
    fn astro_jsx_element_name_eq(lhs: &JSXElementName<'a>, rhs: &JSXElementName<'a>) -> bool {
        match (lhs, rhs) {
            (JSXElementName::Identifier(lhs), JSXElementName::Identifier(rhs)) => {
                lhs.name == rhs.name
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

    /// Parse a `<script>` element encountered inside JSX children (Astro-specific).
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn parse_astro_script_in_jsx(&mut self, span: u32) -> JSXChild<'a> {
        self.bump_any(); // skip `script`

        let attributes = self.parse_astro_jsx_attributes();
        let has_attributes = !attributes.is_empty();

        let is_self_closing = if self.at(Kind::Slash) {
            self.bump_any();
            true
        } else {
            false
        };

        if self.at(Kind::RAngle) {
            self.bump_any();
        }

        if is_self_closing {
            let end = self.prev_token_end;
            let script_span = oxc_span::Span::new(span, end);

            if has_attributes {
                let name =
                    self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
                let elem_name = JSXElementName::Identifier(self.alloc(name));
                let opening = self.ast.alloc_jsx_opening_element(
                    script_span,
                    elem_name,
                    Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                    attributes,
                );
                return JSXChild::Element(self.ast.alloc_jsx_element(
                    script_span,
                    opening,
                    self.ast.vec(),
                    Option::<Box<'a, JSXClosingElement<'a>>>::None,
                ));
            }

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

            let name = self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
            let elem_name = JSXElementName::Identifier(self.alloc(name));
            let opening = self.ast.alloc_jsx_opening_element(
                script_span,
                elem_name,
                Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                attributes,
            );
            return JSXChild::Element(self.ast.alloc_jsx_element(
                script_span,
                opening,
                self.ast.vec1(astro_script),
                Option::<Box<'a, JSXClosingElement<'a>>>::None,
            ));
        }

        let content_start = self.cur_token().span().start as usize;
        let closing_tag = "</script";

        if let Some(rest) = self.source_text.get(content_start..)
            && let Some(end_offset) = rest.find(closing_tag)
        {
            let content_end = content_start + end_offset;

            self.lexer.set_position_for_astro(content_end as u32);
            self.token = self.lexer.next_jsx_child();

            self.bump_any(); // skip `<`
            self.bump_any(); // skip `/`
            self.bump_any(); // Skip `script`
            let end = if self.at(Kind::RAngle) {
                self.bump_any();
                self.prev_token_end
            } else {
                self.cur_token().span().end
            };

            let full_span = oxc_span::Span::new(span, end);

            if has_attributes {
                let opening_name =
                    self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
                let opening_elem_name = JSXElementName::Identifier(self.alloc(opening_name));
                let opening = self.ast.alloc_jsx_opening_element(
                    oxc_span::Span::new(span, content_start as u32),
                    opening_elem_name,
                    Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                    attributes,
                );

                let closing_name = self
                    .ast
                    .jsx_identifier(oxc_span::Span::new(content_end as u32 + 2, end - 1), "script");
                let closing_elem_name = JSXElementName::Identifier(self.alloc(closing_name));
                let closing = self.ast.alloc_jsx_closing_element(
                    oxc_span::Span::new(content_end as u32, end),
                    closing_elem_name,
                );

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

            let opening_name =
                self.ast.jsx_identifier(oxc_span::Span::new(span + 1, span + 7), "script");
            let opening_elem_name = JSXElementName::Identifier(self.alloc(opening_name));
            let opening = self.ast.alloc_jsx_opening_element(
                oxc_span::Span::new(span, content_start as u32),
                opening_elem_name,
                Option::<Box<'a, oxc_ast::ast::TSTypeParameterInstantiation<'a>>>::None,
                attributes,
            );

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

        // No closing tag found - error recovery
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

    /// Skip the raw text content of a raw text element in Astro.
    pub(crate) fn skip_astro_raw_text_element_content(
        &mut self,
        name: &JSXElementName<'a>,
        in_jsx_child: bool,
    ) -> Vec<'a, JSXChild<'a>> {
        let tag_name = match name {
            JSXElementName::Identifier(ident) => ident.name.as_str(),
            JSXElementName::IdentifierReference(ident_ref) => ident_ref.name.as_str(),
            JSXElementName::MemberExpression(_) => name.span().source_text(self.source_text),
            JSXElementName::NamespacedName(_) | JSXElementName::ThisExpression(_) => {
                name.span().source_text(self.source_text)
            }
        };

        let closing_tag = format!("</{tag_name}");

        let start_pos = self.prev_token_end as usize;
        if let Some(rest) = self.source_text.get(start_pos..)
            && let Some(end_pos) = rest.find(&closing_tag)
        {
            #[expect(clippy::cast_possible_truncation)]
            let content_end = (start_pos + end_pos) as u32;
            let content_start = self.prev_token_end;

            self.lexer.set_position_for_astro(content_end);
            if in_jsx_child {
                self.token = self.lexer.next_jsx_child();
            } else {
                self.token = self.lexer.next_token();
            }

            if content_end > content_start {
                let span = Span::new(content_start, content_end);
                let raw_text = span.source_text(self.source_text);
                let jsx_text = self.ast.alloc_jsx_text(span, raw_text, Some(Atom::from(raw_text)));
                return self.ast.vec1(JSXChild::Text(jsx_text));
            }
        }

        self.ast.vec()
    }

    /// Parse HTML comment in JSX (Astro-specific).
    #[expect(clippy::cast_possible_truncation)]
    pub(crate) fn parse_html_comment_in_jsx(&mut self, span: u32) -> Option<JSXChild<'a>> {
        let start_pos = self.prev_token_end as usize;

        if let Some(rest) = self.source_text.get(start_pos..)
            && rest.starts_with("!--")
            && let Some(end_offset) = rest.find("-->")
        {
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
                        if self.cur_src() == "script" {
                            children.push(self.parse_astro_script_in_jsx(child_span));
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
                        self.skip_jsx_html_comment();
                        self.token = self.lexer.next_jsx_child();
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
                    let nested_span = self.start_span();
                    self.bump_any();

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
                    self.token = self.lexer.next_jsx_child();
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

        self.expect_jsx_child(Kind::RCurly);

        if children.len() == 1 {
            match children.pop().unwrap() {
                JSXChild::Element(el) => return JSXExpression::JSXElement(el),
                JSXChild::Fragment(frag) => return JSXExpression::JSXFragment(frag),
                other => children.push(other),
            }
        }

        let fragment_span = Span::new(fragment_span_start, self.prev_token_end);
        let opening = self.ast.jsx_opening_fragment(Span::empty(fragment_span_start));
        let closing = self.ast.jsx_closing_fragment(Span::empty(self.prev_token_end));
        let fragment = self.ast.alloc_jsx_fragment(fragment_span, opening, children, closing);
        JSXExpression::JSXFragment(fragment)
    }
}
