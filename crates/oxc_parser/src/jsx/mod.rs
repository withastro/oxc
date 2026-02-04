//! [JSX](https://facebook.github.io/jsx)

use oxc_allocator::{Box, Dummy, Vec};
use oxc_ast::ast::*;
use oxc_span::{Atom, GetSpan, Span};

use crate::{ParserImpl, diagnostics, lexer::Kind};

impl<'a> ParserImpl<'a> {
    pub(crate) fn parse_jsx_expression(&mut self) -> Expression<'a> {
        let span = self.start_span();
        self.bump_any(); // bump `<`
        let kind = self.cur_kind();
        if kind == Kind::RAngle {
            Expression::JSXFragment(self.parse_jsx_fragment(span, false))
        } else if kind.is_identifier_or_keyword() {
            // In Astro, check for <script> which needs special handling
            #[cfg(feature = "astro")]
            if self.source_type.is_astro() && self.cur_src() == "script" {
                // parse_astro_script_in_jsx returns JSXChild, convert to Expression
                return match self.parse_astro_script_in_jsx(span) {
                    JSXChild::Element(el) => Expression::JSXElement(el),
                    JSXChild::AstroScript(script) => {
                        // Wrap AstroScript in a synthetic JSX element for expression context
                        // This maintains the expectation that JSX expressions return JSXElement
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
            Expression::JSXElement(self.parse_jsx_element(span, false))
        } else {
            self.unexpected()
        }
    }

    /// `JSXFragment` :
    ///   < > `JSXChildren_opt` < / >
    pub(crate) fn parse_jsx_fragment(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXFragment<'a>> {
        self.expect_jsx_child(Kind::RAngle);
        let opening_fragment = self.ast.jsx_opening_fragment(self.end_span(span));
        let children = self.parse_jsx_children();
        let closing_fragment = self.parse_jsx_closing_fragment(in_jsx_child);
        self.ast.alloc_jsx_fragment(
            self.end_span(span),
            opening_fragment,
            children,
            closing_fragment,
        )
    }

    /// </>
    fn parse_jsx_closing_fragment(&mut self, in_jsx_child: bool) -> JSXClosingFragment {
        let span = self.start_span();
        self.expect(Kind::LAngle);
        self.expect(Kind::Slash);
        if in_jsx_child {
            self.expect_jsx_child(Kind::RAngle);
        } else {
            self.expect(Kind::RAngle);
        }
        self.ast.jsx_closing_fragment(self.end_span(span))
    }

    /// `JSXElement` :
    ///   `JSXSelfClosingElement`
    ///   `JSXOpeningElement` `JSXChildren_opt` `JSXClosingElement`
    /// `in_jsx_child`:
    ///     used for telling `JSXClosingElement` to parse the next jsx child or not
    ///     true when inside jsx element, false when at top level expression
    pub(crate) fn parse_jsx_element(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXElement<'a>> {
        // In Astro, raw text elements (script/style) need special handling
        #[cfg(feature = "astro")]
        let (opening_element, self_closing, is_raw_text_element) =
            self.parse_jsx_opening_element(span, in_jsx_child);
        #[cfg(not(feature = "astro"))]
        let (opening_element, self_closing) = self.parse_jsx_opening_element(span, in_jsx_child);

        let (children, closing_element) = if self_closing {
            (self.ast.vec(), None)
        } else {
            #[cfg(feature = "astro")]
            let children = if is_raw_text_element {
                self.skip_astro_raw_text_element_content(&opening_element.name, in_jsx_child)
            } else {
                self.parse_jsx_children()
            };
            #[cfg(not(feature = "astro"))]
            let children = self.parse_jsx_children();

            let closing_element = self.parse_jsx_closing_element(in_jsx_child);
            if !Self::jsx_element_name_eq(&opening_element.name, &closing_element.name) {
                self.error(diagnostics::jsx_element_no_match(
                    opening_element.name.span(),
                    closing_element.name.span(),
                    opening_element.name.span().source_text(self.source_text),
                ));
            }
            (children, Some(closing_element))
        };
        self.ast.alloc_jsx_element(self.end_span(span), opening_element, children, closing_element)
    }

    /// `JSXOpeningElement` :
    /// < `JSXElementName` `JSXAttributes_opt` >
    ///
    /// Returns (opening_element, self_closing) for regular JSX.
    /// Returns (opening_element, self_closing, is_raw_text_element) for Astro.
    #[cfg(feature = "astro")]
    fn parse_jsx_opening_element(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> (
        Box<'a, JSXOpeningElement<'a>>,
        bool, // `true` if self-closing
        bool, // `true` if raw text element (script/style) - Astro only
    ) {
        let name = self.parse_jsx_element_name();
        // <Component<TsType> for tsx
        let type_arguments = if self.is_ts { self.try_parse_type_arguments() } else { None };
        let attributes = self.parse_jsx_attributes();
        let explicit_self_closing = self.eat(Kind::Slash);

        // In Astro, HTML void elements are implicitly self-closing even without `/`
        let self_closing = explicit_self_closing
            || (self.source_type.is_astro() && Self::is_astro_void_element(&name));

        // Check if this is a raw text element (script/style) in Astro
        let is_raw_text = self.source_type.is_astro() && Self::is_astro_raw_text_element(&name);

        // For raw text elements, we must NOT call next_token() or next_jsx_child()
        // because the content should not be parsed as JSX/JS. We just check for `>`
        // without advancing, then record prev_token_end manually.
        // The caller (skip_astro_raw_text_element_content) will reposition the lexer.
        if is_raw_text && !explicit_self_closing {
            self.expect_without_advance(Kind::RAngle);
            // Manually update prev_token_end to point after the `>`
            self.prev_token_end = self.cur_token().span().end;
        } else if !explicit_self_closing || in_jsx_child {
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
        (elem, self_closing, is_raw_text)
    }

    /// `JSXOpeningElement` :
    /// < `JSXElementName` `JSXAttributes_opt` >
    #[cfg(not(feature = "astro"))]
    fn parse_jsx_opening_element(
        &mut self,
        span: u32,
        in_jsx_child: bool,
    ) -> (
        Box<'a, JSXOpeningElement<'a>>,
        bool, // `true` if self-closing
    ) {
        let name = self.parse_jsx_element_name();
        // <Component<TsType> for tsx
        let type_arguments = if self.is_ts { self.try_parse_type_arguments() } else { None };
        let attributes = self.parse_jsx_attributes();
        let self_closing = self.eat(Kind::Slash);

        if !self_closing || in_jsx_child {
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
        (elem, self_closing)
    }

    fn parse_jsx_closing_element(&mut self, in_jsx_child: bool) -> Box<'a, JSXClosingElement<'a>> {
        let span = self.start_span();
        self.expect(Kind::LAngle);
        self.expect(Kind::Slash);
        let name = self.parse_jsx_element_name();
        if in_jsx_child {
            self.expect_jsx_child(Kind::RAngle);
        } else {
            self.expect(Kind::RAngle);
        }
        self.ast.alloc_jsx_closing_element(self.end_span(span), name)
    }

    /// `JSXElementName` :
    ///   `JSXIdentifier`
    ///   `JSXNamespacedName`
    ///   `JSXMemberExpression`
    fn parse_jsx_element_name(&mut self) -> JSXElementName<'a> {
        let span = self.start_span();
        let identifier = self.parse_jsx_identifier();

        // <namespace:property />
        if self.eat(Kind::Colon) {
            let property = self.parse_jsx_identifier();
            return self.ast.jsx_element_name_namespaced_name(
                self.end_span(span),
                identifier,
                property,
            );
        }

        // <member.foo.bar />
        if self.at(Kind::Dot) {
            return JSXElementName::MemberExpression(
                self.parse_jsx_member_expression(span, &identifier),
            );
        }

        if self.fatal_error.is_some() {
            return JSXElementName::dummy(self.ast.allocator);
        }

        // Determine if this JSX element name is a reference (component) or an intrinsic element.
        // References (components) are:
        // - ASCII names that start with uppercase letter, `_` or `$`: `<Foo>`, `<_foo>`, `<$foo>`
        // - All non-ASCII names (e.g., Unicode identifiers like `<테스트>`)
        // - Names without hyphens (hyphenated names like `<my-element>` are custom elements)
        // https://babeljs.io/repl#?code_lz=DwMQ9mAED0B8DcAoYAzCMHIPpqnJwAJLhkkA&presets=react
        //
        // The identifier has already been validated by the parser, so for ASCII characters
        // we know it can only be `a-z`, `A-Z`, `_` or `$`.
        // Use a fast path for ASCII to avoid expensive Unicode operations in the common case.
        let name = identifier.name.as_str();
        let is_reference = match name.as_bytes()[0] {
            b if b.is_ascii() => !b.is_ascii_lowercase(), // Matches A-Z, _, $
            _ => true, // Non-ASCII characters are always treated as references
        } && !name.contains('-'); // Exclude hyphenated custom elements

        if is_reference {
            let identifier = self.ast.alloc_identifier_reference(identifier.span, identifier.name);
            JSXElementName::IdentifierReference(identifier)
        } else if name == "this" {
            JSXElementName::ThisExpression(self.ast.alloc_this_expression(identifier.span))
        } else {
            JSXElementName::Identifier(self.alloc(identifier))
        }
    }

    /// `JSXMemberExpression` :
    /// `JSXIdentifier` . `JSXIdentifier`
    /// `JSXMemberExpression` . `JSXIdentifier`
    fn parse_jsx_member_expression(
        &mut self,
        span: u32,
        object: &JSXIdentifier<'a>,
    ) -> Box<'a, JSXMemberExpression<'a>> {
        let mut object = if object.name == "this" {
            self.ast.jsx_member_expression_object_this_expression(object.span)
        } else {
            self.ast.jsx_member_expression_object_identifier_reference(object.span, object.name)
        };

        let mut span = Span::new(span, 0);
        let mut property = None;

        while self.eat(Kind::Dot) && self.fatal_error.is_none() {
            // <foo.bar.baz>
            if let Some(prop) = property {
                object =
                    self.ast.jsx_member_expression_object_member_expression(span, object, prop);
            }

            // <foo.bar>
            let ident = self.parse_jsx_identifier();
            // `<foo.bar- />` is a syntax error.
            if ident.name.contains('-') {
                let error = diagnostics::identifier_expected_jsx_no_hyphen(ident.span);
                return self.fatal_error(error);
            }
            property = Some(ident);
            span = self.end_span(span.start);
        }

        if let Some(property) = property {
            return self.ast.alloc_jsx_member_expression(
                self.end_span(span.start),
                object,
                property,
            );
        }

        self.unexpected()
    }

    /// `JSXChildren` :
    ///   `JSXChild` `JSXChildren_opt`
    fn parse_jsx_children(&mut self) -> Vec<'a, JSXChild<'a>> {
        let mut children = self.ast.vec();
        while self.fatal_error.is_none() {
            if let Some(child) = self.parse_jsx_child() {
                children.push(child);
            } else {
                break;
            }
        }
        children
    }

    /// `JSXChild` :
    ///   `JSXText`
    ///   `JSXElement`
    ///   `JSXFragment`
    ///   { `JSXChildExpression_opt` }
    fn parse_jsx_child(&mut self) -> Option<JSXChild<'a>> {
        match self.cur_kind() {
            Kind::LAngle => {
                let span = self.start_span();
                let checkpoint = self.checkpoint();
                self.bump_any(); // bump `<`
                let kind = self.cur_kind();
                // <> open fragment
                if kind == Kind::RAngle {
                    return Some(JSXChild::Fragment(self.parse_jsx_fragment(span, true)));
                }
                // <ident open element
                if kind == Kind::Ident || kind.is_any_keyword() {
                    // In Astro, check for <script> which needs special handling
                    #[cfg(feature = "astro")]
                    if self.source_type.is_astro() && self.cur_src() == "script" {
                        return Some(self.parse_astro_script_in_jsx(span));
                    }
                    return Some(JSXChild::Element(self.parse_jsx_element(span, true)));
                }
                // </ close fragment
                if kind == Kind::Slash {
                    self.rewind(checkpoint);
                    return None;
                }
                self.unexpected()
            }
            Kind::LCurly => {
                let span_start = self.start_span();
                self.bump_any(); // bump `{`

                // {...expr}
                if self.eat(Kind::Dot3) {
                    return Some(JSXChild::Spread(self.parse_jsx_spread_child(span_start)));
                }
                // {expr}
                Some(JSXChild::ExpressionContainer(
                    self.parse_jsx_expression_container(span_start, /* in_jsx_child */ true),
                ))
            }
            // text
            Kind::JSXText => Some(JSXChild::Text(self.parse_jsx_text())),
            _ => self.unexpected(),
        }
    }

    ///   { `JSXChildExpression_opt` }
    pub(crate) fn parse_jsx_expression_container(
        &mut self,
        span_start: u32,
        in_jsx_child: bool,
    ) -> Box<'a, JSXExpressionContainer<'a>> {
        // In Astro mode with JSX children starting with `<`, parse as potential JSX children directly.
        // This allows: { <div>1</div> <div>2</div> } without explicit fragments.
        // We must detect this BEFORE calling parse_expr() because parse_expr() would
        // misinterpret the `<` after the first element as a binary comparison operator.
        #[cfg(feature = "astro")]
        if self.source_type.is_astro() && in_jsx_child && self.at(Kind::LAngle) {
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

            // Empty expression is not allowed in JSX attribute value
            // e.g. `<C attr={} />`
            if !in_jsx_child {
                self.error(diagnostics::jsx_attribute_value_empty_expression(span));
            }

            // Handle comment between curly braces (ex. `{/* comment */}`)
            //                                            ^^^^^^^^^^^^^ span
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

    /// `JSXChildExpression` :
    ///   { ... `AssignmentExpression` }
    pub(crate) fn parse_jsx_spread_child(
        &mut self,
        span_start: u32,
    ) -> Box<'a, JSXSpreadChild<'a>> {
        let expr = self.parse_expr();
        self.expect_jsx_child(Kind::RCurly);
        self.ast.alloc_jsx_spread_child(self.end_span(span_start), expr)
    }

    /// `JSXAttributes` :
    ///   `JSXSpreadAttribute` `JSXAttributes_opt`
    ///   `JSXAttribute` `JSXAttributes_opt`
    fn parse_jsx_attributes(&mut self) -> Vec<'a, JSXAttributeItem<'a>> {
        let mut attributes = self.ast.vec();
        #[cfg(feature = "astro")]
        let is_astro = self.source_type.is_astro();
        loop {
            let kind = self.cur_kind();
            if matches!(kind, Kind::Eof | Kind::LAngle | Kind::RAngle | Kind::Slash)
                || self.fatal_error.is_some()
            {
                break;
            }
            let attribute = match kind {
                Kind::LCurly => {
                    // In Astro, `{prop}` can be shorthand for `prop={prop}`
                    #[cfg(feature = "astro")]
                    if is_astro && let Some(attr) = self.try_parse_astro_shorthand_attribute() {
                        attributes.push(JSXAttributeItem::Attribute(attr));
                        continue;
                    }
                    JSXAttributeItem::SpreadAttribute(self.parse_jsx_spread_attribute())
                }
                _ => {
                    // In Astro, use permissive attribute name parsing
                    #[cfg(feature = "astro")]
                    if is_astro {
                        JSXAttributeItem::Attribute(self.parse_astro_attribute())
                    } else {
                        JSXAttributeItem::Attribute(self.parse_jsx_attribute())
                    }
                    #[cfg(not(feature = "astro"))]
                    JSXAttributeItem::Attribute(self.parse_jsx_attribute())
                }
            };
            attributes.push(attribute);
        }
        attributes
    }

    /// `JSXAttribute` :
    ///   `JSXAttributeName` `JSXAttributeInitializer_opt`
    fn parse_jsx_attribute(&mut self) -> Box<'a, JSXAttribute<'a>> {
        let span = self.start_span();
        let name = self.parse_jsx_attribute_name();
        let value = if self.at(Kind::Eq) {
            self.expect_jsx_attribute_value(Kind::Eq);
            Some(self.parse_jsx_attribute_value())
        } else {
            None
        };
        self.ast.alloc_jsx_attribute(self.end_span(span), name, value)
    }

    /// `JSXSpreadAttribute` :
    ///   { ... `AssignmentExpression` }
    fn parse_jsx_spread_attribute(&mut self) -> Box<'a, JSXSpreadAttribute<'a>> {
        let span = self.start_span();
        self.bump_any(); // bump `{`
        self.expect(Kind::Dot3);
        let argument = self.parse_expr();
        self.expect(Kind::RCurly);
        self.ast.alloc_jsx_spread_attribute(self.end_span(span), argument)
    }

    /// `JSXAttributeName` :
    ///   `JSXIdentifier`
    ///   `JSXNamespacedName`
    fn parse_jsx_attribute_name(&mut self) -> JSXAttributeName<'a> {
        let span = self.start_span();
        let identifier = self.parse_jsx_identifier();

        if self.eat(Kind::Colon) {
            let property = self.parse_jsx_identifier();
            return self.ast.jsx_attribute_name_namespaced_name(
                self.end_span(span),
                identifier,
                property,
            );
        }

        JSXAttributeName::Identifier(self.alloc(identifier))
    }

    pub(crate) fn parse_jsx_attribute_value(&mut self) -> JSXAttributeValue<'a> {
        match self.cur_kind() {
            Kind::Str => {
                let str_lit = self.parse_literal_string();
                JSXAttributeValue::StringLiteral(self.alloc(str_lit))
            }
            Kind::LCurly => {
                let span_start = self.start_span();
                self.bump_any(); // bump `{`

                let expr =
                    self.parse_jsx_expression_container(span_start, /* in_jsx_child */ false);
                JSXAttributeValue::ExpressionContainer(expr)
            }
            Kind::LAngle => match self.parse_jsx_expression() {
                Expression::JSXFragment(fragment) => JSXAttributeValue::Fragment(fragment),
                Expression::JSXElement(element) => JSXAttributeValue::Element(element),
                _ => self.unexpected(),
            },
            _ => self.unexpected(),
        }
    }

    /// `JSXIdentifier` :
    ///   `IdentifierStart`
    ///   `JSXIdentifier` `IdentifierPart`
    ///   `JSXIdentifier` [no `WhiteSpace` or Comment here] -
    fn parse_jsx_identifier(&mut self) -> JSXIdentifier<'a> {
        let span = self.start_span();
        let kind = self.cur_kind();
        if kind != Kind::Ident && !kind.is_any_keyword() {
            return self.unexpected();
        }
        // Currently at a valid normal Ident or Keyword, keep on lexing for `-` in `<component-name />`
        self.continue_lex_jsx_identifier();
        self.bump_any();
        let span = self.end_span(span);
        let name = span.source_text(self.source_text);
        self.ast.jsx_identifier(span, name)
    }

    pub(crate) fn parse_jsx_text(&mut self) -> Box<'a, JSXText<'a>> {
        let span = self.cur_token().span();
        let raw = Atom::from(self.cur_src());
        let value = Atom::from(self.cur_string());
        self.bump_any();
        self.ast.alloc_jsx_text(span, value, Some(raw))
    }

    fn jsx_element_name_eq(lhs: &JSXElementName<'a>, rhs: &JSXElementName<'a>) -> bool {
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
                Self::jsx_member_expression_eq(lhs, rhs)
            }
            (JSXElementName::ThisExpression(_), JSXElementName::ThisExpression(_)) => true,
            _ => false,
        }
    }

    fn jsx_member_expression_eq(
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
            ) => Self::jsx_member_expression_eq(lhs, rhs),
            (
                JSXMemberExpressionObject::ThisExpression(_),
                JSXMemberExpressionObject::ThisExpression(_),
            ) => true,
            _ => false,
        }
    }
}
