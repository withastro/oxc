//! Astro-specific JSX parsing extensions.
//!
//! This module contains JSX parsing methods that are only used when parsing Astro files.
//! These are called from the main JSX parser when `source_type.is_astro()` is true.

use oxc_allocator::Box;
use oxc_ast::ast::*;
use oxc_span::Span;

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
                        // `<ident` - element
                        let element = self.parse_jsx_element(child_span, true);
                        children.push(JSXChild::Element(element));
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
            Some(self.parse_jsx_attribute_value())
        } else {
            None
        };

        self.ast.alloc_jsx_attribute(self.end_span(span), attr_name, value)
    }
}
