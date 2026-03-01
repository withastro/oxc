use oxc_allocator::Vec;
use oxc_ast::ast::{
    JSXAttributeItem, JSXChild, JSXElement, JSXExpression, JSXExpressionContainer, JSXFragment,
};
use oxc_span::{GetSpan, Span};

use crate::{
    ast_nodes::{AstNode, AstNodes},
    best_fitting, format_args,
    formatter::{Formatter, prelude::*, trivia::FormatTrailingComments},
    parentheses::NeedsParentheses,
    print::{
        astro::{
            astro_is_whitespace_only_text, astro_should_self_close, is_astro_top_level_parent,
            is_dummy_ancestor,
        },
        jsx::{FormatChildrenResult, FormatOpeningElement},
    },
    utils::{
        jsx::{WrapState, is_meaningful_jsx_text},
        suppressed::FormatSuppressedNode,
    },
    write,
};

use super::{FormatJsxChildList, JsxChildListLayout};

/// Union type for JSX elements and fragments that have children
#[derive(Debug, Clone)]
pub enum AnyJsxTagWithChildren<'a, 'b> {
    Element(&'b AstNode<'a, JSXElement<'a>>),
    Fragment(&'b AstNode<'a, JSXFragment<'a>>),
}

impl<'a> AnyJsxTagWithChildren<'a, '_> {
    fn span(&self) -> Span {
        match self {
            Self::Element(element) => element.span(),
            Self::Fragment(fragment) => fragment.span(),
        }
    }

    fn format_leading_comments(&self, f: &mut Formatter<'_, 'a>) {
        match self {
            Self::Element(element) => element.format_leading_comments(f),
            Self::Fragment(fragment) => fragment.format_leading_comments(f),
        }
    }

    fn format_trailing_comments(&self, f: &mut Formatter<'_, 'a>) {
        let trailing_comments = if let AstNodes::ArrowFunctionExpression(arrow) =
            // Guard: only walk 3 levels up when none of those ancestors is the sentinel Dummy
            // node. Top-level JSX in an Astro body has AstroRoot as grandparent and Dummy as
            // great-grandparent; the triple-parent chain must not be attempted there.
            // See `print::astro::is_dummy_ancestor` for details.
            {
                let p1 = self.parent();
                let p2 = p1.parent();
                let p3 = p2.parent();
                if is_dummy_ancestor(p1, p2, p3) {
                    &AstNodes::Dummy()
                } else {
                    p3
                }
            }
            && arrow.expression
        {
            f.context().comments().comments_before(arrow.span.end)
        } else if let AstNodes::ConditionalExpression(conditional) = self.parent() {
            if self.span() == conditional.alternate.span() {
                // Since `preserveParens` is disabled, `conditional.alternate.span` only covers
                // `<Success />`, not the surrounding parentheses or comments within them:
                // ```jsx
                // false ? (
                //   <Error />
                // ) : (
                //   <Success />
                //   /* comment */
                // )
                // ```
                // To capture comments like the one above, we get all comments before the
                // conditional expression's end (which includes the closing paren).
                f.context().comments().comments_before(conditional.span.end)
            } else {
                f.context().comments().end_of_line_comments_after(self.span().end)
            }
        } else {
            // Fall back to default trailing comments behavior
            return match self {
                Self::Element(element) => element.format_trailing_comments(f),
                Self::Fragment(fragment) => fragment.format_trailing_comments(f),
            };
        };
        FormatTrailingComments::Comments(trailing_comments).fmt(f);
    }

    /// Checks if a JSX Element should be wrapped in parentheses. Returns a [WrapState] which
    /// indicates when the element should be wrapped in parentheses.
    pub fn get_wrap_state(&self) -> WrapState {
        let parent = self.parent();
        // Call site has ensures that only non-nested JSX elements are passed.
        debug_assert!(!matches!(parent, AstNodes::JSXElement(_) | AstNodes::JSXFragment(_)));

        match parent {
            AstNodes::ArrayExpression(_)
            | AstNodes::JSXAttribute(_)
            | AstNodes::JSXExpressionContainer(_)
            | AstNodes::ConditionalExpression(_) => WrapState::NoWrap,
            // Top-level JSX in an Astro body is a direct child of AstroRoot; no wrapping needed.
            _ if is_astro_top_level_parent(parent) => WrapState::NoWrap,
            AstNodes::StaticMemberExpression(member) => {
                if member.optional {
                    WrapState::NoWrap
                } else {
                    WrapState::WrapOnBreak
                }
            }
            // It is a argument of a call expression
            AstNodes::CallExpression(call) if call.is_argument_span(self.span()) => {
                WrapState::NoWrap
            }
            AstNodes::NewExpression(new) if new.is_argument_span(self.span()) => WrapState::NoWrap,
            AstNodes::ExpressionStatement(stmt) => {
                // `() => <div></div>`
                //        ^^^^^^^^^^^
                if stmt.is_arrow_function_body() {
                    WrapState::WrapOnBreak
                } else {
                    WrapState::NoWrap
                }
            }
            AstNodes::ComputedMemberExpression(member) => {
                if member.optional {
                    WrapState::NoWrap
                } else {
                    WrapState::WrapOnBreak
                }
            }
            _ => WrapState::WrapOnBreak,
        }
    }
}

impl<'a> Format<'a> for AnyJsxTagWithChildren<'a, '_> {
    fn fmt(&self, f: &mut Formatter<'_, 'a>) {
        let is_suppressed = f.comments().is_suppressed(self.span().start);

        let format_tag = format_with(|f| {
            if is_suppressed {
                return FormatSuppressedNode(self.span()).fmt(f);
            }

            let layout = self.layout(f);

            // In Astro files, elements whose only child was whitespace-only text
            // (spaces/tabs, no newlines) are rendered as self-closing (`<Tag />`).
            // Elements that are explicitly empty (<div></div>) or have newline-only
            // whitespace use different layouts. Regular JSX is not affected.
            // See `print::astro::astro_should_self_close` for the guard.
            let effective_self_closing =
                astro_should_self_close(f) && matches!(layout, ElementLayout::NoChildren);

            let format_opening = format_with(|f| {
                if effective_self_closing {
                    self.fmt_opening_self_closing(f);
                } else {
                    self.fmt_opening(f);
                }
            });
            let format_closing = format_with(|f| {
                // If we're rendering as self-closing, skip the closing tag.
                if !effective_self_closing {
                    self.fmt_closing(f);
                }
            });

            match layout {
                ElementLayout::NoChildren => {
                    write!(f, [format_opening, format_closing]);
                }
                ElementLayout::ExplicitEmpty => {
                    // Explicitly-empty element (`<div></div>`): keep open+close, no children.
                    write!(f, [format_opening, format_closing]);
                }
                ElementLayout::AstroSingleSpace => {
                    // Newline-only whitespace child in Astro: format as `<elem> </elem>`.
                    write!(f, [format_opening, token(" "), format_closing]);
                }
                ElementLayout::Template(expression) => {
                    write!(f, [format_opening, expression, format_closing]);
                }
                ElementLayout::Default => {
                    let format_opening = format_opening.memoized();
                    let opening_breaks = format_opening.inspect(f).will_break();

                    let multiple_attributes = match self {
                        Self::Element(element) => element.opening_element.attributes.len() > 1,
                        Self::Fragment(_) => false,
                    };

                    let list_layout = if multiple_attributes || opening_breaks {
                        JsxChildListLayout::Multiline
                    } else {
                        JsxChildListLayout::BestFitting
                    };

                    // In Astro files, expression-only children of top-level template
                    // elements are always formatted in multiline style. Prettier's Astro
                    // plugin uses `group(children, { shouldBreak: true })` for these.
                    //
                    // "Top-level" means the element's parent is `AstroRoot` (direct child
                    // of the template body). Nested elements like `<h1>` inside `<body>`
                    // use normal JSX rules (inline if they fit on one line).
                    //
                    // Example (top-level → force multiline):
                    //   `<h1>{obj.prop}</h1>` → `<h1>\n  {obj.prop}\n</h1>`
                    //
                    // Example (nested → keep inline):
                    //   `<h1>{title}</h1>` (inside <body>) → `<h1>{title}</h1>`
                    let is_astro_template_element = f.context().source_type().is_astro()
                        && matches!(self.parent(), AstNodes::AstroRoot(_));

                    let children = self.children();
                    let child_list = FormatJsxChildList::default().with_options(list_layout);
                    let child_list = if is_astro_template_element {
                        child_list.with_astro_force_multiline()
                    } else {
                        child_list
                    };
                    let format_children = child_list.fmt_children(children, f);

                    match format_children {
                        FormatChildrenResult::SingleChild(child) => {
                            write!(f, group(&format_args!(format_opening, child, format_closing)));
                        }
                        FormatChildrenResult::ForceMultiline(multiline) => {
                            write!(f, [format_opening, multiline, format_closing]);
                        }
                        FormatChildrenResult::BestFitting { flat_children, expanded_children } => {
                            let format_closing = format_closing.memoized();
                            write!(
                                f,
                                [best_fitting![
                                    format_args!(format_opening, flat_children, format_closing),
                                    format_args!(format_opening, expanded_children, format_closing)
                                ]]
                            );
                        }
                    }
                }
            }
        });

        // It's a nested JSX element or fragment, no need for parenthesis or wrapping.
        if matches!(self.parent(), AstNodes::JSXElement(_) | AstNodes::JSXFragment(_)) {
            return write!(f, [format_tag]);
        }

        let wrap = self.get_wrap_state();
        match wrap {
            WrapState::NoWrap => {
                write!(
                    f,
                    [
                        &format_with(|f| { self.format_leading_comments(f) }),
                        format_tag,
                        &format_with(|f| { self.format_trailing_comments(f) }),
                    ]
                );
            }
            WrapState::WrapOnBreak => {
                let should_expand = should_expand(self.parent());
                let needs_parentheses = self.needs_parentheses(f);

                let format_inner = format_with(|f| {
                    if !needs_parentheses {
                        write!(f, [if_group_breaks(&token("("))]);
                    }

                    write!(
                        f,
                        [soft_block_indent(&format_args!(
                            &format_with(|f| { self.format_leading_comments(f) }),
                            format_tag,
                            &format_with(|f| { self.format_trailing_comments(f) }),
                        ))]
                    );

                    if !needs_parentheses {
                        write!(f, [if_group_breaks(&token(")"))]);
                    }
                });

                write!(f, [group(&format_inner).should_expand(should_expand)]);
            }
        }
    }
}

/// This is a very special situation where we're returning a JsxElement
/// from an arrow function that's passed as an argument to a function,
/// which is itself inside a JSX expression container.
///
/// This matches Prettier's `shouldBreakJsxElement` behavior.
///
/// ```jsx
/// // As JSX child:
/// let bar = <div>
///   {foo(() => <div> the quick brown fox jumps over the lazy dog </div>)}
/// </div>;
///
/// // As JSX attribute:
/// <Tooltip title={[].map(name => (<Foo>{name}</Foo>))} />;
/// ```
pub fn should_expand(mut parent: &AstNodes<'_>) -> bool {
    if let AstNodes::ExpressionStatement(stmt) = parent {
        // If the parent is a JSXExpressionContainer, we need to check its parent
        // to determine if it should expand.
        parent = stmt.grand_parent();
    }
    let maybe_jsx_expression_container = match parent {
        AstNodes::ArrowFunctionExpression(arrow) if arrow.expression => match arrow.parent() {
            AstNodes::CallExpression(call) => call.parent(),
            _ => return false,
        },
        _ => return false,
    };
    matches!(
        maybe_jsx_expression_container.without_chain_expression(),
        AstNodes::JSXExpressionContainer(_)
    )
}

impl<'a, 'b> AnyJsxTagWithChildren<'a, 'b> {
    fn fmt_opening(&self, f: &mut Formatter<'_, 'a>) {
        match self {
            Self::Element(element) => {
                let is_self_closing = element.closing_element().is_none();
                let opening_formatter =
                    FormatOpeningElement::new(element.opening_element(), is_self_closing);
                write!(f, opening_formatter);
            }
            Self::Fragment(fragment) => {
                write!(f, fragment.opening_fragment());
            }
        }
    }

    /// Like `fmt_opening` but forces the tag to be rendered as self-closing (`<Tag />`).
    /// Used when the element has no meaningful children (NoChildren layout).
    fn fmt_opening_self_closing(&self, f: &mut Formatter<'_, 'a>) {
        match self {
            Self::Element(element) => {
                let opening_formatter =
                    FormatOpeningElement::new(element.opening_element(), true);
                write!(f, opening_formatter);
            }
            Self::Fragment(fragment) => {
                // Fragments always use `<>` / `</>`, can't be self-closing.
                write!(f, fragment.opening_fragment());
            }
        }
    }

    fn fmt_closing(&self, f: &mut Formatter<'_, 'a>) {
        match self {
            Self::Element(element) => {
                write!(f, element.closing_element());
            }
            Self::Fragment(fragment) => {
                write!(f, fragment.closing_fragment());
            }
        }
    }

    fn children(&self) -> &'b AstNode<'a, Vec<'a, JSXChild<'a>>> {
        match self {
            Self::Element(element) => element.children(),
            Self::Fragment(fragment) => fragment.children(),
        }
    }

    fn parent(&self) -> &'b AstNodes<'a> {
        match self {
            Self::Element(element) => element.parent(),
            Self::Fragment(fragment) => fragment.parent(),
        }
    }

    fn needs_parentheses(&self, f: &Formatter<'_, 'a>) -> bool {
        match self {
            Self::Element(element) => element.needs_parentheses(f),
            Self::Fragment(fragment) => fragment.needs_parentheses(f),
        }
    }

    fn layout(&self, f: &Formatter<'_, 'a>) -> ElementLayout<'a, 'b> {
        let children = self.children();
        let is_astro = f.context().source_type().is_astro();

        match children.len() {
            0 => {
                // Element has no children in the source.
                // In Astro, we self-close UNLESS the element has spread attributes
                // (e.g. `<div {...spread}></div>` → keep explicit `<div {...spread}></div>`).
                // Elements with spread attributes must keep their explicit open+close tags
                // because the spread might provide children at runtime.
                //
                // Without spread: `<h1 set:html={content}></h1>` → `<h1 set:html={content} />`
                // With spread: `<div {...spread}></div>` → `<div {...spread}></div>`
                if is_astro {
                    let has_spread = match self {
                        Self::Element(element) => element
                            .opening_element
                            .attributes
                            .iter()
                            .any(|attr| matches!(attr, JSXAttributeItem::SpreadAttribute(_))),
                        Self::Fragment(_) => false,
                    };
                    if has_spread {
                        ElementLayout::ExplicitEmpty
                    } else {
                        ElementLayout::NoChildren
                    }
                } else {
                    ElementLayout::NoChildren
                }
            }
            1 => {
                // Safe because of length check above
                let child = children.first().unwrap();

                match child.as_ast_nodes() {
                    AstNodes::JSXText(text) => {
                        if is_astro {
                            // In Astro files, elements whose only child is pure whitespace
                            // are handled differently depending on whether the whitespace
                            // contains newlines:
                            //
                            // - Spaces/tabs only (no newlines): self-close → `<slot />`
                            //   e.g. `<slot>   </slot>` → `<slot />`
                            //
                            // - Newlines only (including whitespace with newlines): keep
                            //   explicit open+close with a single space between tags
                            //   e.g. `<custom-element>\n\n\n</custom-element>`
                            //        → `<custom-element> </custom-element>`
                            //
                            // - Contains non-whitespace: normal Default layout
                            let raw = text.value.as_str();
                            if raw.bytes().all(|b| matches!(b, b' ' | b'\t')) {
                                // Space/tab only — self-close
                                ElementLayout::NoChildren
                            } else if astro_is_whitespace_only_text(raw) {
                                // All whitespace but contains newlines → single space
                                ElementLayout::AstroSingleSpace
                            } else {
                                ElementLayout::Default
                            }
                        } else {
                            if is_meaningful_jsx_text(&text.value) {
                                ElementLayout::Default
                            } else {
                                ElementLayout::NoChildren
                            }
                        }
                    }
                    AstNodes::JSXExpressionContainer(expression) => match &expression.expression {
                        JSXExpression::TemplateLiteral(_) => ElementLayout::Template(expression),
                        JSXExpression::TaggedTemplateExpression(_) => {
                            ElementLayout::Template(expression)
                        }
                        _ => ElementLayout::Default,
                    },
                    _ => ElementLayout::Default,
                }
            }
            _ => ElementLayout::Default,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ElementLayout<'a, 'b> {
    /// Whitespace-only text child (spaces/tabs, no newlines) ��� self-closes in Astro.
    /// Also used in regular JSX for elements with no meaningful text content.
    ///
    /// In Astro: `<slot>   </slot>` → `<slot />`
    NoChildren,

    /// Element has zero children in the source (`<div></div>`).
    ///
    /// In Astro, keep the explicit open+close tags (do not auto-self-close).
    /// In regular JSX this is treated the same as `NoChildren`.
    ExplicitEmpty,

    /// Astro-only: the element's only child was newline-only whitespace.
    ///
    /// Prettier's Astro plugin formats these as `<elem> </elem>` — explicit
    /// open+close with a single space between tags.
    ///
    /// Example: `<custom-element>\n\n\n</custom-element>` → `<custom-element> </custom-element>`
    AstroSingleSpace,

    /// Prefer breaking the template if it is the only child of the element
    /// ```javascript
    /// <div>{`A Long Template String That uses ${
    ///   5 + 4
    /// } that will eventually break across multiple lines ${(40 / 3) * 45}`}</div>;
    /// ```
    ///
    /// instead of
    ///
    /// ```javascript
    /// <div>
    ///   {`A Long Template String That uses ${
    ///     5 + 4
    ///   } that will eventually break across multiple lines ${(40 / 3) * 45}`}
    /// </div>;
    /// ```
    Template(&'b AstNode<'a, JSXExpressionContainer<'a>>),

    /// Default layout used for all elements that have children and [ElementLayout::Template] does not apply.
    ///
    /// ```javascript
    /// <Element2>
    ///   Some more content
    ///   <Sub />
    ///   <Sub />
    ///   <Sub />
    /// </Element2>;
    /// ```
    Default,
}
