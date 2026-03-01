use oxc_allocator::Vec;
use oxc_ast::ast::*;

pub mod child_list;
pub mod element;
pub mod opening_element;

use child_list::{FormatChildrenResult, FormatJsxChildList, JsxChildListLayout};
use element::AnyJsxTagWithChildren;
use opening_element::FormatOpeningElement;
use oxc_span::GetSpan;

use crate::{
    AttributePosition, Format,
    ast_nodes::{AstNode, AstNodes},
    format_args,
    formatter::{
        TailwindContextEntry,
        prelude::*,
        trivia::{DanglingIndentMode, FormatDanglingComments, FormatTrailingComments},
    },
    print::astro::astro_spread_is_shorthand,
    utils::tailwindcss::is_tailwind_jsx_attribute,
    write,
};

use super::FormatWrite;

impl<'a> FormatWrite<'a> for AstNode<'a, JSXElement<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        AnyJsxTagWithChildren::Element(self).fmt(f);
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXOpeningElement<'a>> {
    fn write(&self, _f: &mut Formatter<'_, 'a>) {
        unreachable!("`AnyJsxTagWithChildren` will print it.")
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXClosingElement<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let name = self.name();
        let mut name_has_leading_comment = false;
        let mut name_has_own_line_leading_comment = false;
        for leading_comment in f.comments().comments_before(name.span().start) {
            name_has_leading_comment = true;
            name_has_own_line_leading_comment =
                name_has_own_line_leading_comment || leading_comment.is_line();
        }

        let format_name = format_with(|f| {
            if name_has_own_line_leading_comment {
                write!(f, [hard_line_break()]);
            } else if name_has_leading_comment {
                write!(f, [space()]);
            }
            if name_has_own_line_leading_comment {
                write!(f, [block_indent(&name), hard_line_break()]);
            } else {
                write!(f, [name]);
            }
        });

        write!(f, ["</", &format_name, ">",]);
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXFragment<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        AnyJsxTagWithChildren::Fragment(self).fmt(f);
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXOpeningFragment> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let comments = f.context().comments().comments_before(self.span.end);

        if comments.is_empty() {
            write!(f, "<>");
            return;
        }

        let has_own_line_comment = comments.iter().any(|c| c.is_line());

        let format_comments = format_with(|f| {
            if has_own_line_comment {
                write!(f, [hard_line_break()]);
            }

            write!(
                f,
                [FormatDanglingComments::Comments { comments, indent: DanglingIndentMode::None }]
            );
        });

        write!(
            f,
            ["<", indent(&format_comments), has_own_line_comment.then_some(hard_line_break()), ">"]
        );
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXClosingFragment> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let comments = f.context().comments().comments_before(self.span.end);

        if comments.is_empty() {
            write!(f, "</>");
            return;
        }

        let has_own_line_comment = comments.iter().any(|c| c.is_line());

        let format_comments = format_with(|f| {
            if has_own_line_comment {
                write!(f, [hard_line_break()]);
            } else if !comments.is_empty() {
                write!(f, [space()]);
            }

            write!(
                f,
                [FormatDanglingComments::Comments { comments, indent: DanglingIndentMode::None }]
            );
        });

        write!(
            f,
            [
                "</",
                indent(&format_comments),
                has_own_line_comment.then_some(hard_line_break()),
                ">"
            ]
        );
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXNamespacedName<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        write!(f, [self.namespace(), ":", self.name()]);
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXMemberExpression<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        write!(f, [self.object(), ".", self.property()]);
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXExpressionContainer<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let has_comment = |f: &mut Formatter<'_, '_>| {
            let expression_span = self.expression.span();
            f.comments().has_comment_before(expression_span.start)
                || f.comments().has_comment_in_range(expression_span.end, self.span.end)
        };

        // Expression child (also includes Astro top-level: children of AstroRoot)
        if matches!(
            self.parent(),
            AstNodes::JSXElement(_) | AstNodes::JSXFragment(_) | AstNodes::AstroRoot(_)
        ) {
            if let JSXExpression::EmptyExpression(_) = self.expression {
                let comments = f.context().comments().comments_before(self.span.end);
                // Use block-indent when any comment requires its own line:
                // - Line comments (`// ...`) always need a hard line break after.
                // - In Astro files, multiline block comments (`/* ...\n... */`) also
                //   get block-indent (Astro-specific Prettier behavior).
                // - In regular JSX, multiline block comments stay inline.
                // Single-line block comments (`/* inline */`) always stay inline:
                // `{/* Hello */}`.
                let is_astro = f.context().source_type().is_astro();
                let needs_block_indent = comments.iter().any(|c| {
                    c.is_line() || (is_astro && c.is_multiline_block())
                });

                write!(f, ["{"]);

                if needs_block_indent {
                    write!(
                        f,
                        [FormatDanglingComments::Comments {
                            comments,
                            indent: DanglingIndentMode::Block
                        },]
                    );
                } else {
                    write!(
                        f,
                        [FormatDanglingComments::Comments {
                            comments,
                            indent: DanglingIndentMode::None
                        },]
                    );
                }

                write!(f, ["}"]);
            } else {
                let is_conditional_or_binary = matches!(
                    self.expression,
                    JSXExpression::ConditionalExpression(_)
                        | JSXExpression::LogicalExpression(_)
                        | JSXExpression::BinaryExpression(_)
                );

                // In Astro files, JSX expression containers that are children of
                // an element/fragment always use `soft_block_indent`. This means
                // short expressions like `{hello}` stay on one line (soft breaks
                // collapse), while complex expressions like `{numbers.map(…)}`
                // that contain internal hard breaks expand to the block form:
                //   `{\n  numbers.map(…)\n}`
                //
                // Standard JSX uses `should_inline` to decide whether to wrap —
                // returning `true` for CallExpression, ArrowFunctionExpression, etc.
                // In Astro those "inline" expressions still need the soft_block_indent
                // so that the `{` and `}` move to their own lines when the content breaks.
                let should_inline = !has_comment(f)
                    && !f.context().source_type().is_astro()
                    && (is_conditional_or_binary || should_inline_jsx_expression(self));

                let format_expression = format_with(|f| {
                    if should_inline {
                        write!(f, self.expression());
                    } else {
                        write!(
                            f,
                            soft_block_indent(&format_with(|f| {
                                write!(f, [self.expression()]);
                                let comments =
                                    f.context().comments().comments_before(self.span.end);
                                write!(f, [FormatTrailingComments::Comments(comments)]);
                            }))
                        );
                    }
                });

                write!(
                    f,
                    [group(&format_args!("{", format_expression, line_suffix_boundary(), "}"))]
                );
            }
        } else {
            // JSXAttributeValue
            let should_inline = !has_comment(f) && should_inline_jsx_expression(self);

            if should_inline {
                write!(f, ["{", self.expression(), line_suffix_boundary(), "}"]);
            } else {
                write!(
                    f,
                    [group(&format_args!(
                        "{",
                        soft_block_indent(&format_with(|f| {
                            write!(f, [self.expression()]);
                            let comments = f.context().comments().comments_before(self.span.end);
                            write!(f, [FormatTrailingComments::Comments(comments)]);
                        })),
                        line_suffix_boundary(),
                        "}"
                    ))]
                );
            }
        }
    }
}

/// Tests if an expression inside of a [`JSXExpressionContainer`] should be inlined.
/// Good:
/// ```jsx
///  <ColorPickerPage
///     colors={[
///        "blue",
///        "brown",
///        "green",
///        "orange",
///        "purple",
///     ]} />
/// ```
///
/// Bad:
/// ```jsx
///  <ColorPickerPage
///     colors={
///       [
///         "blue",
///          "brown",
///         "green",
///         "orange",
///         "purple",
///       ]
///     } />
/// ```
pub fn should_inline_jsx_expression(container: &JSXExpressionContainer<'_>) -> bool {
    match &container.expression {
        JSXExpression::ArrayExpression(_)
        | JSXExpression::ObjectExpression(_)
        | JSXExpression::ArrowFunctionExpression(_)
        | JSXExpression::CallExpression(_)
        | JSXExpression::ImportExpression(_)
        | JSXExpression::MetaProperty(_)
        | JSXExpression::FunctionExpression(_)
        | JSXExpression::TemplateLiteral(_)
        | JSXExpression::TaggedTemplateExpression(_) => true,
        JSXExpression::ChainExpression(chain_expression) => {
            matches!(chain_expression.expression, ChainElement::CallExpression(_))
        }
        JSXExpression::AwaitExpression(await_expression) => {
            matches!(
                await_expression.argument,
                Expression::ArrayExpression(_)
                    | Expression::ObjectExpression(_)
                    | Expression::ArrowFunctionExpression(_)
                    | Expression::CallExpression(_)
                    | Expression::ImportExpression(_)
                    | Expression::MetaProperty(_)
                    | Expression::FunctionExpression(_)
                    | Expression::TemplateLiteral(_)
                    | Expression::TaggedTemplateExpression(_)
                    | Expression::JSXElement(_)
                    | Expression::JSXFragment(_)
            )
        }
        _ => false,
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXEmptyExpression> {
    fn write(&self, _f: &mut Formatter<'_, 'a>) {}
}

impl<'a> Format<'a> for AstNode<'a, Vec<'a, JSXAttributeItem<'a>>> {
    fn fmt(&self, f: &mut Formatter<'_, 'a>) {
        let line_break = if f.options().attribute_position == AttributePosition::Multiline {
            hard_line_break()
        } else {
            soft_line_break_or_space()
        };

        f.join_with(&line_break).entries(self.iter());
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXAttribute<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        // In Astro, `{prop}` shorthand attributes are parsed as a regular
        // `JSXAttribute` (name=`prop`, value=`{prop}`) but the span of the
        // attribute itself starts *inside* the braces (i.e. at the identifier,
        // not at `{`).  Detect this by peeking at the character just before the
        // span start; if it is `{`, the attribute was written as a shorthand and
        // we must re-emit it as `{name}` (without the `name=` prefix).
        if f.context().source_type().is_astro()
            && self.span.start > 0
            && f.source_text()
                .slice_range(self.span.start - 1, self.span.start)
                == "{"
        {
            // Re-emit as Astro shorthand: `{name}` (the closing `}` is part of
            // the value ExpressionContainer, so we emit `{name}` directly and
            // skip the normal `name={value}` form).
            if let JSXAttributeName::Identifier(ident) = &self.name {
                write!(f, ["{", text(ident.name.as_str()), "}"]);
                return;
            }
        }

        write!(f, self.name());

        if let Some(value) = &self.value() {
            // Check if this is a Tailwind attribute and push context
            // Extract context entry before mutating f
            let tailwind_ctx_to_push = f
                .options()
                .sort_tailwindcss
                .as_ref()
                .filter(|opts| is_tailwind_jsx_attribute(&self.name, opts))
                .map(|opts| TailwindContextEntry::new(opts.preserve_whitespace));

            if let Some(ctx) = tailwind_ctx_to_push {
                f.context_mut().push_tailwind_context(ctx);
            }

            write!(f, ["=", value]);

            if tailwind_ctx_to_push.is_some() {
                f.context_mut().pop_tailwind_context();
            }
        }
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXSpreadAttribute<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let comments = f.context().comments();
        let has_comment = comments.has_comment_before(self.argument.span().start)
            || comments.has_comment_in_range(self.argument.span().end, self.span.end);
        // In Astro, `{expr}` (without `...`) is a shorthand attribute; preserve
        // the no-dots form. For regular JSX and real spreads always print `...`.
        let source = f.source_text().slice_range(self.span.start, self.span.end);
        let is_astro_shorthand =
            f.context().source_type().is_astro() && astro_spread_is_shorthand(self.span, source);
        let format_inner = format_with(|f| {
            write!(f, format_leading_comments(self.argument.span()));
            if !is_astro_shorthand {
                write!(f, "...");
            }
            self.argument().fmt(f);
        });

        write!(f, ["{"]);

        if has_comment {
            write!(f, [soft_block_indent(&format_inner)]);
        } else {
            write!(f, [format_inner]);
        }

        write!(f, "}");
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXIdentifier<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        write!(f, text_without_whitespace(self.name().as_str()));
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXSpreadChild<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        let comments = f.context().comments();
        let has_comment = comments.has_comment_before(self.expression.span().start)
            || comments.has_comment_in_range(self.expression.span().end, self.span.end);
        let format_inner = format_with(|f| {
            write!(f, [format_leading_comments(self.expression.span()), "..."]);
            self.expression().fmt(f);
        });

        write!(f, "{");

        if has_comment {
            write!(f, [soft_block_indent(&format_inner)]);
        } else {
            write!(f, [format_inner]);
        }

        write!(f, "}");
    }
}

impl<'a> FormatWrite<'a> for AstNode<'a, JSXText<'a>> {
    fn write(&self, f: &mut Formatter<'_, 'a>) {
        write!(f, text(self.value().as_str()));
    }
}
