//! Script parsing for Astro files.
//!
//! This module handles parsing TypeScript content inside `<script>` tags
//! in Astro files. Scripts are parsed after the initial JSX parsing pass.

use oxc_allocator::Allocator;
use oxc_ast::ast::{Argument, Expression, JSXChild, JSXExpression, Statement};
use oxc_diagnostics::OxcDiagnostic;
use oxc_span::SourceType;

use crate::{ParseOptions, ParserImpl, config::NoTokensParserConfig, parser_parse::UniquePromise};

/// Parse script content in AstroScript nodes.
/// This traverses the body and replaces placeholder programs with parsed TypeScript.
pub fn parse_astro_scripts<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    body: &mut oxc_allocator::Vec<'a, JSXChild<'a>>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    for child in body.iter_mut() {
        parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
    }
}

/// Recursively parse scripts in a JSX child and its descendants.
fn parse_scripts_in_child<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    child: &mut JSXChild<'a>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    match child {
        JSXChild::AstroScript(script) => {
            // The script's program span contains the script content span
            let content_span = script.program.span;
            if content_span.start < content_span.end {
                // To get correct span offsets, we prepend whitespace to push the content
                // to its actual position in the original file.
                let script_content =
                    &source_text[content_span.start as usize..content_span.end as usize];
                let padding = " ".repeat(content_span.start as usize);
                let padded_source = allocator.alloc_str(&format!("{padding}{script_content}"));

                let ts_source_type =
                    SourceType::ts().with_module(true).with_jsx(source_type.is_jsx());

                // Create a new parser for the script content
                let unique = UniquePromise::new_for_astro();
                let parser = ParserImpl::new(
                    allocator,
                    padded_source,
                    ts_source_type,
                    options,
                    NoTokensParserConfig,
                    unique,
                );
                let result = parser.parse();

                // Replace the placeholder program with the parsed one
                script.program = result.program;
                errors.extend(result.errors);
            }
        }
        JSXChild::Element(element) => {
            // Recurse into element children
            for child in &mut element.children {
                parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
            }
        }
        JSXChild::Fragment(fragment) => {
            // Recurse into fragment children
            for child in &mut fragment.children {
                parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
            }
        }
        JSXChild::ExpressionContainer(container) => {
            // Recurse into expressions - they may contain JSXElement with AstroScript children
            parse_scripts_in_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut container.expression,
                errors,
            );
        }
        JSXChild::Text(_)
        | JSXChild::Spread(_)
        | JSXChild::AstroDoctype(_)
        | JSXChild::AstroComment(_) => {
            // No scripts to parse in these
        }
    }
}

/// Recursively parse scripts in a JSX expression.
/// Handles cases like `{condition && <script>...</script>}`.
fn parse_scripts_in_expression<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    expr: &mut JSXExpression<'a>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    match expr {
        JSXExpression::JSXElement(element) => {
            for child in &mut element.children {
                parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
            }
        }
        JSXExpression::JSXFragment(fragment) => {
            for child in &mut fragment.children {
                parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
            }
        }
        // For complex expressions, we'd need to recurse into their sub-expressions
        // to find any JSX elements they might contain. For now, handle common cases:
        JSXExpression::ParenthesizedExpression(paren) => {
            if let Expression::JSXElement(elem) = &mut paren.expression {
                for child in &mut elem.children {
                    parse_scripts_in_child(
                        allocator,
                        source_text,
                        source_type,
                        options,
                        child,
                        errors,
                    );
                }
            } else if let Expression::JSXFragment(frag) = &mut paren.expression {
                for child in &mut frag.children {
                    parse_scripts_in_child(
                        allocator,
                        source_text,
                        source_type,
                        options,
                        child,
                        errors,
                    );
                }
            }
        }
        JSXExpression::LogicalExpression(logical) => {
            // Handle `{condition && <script>...}` or `{a || <script>...}`
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut logical.left,
                errors,
            );
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut logical.right,
                errors,
            );
        }
        JSXExpression::ConditionalExpression(cond) => {
            // Handle `{condition ? <script>... : <other>...}`
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut cond.consequent,
                errors,
            );
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut cond.alternate,
                errors,
            );
        }
        JSXExpression::CallExpression(call) => {
            // Handle `{items.map(item => <script>...)}` and similar patterns
            for arg in &mut call.arguments {
                parse_scripts_in_argument(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    arg,
                    errors,
                );
            }
        }
        JSXExpression::ArrowFunctionExpression(arrow) => {
            // Handle arrow functions that return JSX with scripts
            parse_scripts_in_function_body(
                allocator,
                source_text,
                source_type,
                options,
                &mut arrow.body,
                errors,
            );
        }
        JSXExpression::FunctionExpression(func) => {
            // Handle regular function expressions
            if let Some(body) = &mut func.body {
                parse_scripts_in_function_body(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    body,
                    errors,
                );
            }
        }
        // Other expression types - skip for now, scripts are unlikely in these
        _ => {}
    }
}

/// Helper to parse scripts in an inner Expression (not JSXExpression)
fn parse_scripts_in_inner_expression<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    expr: &mut Expression<'a>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    match expr {
        Expression::JSXElement(element) => {
            for child in &mut element.children {
                parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
            }
        }
        Expression::JSXFragment(fragment) => {
            for child in &mut fragment.children {
                parse_scripts_in_child(allocator, source_text, source_type, options, child, errors);
            }
        }
        Expression::ParenthesizedExpression(paren) => {
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut paren.expression,
                errors,
            );
        }
        Expression::LogicalExpression(logical) => {
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut logical.left,
                errors,
            );
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut logical.right,
                errors,
            );
        }
        Expression::ConditionalExpression(cond) => {
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut cond.consequent,
                errors,
            );
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut cond.alternate,
                errors,
            );
        }
        Expression::CallExpression(call) => {
            // Handle `{items.map(item => <script>...)}` and similar patterns
            for arg in &mut call.arguments {
                parse_scripts_in_argument(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    arg,
                    errors,
                );
            }
        }
        Expression::ArrowFunctionExpression(arrow) => {
            // Handle arrow function bodies that may return JSX with scripts
            parse_scripts_in_function_body(
                allocator,
                source_text,
                source_type,
                options,
                &mut arrow.body,
                errors,
            );
        }
        Expression::FunctionExpression(func) => {
            // Handle regular function expressions that may return JSX with scripts
            if let Some(body) = &mut func.body {
                parse_scripts_in_function_body(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    body,
                    errors,
                );
            }
        }
        Expression::SequenceExpression(seq) => {
            // Handle `(expr1, expr2)` - the last expression is the return value
            for expr in &mut seq.expressions {
                parse_scripts_in_inner_expression(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    expr,
                    errors,
                );
            }
        }
        // Other expression types - skip for now
        _ => {}
    }
}

/// Helper to parse scripts in a function argument (handles Argument enum)
fn parse_scripts_in_argument<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    arg: &mut Argument<'a>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    match arg {
        Argument::ArrowFunctionExpression(arrow) => {
            parse_scripts_in_function_body(
                allocator,
                source_text,
                source_type,
                options,
                &mut arrow.body,
                errors,
            );
        }
        Argument::FunctionExpression(func) => {
            if let Some(body) = &mut func.body {
                parse_scripts_in_function_body(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    body,
                    errors,
                );
            }
        }
        // Handle spread element
        Argument::SpreadElement(spread) => {
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut spread.argument,
                errors,
            );
        }
        // For other arguments that are expressions, check if they contain JSX
        _ => {
            if let Some(expr) = arg.as_expression_mut() {
                parse_scripts_in_inner_expression(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    expr,
                    errors,
                );
            }
        }
    }
}

/// Helper to parse scripts in a function body
fn parse_scripts_in_function_body<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    body: &mut oxc_ast::ast::FunctionBody<'a>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    for stmt in &mut body.statements {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                parse_scripts_in_inner_expression(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    &mut expr_stmt.expression,
                    errors,
                );
            }
            Statement::ReturnStatement(ret) => {
                if let Some(argument) = &mut ret.argument {
                    parse_scripts_in_inner_expression(
                        allocator,
                        source_text,
                        source_type,
                        options,
                        argument,
                        errors,
                    );
                }
            }
            Statement::BlockStatement(block) => {
                // Recursively handle nested blocks
                parse_scripts_in_statements(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    &mut block.body,
                    errors,
                );
            }
            Statement::IfStatement(if_stmt) => {
                parse_scripts_in_statement(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    &mut if_stmt.consequent,
                    errors,
                );
                if let Some(alternate) = &mut if_stmt.alternate {
                    parse_scripts_in_statement(
                        allocator,
                        source_text,
                        source_type,
                        options,
                        alternate,
                        errors,
                    );
                }
            }
            _ => {}
        }
    }
}

/// Helper to parse scripts in a slice of statements
fn parse_scripts_in_statements<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    statements: &mut oxc_allocator::Vec<'a, Statement<'a>>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    for stmt in statements.iter_mut() {
        parse_scripts_in_statement(allocator, source_text, source_type, options, stmt, errors);
    }
}

/// Helper to parse scripts in a single statement
fn parse_scripts_in_statement<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
    stmt: &mut Statement<'a>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    match stmt {
        Statement::ExpressionStatement(expr_stmt) => {
            parse_scripts_in_inner_expression(
                allocator,
                source_text,
                source_type,
                options,
                &mut expr_stmt.expression,
                errors,
            );
        }
        Statement::ReturnStatement(ret) => {
            if let Some(argument) = &mut ret.argument {
                parse_scripts_in_inner_expression(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    argument,
                    errors,
                );
            }
        }
        Statement::BlockStatement(block) => {
            parse_scripts_in_statements(
                allocator,
                source_text,
                source_type,
                options,
                &mut block.body,
                errors,
            );
        }
        Statement::IfStatement(if_stmt) => {
            parse_scripts_in_statement(
                allocator,
                source_text,
                source_type,
                options,
                &mut if_stmt.consequent,
                errors,
            );
            if let Some(alternate) = &mut if_stmt.alternate {
                parse_scripts_in_statement(
                    allocator,
                    source_text,
                    source_type,
                    options,
                    alternate,
                    errors,
                );
            }
        }
        _ => {}
    }
}
