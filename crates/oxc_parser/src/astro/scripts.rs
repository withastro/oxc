//! Script parsing for Astro files.
//!
//! This module handles parsing TypeScript content inside `<script>` tags
//! in Astro files. Scripts are parsed after the initial JSX parsing pass.

use oxc_allocator::Allocator;
use oxc_ast::ast::{AstroScript, JSXChild};
use oxc_ast_visit::VisitMut;
use oxc_diagnostics::OxcDiagnostic;
use oxc_span::SourceType;

use crate::{ParseOptions, ParserImpl, config::NoTokensParserConfig, parser_parse::UniquePromise};

/// Parse script content in AstroScript nodes.
///
/// This traverses the whole body, including expressions, attributes and spreads at any
/// depth, and replaces each placeholder program with the parsed TypeScript.
pub fn parse_astro_scripts<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    _source_type: SourceType,
    options: ParseOptions,
    body: &mut oxc_allocator::Vec<'a, JSXChild<'a>>,
    errors: &mut Vec<OxcDiagnostic>,
) {
    let mut parser = AstroScriptParser { allocator, source_text, options, errors };
    for child in body.iter_mut() {
        parser.visit_jsx_child(child);
    }
}

struct AstroScriptParser<'a, 'e> {
    allocator: &'a Allocator,
    source_text: &'a str,
    options: ParseOptions,
    errors: &'e mut Vec<OxcDiagnostic>,
}

impl<'a> VisitMut<'a> for AstroScriptParser<'a, '_> {
    fn visit_astro_script(&mut self, script: &mut AstroScript<'a>) {
        // The placeholder program's span is the script content's span.
        // Don't walk into the program: script content can't contain more scripts.
        let content_span = script.program.span;
        if content_span.start >= content_span.end {
            return;
        }

        // To get correct span offsets, we prepend whitespace to push the content
        // to its actual position in the original file.
        let script_content =
            &self.source_text[content_span.start as usize..content_span.end as usize];
        let padding = " ".repeat(content_span.start as usize);
        let padded_source = self.allocator.alloc_str(&format!("{padding}{script_content}"));

        // TS, not TSX: `<T>expr` in a bare `<script>` is a type assertion, not a tag.
        let ts_source_type = SourceType::ts().with_module(true);

        let unique = UniquePromise::new_for_astro();
        let parser = ParserImpl::new(
            self.allocator,
            padded_source,
            ts_source_type,
            self.options,
            NoTokensParserConfig,
            unique,
        );
        let result = parser.parse();

        // Replace the placeholder program with the parsed one
        script.program = result.program;
        self.errors.extend(result.errors);
    }
}
