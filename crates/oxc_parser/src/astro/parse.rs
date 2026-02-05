//! Main entry point for Astro file parsing.

use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_diagnostics::OxcDiagnostic;
use oxc_span::{SourceType, Span};

use crate::{ParseOptions, ParserImpl, parser_parse::UniquePromise};

use super::parse_astro_scripts;

/// Return value of [`parse_astro`] for Astro files.
///
/// ## AST Validity
///
/// [`root`] will always contain a structurally valid AST, even if there are syntax errors.
///
/// [`root`]: AstroParserReturn::root
#[non_exhaustive]
pub struct AstroParserReturn<'a> {
    /// The parsed Astro AST.
    ///
    /// Contains the frontmatter (TypeScript code) and HTML body.
    pub root: oxc_ast::ast::AstroRoot<'a>,

    /// Syntax errors encountered while parsing.
    pub errors: Vec<OxcDiagnostic>,

    /// Whether the parser panicked and terminated early.
    pub panicked: bool,
}

/// Information about frontmatter boundaries
struct AstroFrontmatterInfo {
    /// Start of frontmatter content (after opening `---`)
    content_start: usize,
    /// End of frontmatter content (before closing `---`)
    content_end: usize,
    /// End of entire frontmatter section (after closing `---`)
    frontmatter_end: usize,
    /// Start of body (after frontmatter, skipping optional newline)
    body_start: usize,
}

/// Scan the source text to find frontmatter boundaries without parsing.
///
/// According to the Astro spec:
/// - Content may appear before the opening fence (customarily ignored)
/// - Whitespace may appear before the opening fence
/// - Opening/closing fences don't need to be on their own line
/// - Code can appear on the same line as fences
fn scan_astro_frontmatter(source_text: &str) -> Option<AstroFrontmatterInfo> {
    // Find the opening `---` fence
    // According to spec: any content may appear before the fence (ignored),
    // and whitespace may appear before it
    let opening_fence_start = source_text.find("---")?;

    // Content starts immediately after the opening `---`
    let content_start = opening_fence_start + 3;

    // Find the closing `---` fence
    // It can appear anywhere after the opening fence - doesn't need to be on its own line
    // We need to find `---` that is NOT the opening fence
    let search_area = &source_text[content_start..];

    // Find the closing `---` - it can be:
    // 1. On its own line (most common)
    // 2. Immediately after content on same line
    // 3. At the start of a line with content after it
    //
    // We look for `---` that marks the end of frontmatter
    let closing_fence_pos = search_area.find("---")?;

    let content_end = content_start + closing_fence_pos;
    let frontmatter_end = content_end + 3;

    // Calculate where the body starts (after closing `---`)
    // Skip optional newline after closing fence
    let mut body_start = frontmatter_end;
    if source_text.get(body_start..).is_some_and(|s| s.starts_with('\n')) {
        body_start += 1;
    } else if source_text.get(body_start..).is_some_and(|s| s.starts_with("\r\n")) {
        body_start += 2;
    }

    Some(AstroFrontmatterInfo { content_start, content_end, frontmatter_end, body_start })
}

/// Parse an Astro file.
///
/// This is the main entry point for Astro parsing, called from `Parser::parse_astro`.
pub fn parse_astro<'a>(
    allocator: &'a Allocator,
    source_text: &'a str,
    source_type: SourceType,
    options: ParseOptions,
) -> AstroParserReturn<'a> {
    // First, scan for frontmatter boundaries
    let frontmatter_info = scan_astro_frontmatter(source_text);

    // Parse the body JSX
    let unique = UniquePromise::new_for_astro();
    let parser = ParserImpl::new(allocator, source_text, source_type, options, unique);
    let (mut body, body_errors, body_panicked) =
        parser.parse_astro_body_only(frontmatter_info.as_ref().map(|f| f.body_start));

    // Parse the frontmatter TypeScript if present
    // If no frontmatter, create a synthetic empty one so semantic analysis has a Program root
    let (frontmatter, frontmatter_errors) = if let Some(info) = frontmatter_info {
        // To get correct span offsets, we prepend whitespace to push the content
        // to its actual position in the original file. This way the lexer produces
        // spans that match the original source positions.
        let frontmatter_content = &source_text[info.content_start..info.content_end];
        let padding = " ".repeat(info.content_start);
        let padded_source = allocator.alloc_str(&format!("{padding}{frontmatter_content}"));

        let ts_source_type = SourceType::ts().with_module(true).with_jsx(source_type.is_jsx());

        // Create a new parser for the frontmatter content
        // Enable allow_return_outside_function for Astro frontmatter per spec §2.1
        let frontmatter_options = ParseOptions { allow_return_outside_function: true, ..options };
        let unique = UniquePromise::new_for_astro();
        let parser =
            ParserImpl::new(allocator, padded_source, ts_source_type, frontmatter_options, unique);
        let result = parser.parse();

        let ast = AstBuilder::new(allocator);
        #[expect(clippy::cast_possible_truncation)]
        let frontmatter =
            ast.alloc_astro_frontmatter(Span::new(0, info.frontmatter_end as u32), result.program);
        (Some(frontmatter), result.errors)
    } else {
        // No frontmatter - create a synthetic empty frontmatter with empty Program
        // This is needed so semantic analysis has a Program root node
        let ast = AstBuilder::new(allocator);
        let ts_source_type = SourceType::ts().with_module(true).with_jsx(true);
        let empty_program = ast.program(
            Span::new(0, 0),
            ts_source_type,
            source_text,
            ast.vec(), // comments
            None,      // hashbang
            ast.vec(), // directives
            ast.vec(), // body
        );
        let frontmatter = ast.alloc_astro_frontmatter(Span::new(0, 0), empty_program);
        (Some(frontmatter), vec![])
    };

    // Parse script content in the body
    // Traverse all JSXChild::AstroScript nodes and parse their content
    let mut script_errors = Vec::new();
    parse_astro_scripts(
        allocator,
        source_text,
        source_type,
        options,
        &mut body,
        &mut script_errors,
    );

    // Combine errors
    let mut errors =
        Vec::with_capacity(frontmatter_errors.len() + body_errors.len() + script_errors.len());
    errors.extend(frontmatter_errors);
    errors.extend(body_errors);
    errors.extend(script_errors);

    // Build the root
    let ast = AstBuilder::new(allocator);
    #[expect(clippy::cast_possible_truncation)]
    let span = Span::new(0, source_text.len() as u32);
    let root = ast.astro_root(span, frontmatter, body);

    AstroParserReturn { root, errors, panicked: body_panicked }
}
