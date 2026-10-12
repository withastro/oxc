//! Main entry point for Astro file parsing.

use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_diagnostics::OxcDiagnostic;
use oxc_span::{SourceType, Span};

use crate::{ParseOptions, ParserImpl, config::NoTokensParserConfig, parser_parse::UniquePromise};

use super::parse_astro_scripts;

/// Return value for parsing Astro files.
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

    /// JS-style comments encountered in the template body (expression containers, JSX
    /// attributes, etc.).  These are **not** stored on any AST node — the body parser's
    /// trivia builder is the only place they live.  Spans are absolute byte offsets into
    /// the original source text, so they can be merged directly with frontmatter and
    /// `<script>`-block comments before being fed to `DisableDirectivesBuilder`.
    pub body_comments: Vec<oxc_ast::ast::Comment>,
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
///
/// Following the behavior of the official Astro compiler (`@astrojs/compiler`):
/// if an HTML element (`<`), expression (`{`), or closing expression (`}`) appears
/// before the first `---`, the file is considered to have **no frontmatter**.
/// This prevents `---` inside `<style>`, `<script>`, or other body content from
/// being misinterpreted as a frontmatter fence.
fn scan_astro_frontmatter(source_text: &str) -> Option<AstroFrontmatterInfo> {
    // Find the opening `---` fence, but only if no HTML element or expression
    // delimiter appears before it. Per the official Astro compiler, encountering
    // `<`, `{`, or `}` before `---` means there is no frontmatter.
    let opening_fence_start = find_opening_fence(source_text)?;

    // Content starts immediately after the opening `---`
    let content_start = opening_fence_start + 3;

    // Find the closing `---` fence that is NOT inside a string, template literal, or comment
    let closing_fence_pos = find_closing_fence(&source_text[content_start..])?;

    let content_end = content_start + closing_fence_pos;
    let frontmatter_end = content_end + 3;

    // Calculate where the body starts (after closing `---`)
    // Skip optional newline after closing fence
    let mut body_start = frontmatter_end;
    let after_fence = source_text.get(body_start..).unwrap_or_default();
    if after_fence.starts_with("\r\n") {
        body_start += 2;
    } else if after_fence.starts_with('\n') || after_fence.starts_with('\r') {
        body_start += 1;
    }

    Some(AstroFrontmatterInfo { content_start, content_end, frontmatter_end, body_start })
}

/// Find the position of the opening `---` fence.
///
/// Scans from the start of the source text. If an HTML element delimiter (`<`),
/// expression start (`{`), or expression end (`}`) is encountered before `---`,
/// the file is considered to have no frontmatter.
fn find_opening_fence(source_text: &str) -> Option<usize> {
    let bytes = source_text.as_bytes();
    let len = bytes.len();
    let mut i = 0;
    let mut dash_count: u32 = 0;

    while i < len {
        let b = bytes[i];
        match b {
            b'-' => {
                dash_count += 1;
                if dash_count == 3 {
                    // Found `---` — this is the opening fence
                    return Some(i + 1 - 3);
                }
                i += 1;
                continue;
            }
            // HTML element or expression delimiter before `---` → no frontmatter
            b'<' | b'{' | b'}' => return None,
            _ => {}
        }
        dash_count = 0;
        i += 1;
    }

    None
}

/// Find the position of the closing `---` fence, skipping over strings, template literals,
/// comments, and regex literals.
///
/// This uses a simple state machine to track whether we're inside:
/// - Single-quoted strings ('...')
/// - Double-quoted strings ("...")
/// - Template literals (`...`)
/// - Line comments (// ...)
/// - Block comments (/* ... */)
/// - Regex literals (/.../)
///
/// Returns the byte offset of the closing `---` in the search area, or None if not found.
fn find_closing_fence(search_area: &str) -> Option<usize> {
    #[derive(Clone, Copy, PartialEq)]
    enum State {
        Normal,
        SingleQuote,
        DoubleQuote,
        TemplateLiteral,
        TemplateExpression,
        LineComment,
        BlockComment,
        Regex,
        /// Inside a character class `[...]` within a regex literal.
        RegexCharClass,
    }

    let bytes = search_area.as_bytes();
    let len = bytes.len();
    let mut state = State::Normal;
    // State to resume after a string, comment, or regex encountered in code.
    let mut return_state = State::Normal;
    // One entry per open template literal. Zero means template text; a positive
    // value is the brace depth of the current `${...}` expression.
    let mut template_stack: Vec<u32> = Vec::new();
    let mut i = 0;

    // Whether a `/` in code starts a regex literal or is division.
    let mut regex = RegexContext::default();

    while i < len {
        let b = bytes[i];

        match state {
            State::Normal => {
                // Check for `---` closing fence
                if b == b'-' && i + 2 < len && bytes[i + 1] == b'-' && bytes[i + 2] == b'-' {
                    return Some(i);
                }

                // Check for string/comment/regex starts
                match b {
                    b'\'' => {
                        return_state = State::Normal;
                        state = State::SingleQuote;
                        regex.set(true);
                    }
                    b'"' => {
                        return_state = State::Normal;
                        state = State::DoubleQuote;
                        regex.set(true);
                    }
                    b'`' => {
                        template_stack.push(0);
                        state = State::TemplateLiteral;
                        regex.set(true);
                    }
                    b'/' if i + 1 < len && bytes[i + 1] == b'/' => {
                        return_state = State::Normal;
                        state = State::LineComment;
                        i += 1; // Skip the second '/'
                    }
                    b'/' if i + 1 < len && bytes[i + 1] == b'*' => {
                        return_state = State::Normal;
                        state = State::BlockComment;
                        i += 1; // Skip the '*'
                    }
                    b'/' if regex.slash_is_regex => {
                        return_state = State::Normal;
                        state = State::Regex;
                    }
                    _ => i = regex.code_byte(bytes, i),
                }
            }

            State::SingleQuote => {
                if b == b'\\' && i + 1 < len {
                    // Skip escaped character
                    i += 1;
                } else if b == b'\'' {
                    state = return_state;
                    regex.set(false);
                } else if b == b'\n' || b == b'\r' {
                    // Newline ends single-quote string (syntax error in JS, but we recover)
                    state = return_state;
                    regex.set(true);
                }
            }

            State::DoubleQuote => {
                if b == b'\\' && i + 1 < len {
                    // Skip escaped character
                    i += 1;
                } else if b == b'"' {
                    state = return_state;
                    regex.set(false);
                } else if b == b'\n' || b == b'\r' {
                    // Newline ends double-quote string (syntax error in JS, but we recover)
                    state = return_state;
                    regex.set(true);
                }
            }

            State::TemplateLiteral => {
                if b == b'\\' && i + 1 < len {
                    // Skip escaped character
                    i += 1;
                } else if b == b'$' && i + 1 < len && bytes[i + 1] == b'{' {
                    *template_stack.last_mut().unwrap() = 1;
                    state = State::TemplateExpression;
                    regex.set(true);
                    i += 1; // Skip the '{'
                } else if b == b'`' {
                    template_stack.pop();
                    state = if template_stack.is_empty() {
                        State::Normal
                    } else {
                        State::TemplateExpression
                    };
                    regex.set(false);
                }
                // Template literals can span multiple lines.
            }

            State::TemplateExpression => match b {
                b'\'' => {
                    return_state = State::TemplateExpression;
                    state = State::SingleQuote;
                    regex.set(true);
                }
                b'"' => {
                    return_state = State::TemplateExpression;
                    state = State::DoubleQuote;
                    regex.set(true);
                }
                b'`' => {
                    template_stack.push(0);
                    state = State::TemplateLiteral;
                    regex.set(true);
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'/' => {
                    return_state = State::TemplateExpression;
                    state = State::LineComment;
                    i += 1;
                }
                b'/' if i + 1 < len && bytes[i + 1] == b'*' => {
                    return_state = State::TemplateExpression;
                    state = State::BlockComment;
                    i += 1;
                }
                b'/' if regex.slash_is_regex => {
                    return_state = State::TemplateExpression;
                    state = State::Regex;
                }
                b'{' => {
                    *template_stack.last_mut().unwrap() += 1;
                    regex.code_byte(bytes, i);
                }
                b'}' => {
                    let depth = template_stack.last_mut().unwrap();
                    *depth -= 1;
                    if *depth == 0 {
                        state = State::TemplateLiteral;
                        regex.set(false);
                    } else {
                        regex.code_byte(bytes, i);
                    }
                }
                _ => i = regex.code_byte(bytes, i),
            },

            State::LineComment => {
                // Missing a terminator runs the comment to EOF and silently drops the frontmatter.
                // Like a block comment, a line comment preserves the context before it.
                if b == b'\n' || b == b'\r' {
                    state = return_state;
                } else if b == 0xE2
                    && i + 2 < len
                    && bytes[i + 1] == 0x80
                    && matches!(bytes[i + 2], 0xA8 | 0xA9)
                {
                    state = return_state;
                    i += 2;
                }
            }

            State::BlockComment => {
                // Block comment ends at */
                if b == b'*' && i + 1 < len && bytes[i + 1] == b'/' {
                    state = return_state;
                    i += 1; // Skip the '/'
                    // Don't change the regex context - preserve the context from before the comment
                }
            }

            State::Regex => {
                if b == b'\\' && i + 1 < len {
                    // Skip escaped character in regex
                    i += 1;
                } else if b == b'[' {
                    // Enter character class - `/` inside `[...]` doesn't end the regex
                    state = State::RegexCharClass;
                } else if b == b'/' {
                    // End of regex literal - skip optional flags (g, i, m, s, u, v, y, d)
                    state = return_state;
                    regex.set(false);
                    while i + 1 < len && bytes[i + 1].is_ascii_alphabetic() {
                        i += 1;
                    }
                } else if b == b'\n' || b == b'\r' {
                    // Regex can't span lines - this was actually a division, recover
                    state = return_state;
                    regex.set(true);
                }
            }

            State::RegexCharClass => {
                if b == b'\\' && i + 1 < len {
                    // Skip escaped character in character class
                    i += 1;
                } else if b == b']' {
                    // End of character class, back to regex body
                    state = State::Regex;
                } else if b == b'\n' || b == b'\r' {
                    // Regex can't span lines - recover
                    state = return_state;
                    regex.set(true);
                }
            }
        }

        i += 1;
    }

    None
}

/// Tracks just enough JavaScript token context to tell whether a `/` starts a regex
/// literal or is the division operator, for [`find_closing_fence`].
///
/// A `/` starts a regex where an expression may begin: after most punctuation and
/// operators, after keywords such as `return` or `typeof`, and after the `)` that
/// closes an `if (…)`, `while (…)`, `for (…)` or `with (…)` head. It is division
/// after an identifier, literal, `]`, or any other `)`.
struct RegexContext {
    /// `true` when a `/` here would start a regex literal.
    slash_is_regex: bool,
    /// `true` right after an `if`/`while`/`for`/`with` keyword, so a following `(`
    /// opens a statement head.
    after_control_keyword: bool,
    /// `true` right after `.`, where a keyword is a property name.
    after_dot: bool,
    /// One entry per open `(`: whether its `)` ends a statement head.
    parens: Vec<bool>,
}

impl Default for RegexContext {
    fn default() -> Self {
        Self {
            slash_is_regex: true,
            after_control_keyword: false,
            after_dot: false,
            parens: vec![],
        }
    }
}

impl RegexContext {
    /// Record a token (string, template, regex, …) that leaves `slash_is_regex`.
    fn set(&mut self, slash_is_regex: bool) {
        self.slash_is_regex = slash_is_regex;
        self.after_control_keyword = false;
        self.after_dot = false;
    }

    /// Consume the code byte at `i`, which does not start a string, template,
    /// comment or regex. Returns the index of the last byte consumed: a whole word
    /// is consumed at once so keywords can be recognized.
    fn code_byte(&mut self, bytes: &[u8], i: usize) -> usize {
        let b = bytes[i];
        match b {
            // Whitespace and line breaks don't change the context.
            b' ' | b'\t' | b'\n' | b'\r' => i,
            b'(' => {
                self.parens.push(self.after_control_keyword);
                self.set(true);
                i
            }
            b')' => {
                let ends_statement_head = self.parens.pop().unwrap_or(false);
                self.set(ends_statement_head);
                i
            }
            b']' => {
                self.set(false);
                i
            }
            b'.' => {
                // A digit after `.` starts a number (`.5`); handled as a word below.
                if bytes.get(i + 1).is_some_and(u8::is_ascii_digit) {
                    return self.word(bytes, i);
                }
                self.set(true);
                self.after_dot = true;
                i
            }
            b if is_identifier_byte(b) => self.word(bytes, i),
            // Any other punctuation or operator.
            _ => {
                self.set(true);
                i
            }
        }
    }

    /// Consume the identifier, keyword or number starting at `i`.
    fn word(&mut self, bytes: &[u8], i: usize) -> usize {
        let mut end = i + 1;
        while end < bytes.len()
            && (is_identifier_byte(bytes[end]) || bytes[end] == b'.' && bytes[i].is_ascii_digit())
        {
            end += 1;
        }
        let word = &bytes[i..end];
        let is_keyword = !self.after_dot;
        let precedes_expression = is_keyword
            && matches!(
                word,
                b"return"
                    | b"typeof"
                    | b"instanceof"
                    | b"in"
                    | b"of"
                    | b"new"
                    | b"delete"
                    | b"void"
                    | b"throw"
                    | b"case"
                    | b"do"
                    | b"else"
                    | b"yield"
                    | b"await"
            );
        let is_control = is_keyword && matches!(word, b"if" | b"while" | b"for" | b"with");
        self.set(precedes_expression || is_control);
        self.after_control_keyword = is_control;
        end - 1
    }
}

/// Whether `b` can be part of an identifier, keyword or number. Non-ASCII bytes are
/// treated as identifier bytes, so a multi-byte identifier is consumed as one word.
fn is_identifier_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_' || b == b'$' || !b.is_ascii()
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
    let parser =
        ParserImpl::new(allocator, source_text, source_type, options, NoTokensParserConfig, unique);
    let (mut body, body_errors, body_panicked, body_comments) =
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

        // TS, not TSX: `<T>expr` in the component script is a type assertion, not a tag.
        let ts_source_type = SourceType::ts().with_module(true);

        // Create a new parser for the frontmatter content
        // Enable allow_return_outside_function for Astro frontmatter per spec §2.1
        let frontmatter_options = ParseOptions { allow_return_outside_function: true, ..options };
        let unique = UniquePromise::new_for_astro();
        let parser = ParserImpl::new(
            allocator,
            padded_source,
            ts_source_type,
            frontmatter_options,
            NoTokensParserConfig,
            unique,
        );
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
        let ts_source_type = SourceType::ts().with_module(true);
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

    AstroParserReturn { root, errors, panicked: body_panicked, body_comments }
}

#[cfg(test)]
mod tests {
    use oxc_allocator::Allocator;
    use oxc_span::SourceType;

    use super::{parse_astro, scan_astro_frontmatter};
    use crate::ParseOptions;

    fn assert_frontmatter_body(source: &str, expected_body: &str) {
        let info = scan_astro_frontmatter(source).expect("frontmatter should be found");
        assert_eq!(&source[info.body_start..], expected_body);
    }

    #[test]
    fn finds_fence_after_nested_template_literals() {
        let source = r#"---
const value = `<div>${condition ? "" : `<span>${title}</span>`}</div>`;
---
<div>{value}</div>"#;

        assert_frontmatter_body(source, "<div>{value}</div>");
    }

    #[test]
    fn tracks_javascript_context_inside_template_interpolations() {
        let source = r#"---
const value = `outer ${(() => {
    const object = { nested: { value: "}" } };
    const pattern = /[{}]/;
    // A backtick and fence in a comment must not close anything: ` ---
    /* Neither should these: ` } --- */
    return `inner ${object.nested.value}`;
})()}`;
---
<p>{value}</p>"#;

        assert_frontmatter_body(source, "<p>{value}</p>");
    }

    #[test]
    fn handles_template_literal_edge_cases() {
        let cases = [
            (
                "escaped template characters",
                r#"---
const value = `escaped \` fence --- and \${notAnInterpolation}`;
---
<p>{value}</p>"#,
                "<p>{value}</p>",
            ),
            (
                "deep and sibling interpolations",
                r#"---
const value = `outer ${`middle ${`inner`}`} ${one} ${two}`;
---
<p>{value}</p>"#,
                "<p>{value}</p>",
            ),
            (
                "division and regex",
                r#"---
const value = `${total / divisor} ${/---/.test(text)}`;
---
<p>{value}</p>"#,
                "<p>{value}</p>",
            ),
            (
                "same-line fences",
                r#"---const value = `outer ${`inner`}`;---<p>{value}</p>"#,
                "<p>{value}</p>",
            ),
            (
                "CRLF line endings",
                "---\r\nconst value = `outer ${`inner`}`;\r\n---\r\n<p>{value}</p>",
                "<p>{value}</p>",
            ),
        ];

        for (name, source, expected_body) in cases {
            let info = scan_astro_frontmatter(source)
                .unwrap_or_else(|| panic!("frontmatter should be found for {name}"));
            assert_eq!(&source[info.body_start..], expected_body, "{name}");
        }
    }

    #[test]
    fn rejects_unclosed_nested_template_literal() {
        let source = r#"---
const value = `outer ${`unterminated`;
---
<p>{value}</p>"#;

        assert!(scan_astro_frontmatter(source).is_none());
    }

    #[test]
    fn parses_nested_template_literals_in_frontmatter() {
        let allocator = Allocator::default();
        let source = r#"---
let hideTitle = false;
let title = "Hello";
let srcdoc = `<div>${hideTitle ? "" : `<span>${title}</span>`}</div>`;
---
<div>{srcdoc}</div>"#;

        let ret = parse_astro(&allocator, source, SourceType::astro(), ParseOptions::default());

        assert!(!ret.panicked);
        assert!(ret.errors.is_empty(), "errors: {:?}", ret.errors);
        let frontmatter = ret.root.frontmatter.as_ref().unwrap();
        assert_eq!(frontmatter.program.body.len(), 3);
        assert!(!ret.root.body.is_empty());
    }

    #[test]
    fn regex_literals_containing_fences_do_not_end_frontmatter() {
        let cases = [
            "function f() { return /---/.test(x); }",
            "if (x) /---/.test(x);",
            "while (ok) /---/.exec(s)",
            "for (const s of list) /---/.test(s);",
            "if (a && (b || c)) /---/.test(x);",
            "const t = typeof /---/;",
            "const t = x instanceof /---/.constructor;",
            "function* g() { yield /---/; }",
            "async function f() { await /---/; }",
            "if (x) {} else /---/.test(x);",
            "throw /---/;",
            "switch (x) { case /---/.source: break; }",
            "const r = /[/]---/;",
            "const v = `${ (() => { if (x) return /---/; })() }`;",
            "return /---/;",
        ];
        for code in cases {
            let source = format!("---\n{code}\n---\n<p/>");
            let info = scan_astro_frontmatter(&source)
                .unwrap_or_else(|| panic!("frontmatter should be found for {code:?}"));
            assert_eq!(&source[info.body_start..], "<p/>", "{code:?}");
        }
    }

    #[test]
    fn division_is_not_mistaken_for_regex_in_frontmatter() {
        // If any of these `/` were read as a regex start, the scanner would skip the
        // fence (or the string) after it.
        let cases = [
            "const a = b / c; const d = e / f; // ---",
            "const a = (b) / 2; const c = 1 / 2 /* --- */;",
            "const a = x.return / 2 / 3;",
            "const a = 1.5 / 2; const b = arr[0] / 2; const c = 'x' / 2;",
            "const a = f(x) / 2 + `${(y) / 2}`;",
            "const a = b\n  / c / d;",
            "const a = b // comment\n  / c / d;",
            "const a = b /* comment */ / c / d;",
            "const a = 10 / 2; const b = \"---\";",
        ];
        for code in cases {
            let source = format!("---\n{code}\n---\n<p/>");
            let info = scan_astro_frontmatter(&source)
                .unwrap_or_else(|| panic!("frontmatter should be found for {code:?}"));
            assert_eq!(&source[info.body_start..], "<p/>", "{code:?}");
        }
    }

    #[test]
    fn parses_frontmatter_with_regex_after_return() {
        let allocator = Allocator::default();
        let source =
            "---\nfunction f() { return /---/.test(x); }\nif (x) /---/.test(x);\n---\n<p/>";
        let ret = parse_astro(&allocator, source, SourceType::astro(), ParseOptions::default());
        assert!(ret.errors.is_empty(), "errors: {:?}", ret.errors);
        let frontmatter = ret.root.frontmatter.as_ref().unwrap();
        assert_eq!(frontmatter.program.body.len(), 2);
        assert_eq!(frontmatter.span.end as usize, source.len() - "\n<p/>".len());
        assert_eq!(ret.root.body.len(), 1);
    }
}
