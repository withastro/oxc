#![expect(clippy::print_stdout)]

//! Conformance test runner for Astro file formatting.
//!
//! Reads `input.astro` + optional `options.json` from each fixture directory,
//! formats with `oxc_formatter::Formatter::format_astro`, and compares against
//! `output.astro`.  Produces a Markdown summary at
//! `tasks/prettier_conformance/snapshots/prettier.astro.snap.md`.

use std::{
    fmt::Write as _,
    path::{Path, PathBuf},
};

use oxc_allocator::Allocator;
use oxc_formatter::{FormatOptions, Formatter, get_parse_options};
use oxc_parser::Parser;
use oxc_span::SourceType;
use similar::TextDiff;

// ---------------------------------------------------------------------------
// Fixtures that are intentionally skipped (unsupported features).
// ---------------------------------------------------------------------------

/// Fixture paths (relative to the `astro/` fixtures root) that are skipped
/// because they require features not yet implemented:
///
/// * Style/CSS/SASS/SCSS formatting
/// * Markdown embedding
/// * Astro-specific options (`astroSortOrder`, `astroAllowShorthand`, etc.)
/// * `htmlWhitespaceSensitivity` option
/// * `proseWrap` option
/// * Cursor-position formatting
/// * Embedded `<script>` reformatting
/// * `singleAttributePerLine` / `quoteProps` options
const SKIP_FIXTURES: &[&str] = &[
    // --- Style tag formatting (CSS/SCSS/SASS/Less/unknown) ---
    "styles/",
    // --- Markdown embedding ---
    "markdown/",
    // --- Astro-specific options ---
    "options/option-astro-",
    // --- htmlWhitespaceSensitivity ---
    "options/option-html-whitespace-sensitivity",
    // --- proseWrap ---
    "options/option-prose-wrap",
    // --- cursor position ---
    "other/format-with-cursor-position",
    // --- embedded <script> reformatting ---
    "other/with-script",
    "other/script-types",
    // --- singleAttributePerLine ---
    "options/single-attribute-per-line",
    // --- quoteProps ---
    "options/option-quote-props",
    // --- printWidth (Prettier default is 80, oxfmt default is 100 – skip for now) ---
    "options/option-print-width",
    // --- bracketSameLine with htmlWhitespaceSensitivity ---
    "options/option-bracket-same-line-html-true-whitespace-sensitivity-ignore",
    // --- errors fixtures (parse errors, not formatting) ---
    "errors/",
    // --- unclosed tag (parse error) ---
    "other/unclosed-tag",
    // --- CSS/style formatting ---
    "basic/single-style-element",
];

fn astro_fixtures_root() -> PathBuf {
    oxc_tasks_common::project_root().join("tasks").join("prettier_conformance").join("astro")
}

fn snap_root() -> PathBuf {
    oxc_tasks_common::project_root().join("tasks").join("prettier_conformance").join("snapshots")
}

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

pub struct AstroTestRunner {
    pub filter: Option<String>,
    pub debug: bool,
}

impl AstroTestRunner {
    pub fn new(filter: Option<String>, debug: bool) -> Self {
        Self { filter, debug }
    }

    /// Collect every `input.astro` file that is not in a skipped category.
    fn collect_fixtures(&self) -> Vec<PathBuf> {
        let root = astro_fixtures_root();
        let mut fixtures: Vec<PathBuf> = walkdir::WalkDir::new(&root)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|e| e.file_name() == "input.astro")
            .map(|e| e.into_path())
            .filter(|p| {
                let rel = p.strip_prefix(&root).unwrap().to_string_lossy();
                !SKIP_FIXTURES.iter().any(|skip| rel.contains(skip))
            })
            .filter(|p| {
                self.filter.as_ref().map_or(true, |f| p.to_string_lossy().contains(f.as_str()))
            })
            .collect();
        fixtures.sort_unstable();
        fixtures
    }

    /// # Panics
    pub fn run(&self) {
        let fixtures = self.collect_fixtures();
        let root = astro_fixtures_root();

        if self.debug || self.filter.is_some() {
            for path in &fixtures {
                let rel = path.strip_prefix(&root).unwrap();
                let fixture_dir = path.parent().unwrap();
                let options = parse_options(fixture_dir);
                println!("Fixture: {}", rel.display());

                match run_formatter(path, &options) {
                    None => println!("  => Skipped (parse error)"),
                    Some(actual) => {
                        let expected = read_output(fixture_dir);
                        let passed = expected == actual;
                        if passed {
                            println!("  => Passed ✅");
                        } else {
                            println!("  => Failed ❌");
                            let diff = TextDiff::from_lines(&expected, &actual);
                            oxc_tasks_common::print_text_diff(&diff);
                        }
                    }
                }
                println!();
            }
            return;
        }

        let mut passed_count = 0usize;
        let mut total_count = 0usize;
        let mut failed_report = String::new();
        failed_report.push_str("# Failed\n\n");
        failed_report.push_str("| Fixture | Status |\n");
        failed_report.push_str("| :------ | :----: |\n");

        for path in &fixtures {
            total_count += 1;
            let rel = path.strip_prefix(&root).unwrap().with_file_name(""); // dir part
            let fixture_dir = path.parent().unwrap();
            let options = parse_options(fixture_dir);
            let expected = read_output(fixture_dir);

            let passed = match run_formatter(path, &options) {
                None => false, // parse error �� fail
                Some(actual) => expected == actual,
            };

            if passed {
                passed_count += 1;
            } else {
                writeln!(failed_report, "| {} | �� |", rel.display()).unwrap();
            }
        }

        #[expect(clippy::cast_precision_loss)]
        let pct = if total_count == 0 {
            100.0_f64
        } else {
            (passed_count as f64 / total_count as f64) * 100.0
        };
        let summary = format!("astro compatibility: {passed_count}/{total_count} ({pct:.2}%)");
        println!("{summary}");

        let snapshot = format!("{summary}\n\n{failed_report}");
        std::fs::create_dir_all(snap_root()).unwrap();
        std::fs::write(snap_root().join("prettier.astro.snap.md"), snapshot).unwrap();
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Read and normalise the expected `output.astro` next to the input.
fn read_output(fixture_dir: &Path) -> String {
    let output_path = fixture_dir.join("output.astro");
    let raw = std::fs::read_to_string(&output_path)
        .unwrap_or_else(|_| panic!("Missing output.astro in {}", fixture_dir.display()));
    // Normalise line endings.
    raw.replace("\r\n", "\n").replace('\r', "\n")
}

/// Parse `options.json` in the fixture directory (if present) into
/// `FormatOptions`.  Unknown / unsupported keys are silently ignored.
fn parse_options(fixture_dir: &Path) -> FormatOptions {
    let opts_path = fixture_dir.join("options.json");
    let mut options = FormatOptions::new();
    // Prettier default printWidth is 80; match it here.
    options.line_width = 80_u16.try_into().unwrap();

    let Ok(content) = std::fs::read_to_string(&opts_path) else {
        return options;
    };

    // Simple hand-rolled JSON key/value extraction – avoids adding a JSON
    // dependency just for the conformance binary.
    for (key, value) in extract_json_kv(&content) {
        match key.as_str() {
            "semi" => {
                options.semicolons = if value == "true" {
                    oxc_formatter::Semicolons::Always
                } else {
                    oxc_formatter::Semicolons::AsNeeded
                };
            }
            "singleQuote" => {
                if value == "true" {
                    options.quote_style = oxc_formatter::QuoteStyle::Single;
                } else {
                    options.quote_style = oxc_formatter::QuoteStyle::Double;
                }
            }
            "jsxSingleQuote" => {
                if value == "true" {
                    options.jsx_quote_style = oxc_formatter::QuoteStyle::Single;
                } else {
                    options.jsx_quote_style = oxc_formatter::QuoteStyle::Double;
                }
            }
            "trailingComma" => {
                options.trailing_commas = match value.trim_matches('"') {
                    "all" => oxc_formatter::TrailingCommas::All,
                    "es5" => oxc_formatter::TrailingCommas::Es5,
                    "none" => oxc_formatter::TrailingCommas::None,
                    _ => oxc_formatter::TrailingCommas::Es5,
                };
            }
            "bracketSpacing" => {
                options.bracket_spacing = oxc_formatter::BracketSpacing::from(value == "true");
            }
            "arrowParens" => {
                options.arrow_parentheses = match value.trim_matches('"') {
                    "avoid" => oxc_formatter::ArrowParentheses::AsNeeded,
                    _ => oxc_formatter::ArrowParentheses::Always,
                };
            }
            "useTabs" => {
                if value == "true" {
                    options.indent_style = oxc_formatter::IndentStyle::Tab;
                } else {
                    options.indent_style = oxc_formatter::IndentStyle::Space;
                }
            }
            "tabWidth" => {
                if let Ok(w) = value.trim().parse::<u8>() {
                    if let Ok(iw) = oxc_formatter::IndentWidth::try_from(w) {
                        options.indent_width = iw;
                    }
                }
            }
            "printWidth" => {
                if let Ok(w) = value.trim().parse::<u16>() {
                    if let Ok(lw) = oxc_formatter::LineWidth::try_from(w) {
                        options.line_width = lw;
                    }
                }
            }
            "bracketSameLine" => {
                options.bracket_same_line = oxc_formatter::BracketSameLine::from(value == "true");
            }
            // Astro-specific / unsupported options are silently ignored.
            _ => {}
        }
    }

    options
}

/// Very small JSON key/value extractor for flat objects.
/// Returns pairs of (key, value) where both are unquoted strings or raw bools/numbers.
fn extract_json_kv(json: &str) -> Vec<(String, String)> {
    let mut pairs = Vec::new();
    // Strip outer braces.
    let inner = json.trim().trim_start_matches('{').trim_end_matches('}');
    for chunk in inner.split(',') {
        let chunk = chunk.trim();
        if chunk.is_empty() {
            continue;
        }
        let Some(colon) = chunk.find(':') else { continue };
        let raw_key = chunk[..colon].trim().trim_matches('"').to_string();
        let raw_val = chunk[colon + 1..].trim().to_string();
        pairs.push((raw_key, raw_val));
    }
    pairs
}

/// Format `input.astro` with the given options.  Returns `None` if parsing
/// fails (the test is considered a skip/failure in that case).
fn run_formatter(input_path: &Path, options: &FormatOptions) -> Option<String> {
    let source_text = std::fs::read_to_string(input_path).unwrap();
    // Normalise line endings in the input too.
    let source_text = source_text.replace("\r\n", "\n").replace('\r', "\n");

    let allocator = Allocator::default();

    let ret = Parser::new(&allocator, &source_text, SourceType::astro())
        .with_options(get_parse_options())
        .parse_astro();

    if !ret.errors.is_empty() {
        return None;
    }

    // Merge frontmatter + body comments, sorted by span start.
    let mut all_comments: Vec<oxc_ast::Comment> = Vec::new();
    if let Some(fm) = &ret.root.frontmatter {
        all_comments.extend(fm.program.comments.iter().copied());
    }
    all_comments.extend(ret.body_comments.iter().copied());
    all_comments.sort_unstable_by_key(|c| c.span.start);
    let comments = allocator.alloc_slice_copy(&all_comments);

    let formatted = Formatter::new(&allocator, options.clone()).format_astro(
        &ret.root,
        &source_text,
        comments,
        None,
    );

    let result = formatted.print().ok()?;
    Some(result.into_code())
}
