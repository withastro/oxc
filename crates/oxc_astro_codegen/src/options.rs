//! Options for Astro codegen.
//!
//! These options mirror the Go Astro compiler's `TransformOptions` from
//! `@astrojs/compiler`. Fields that require runtime callbacks or CSS processing
//! are accepted but stubbed (they exist for API compatibility).

/// Scoped style strategy for CSS scoping.
///
/// Determines how Astro scopes CSS selectors to components.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ScopedStyleStrategy {
    /// Use `:where(.astro-XXXX)` selector (default).
    #[default]
    Where,
    /// Use `.astro-XXXX` class selector.
    Class,
    /// Use `[data-astro-cid-XXXX]` attribute selector.
    Attribute,
}

/// Options for Astro code generation.
///
/// Matches the Go compiler's `TransformOptions` shape from `@astrojs/compiler`.
#[derive(Debug, Clone)]
pub struct TransformOptions {
    /// The filename of the Astro component being compiled.
    /// Used in `$$createComponent` for debugging and scope hash computation.
    pub filename: Option<String>,

    /// A normalized version of the filename used for scope hash generation.
    /// If not provided, falls back to `filename`.
    pub normalized_filename: Option<String>,

    /// The import specifier for Astro runtime functions.
    /// Defaults to `"astro/runtime/server/index.js"`.
    pub internal_url: Option<String>,

    /// Whether to generate a source map.
    ///
    /// Accepts `true`/`false`. In the Go compiler this also accepts
    /// `"inline"`, `"external"`, `"both"` — we currently only support
    /// a boolean (any truthy value enables source map generation).
    ///
    /// **Stub**: source map generation is not yet implemented; this field
    /// is accepted for API compatibility.
    pub sourcemap: bool,

    /// Arguments passed to `$$createAstro` when the Astro global is used.
    /// Defaults to `"https://astro.build"`.
    pub astro_global_args: Option<String>,

    /// Whether to collapse whitespace in the HTML output.
    ///
    /// **Stub**: compact mode is not yet implemented; this field is accepted
    /// for API compatibility.
    pub compact: bool,

    /// Enable scoped slot result handling.
    ///
    /// When `true`, enables the `resultScopedSlot` behavior from the Go compiler.
    ///
    /// **Stub**: not yet implemented; accepted for API compatibility.
    pub result_scoped_slot: bool,

    /// Strategy for CSS scoping.
    ///
    /// **Stub**: CSS scoping is not yet implemented in the Rust compiler;
    /// this is accepted for API compatibility. The value will be used once
    /// CSS scoping support is added.
    pub scoped_style_strategy: ScopedStyleStrategy,

    /// URL for the view transitions animation CSS.
    /// Defaults to `"astro/components/viewtransitions.css"` in the Go compiler.
    ///
    /// **Stub**: accepted for API compatibility.
    pub transitions_animation_url: Option<String>,

    /// Whether to annotate generated code with the source file path.
    ///
    /// **Stub**: accepted for API compatibility.
    pub annotate_source_file: bool,

    /// Whether to render processed script tags using `$$renderScript`.
    ///
    /// When `true`, script tags that would normally be hoisted are instead
    /// rendered via `$$renderScript` from `internalURL`.
    pub render_script: bool,

    /// Enable experimental script ordering behavior.
    ///
    /// **Stub**: accepted for API compatibility.
    pub experimental_script_order: bool,

    // --- oxc-specific options (not in the Go compiler) ---
    /// Whether to include the `$$metadata` export.
    /// The Go compiler always includes metadata; this option lets you skip it.
    ///
    /// When `true`, the generated code includes `$$metadata` and `$$createMetadata`.
    /// When `false` (default), metadata is omitted for smaller output.
    pub include_metadata: bool,

    /// Whether to strip HTML comments from component slot children.
    ///
    /// When `true` (default), HTML comments inside component children are not
    /// included in slot content. This matches the Go compiler behavior which
    /// explicitly excludes `CommentNode` from slots.
    ///
    /// When `false`, HTML comments are preserved in slot content.
    ///
    /// This only affects comments inside component children (slots), not comments
    /// in regular HTML elements which are always preserved.
    pub strip_slot_comments: bool,
}

impl Default for TransformOptions {
    fn default() -> Self {
        Self {
            filename: None,
            normalized_filename: None,
            internal_url: None,
            sourcemap: false,
            astro_global_args: None,
            compact: false,
            result_scoped_slot: false,
            scoped_style_strategy: ScopedStyleStrategy::default(),
            transitions_animation_url: None,
            annotate_source_file: false,
            render_script: false,
            experimental_script_order: false,
            include_metadata: false,
            strip_slot_comments: true, // Match Go compiler behavior by default
        }
    }
}

impl TransformOptions {
    /// Create new options with default values.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the filename.
    #[must_use]
    pub fn with_filename(mut self, filename: impl Into<String>) -> Self {
        self.filename = Some(filename.into());
        self
    }

    /// Set the normalized filename for scope hash generation.
    #[must_use]
    pub fn with_normalized_filename(mut self, filename: impl Into<String>) -> Self {
        self.normalized_filename = Some(filename.into());
        self
    }

    /// Set the internal URL for Astro runtime imports.
    #[must_use]
    pub fn with_internal_url(mut self, url: impl Into<String>) -> Self {
        self.internal_url = Some(url.into());
        self
    }

    /// Enable or disable source map generation (stub).
    #[must_use]
    pub fn with_sourcemap(mut self, enabled: bool) -> Self {
        self.sourcemap = enabled;
        self
    }

    /// Set the Astro global arguments.
    #[must_use]
    pub fn with_astro_global_args(mut self, args: impl Into<String>) -> Self {
        self.astro_global_args = Some(args.into());
        self
    }

    /// Enable or disable compact mode (stub).
    #[must_use]
    pub fn with_compact(mut self, compact: bool) -> Self {
        self.compact = compact;
        self
    }

    /// Enable or disable scoped slot result handling (stub).
    #[must_use]
    pub fn with_result_scoped_slot(mut self, enabled: bool) -> Self {
        self.result_scoped_slot = enabled;
        self
    }

    /// Set the scoped style strategy (stub).
    #[must_use]
    pub fn with_scoped_style_strategy(mut self, strategy: ScopedStyleStrategy) -> Self {
        self.scoped_style_strategy = strategy;
        self
    }

    /// Set the view transitions animation URL (stub).
    #[must_use]
    pub fn with_transitions_animation_url(mut self, url: impl Into<String>) -> Self {
        self.transitions_animation_url = Some(url.into());
        self
    }

    /// Enable or disable source file annotation (stub).
    #[must_use]
    pub fn with_annotate_source_file(mut self, enabled: bool) -> Self {
        self.annotate_source_file = enabled;
        self
    }

    /// Enable or disable `$$renderScript` for processed script tags.
    #[must_use]
    pub fn with_render_script(mut self, enabled: bool) -> Self {
        self.render_script = enabled;
        self
    }

    /// Enable or disable experimental script ordering (stub).
    #[must_use]
    pub fn with_experimental_script_order(mut self, enabled: bool) -> Self {
        self.experimental_script_order = enabled;
        self
    }

    /// Include metadata export.
    #[must_use]
    pub fn with_metadata(mut self, include: bool) -> Self {
        self.include_metadata = include;
        self
    }

    /// Set whether to strip HTML comments from component slot children.
    ///
    /// When `true` (default), matches Go compiler behavior by excluding comments from slots.
    /// When `false`, preserves HTML comments in slot content.
    #[must_use]
    pub fn with_strip_slot_comments(mut self, strip: bool) -> Self {
        self.strip_slot_comments = strip;
        self
    }

    /// Get the internal URL, with default fallback.
    pub fn get_internal_url(&self) -> &str {
        self.internal_url.as_deref().unwrap_or("astro/runtime/server/index.js")
    }
}

// Keep the old name as a type alias during migration
/// Deprecated: Use [`TransformOptions`] instead.
pub type AstroCodegenOptions = TransformOptions;
