//! Options for Astro codegen.

/// Options for Astro code generation.
#[derive(Debug, Clone)]
pub struct AstroCodegenOptions {
    /// The import specifier for Astro runtime functions.
    /// Defaults to `"astro/runtime/server/index.js"`.
    pub internal_url: Option<String>,

    /// The filename of the Astro component being compiled.
    /// Used in the `$$createComponent` call for debugging.
    pub filename: Option<String>,

    /// Whether to include the `$$metadata` export.
    /// This is used for hydration and module tracking.
    pub include_metadata: bool,

    /// Arguments passed to $$createAstro when the Astro global is used.
    /// Defaults to `"https://astro.build"`.
    pub astro_global_args: Option<String>,

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

impl Default for AstroCodegenOptions {
    fn default() -> Self {
        Self {
            internal_url: None,
            filename: None,
            include_metadata: false,
            astro_global_args: None,
            strip_slot_comments: true, // Match Go compiler behavior by default
        }
    }
}

impl AstroCodegenOptions {
    /// Create new options with default values.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the internal URL for Astro runtime imports.
    #[must_use]
    pub fn with_internal_url(mut self, url: impl Into<String>) -> Self {
        self.internal_url = Some(url.into());
        self
    }

    /// Set the filename.
    #[must_use]
    pub fn with_filename(mut self, filename: impl Into<String>) -> Self {
        self.filename = Some(filename.into());
        self
    }

    /// Include metadata export.
    #[must_use]
    pub fn with_metadata(mut self, include: bool) -> Self {
        self.include_metadata = include;
        self
    }

    /// Set the Astro global arguments.
    #[must_use]
    pub fn with_astro_global_args(mut self, args: impl Into<String>) -> Self {
        self.astro_global_args = Some(args.into());
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
