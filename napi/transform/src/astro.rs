//! Astro file compilation to JavaScript.

use std::mem;

use napi::{Task, bindgen_prelude::AsyncTask};
use napi_derive::napi;

use oxc::{
    allocator::Allocator,
    codegen::astro::{AstroCodegen, AstroCodegenOptions},
    parser::{ParseOptions, Parser},
    span::SourceType,
};
use oxc_napi::OxcError;

/// Options for compiling Astro files to JavaScript.
#[napi(object)]
#[derive(Default)]
pub struct AstroCompileOptions {
    /// The import specifier for Astro runtime functions.
    /// Defaults to `"astro/runtime/server/index.js"`.
    pub internal_url: Option<String>,

    /// The filename of the Astro component being compiled.
    /// Used in the `$$createComponent` call for debugging.
    pub filename: Option<String>,

    /// Whether to include the `$$metadata` export.
    /// This is used for hydration and module tracking.
    ///
    /// @default false
    pub include_metadata: Option<bool>,
}

/// Result of compiling an Astro file.
#[napi]
pub struct AstroCompileResult {
    code: String,
    errors: Vec<OxcError>,
}

#[napi]
impl AstroCompileResult {
    /// The generated JavaScript code.
    #[napi(getter)]
    pub fn code(&mut self) -> String {
        mem::take(&mut self.code)
    }

    /// Any compilation errors encountered.
    #[napi(getter)]
    pub fn errors(&mut self) -> Vec<OxcError> {
        mem::take(&mut self.errors)
    }
}

fn compile_astro_impl(source_text: &str, options: &AstroCompileOptions) -> AstroCompileResult {
    let allocator = Allocator::default();
    let source_type = SourceType::astro();

    // Parse the Astro file
    let ret = Parser::new(&allocator, source_text, source_type)
        .with_options(ParseOptions::default())
        .parse_astro();

    // If there are parse errors, return them
    if !ret.errors.is_empty() {
        let errors = OxcError::from_diagnostics("", source_text, ret.errors);
        return AstroCompileResult { code: String::new(), errors };
    }

    // Build codegen options
    let codegen_options = AstroCodegenOptions {
        internal_url: options.internal_url.clone(),
        filename: options.filename.clone(),
        include_metadata: options.include_metadata.unwrap_or(false),
    };

    // Generate JavaScript code
    let codegen = AstroCodegen::new(&allocator, source_text, codegen_options);
    let result = codegen.build(&ret.root);

    AstroCompileResult { code: result.code, errors: Vec::new() }
}

/// Compile Astro file to JavaScript synchronously on current thread.
///
/// This transforms an Astro file into JavaScript code compatible with the Astro runtime.
/// The output follows the same format as the official Astro compiler.
///
/// @example
/// ```javascript
/// import { compileAstroSync } from '@aspect-build/oxc-transform';
///
/// const result = compileAstroSync(`---
/// const name = "World";
/// ---
/// <h1>Hello {name}!</h1>`, {
///   filename: 'Component.astro',
///   includeMetadata: true,
/// });
///
/// console.log(result.code); // Generated JavaScript
/// ```
#[napi]
#[allow(clippy::needless_pass_by_value, clippy::allow_attributes)]
pub fn compile_astro_sync(
    source_text: String,
    options: Option<AstroCompileOptions>,
) -> AstroCompileResult {
    let options = options.unwrap_or_default();
    compile_astro_impl(&source_text, &options)
}

pub struct AstroCompileTask {
    source_text: String,
    options: AstroCompileOptions,
}

#[napi]
impl Task for AstroCompileTask {
    type JsValue = AstroCompileResult;
    type Output = AstroCompileResult;

    fn compute(&mut self) -> napi::Result<Self::Output> {
        let source_text = mem::take(&mut self.source_text);
        Ok(compile_astro_impl(&source_text, &self.options))
    }

    fn resolve(&mut self, _: napi::Env, result: Self::Output) -> napi::Result<Self::JsValue> {
        Ok(result)
    }
}

/// Compile Astro file to JavaScript asynchronously on a separate thread.
///
/// This transforms an Astro file into JavaScript code compatible with the Astro runtime.
/// The output follows the same format as the official Astro compiler.
///
/// Generally `compileAstroSync` is preferable to use as it does not have the overhead
/// of spawning a thread. If you need to parallelize compilation of multiple files,
/// it is recommended to use worker threads.
///
/// @example
/// ```javascript
/// import { compileAstro } from '@aspect-build/oxc-transform';
///
/// const result = await compileAstro(`---
/// const name = "World";
/// ---
/// <h1>Hello {name}!</h1>`, {
///   filename: 'Component.astro',
///   includeMetadata: true,
/// });
///
/// console.log(result.code); // Generated JavaScript
/// ```
#[napi]
pub fn compile_astro(
    source_text: String,
    options: Option<AstroCompileOptions>,
) -> AsyncTask<AstroCompileTask> {
    let options = options.unwrap_or_default();
    AsyncTask::new(AstroCompileTask { source_text, options })
}
