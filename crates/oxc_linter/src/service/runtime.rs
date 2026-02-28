use std::{
    borrow::Cow,
    ffi::OsStr,
    fs,
    hash::BuildHasherDefault,
    mem::take,
    path::{Path, PathBuf},
    sync::{Arc, Mutex, mpsc},
};

use indexmap::IndexSet;
use rayon::iter::ParallelDrainRange;
use rayon::{
    Scope,
    iter::IntoParallelRefIterator,
    prelude::{ParallelIterator, ParallelSliceMut},
};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet, FxHasher};
use self_cell::self_cell;
use smallvec::SmallVec;

use oxc_allocator::{Allocator, AllocatorGuard, AllocatorPool, Vec as ArenaVec};
use oxc_diagnostics::{DiagnosticSender, DiagnosticService, Error, OxcDiagnostic};
use oxc_parser::{ParseOptions, Parser, Token, config::RuntimeParserConfig};
use oxc_resolver::Resolver;
#[cfg(feature = "astro")]
use oxc_semantic::SemanticBuilderAstroExt;
use oxc_semantic::{Semantic, SemanticBuilder};
use oxc_span::{CompactStr, SourceType, VALID_EXTENSIONS};

use crate::{
    Fixer, Linter, Message, PossibleFixes,
    context::ContextSubHost,
    disable_directives::DisableDirectives,
    loader::{JavaScriptSource, LINT_PARTIAL_LOADER_EXTENSIONS, PartialLoader},
    module_record::ModuleRecord,
    utils::read_to_arena_str,
};

use super::LintServiceOptions;

type ModulesByPath =
    papaya::HashMap<Arc<OsStr>, SmallVec<[Arc<ModuleRecord>; 1]>, BuildHasherDefault<FxHasher>>;

pub struct Runtime {
    cwd: Box<Path>,
    pub(super) linter: Linter,
    resolver: Option<Resolver>,

    /// Pool of allocators for parsing and linting.
    allocator_pool: AllocatorPool,

    /// Separate pool of fixed-size allocators for copying AST before JS transfer.
    /// Only created when using the copy-to-fixed-allocator approach
    /// (both import plugin and JS plugins enabled).
    #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
    js_allocator_pool: Option<AllocatorPool>,

    /// The module graph keyed by module paths. It is looked up when populating `loaded_modules`.
    /// The values are module records of sections (check the docs of `ProcessedModule.section_module_records`)
    /// Its entries are kept across groups because modules discovered in former groups could be referenced by modules in latter groups.
    ///
    /// `ModuleRecord` is a cyclic data structure.
    /// To make sure all `ModuleRecord` gets dropped after `Runtime` is dropped,
    /// `modules_by_path` must own `ModuleRecord` with `Arc`, all other references must use `Weak<ModuleRecord>`.
    modules_by_path: ModulesByPath,
    /// Collected disable directives from linted files
    disable_directives_map: Arc<Mutex<FxHashMap<PathBuf, DisableDirectives>>>,
}

/// Output of `Runtime::process_path`
struct ModuleProcessOutput<'alloc_pool> {
    /// All paths in `Runtime` are stored as `OsStr`, because `OsStr` hash is faster
    /// than `Path` - go checkout their source code.
    path: Arc<OsStr>,
    processed_module: ProcessedModule<'alloc_pool>,
}

/// A module processed from a path
#[derive(Default)]
struct ProcessedModule<'alloc_pool> {
    /// Module records of source sections, or diagnostics if parsing failed on that section.
    ///
    /// Modules with special extensions such as .vue could contain multiple source sections (see `PartialLoader::PartialLoader`).
    /// Plain ts/js modules have one section. Using `SmallVec` to avoid allocations for plain modules.
    section_module_records: SmallVec<[Result<ResolvedModuleRecord, Vec<OxcDiagnostic>>; 1]>,

    /// Source code and semantic of the module.
    ///
    /// This value is required for linter to run on the module.  There are two cases where `content` is `None`:
    /// - Import plugin is enabled and the module is a dependency, which is processed only to construct the module graph, not for linting.
    /// - Couldn't get the source text of the module to lint, e.g. the file doesn't exist or the source isn't valid utf-8.
    ///
    /// Note that `content` is `Some` even if parsing is unsuccessful as long as the source to lint is valid utf-8.
    /// It is designed this way to cover the case where some but not all the sections fail to parse.
    content: Option<ModuleContent<'alloc_pool>>,
}

struct ResolvedModuleRequest {
    specifier: CompactStr,
    resolved_requested_path: Arc<OsStr>,
}

/// ModuleRecord with all specifiers in import statements resolved to real paths.
struct ResolvedModuleRecord {
    module_record: Arc<ModuleRecord>,
    resolved_module_requests: Vec<ResolvedModuleRequest>,
}

self_cell! {
    struct ModuleContent<'alloc_pool> {
        owner: AllocatorGuard<'alloc_pool>,
        #[not_covariant]
        dependent: ModuleContentDependent,
    }
}
struct ModuleContentDependent<'a> {
    source_text: &'a str,
    section_contents: SectionContents<'a>,
}

// Safety: dependent borrows from owner. They're safe to be sent together.
unsafe impl Send for ModuleContent<'_> {}

/// source text and semantic for each source section. They are in the same order as `ProcessedModule.section_module_records`
type SectionContents<'a> = SmallVec<[SectionContent<'a>; 1]>;
struct SectionContent<'a> {
    source: JavaScriptSource<'a>,
    /// None if section parsing failed. The corresponding item with the same index in
    /// `ProcessedModule.section_module_records` would be `Err(Vec<OxcDiagnostic>)`.
    semantic: Option<Semantic<'a>>,
    /// None if section parsing failed, or token collection was not requested.
    parser_tokens: Option<ArenaVec<'a, Token>>,

    /// For Astro files: maps each `<script>` block's content byte range to its
    /// `section_offset` (= the byte offset to use as the insertion anchor for
    /// "disable for this whole file" / "disable for this line" code actions).
    ///
    /// Each entry is `(content_start, content_end, insert_offset)`:
    /// - `content_start..content_end`: the byte range of the script block's content in the
    ///   full `.astro` source text.
    /// - `insert_offset`: the byte offset at which a "disable whole section" comment should
    ///   be inserted (= the `\n` after the opening `<script>` tag, so that the LSP inserts
    ///   the comment on the first line of the block's content).
    ///
    /// Entries are sorted by `content_start`. For diagnostics whose `span.start` falls
    /// inside one of these ranges, `message.section_offset` is overridden with that entry's
    /// `insert_offset`. All other diagnostics (frontmatter, template body) have their
    /// `section_offset` set to `astro_frontmatter_offset` (see below).
    #[cfg(feature = "astro")]
    astro_section_map: Option<AstroSectionMap>,

    /// For Astro files: the byte offset of the `\n` that terminates the opening `---` fence.
    /// Passing this as `section_offset` to the LSP code-action helpers causes "disable for
    /// this whole file" to insert inside the frontmatter (after `---\n`) rather than before
    /// the opening `---`.
    ///
    /// `0` for non-Astro sections (no special handling).
    #[cfg(feature = "astro")]
    astro_frontmatter_offset: u32,
}

/// Per-`<script>`-block section boundaries for an Astro file.
///
/// Each entry: `(content_start, content_end, insert_offset)` — all byte offsets into the
/// full `.astro` source text.
#[cfg(feature = "astro")]
type AstroSectionMap = Vec<(u32, u32, u32)>;

/// A module with its source text and semantic, ready to be linted.
///
/// A `ModuleWithContent` is generated for each path in `runtime.paths`. It's basically the same
/// as [`ProcessedModule`], except `content` is non-Option.
struct ModuleToLint<'alloc_pool> {
    path: Arc<OsStr>,
    section_module_records: SmallVec<[Result<Arc<ModuleRecord>, Vec<OxcDiagnostic>>; 1]>,
    content: ModuleContent<'alloc_pool>,
}
impl<'alloc_pool> ModuleToLint<'alloc_pool> {
    fn from_processed_module(
        path: Arc<OsStr>,
        processed_module: ProcessedModule<'alloc_pool>,
    ) -> Option<Self> {
        processed_module.content.map(|content| Self {
            path,
            section_module_records: processed_module
                .section_module_records
                .into_iter()
                .map(|record_result| record_result.map(|ok| ok.module_record))
                .collect(),
            content,
        })
    }
}

/// A simple trait for the `Runtime` to load and save file from a filesystem
/// The `Runtime` uses OsFileSystem as a default
/// The Tester and `oxc_language_server` would like to provide the content from memory
pub trait RuntimeFileSystem {
    /// reads the content of a file path
    ///
    /// # Errors
    /// When no valid path is provided or the content is not valid UTF-8 Stream
    fn read_to_arena_str<'a>(
        &self,
        path: &Path,
        allocator: &'a Allocator,
    ) -> Result<&'a str, std::io::Error>;

    /// write a file to the file system
    ///
    /// # Errors
    /// When the program does not have write permission for the file system
    fn write_file(&self, path: &Path, content: &str) -> Result<(), std::io::Error>;
}

pub struct OsFileSystem;

impl RuntimeFileSystem for OsFileSystem {
    fn read_to_arena_str<'a>(
        &self,
        path: &Path,
        allocator: &'a Allocator,
    ) -> Result<&'a str, std::io::Error> {
        read_to_arena_str(path, allocator)
    }

    fn write_file(&self, path: &Path, content: &str) -> Result<(), std::io::Error> {
        fs::write(path, content)
    }
}

impl Runtime {
    pub(super) fn new(linter: Linter, options: LintServiceOptions) -> Self {
        // If global thread pool wasn't already initialized, do it now.
        // This "locks" config for the thread pool, which ensures `rayon::current_num_threads()`
        // cannot change from now on.
        //
        // Initializing the thread pool without specifying `num_threads` produces a threadpool size
        // based on `std::thread::available_parallelism`. However, Rayon's docs state that:
        // > In the future, the default behavior may change to dynamically add or remove threads as needed.
        // https://docs.rs/rayon/1.11.0/rayon/struct.ThreadPoolBuilder.html#method.num_threads
        //
        // However, I (@overlookmotel) assume that would be considered a breaking change,
        // so we don't have to worry about it until Rayon v2.
        // When Rayon v2 is released and we upgrade to it, we'll need to revisit this and make sure
        // we still guarantee that thread count is locked.
        //
        // If thread pool was already initialized, this won't do anything.
        // `build_global` will return `Err` in that case, but we can ignore it.
        // That just means the config (and so number of threads) is already locked.
        // https://docs.rs/rayon/1.11.0/rayon/struct.ThreadPoolBuilder.html#method.build_global
        let _ = rayon::ThreadPoolBuilder::new().build_global();

        let thread_count = rayon::current_num_threads();

        // Create allocator pools.
        //
        // * If both JS plugins and import plugin enabled, use copy-to-fixed-allocator approach.
        //   This approach is to use standard allocators for parsing/linting (lower memory usage),
        //   and copy ASTs to a fixed-size allocator only when passing to JS plugins.
        //
        // * If JS plugins, but no import plugin, use fixed-size allocators for everything.
        //   Without import plugin, there's no danger of memory exhaustion, as no more than <thread count>
        //   ASTs are live at any given time.
        //
        // * If no JS plugins, use standard allocators for parsing/linting.
        #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
        let (allocator_pool, js_allocator_pool) = if linter.has_external_linter() {
            if options.cross_module {
                (
                    AllocatorPool::new(thread_count),
                    Some(AllocatorPool::new_fixed_size(thread_count)),
                )
            } else {
                (AllocatorPool::new_fixed_size(thread_count), None)
            }
        } else {
            (AllocatorPool::new(thread_count), None)
        };

        #[cfg(not(all(target_pointer_width = "64", target_endian = "little")))]
        let allocator_pool = AllocatorPool::new(thread_count);

        let resolver = options.cross_module.then(|| Self::get_resolver(options.tsconfig));

        Self {
            allocator_pool,
            #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
            js_allocator_pool,
            cwd: options.cwd,
            linter,
            resolver,
            modules_by_path: papaya::HashMap::builder()
                .hasher(BuildHasherDefault::default())
                .resize_mode(papaya::ResizeMode::Blocking)
                .build(),
            disable_directives_map: Arc::new(Mutex::new(FxHashMap::default())),
        }
    }

    /// Get [`AllocatorPool`] for copying ASTs to fixed-size allocators, if one is required.
    fn js_allocator_pool(&self) -> Option<&AllocatorPool> {
        #[cfg(all(target_pointer_width = "64", target_endian = "little"))]
        let pool = self.js_allocator_pool.as_ref();

        #[cfg(not(all(target_pointer_width = "64", target_endian = "little")))]
        let pool = None;

        pool
    }

    pub fn set_disable_directives_map(
        &mut self,
        map: Arc<Mutex<FxHashMap<PathBuf, DisableDirectives>>>,
    ) {
        self.disable_directives_map = map;
    }

    fn get_resolver(tsconfig_path: Option<PathBuf>) -> Resolver {
        use oxc_resolver::{
            ResolveOptions, TsconfigDiscovery, TsconfigOptions, TsconfigReferences,
        };
        let tsconfig = match tsconfig_path {
            Some(path) if path.is_file() => Some(TsconfigDiscovery::Manual(TsconfigOptions {
                config_file: path,
                references: TsconfigReferences::Auto,
            })),
            Some(_) => None, // Path provided but file doesn't exist
            None => Some(TsconfigDiscovery::Auto),
        };
        let extension_alias = tsconfig.as_ref().map_or_else(Vec::new, |_| {
            vec![
                (".js".into(), vec![".js".into(), ".ts".into()]),
                (".mjs".into(), vec![".mjs".into(), ".mts".into()]),
                (".cjs".into(), vec![".cjs".into(), ".cts".into()]),
            ]
        });
        Resolver::new(ResolveOptions {
            extensions: VALID_EXTENSIONS.iter().map(|ext| format!(".{ext}")).collect(),
            main_fields: vec!["module".into(), "main".into()],
            condition_names: vec!["module".into(), "import".into()],
            extension_alias,
            tsconfig,
            ..ResolveOptions::default()
        })
    }

    fn get_source_type_and_text<'a>(
        file_system: &(dyn RuntimeFileSystem + Sync + Send),
        path: &Path,
        ext: &str,
        allocator: &'a Allocator,
    ) -> Option<Result<(SourceType, &'a str), Error>> {
        let source_type = SourceType::from_path(path);
        let not_supported_yet =
            source_type.as_ref().is_err_and(|_| !LINT_PARTIAL_LOADER_EXTENSIONS.contains(&ext));
        if not_supported_yet {
            return None;
        }

        let mut source_type = source_type.unwrap_or_default();
        // Treat JS and JSX files to maximize chance of parsing files.
        if source_type.is_javascript() {
            source_type = source_type.with_jsx(true);
        }

        let file_result = file_system.read_to_arena_str(path, allocator).map_err(|e| {
            Error::new(OxcDiagnostic::error(format!(
                "Failed to open file {} with error \"{e}\"",
                path.display()
            )))
        });
        Some(match file_result {
            Ok(source_text) => Ok((source_type, source_text)),
            Err(e) => Err(e),
        })
    }

    /// Prepare entry modules for linting.
    ///
    /// `on_module_to_lint` is called for each entry modules in `paths` when it's ready for linting,
    /// which means all its dependencies are resolved if import plugin is enabled.
    fn resolve_modules<'a>(
        &'a self,
        file_system: &'a (dyn RuntimeFileSystem + Sync + Send),
        paths: &'a IndexSet<Arc<OsStr>, FxBuildHasher>,
        scope: &Scope<'a>,
        check_syntax_errors: bool,
        tx_error: Option<&'a DiagnosticSender>,
        on_module_to_lint: impl Fn(&'a Self, ModuleToLint) + Send + Sync + Clone + 'a,
    ) {
        if self.resolver.is_none() {
            paths.par_iter().for_each(|path| {
                let output =
                    self.process_path(file_system, paths, path, check_syntax_errors, tx_error);
                let Some(entry) =
                    ModuleToLint::from_processed_module(output.path, output.processed_module)
                else {
                    return;
                };
                on_module_to_lint(self, entry);
            });
            return;
        }
        // The goal of code below is to construct the module graph bootstrapped by the entry modules (`paths`),
        // and call `on_entry` when all dependencies of that entry is resolved. We want to call `on_entry` for each
        // entry as soon as possible, so that the memory for source texts and semantics can be released early.

        // Sorting paths to make deeper paths appear first.
        // Consider a typical scenario:
        //
        // - src/index.js
        // - src/a/foo.js
        // - src/b/bar.js
        // ..... (thousands of sources)
        // - src/very/deep/path/baz.js
        //
        // All paths above are in `paths`. `src/index.js`, the entrypoint of the application, references
        // almost all the other paths as its direct or indirect dependencies.
        //
        // If we construct the module graph starting from `src/index.js`, contents (sources and semantics) of
        // all these paths must stay in memory (because they are both entries and part of `src/index.js` dependencies)
        // until the last dependency is processed.
        // The more efficient way is to start from "leaf" modules: their dependencies are ready earlier, thus we
        // can run lint on them and then released their content earlier.
        //
        // But it's impossible to know which ones are "leaf" modules before parsing even starts. Here we assume
        // deeper paths are more likely to be leaf modules  (src/very/deep/path/baz.js is likely to have
        // fewer dependencies than src/index.js).
        // This heuristic is not always true, but it works well enough for real world codebases.

        // Create a sorted copy of paths for processing
        let mut sorted_paths: Vec<_> = paths.iter().cloned().collect();
        // Sort by path length descending - longer paths tend to be deeper in the directory tree.
        // This achieves the "deeper paths first" heuristic described above in O(1) per comparison.
        sorted_paths.par_sort_unstable_by(|a, b| b.len().cmp(&a.len()));

        // The general idea is processing `sorted_paths` and their dependencies in groups. We start from a group of modules
        // in `sorted_paths` that is small enough to hold in memory but big enough to make use of the rayon thread pool.
        // We build the module graph from one group, run lint on them, drop sources and semantics but keep the module
        // graph, and then move on to the next group.
        // This size is empirical based on AFFiNE@97cc814a.
        let group_size = rayon::current_num_threads() * 4;

        // Stores modules that belongs to `self.paths` in current group.
        // They are passed to `on_module_to_lint` at the end of each group.
        let mut modules_to_lint: Vec<ModuleToLint> = Vec::with_capacity(group_size);

        // Set self to immutable reference so it can be shared among spawned tasks.
        let me: &Self = self;

        // `encountered_paths` prevents duplicated processing.
        // It is a superset of keys of `modules_by_path` as it also contains paths that are queued to process.
        let mut encountered_paths =
            FxHashSet::<Arc<OsStr>>::with_capacity_and_hasher(sorted_paths.len(), FxBuildHasher);

        // Resolved module requests from modules in current group.
        // This is used to populate `loaded_modules` at the end of each group.
        let mut module_paths_and_resolved_requests =
            Vec::<(Arc<OsStr>, SmallVec<[Vec<ResolvedModuleRequest>; 1]>)>::new();

        // There are two sets of threads: threads for the graph and threads for the modules.
        // - The graph thread is the one thread that calls `resolve_modules`. It's the only thread that updates the module graph, so no need for locks.
        // - Module threads accept paths and produces `ModuleProcessOutput` (the logic is in `self.process_path`). They are isolated to each
        //   other and paralleled in the rayon thread pool.

        // This channel is for posting `ModuleProcessOutput` from module threads to the graph thread.
        let (tx_process_output, rx_process_output) = mpsc::channel::<ModuleProcessOutput>();

        // The cursor of `sorted_paths` that points to the start path of the next group.
        let mut group_start = 0usize;

        // The group loop. Each iteration of this loop processes a group of modules.
        while group_start < sorted_paths.len() {
            // How many modules are queued but not processed in this group.
            let mut pending_module_count = 0;

            // Bootstrap the group by processing modules to be linted.
            while pending_module_count < group_size && group_start < sorted_paths.len() {
                let path = &sorted_paths[group_start];
                group_start += 1;

                // Check if this module to be linted is already processed as a dependency in former groups
                if encountered_paths.insert(Arc::clone(path)) {
                    pending_module_count += 1;
                    let path = Arc::clone(path);
                    let tx_process_output = tx_process_output.clone();
                    scope.spawn(move |_| {
                        tx_process_output
                            .send(me.process_path(
                                file_system,
                                paths,
                                &path,
                                check_syntax_errors,
                                tx_error,
                            ))
                            .unwrap();
                    });
                }
            }

            // Loop until all queued modules in this group are processed.
            // Each iteration adds one module to the module graph.
            while pending_module_count > 0 {
                let Ok(ModuleProcessOutput { path, mut processed_module }) =
                    // Most heavy-lifting is done in the module threads. The graph thread would be mostly idle if it
                    // only updates the graph and blocks on awaiting `rx_process_output`.
                    // To avoid this waste, the graph module peeks the `rx_process_output` without blocking, and ...
                    rx_process_output.try_recv()
                else {
                    // yield if `rx_process_output` is empty, giving rayon chances to dispatch module processing or linting to this thread.
                    rayon::yield_now();
                    continue;
                };
                pending_module_count -= 1;

                // Spawns tasks for processing dependencies to module threads
                for record_result in &processed_module.section_module_records {
                    let Ok(record) = record_result.as_ref() else {
                        continue;
                    };
                    for request in &record.resolved_module_requests {
                        let dep_path = &request.resolved_requested_path;
                        if encountered_paths.insert(Arc::clone(dep_path)) {
                            scope.spawn({
                                let tx_process_output = tx_process_output.clone();
                                let dep_path = Arc::clone(dep_path);
                                move |_| {
                                    tx_process_output
                                        .send(me.process_path(
                                            file_system,
                                            paths,
                                            &dep_path,
                                            check_syntax_errors,
                                            tx_error,
                                        ))
                                        .unwrap();
                                }
                            });
                            pending_module_count += 1;
                        }
                    }
                }

                // Populate this module to `modules_by_path`
                self.modules_by_path.pin().insert(
                    Arc::clone(&path),
                    processed_module
                        .section_module_records
                        .iter()
                        .filter_map(|resolved_module_record| {
                            Some(Arc::clone(&resolved_module_record.as_ref().ok()?.module_record))
                        })
                        .collect(),
                );

                // We want to write to `loaded_modules` when the dependencies of this module are processed, but it's hard
                // to track when that happens, so here we store dependency relationships in `module_paths_and_resolved_requests`,
                // and use it to populate `loaded_modules` after `pending_module_count` reaches 0. That's when all dependencies
                // in this group are processed.
                module_paths_and_resolved_requests.push((
                    Arc::clone(&path),
                    processed_module
                        .section_module_records
                        .iter_mut()
                        .filter_map(|record_result| {
                            Some(take(&mut record_result.as_mut().ok()?.resolved_module_requests))
                        })
                        .collect(),
                ));

                // This module has `content` which means it's one of `self.paths`.
                // Store it to `modules_to_lint`
                if let Some(entry_module) =
                    ModuleToLint::from_processed_module(path, processed_module)
                {
                    modules_to_lint.push(entry_module);
                }
            } // while pending_module_count > 0

            // Now all dependencies in this group are processed.
            // Writing to `loaded_modules` based on `module_paths_and_resolved_requests`
            module_paths_and_resolved_requests.par_drain(..).for_each(|(path, requested_module_paths)| {
                if requested_module_paths.is_empty() {
                    return;
                }
                let modules_by_path = self.modules_by_path.pin();
                let records = modules_by_path.get(&path).unwrap();
                assert_eq!(
                    records.len(), requested_module_paths.len(),
                    "This is an internal logic error. Please file an issue at https://github.com/oxc-project/oxc/issues",
                );
                for (record, requested_module_paths) in
                    records.iter().zip(requested_module_paths.into_iter())
                {
                    let mut loaded_modules = record.write_loaded_modules();
                    for request in requested_module_paths {
                        // TODO: revise how to store multiple sections in loaded_modules
                        let Some(dep_module_record) =
                            modules_by_path.get(&request.resolved_requested_path).unwrap().last()
                        else {
                            continue;
                        };
                        loaded_modules.insert(request.specifier, Arc::downgrade(dep_module_record));
                    }
                }
            });
            #[expect(clippy::iter_with_drain)]
            for entry in modules_to_lint.drain(..) {
                let on_entry = on_module_to_lint.clone();
                scope.spawn(move |_| {
                    on_entry(me, entry);
                });
            }
        }
    }

    pub(super) fn run(
        &self,
        file_system: &(dyn RuntimeFileSystem + Sync + Send),
        paths: Vec<Arc<OsStr>>,
        tx_error: &DiagnosticSender,
    ) {
        self.modules_by_path.pin().reserve(paths.len());
        let paths_set: IndexSet<Arc<OsStr>, FxBuildHasher> = paths.into_iter().collect();

        rayon::scope(|scope| {
            self.resolve_modules(
                file_system,
                &paths_set,
                scope,
                true,
                Some(tx_error),
                move |me, mut module_to_lint| {
                    module_to_lint.content.with_dependent_mut(|allocator_guard, dep| {
                        // If there are fixes, we will accumulate all of them and write to the file at the end.
                        // This means we do not write multiple times to the same file if there are multiple sources
                        // in the same file (for example, multiple scripts in an `.astro` file).
                        let mut new_source_text = Cow::from(dep.source_text);

                        let path = Path::new(&module_to_lint.path);

                        assert_eq!(
                            module_to_lint.section_module_records.len(),
                            dep.section_contents.len()
                        );

                        #[cfg(feature = "astro")]
                        let mut astro_info: Option<(AstroSectionMap, u32)> = None;

                        let context_sub_hosts: Vec<ContextSubHost<'_>> = module_to_lint
                            .section_module_records
                            .into_iter()
                            .zip(dep.section_contents.drain(..))
                            .filter_map(|(record_result, section)| match record_result {
                                Ok(module_record) => {
                                    #[cfg(feature = "astro")]
                                    if let Some(map) = section.astro_section_map {
                                        astro_info = Some((map, section.astro_frontmatter_offset));
                                    }
                                    Some(ContextSubHost::new_with_framework_options(
                                        section.semantic.unwrap(),
                                        Arc::clone(&module_record),
                                        section.source.start,
                                        section.source.framework_options,
                                        section.parser_tokens,
                                    ))
                                }
                                Err(messages) => {
                                    if !messages.is_empty() {
                                        let diagnostics = DiagnosticService::wrap_diagnostics(
                                            &me.cwd,
                                            path,
                                            dep.source_text,
                                            messages,
                                        );
                                        tx_error.send(diagnostics).unwrap();
                                    }
                                    None
                                }
                            })
                            .collect();

                        if context_sub_hosts.is_empty() {
                            return;
                        }

                        let (mut messages, disable_directives) =
                            me.linter.run_with_disable_directives(
                                path,
                                context_sub_hosts,
                                allocator_guard,
                                me.js_allocator_pool(),
                            );

                        // Store the disable directives for this file
                        if let Some(disable_directives) = disable_directives {
                            me.disable_directives_map
                                .lock()
                                .expect("disable_directives_map mutex poisoned")
                                .insert(path.to_path_buf(), disable_directives);
                        }

                        // For Astro files, fix up section_offset per message so that
                        // "disable for this section" code actions insert inside the right
                        // block (script vs frontmatter) rather than always at byte 0.
                        #[cfg(feature = "astro")]
                        if let Some((ref map, frontmatter_offset)) = astro_info {
                            apply_astro_section_offsets(&mut messages, map, frontmatter_offset);
                        }

                        if me.linter.options().fix.is_some() {
                            let fix_result = Fixer::new(
                                dep.source_text,
                                messages,
                                SourceType::from_path(path).ok().map(|st| {
                                    if st.is_javascript() { st.with_jsx(true) } else { st }
                                }),
                            )
                            .fix();
                            if fix_result.fixed {
                                // write to file, replacing only the changed part
                                let start = 0;
                                let end = start + dep.source_text.len();
                                new_source_text
                                    .to_mut()
                                    .replace_range(start..end, &fix_result.fixed_code);
                            }
                            messages = fix_result.messages;
                        }

                        if !messages.is_empty() {
                            let errors = messages.into_iter().map(Into::into).collect();
                            let diagnostics = DiagnosticService::wrap_diagnostics(
                                &me.cwd,
                                path,
                                dep.source_text,
                                errors,
                            );
                            tx_error.send(diagnostics).unwrap();
                        }

                        // If the new source text is owned, that means it was modified,
                        // so we write the new source text to the file.
                        if let Cow::Owned(new_source_text) = &new_source_text {
                            file_system.write_file(path, new_source_text).unwrap();
                        }
                    });
                },
            );
        });
    }

    // language_server: the language server needs line and character position
    // the struct not using `oxc_diagnostic::Error, because we are just collecting information
    // and returning it to the client to let him display it.
    pub(super) fn run_source(
        &self,
        file_system: &(dyn RuntimeFileSystem + Sync + Send),
        paths: Vec<Arc<OsStr>>,
    ) -> Vec<Message> {
        use std::sync::Mutex;

        self.modules_by_path.pin().reserve(paths.len());
        let paths_set: IndexSet<Arc<OsStr>, FxBuildHasher> = paths.into_iter().collect();

        let messages = Mutex::new(Vec::<Message>::new());
        rayon::scope(|scope| {
            self.resolve_modules(
                file_system,
                &paths_set,
                scope,
                true,
                None,
                |me, mut module_to_lint| {
                    module_to_lint.content.with_dependent_mut(
                    |allocator_guard, ModuleContentDependent { source_text: _, section_contents }| {
                        assert_eq!(
                            module_to_lint.section_module_records.len(),
                            section_contents.len()
                        );

                        #[cfg(feature = "astro")]
                        let mut astro_info: Option<(AstroSectionMap, u32)> = None;

                        let context_sub_hosts: Vec<ContextSubHost<'_>> = module_to_lint
                            .section_module_records
                            .into_iter()
                            .zip(section_contents.drain(..))
                            .filter_map(|(record_result, section)| match record_result {
                                Ok(module_record) => {
                                    #[cfg(feature = "astro")]
                                    if let Some(map) = section.astro_section_map {
                                        astro_info =
                                            Some((map, section.astro_frontmatter_offset));
                                    }
                                    Some(ContextSubHost::new_with_framework_options(
                                        section.semantic.unwrap(),
                                        Arc::clone(&module_record),
                                        section.source.start,
                                        section.source.framework_options,
                                        section.parser_tokens,
                                    ))
                                }
                                Err(diagnostics) => {
                                    if !diagnostics.is_empty() {
                                        messages.lock().unwrap().extend(
                                            diagnostics.into_iter().map(|diagnostic| {
                                                Message::new(diagnostic, PossibleFixes::None)
                                            }),
                                        );
                                    }
                                    None
                                }
                            })
                            .collect();

                        if context_sub_hosts.is_empty() {
                            return;
                        }

                        let path = Path::new(&module_to_lint.path);

                        let (mut section_messages, disable_directives) = me
                            .linter
                            .run_with_disable_directives(path, context_sub_hosts, allocator_guard, me.js_allocator_pool());

                        if let Some(disable_directives) = disable_directives {
                            me.disable_directives_map
                                .lock()
                                .expect("disable_directives_map mutex poisoned")
                                .insert(path.to_path_buf(), disable_directives);
                        }

                        // For Astro files, fix up section_offset per message so that
                        // "disable for this section" code actions insert inside the right
                        // block (script vs frontmatter) rather than always at byte 0.
                        #[cfg(feature = "astro")]
                        if let Some((ref map, frontmatter_offset)) = astro_info {
                            apply_astro_section_offsets(
                                &mut section_messages,
                                map,
                                frontmatter_offset,
                            );
                        }

                        messages.lock().unwrap().extend(section_messages);
                    },
                );
                },
            );
        });

        messages.into_inner().unwrap()
    }

    #[cfg(test)]
    pub(super) fn run_test_source(
        &self,
        file_system: &(dyn RuntimeFileSystem + Sync + Send),
        paths: Vec<Arc<OsStr>>,
        check_syntax_errors: bool,
        tx_error: &DiagnosticSender,
    ) -> Vec<Message> {
        use std::sync::Mutex;

        self.modules_by_path.pin().reserve(paths.len());
        let paths_set: IndexSet<Arc<OsStr>, FxBuildHasher> = paths.into_iter().collect();

        let messages = Mutex::new(Vec::<Message>::new());
        rayon::scope(|scope| {
            self.resolve_modules(
                file_system,
                &paths_set,
                scope,
                check_syntax_errors,
                Some(tx_error),
                |me, mut module| {
                    module.content.with_dependent_mut(
                    |allocator_guard, ModuleContentDependent { source_text: _, section_contents }| {
                        assert_eq!(module.section_module_records.len(), section_contents.len());

                        #[cfg(feature = "astro")]
                        let mut astro_info: Option<(AstroSectionMap, u32)> = None;

                        let context_sub_hosts: Vec<ContextSubHost<'_>> = module
                            .section_module_records
                            .into_iter()
                            .zip(section_contents.drain(..))
                            .filter_map(|(record_result, section)| match record_result {
                                Ok(module_record) => {
                                    #[cfg(feature = "astro")]
                                    if let Some(map) = section.astro_section_map {
                                        astro_info =
                                            Some((map, section.astro_frontmatter_offset));
                                    }
                                    Some(ContextSubHost::new_with_framework_options(
                                        section.semantic.unwrap(),
                                        Arc::clone(&module_record),
                                        section.source.start,
                                        section.source.framework_options,
                                        section.parser_tokens,
                                    ))
                                }
                                Err(errors) => {
                                    if !errors.is_empty() {
                                        messages
                                            .lock()
                                            .unwrap()
                                            .extend(errors
                                        .into_iter()
                                        .map(|err| Message::new(err, PossibleFixes::None))
                                    );
                                    }
                                    None
                                }
                            })
                            .collect();

                        if context_sub_hosts.is_empty() {
                            return;
                        }

                        let mut run_messages = me.linter.run(
                            Path::new(&module.path),
                            context_sub_hosts,
                            allocator_guard,
                        );

                        // For Astro files, fix up section_offset per message so that
                        // "disable for this section" code actions insert inside the right
                        // block (script vs frontmatter) rather than always at byte 0.
                        #[cfg(feature = "astro")]
                        if let Some((ref map, frontmatter_offset)) = astro_info {
                            apply_astro_section_offsets(&mut run_messages, map, frontmatter_offset);
                        }

                        messages.lock().unwrap().extend(run_messages);
                    },
                );
                },
            );
        });
        messages.into_inner().unwrap()
    }

    fn process_path<'a>(
        &'a self,
        file_system: &(dyn RuntimeFileSystem + Sync + Send),
        paths: &IndexSet<Arc<OsStr>, FxBuildHasher>,
        path: &Arc<OsStr>,
        check_syntax_errors: bool,
        tx_error: Option<&DiagnosticSender>,
    ) -> ModuleProcessOutput<'a> {
        let processed_module = self
            .process_path_to_module(file_system, paths, path, check_syntax_errors, tx_error)
            .unwrap_or_default();
        ModuleProcessOutput { path: Arc::clone(path), processed_module }
    }

    fn process_path_to_module<'a>(
        &'a self,
        file_system: &(dyn RuntimeFileSystem + Sync + Send),
        paths: &IndexSet<Arc<OsStr>, FxBuildHasher>,
        path: &Arc<OsStr>,
        check_syntax_errors: bool,
        tx_error: Option<&DiagnosticSender>,
    ) -> Option<ProcessedModule<'a>> {
        let ext = Path::new(path).extension().and_then(OsStr::to_str)?;

        if SourceType::from_path(Path::new(path))
            .as_ref()
            .is_err_and(|_| !LINT_PARTIAL_LOADER_EXTENSIONS.contains(&ext))
        {
            return None;
        }

        let allocator_guard = self.allocator_pool.get();

        if paths.contains(path) {
            let mut records =
                SmallVec::<[Result<ResolvedModuleRecord, Vec<OxcDiagnostic>>; 1]>::new();

            let module_content = ModuleContent::try_new(allocator_guard, |allocator_guard| {
                let allocator = &**allocator_guard;

                let Some(stt) =
                    Self::get_source_type_and_text(file_system, Path::new(path), ext, allocator)
                else {
                    return Err(());
                };

                let (source_type, source_text) = match stt {
                    Ok(v) => v,
                    Err(e) => {
                        if let Some(tx_error) = tx_error {
                            tx_error.send(vec![e]).unwrap();
                        }
                        return Err(());
                    }
                };

                let mut section_contents = SmallVec::new();
                records = self.process_source(
                    Path::new(path),
                    ext,
                    check_syntax_errors,
                    source_type,
                    source_text,
                    allocator,
                    Some(&mut section_contents),
                );

                Ok(ModuleContentDependent { source_text, section_contents })
            });
            let module_content = module_content.ok()?;

            Some(ProcessedModule { section_module_records: records, content: Some(module_content) })
        } else {
            let allocator = &*allocator_guard;

            let stt = Self::get_source_type_and_text(file_system, Path::new(path), ext, allocator)?;

            let (source_type, source_text) = match stt {
                Ok(v) => v,
                Err(e) => {
                    if let Some(tx_error) = tx_error {
                        tx_error.send(vec![e]).unwrap();
                    }
                    return None;
                }
            };

            let records = self.process_source(
                Path::new(path),
                ext,
                check_syntax_errors,
                source_type,
                source_text,
                allocator,
                None,
            );

            Some(ProcessedModule { section_module_records: records, content: None })
        }
    }

    #[expect(clippy::too_many_arguments)]
    fn process_source<'a>(
        &self,
        path: &Path,
        ext: &str,
        check_syntax_errors: bool,
        source_type: SourceType,
        source_text: &'a str,
        allocator: &'a Allocator,
        mut out_sections: Option<&mut SectionContents<'a>>,
    ) -> SmallVec<[Result<ResolvedModuleRecord, Vec<OxcDiagnostic>>; 1]> {
        // Special handling for Astro files - use full AST parsing
        #[cfg(feature = "astro")]
        if ext == "astro" {
            return self.process_astro_source(
                path,
                source_text,
                allocator,
                check_syntax_errors,
                out_sections,
            );
        }

        let section_sources = PartialLoader::parse(ext, source_text)
            .unwrap_or_else(|| vec![JavaScriptSource::partial(source_text, source_type, 0)]);

        let mut section_module_records = SmallVec::<
            [Result<ResolvedModuleRecord, Vec<OxcDiagnostic>>; 1],
        >::with_capacity(section_sources.len());
        for section_source in section_sources {
            match self.process_source_section(
                path,
                allocator,
                section_source.source_text,
                section_source.source_type,
                check_syntax_errors,
            ) {
                Ok((record, semantic, parser_tokens)) => {
                    section_module_records.push(Ok(record));
                    if let Some(sections) = &mut out_sections {
                        sections.push(SectionContent {
                            source: section_source,
                            semantic: Some(semantic),
                            parser_tokens,
                            #[cfg(feature = "astro")]
                            astro_section_map: None,
                            #[cfg(feature = "astro")]
                            astro_frontmatter_offset: 0,
                        });
                    }
                }
                Err(err) => {
                    let err: Vec<OxcDiagnostic> = err
                        .into_iter()
                        .map(|mut diagnostic| {
                            if let Some(labels) = &mut diagnostic.labels {
                                for label in labels.iter_mut() {
                                    label.set_span_offset(
                                        label.offset() + section_source.start as usize,
                                    );
                                }
                            }
                            diagnostic
                        })
                        .collect();

                    section_module_records.push(Err(err));
                    if let Some(sections) = &mut out_sections {
                        sections.push(SectionContent {
                            source: section_source,
                            semantic: None,
                            parser_tokens: None,
                            #[cfg(feature = "astro")]
                            astro_section_map: None,
                            #[cfg(feature = "astro")]
                            astro_frontmatter_offset: 0,
                        });
                    }
                }
            }
        }
        section_module_records
    }

    #[expect(clippy::type_complexity)]
    fn process_source_section<'a>(
        &self,
        path: &Path,
        allocator: &'a Allocator,
        source_text: &'a str,
        source_type: SourceType,
        check_syntax_errors: bool,
    ) -> Result<(ResolvedModuleRecord, Semantic<'a>, Option<ArenaVec<'a, Token>>), Vec<OxcDiagnostic>>
    {
        let collect_tokens = self.linter.has_external_linter();
        let ret = Parser::new(allocator, source_text, source_type)
            .with_options(ParseOptions {
                parse_regular_expression: true,
                allow_return_outside_function: true,
                ..ParseOptions::default()
            })
            .with_config(RuntimeParserConfig::new(collect_tokens))
            .parse();

        if !ret.errors.is_empty() {
            return Err(if ret.is_flow_language { vec![] } else { ret.errors });
        }

        let semantic_ret = SemanticBuilder::new()
            .with_cfg(true)
            .with_check_syntax_error(check_syntax_errors)
            .build(allocator.alloc(ret.program));

        if !semantic_ret.errors.is_empty() {
            return Err(semantic_ret.errors);
        }

        let mut semantic = semantic_ret.semantic;
        semantic.set_irregular_whitespaces(ret.irregular_whitespaces);

        let module_record = Arc::new(ModuleRecord::new(path, &ret.module_record, &semantic));

        let mut resolved_module_requests: Vec<ResolvedModuleRequest> = vec![];

        // If import plugin is enabled.
        if let Some(resolver) = &self.resolver {
            // Retrieve all dependent modules from this module.
            let dir = path.parent().unwrap();
            resolved_module_requests = module_record
                .requested_modules
                .keys()
                .filter_map(|specifier| {
                    let resolution = resolver.resolve(dir, specifier).ok()?;
                    Some(ResolvedModuleRequest {
                        specifier: specifier.clone(),
                        resolved_requested_path: Arc::<OsStr>::from(resolution.path().as_os_str()),
                    })
                })
                .collect();
        }
        let parser_tokens = collect_tokens.then_some(ret.tokens);
        Ok((
            ResolvedModuleRecord { module_record, resolved_module_requests },
            semantic,
            parser_tokens,
        ))
    }

    /// Process an Astro file using the full Astro parser.
    /// This enables linting of both frontmatter code and JSX template.
    #[cfg(feature = "astro")]
    fn process_astro_source<'a>(
        &self,
        path: &Path,
        source_text: &'a str,
        allocator: &'a Allocator,
        check_syntax_errors: bool,
        mut out_sections: Option<&mut SectionContents<'a>>,
    ) -> SmallVec<[Result<ResolvedModuleRecord, Vec<OxcDiagnostic>>; 1]> {
        let source_type = SourceType::astro();

        // Parse as Astro
        let ret = Parser::new(allocator, source_text, source_type)
            .with_options(ParseOptions {
                parse_regular_expression: true,
                allow_return_outside_function: true,
                ..ParseOptions::default()
            })
            .parse_astro();

        if !ret.errors.is_empty() {
            let section_source = JavaScriptSource::new(source_text, source_type);
            if let Some(sections) = &mut out_sections {
                sections.push(SectionContent {
                    source: section_source,
                    semantic: None,
                    parser_tokens: None,
                    astro_section_map: None,
                    astro_frontmatter_offset: 0,
                });
            }
            return smallvec::smallvec![Err(ret.errors)];
        }

        // Allocate the root first, then get comments reference from it
        let root = allocator.alloc(ret.root);

        // Collect comments from all sources:
        //   1. Frontmatter (`--- ... ---`) — stored on `frontmatter.program.comments`
        //   2. `<script>` blocks          — stored on each `AstroScript.program.comments`
        //   3. Template body              — comments inside expression containers `{ }` and
        //                                   JSX attributes; live only in `ret.body_comments`
        //                                   because the body parser's trivia builder is not
        //                                   attached to any AST node.
        //
        // All spans are absolute byte offsets into `source_text`, so they can be merged
        // directly.  Without this merge, `// eslint-disable-next-line` in any of these
        // positions would be invisible to `DisableDirectivesBuilder`.
        let comments: &oxc_allocator::Vec<'_, oxc_ast::ast::Comment> = {
            let mut merged = oxc_allocator::Vec::new_in(allocator);

            // 1. Frontmatter comments
            if let Some(ref frontmatter) = root.frontmatter {
                merged.extend_from_slice(&frontmatter.program.comments);
            }

            // 2. <script> block comments — walk the body looking for AstroScript nodes.
            collect_astro_script_comments(&root.body, &mut merged);

            // 3. Body (template) comments — expression containers, JSX attributes, etc.
            merged.extend_from_slice(&ret.body_comments);

            // Sort by span start so the interval builder sees them in source order.
            merged.sort_unstable_by_key(|c| c.span.start);

            allocator.alloc(merged)
        };

        // Build semantic using our new Astro-aware method
        let semantic_ret = SemanticBuilder::new()
            .with_cfg(true)
            .with_check_syntax_error(check_syntax_errors)
            .build_astro(root, source_text, comments);

        if !semantic_ret.errors.is_empty() {
            let section_source = JavaScriptSource::new(source_text, source_type);
            if let Some(sections) = &mut out_sections {
                sections.push(SectionContent {
                    source: section_source,
                    semantic: None,
                    parser_tokens: None,
                    astro_section_map: None,
                    astro_frontmatter_offset: 0,
                });
            }
            return smallvec::smallvec![Err(semantic_ret.errors)];
        }

        let semantic = semantic_ret.semantic;

        // Build the Astro section map and frontmatter offset for code-action insertion.
        //
        // `section_offset` on a `Message` tells the LSP where to insert "disable for this
        // whole file / section" comments.  Because Astro is processed as a single semantic
        // unit (unlike Vue/Svelte which split into per-script sections), all messages would
        // otherwise share `section_offset = 0`, causing every "disable whole file" action to
        // insert at byte 0 — before the opening `---` fence.
        //
        // After linting, `apply_astro_section_offsets` corrects each message's
        // `section_offset` using this map.
        let astro_section_map = build_astro_section_map(source_text, &root.body);

        // Find the `\n` that terminates the opening `---` fence.  Passing this offset as
        // `section_offset` to the LSP helpers causes `get_section_insert_position` to
        // detect the `\n` and insert after it (i.e. on the first line of frontmatter
        // content), matching the same pattern used for Vue/Svelte `<script>\n` offsets.
        let astro_frontmatter_offset: u32 = root
            .frontmatter
            .as_ref()
            .map(|_| {
                let bytes = source_text.as_bytes();
                if bytes.starts_with(b"---") {
                    // Offset 3 is the char right after `---`.  If it's a newline, point
                    // at it (the LSP helper will skip past it).  If it's `\r\n`, same.
                    // Otherwise fall back to 3.
                    3u32
                } else {
                    0
                }
            })
            .unwrap_or(0);

        // Create module record (may be empty for Astro files without imports)
        let mut module_record = ModuleRecord::default();
        module_record.resolved_absolute_path = path.to_path_buf();
        let module_record = Arc::new(module_record);

        let mut resolved_module_requests: Vec<ResolvedModuleRequest> = vec![];

        // If import plugin is enabled, resolve imports from frontmatter
        if let Some(resolver) = &self.resolver {
            let dir = path.parent().unwrap();
            resolved_module_requests = module_record
                .requested_modules
                .keys()
                .filter_map(|specifier| {
                    let resolution = resolver.resolve(dir, specifier).ok()?;
                    Some(ResolvedModuleRequest {
                        specifier: specifier.clone(),
                        resolved_requested_path: Arc::<OsStr>::from(resolution.path().as_os_str()),
                    })
                })
                .collect();
        }

        let section_source = JavaScriptSource::new(source_text, source_type);
        if let Some(sections) = &mut out_sections {
            sections.push(SectionContent {
                source: section_source,
                semantic: Some(semantic),
                parser_tokens: None,
                astro_section_map: Some(astro_section_map),
                astro_frontmatter_offset,
            });
        }

        smallvec::smallvec![Ok(ResolvedModuleRecord { module_record, resolved_module_requests })]
    }
}

/// Recursively collect comments from all `AstroScript` nodes in an Astro JSX body.
///
/// Astro `<script>` blocks are represented in the AST as `JSXElement` nodes whose
/// children contain a single `JSXChild::AstroScript`.  The script's `Program` holds
/// its own `comments` vec with byte spans that are already absolute (relative to the
/// start of the full `.astro` source text), so they can be merged directly with
/// frontmatter comments and fed to `DisableDirectivesBuilder`.
#[cfg(feature = "astro")]
fn collect_astro_script_comments<'a>(
    children: &oxc_allocator::Vec<'a, oxc_ast::ast::JSXChild<'a>>,
    out: &mut oxc_allocator::Vec<'a, oxc_ast::ast::Comment>,
) {
    use oxc_ast::ast::JSXChild;

    for child in children {
        match child {
            JSXChild::AstroScript(script) => {
                out.extend_from_slice(&script.program.comments);
            }
            JSXChild::Element(el) => {
                // <script> blocks are wrapped as JSXElement children containing AstroScript
                collect_astro_script_comments(&el.children, out);
            }
            JSXChild::Fragment(frag) => {
                collect_astro_script_comments(&frag.children, out);
            }
            _ => {}
        }
    }
}

/// Build an [`AstroSectionMap`] by walking the JSX body of a parsed Astro file.
///
/// Each entry is `(content_start, content_end, insert_offset)` where:
/// - `content_start..content_end` is the byte range of the `<script>` block's content
///   (excluding the `<script>` and `</script>` tags) in the full `.astro` source text.
/// - `insert_offset` is the byte offset of the `\n` that terminates the `<script>`
///   opening tag, passed to `get_section_insert_position` which inserts after it
///   (i.e. on the first line of the block's content), matching Vue/Svelte behaviour.
///
/// **Important**: we use `script.span` (the whole-element absolute span) rather than
/// `script.program.span` because `parse_astro_scripts` replaces placeholder programs
/// with re-parsed versions whose `program.span` starts at 0, not at the original
/// content offset.  `script.span` is set by the JSX parser and is always absolute.
///
/// Entries are returned sorted by `content_start`.
#[cfg(feature = "astro")]
fn build_astro_section_map(
    source_text: &str,
    children: &oxc_allocator::Vec<'_, oxc_ast::ast::JSXChild<'_>>,
) -> AstroSectionMap {
    let mut map = AstroSectionMap::new();
    collect_script_spans(source_text, children, &mut map);
    map.sort_unstable_by_key(|&(start, _, _)| start);
    map
}

/// Recursive helper for [`build_astro_section_map`].
#[cfg(feature = "astro")]
fn collect_script_spans(
    source_text: &str,
    children: &oxc_allocator::Vec<'_, oxc_ast::ast::JSXChild<'_>>,
    out: &mut AstroSectionMap,
) {
    use oxc_ast::ast::JSXChild;

    for child in children {
        match child {
            JSXChild::AstroScript(script) => {
                // `script.span` covers the entire `<script>...</script>` element with
                // absolute byte offsets into the full `.astro` source.
                //
                // We find the first `>` inside `script.span` — that's the end of the
                // opening tag.  The byte right after it is either `\n` (line-break after
                // `<script>`) or the first character of content.  We set `insert_offset`
                // to that `>` position so `get_section_insert_position` detects the `\n`
                // and inserts the disable comment after it (matching Vue/Svelte logic).
                let tag_start = script.span.start as usize;
                let tag_end = script.span.end as usize;
                let bytes = source_text.as_bytes();

                // Find the `>` that closes the opening tag.
                let Some(gt_offset) = bytes[tag_start..tag_end].iter().position(|&b| b == b'>')
                else {
                    continue;
                };

                // Content starts right after `>`.
                let content_start = (tag_start + gt_offset) as u32 + 1;

                // Content ends at the start of `</script`.
                let closing = b"</script";
                let content_end = if let Some(rest) =
                    source_text.as_bytes().get(content_start as usize..tag_end)
                {
                    if let Some(close_offset) =
                        rest.windows(closing.len()).position(|w| w == closing)
                    {
                        content_start + close_offset as u32
                    } else {
                        // Malformed — treat the whole remaining span as content
                        tag_end as u32
                    }
                } else {
                    tag_end as u32
                };

                // `insert_offset` is passed as `section_offset` to the LSP code-action
                // helpers.  When `section_offset == target_offset` and
                // `bytes[section_offset] == '\n'`, `get_section_insert_position` returns
                // `("", section_offset + 1)` — i.e. it inserts right after the newline
                // at the top of the script content.  Setting `insert_offset = content_start`
                // (the byte right after `>`, which is `\n`) achieves this.
                let insert_offset = content_start;

                out.push((content_start, content_end, insert_offset));
            }
            JSXChild::Element(el) => {
                collect_script_spans(source_text, &el.children, out);
            }
            JSXChild::Fragment(frag) => {
                collect_script_spans(source_text, &frag.children, out);
            }
            _ => {}
        }
    }
}

/// Given an [`AstroSectionMap`] and a diagnostic byte offset, return the correct
/// `section_offset` to use for code-action insertion:
/// - If the offset falls inside a `<script>` block, return that block's `insert_offset`.
/// - Otherwise return `None` (caller uses the default frontmatter `section_offset`).
#[cfg(feature = "astro")]
fn astro_section_offset_for(map: &AstroSectionMap, span_start: u32) -> Option<u32> {
    // Binary search for the last entry whose content_start <= span_start
    let idx = map.partition_point(|&(start, _, _)| start <= span_start);
    if idx == 0 {
        return None;
    }
    let (content_start, content_end, insert_offset) = map[idx - 1];
    if span_start >= content_start && span_start < content_end { Some(insert_offset) } else { None }
}

/// Post-correct `section_offset` on each Astro diagnostic message.
///
/// Since the Astro linting pass uses a single [`ContextSubHost`] (unlike Vue/Svelte which
/// have one per script block), all messages start with `section_offset = 0`.  This function:
///
/// - Skips file-level diagnostics (span `(0,0)`) — leaving their `section_offset = 0` so
///   the existing guard in `error_with_position.rs` correctly suppresses code actions.
/// - Sets `section_offset` to the script block's `insert_offset` for diagnostics whose
///   `span.start` falls inside a `<script>` block.
/// - Sets `section_offset` to `frontmatter_offset` for all other diagnostics (frontmatter,
///   template body), so that "disable for this whole file" inserts inside the frontmatter
///   section rather than before the opening `---` fence.
#[cfg(feature = "astro")]
fn apply_astro_section_offsets(
    messages: &mut Vec<Message>,
    map: &AstroSectionMap,
    frontmatter_offset: u32,
) {
    for msg in messages {
        // File-level diagnostics (e.g. `filename-case`) have a zero-length span at offset 0.
        // Assigning a non-zero `section_offset` to them would cause a panic in the LSP code
        // action generation (which slices `bytes[section_offset..error_offset]`), and the
        // existing guard that suppresses ignore-fixes for such diagnostics relies on
        // `error_offset == section_offset`.  Leave them unchanged.
        if msg.span.start == 0 && msg.span.end == 0 {
            continue;
        }
        msg.section_offset =
            astro_section_offset_for(map, msg.span.start).unwrap_or(frontmatter_offset);
    }
}
