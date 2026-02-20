//! Astro-specific semantic builder extensions.

// The control_flow macro has multiple definitions (depending on which cfg features are enabled).
// Only one is used at a time, so allow unused_macros.
#![allow(unused_macros)]

use oxc_ast::{AstKind, Comment, ast::*};
use oxc_ast_visit::Visit;
#[cfg(feature = "cfg")]
use oxc_cfg::{ControlFlowGraphBuilder, ErrorEdgeKind};
use oxc_span::{Ident, SourceType};
use oxc_syntax::{reference::ReferenceFlags, scope::ScopeFlags};

#[cfg(feature = "linter")]
use crate::jsdoc::JSDocBuilder;
use crate::{Semantic, SemanticBuilder, SemanticBuilderReturn, stats::Stats};

#[cfg(feature = "cfg")]
macro_rules! control_flow {
    ($builder:ident, |$cfg:tt| $body:expr) => {
        if let Some($cfg) = &mut $builder.cfg { $body } else { Default::default() }
    };
}

#[cfg(not(feature = "cfg"))]
macro_rules! control_flow {
    ($builder:ident, |$cfg:tt| $body:expr) => {{
        #[expect(clippy::unused_unit)]
        {
            let _ = $builder; // Suppress unused variable warning
            ()
        }
    }};
}

#[cfg(not(feature = "cfg"))]
macro_rules! control_flow {
    ($builder:ident, |$cfg:tt| $body:expr) => {{
        #[expect(clippy::unused_unit)]
        {
            let _ = $builder; // Suppress unused variable warning
            ()
        }
    }};
}

#[cfg(not(feature = "cfg"))]
macro_rules! control_flow {
    ($self:ident, |$cfg:tt| $body:expr) => {{
        #[expect(clippy::unused_unit)]
        {
            let _ = $self; // Suppress unused variable warning
            ()
        }
    }};
}

/// Extension trait for SemanticBuilder to handle Astro files.
pub trait SemanticBuilderAstroExt<'a> {
    /// Build semantic information for an Astro file.
    ///
    /// This processes the frontmatter Program (if present) and JSX body, creating a unified scope
    /// where frontmatter variables are accessible in JSX expressions.
    fn build_astro(
        self,
        root: &'a AstroRoot<'a>,
        source_text: &'a str,
        comments: &'a oxc_allocator::Vec<'a, Comment>,
    ) -> SemanticBuilderReturn<'a>;
}

impl<'a> SemanticBuilderAstroExt<'a> for SemanticBuilder<'a> {
    fn build_astro(
        mut self,
        root: &'a AstroRoot<'a>,
        source_text: &'a str,
        comments: &'a oxc_allocator::Vec<'a, Comment>,
    ) -> SemanticBuilderReturn<'a> {
        self.source_text = source_text;
        // Astro files are TypeScript + JSX
        self.source_type = SourceType::ts().with_jsx(true);

        #[cfg(feature = "linter")]
        {
            self.jsdoc = JSDocBuilder::new(self.source_text, comments);
        }

        // Use stats from frontmatter Program for capacity estimation
        // Parser always creates a frontmatter (synthetic if needed) so unwrap is safe
        let frontmatter =
            root.frontmatter.as_ref().expect("Astro parser should always create frontmatter");
        let stats = if let Some(stats) = self.stats {
            stats
        } else {
            Stats::count(&frontmatter.program).increase_by(self.excess_capacity + 0.5)
        };
        self.nodes.reserve(stats.nodes as usize);
        self.scoping.reserve(
            stats.symbols as usize,
            stats.references as usize,
            stats.scopes as usize,
        );

        // Visit the Astro AST - frontmatter's Program is the root, JSX body is visited in same scope
        astro_visit_frontmatter_with_body(&mut self, frontmatter, &root.body);

        debug_assert_eq!(self.unresolved_references.scope_depth(), 1);
        self.scoping
            .set_root_unresolved_references(self.unresolved_references.into_root().into_iter());

        #[cfg(feature = "linter")]
        let jsdoc = self.jsdoc.build();

        #[cfg(debug_assertions)]
        self.unused_labels.assert_empty();

        let semantic = Semantic {
            source_text: self.source_text,
            source_type: self.source_type,
            comments,
            irregular_whitespaces: [].into(),
            nodes: self.nodes,
            scoping: self.scoping,
            classes: self.class_table_builder.build(),
            #[cfg(feature = "linter")]
            jsdoc,
            unused_labels: self.unused_labels.labels,
            #[cfg(feature = "cfg")]
            cfg: self.cfg.map(ControlFlowGraphBuilder::build),
            #[cfg(not(feature = "cfg"))]
            cfg: (),
        };
        SemanticBuilderReturn { semantic, errors: self.errors.into_inner() }
    }
}

/// Visit Astro with frontmatter - use frontmatter's Program as root.
fn astro_visit_frontmatter_with_body<'a>(
    builder: &mut SemanticBuilder<'a>,
    frontmatter: &AstroFrontmatter<'a>,
    body: &oxc_allocator::Vec<'a, JSXChild<'a>>,
) {
    let program = &frontmatter.program;
    astro_setup_root_program(builder, program);

    // Visit frontmatter program contents
    if let Some(hashbang) = &program.hashbang {
        builder.visit_hashbang(hashbang);
    }
    for directive in &program.directives {
        builder.visit_directive(directive);
    }
    builder.visit_statements(&program.body);

    // Visit the JSX body within the same scope
    builder.visit_jsx_children(body);

    astro_finish_root_program(builder);
}

/// Set up the root Program node and scope for Astro semantic analysis.
fn astro_setup_root_program<'a>(builder: &mut SemanticBuilder<'a>, program: &Program<'a>) {
    let prog_kind = AstKind::Program(builder.alloc(program));

    #[cfg(feature = "cfg")]
    control_flow!(builder, |cfg| {
        cfg.attach_error_harness(ErrorEdgeKind::Implicit);
        let _ = cfg.new_basic_block_normal();
    });

    builder.current_node_id = builder.nodes.add_program_node(
        prog_kind,
        builder.current_scope_id,
        #[cfg(feature = "cfg")]
        control_flow!(builder, |cfg| cfg.current_node_ix),
        builder.current_node_flags,
    );

    let mut flags = ScopeFlags::Top;
    if builder.source_type.is_strict() || program.has_use_strict_directive() {
        flags |= ScopeFlags::StrictMode;
    }
    builder.current_scope_id = builder.scoping.add_scope(None, builder.current_node_id, flags);
    program.scope_id.set(Some(builder.current_scope_id));
}

/// Finish the root Program - resolve references and pop the node.
fn astro_finish_root_program(builder: &mut SemanticBuilder<'_>) {
    builder.resolve_references_for_current_scope();
    builder.pop_ast_node();
}

/// Resolve references for an Astro script scope.
///
/// Unlike `resolve_references_for_current_scope`, this sends unresolved references
/// directly to the root scope (depth 1) instead of the immediate parent scope.
/// This provides proper isolation for Astro scripts which run in the browser
/// and cannot access frontmatter variables.
fn resolve_references_for_astro_script(builder: &mut SemanticBuilder<'_>) {
    let current_refs = builder.unresolved_references.current_mut();
    let mut unresolved_for_root: Vec<(Ident<'_>, crate::unresolved_stack::ReferenceIds)> =
        Vec::new();

    for (name, mut references) in current_refs.drain() {
        // Try to resolve references against bindings in the current (script) scope
        let bindings = builder.scoping.get_bindings(builder.current_scope_id);
        if let Some(symbol_id) = bindings.get(name.as_str()).copied() {
            let symbol_flags = builder.scoping.symbol_flags(symbol_id);
            references.retain(|reference_id| {
                let reference_id = *reference_id;
                let reference = &mut builder.scoping.references[reference_id];

                let flags = reference.flags_mut();

                let resolved = if flags.is_namespace()
                    && !flags.is_value()
                    && !flags.is_value_as_type()
                    && !symbol_flags.can_be_referenced_as_namespace()
                {
                    false
                } else {
                    (flags.is_value() && symbol_flags.can_be_referenced_by_value())
                        || (flags.is_type() && symbol_flags.can_be_referenced_by_type())
                        || (flags.is_value_as_type()
                            && symbol_flags.can_be_referenced_by_value_as_type())
                };

                if !resolved {
                    return true;
                }

                if symbol_flags.is_value() && flags.is_value() {
                    *flags -= ReferenceFlags::Type;
                } else {
                    *flags = ReferenceFlags::Type;
                }
                reference.set_symbol_id(symbol_id);
                builder.scoping.add_resolved_reference(symbol_id, reference_id);

                false
            });

            if references.is_empty() {
                continue;
            }
        }

        // Unresolved references go to root, not parent
        unresolved_for_root.push((name, references));
    }

    // Add unresolved references directly to the root scope (depth 1)
    // This bypasses the frontmatter scope
    let root_refs = builder.unresolved_references.root_mut();
    for (name, references) in unresolved_for_root {
        if let Some(root_reference_ids) = root_refs.get_mut(&name) {
            root_reference_ids.extend(references);
        } else {
            root_refs.insert(name, references);
        }
    }
}

/// Custom visitor for AstroScript that creates an isolated scope.
///
/// Astro `<script>` tags run in the browser and cannot access frontmatter variables.
/// This method creates a scope that doesn't inherit from the frontmatter scope -
/// unresolved references go directly to the root scope (globals) rather than
/// resolving against frontmatter bindings.
pub fn visit_astro_script<'a>(builder: &mut SemanticBuilder<'a>, script: &AstroScript<'a>) {
    let kind = AstKind::AstroScript(builder.alloc(script));
    builder.create_ast_node(kind);

    // Create a new scope for this script
    // Use Top flag to indicate this is a top-level scope for the script
    builder.enter_scope(
        ScopeFlags::Top | ScopeFlags::Function | ScopeFlags::StrictMode,
        &script.program.scope_id,
    );

    // Visit the script's program contents
    if let Some(hashbang) = &script.program.hashbang {
        builder.visit_hashbang(hashbang);
    }
    for directive in &script.program.directives {
        builder.visit_directive(directive);
    }
    builder.visit_statements(&script.program.body);

    // Resolve references within this script's scope, but send unresolved ones
    // directly to the root scope (not to the frontmatter parent scope)
    resolve_references_for_astro_script(builder);

    // Restore parent scope manually (don't use leave_scope which would resolve again)
    let parent_id = builder.scoping.scope_parent_id(builder.current_scope_id);
    if let Some(parent_id) = parent_id {
        builder.current_scope_id = parent_id;
    }
    builder.unresolved_references.decrement_scope_depth();

    builder.pop_ast_node();
}
