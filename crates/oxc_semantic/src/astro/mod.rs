//! Astro-specific semantic analysis.
//!
//! This module contains all Astro-related semantic analysis functionality,
//! including building semantic information for Astro files with frontmatter
//! and isolated script scopes.

mod builder;
mod visit;

pub use builder::SemanticBuilderAstroExt;
pub use visit::visit_astro_script;
