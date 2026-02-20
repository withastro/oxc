//! Astro-specific Visit trait extensions.
//!
//! This module provides the `visit_astro_script` implementation that integrates
//! with the SemanticBuilder's Visit trait implementation.

use oxc_ast::ast::AstroScript;

use crate::SemanticBuilder;

/// Visit an AstroScript node with proper scope isolation.
///
/// This is called from the Visit trait implementation for SemanticBuilder
/// when the astro feature is enabled.
#[inline]
pub fn visit_astro_script<'a>(builder: &mut SemanticBuilder<'a>, script: &AstroScript<'a>) {
    super::builder::visit_astro_script(builder, script);
}
