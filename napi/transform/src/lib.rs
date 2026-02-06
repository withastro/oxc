#[cfg(all(
    feature = "allocator",
    not(any(
        target_arch = "arm",
        target_os = "freebsd",
        target_os = "windows",
        target_family = "wasm"
    ))
))]
#[global_allocator]
static ALLOC: mimalloc_safe::MiMalloc = mimalloc_safe::MiMalloc;

#[cfg(feature = "astro")]
mod astro;
#[cfg(feature = "astro")]
pub use astro::*;

mod isolated_declaration;
pub use isolated_declaration::*;

mod transformer;
pub use transformer::*;
