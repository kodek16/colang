//! CO standard library definition and implementation.
//!
//! Standard library is currently embedded in the compiler. It is treated the same way as
//! other source files that take part in compilation, but its code is not present in the filesystem.
//! Snippets from standard library source code are shown in error messages normally.

/// CO standard library source code.
pub const STD_SOURCE: &'static str = include_str!("std.co");
