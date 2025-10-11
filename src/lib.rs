//! `qu` codegen
#![warn(
    missing_docs,
    clippy::missing_docs_in_private_items,
    clippy::pedantic,
    clippy::all,
    clippy::ignore_without_reason,
    clippy::alloc_instead_of_core,
    clippy::as_underscore,
    clippy::assertions_on_result_states,
    clippy::panic,
    clippy::unwrap_used,
    clippy::missing_asserts_for_indexing
)]
#![allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss,
    clippy::cast_possible_wrap,
    clippy::too_many_lines,
    unused_parens, // needed in some macro expansions
    reason = "unwanted"
)]
#![feature(trim_prefix_suffix)]
#![feature(phantom_variance_markers)]
#![feature(mpmc_channel)]
#![feature(more_qualified_paths)]
#![feature(exit_status_error)]

use std::{
    borrow::Cow,
    str::FromStr,
};

pub mod codegen;
pub mod errors;
pub mod ir;
pub mod transformers;

#[expect(clippy::doc_markdown, reason = "CoW is not an item")]
/// A type alias for CoW strings.
pub type Str<'a> = Cow<'a, str>;
#[expect(clippy::doc_markdown, reason = "CoW is not an item")]
/// A type alias for CoW lists.
pub type List<'a, T> = Cow<'a, [T]>;

/// Supported architectures.
pub const ARCHS: &[&str] = &["x86_64"];

/// Supported targets.
pub const TARGETS: &[&str] = &["x86_64-linux"];

/// A semver-style version.
#[derive(Clone, Debug, Default, PartialOrd, Ord, Hash)]
#[must_use]
pub struct Version<'a> {
    /// Specifies strict equality. If unsure, set to false.
    pub strict: bool,
    /// The major component.
    pub major: u16,
    /// The minor component.
    pub minor: u16,
    /// The patch component.
    pub patch: u16,
    /// The prerelease metadata.
    pub prerelease: Option<Str<'a>>,
    /// The build metadata. If it exists, then it enforces strict equality.
    pub build: Option<Str<'a>>,
}

impl Into<OwnedVersion> for Version<'_> {
    fn into(self) -> OwnedVersion {
        OwnedVersion {
            strict: self.strict,
            major: self.major,
            minor: self.minor,
            patch: self.patch,
            prerelease: self.prerelease.clone().map(|v| v.into_owned()),
            build: self.build.clone().map(|v| v.into_owned()),
        }
    }
}

/// A semver-style version.
#[derive(Clone, Debug, Default, PartialOrd, Ord, Hash)]
#[must_use]
pub struct OwnedVersion {
    /// Specifies strict equality. If unsure, set to false.
    pub strict: bool,
    /// The major component.
    pub major: u16,
    /// The minor component.
    pub minor: u16,
    /// The patch component.
    pub patch: u16,
    /// The prerelease metadata.
    pub prerelease: Option<String>,
    /// The build metadata. If it exists, then it enforces strict equality.
    pub build: Option<String>,
}

impl Eq for OwnedVersion {}

impl PartialEq for OwnedVersion {
    fn eq(&self, other: &Self) -> bool {
        // ignores OwnedVersion::strict
        self.major == other.major
            && self.minor == other.minor
            && self.patch == other.patch
            && self.prerelease == other.prerelease
            && self.build == other.build
    }
}

/// Convinence macro for defining a [`Version`].
#[macro_export]
macro_rules! version {
    ($major:expr, $minor:expr, $patch:expr) => {{
        $crate::Version {
            strict: false,
            major: $major,
            minor: $minor,
            patch: $patch,
            prerelease: None,
            build: None,
        }
    }};
    ($strict:expr, $major:expr, $minor:expr, $patch:expr) => {{
        $crate::Version {
            strict: $strict,
            major: $major,
            minor: $minor,
            patch: $patch,
            prerelease: None,
            build: None,
        }
    }};
}

impl Eq for Version<'_> {}

impl PartialEq for Version<'_> {
    fn eq(&self, other: &Self) -> bool {
        // ignores Version::strict
        self.major == other.major
            && self.minor == other.minor
            && self.patch == other.patch
            && self.prerelease == other.prerelease
            && self.build == other.build
    }
}

impl<'a> FromStr for Version<'a> {
    type Err = errors::ParseVersionError<'a>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut s = s.trim_prefix('v');
        let strict = s.starts_with('=');
        if strict {
            s = s.trim_prefix('=');
        }
        let mut components = s.split('.');

        let major: u16 = components
            .next()
            .ok_or(errors::ParseVersionError::MissingComponentMajor)?
            .parse()
            .map_err(errors::ParseVersionError::ParsingComponentMajorError)?;

        let minor: u16 = components
            .next()
            .ok_or(errors::ParseVersionError::MissingComponentMinor)?
            .parse()
            .map_err(errors::ParseVersionError::ParsingComponentMinorError)?;

        let last_component = s
            .split_once('.')
            .unwrap()
            .1
            .split_once('.')
            .ok_or(errors::ParseVersionError::MissingComponentPatch)?
            .1;

        let (comp_without_prerelease, prerelease) = last_component
            .split_once('-')
            .unwrap_or((last_component, ""));
        let (patch_s, build) = comp_without_prerelease
            .split_once('+')
            .unwrap_or((comp_without_prerelease, ""));

        let patch: u16 = patch_s
            .parse()
            .map_err(errors::ParseVersionError::ParsingComponentPatchError)?;

        Ok(Self {
            strict,
            major,
            minor,
            patch,
            prerelease: if prerelease.is_empty() {
                None
            } else {
                Some(prerelease.to_string().into())
            },
            build: if build.is_empty() {
                None
            } else {
                Some(build.to_string().into())
            },
        })
    }
}

impl Version<'_> {
    /// Get Qu's version.
    #[allow(clippy::missing_panics_doc, reason = "false positive")]
    pub fn crate_version() -> Self {
        env!("CARGO_PKG_VERSION").parse().unwrap()
    }
    /// Tests for if two versions are semver compatible.
    #[must_use]
    pub fn compatible(&self, other: &Self) -> bool {
        if self.build.is_some() || other.build.is_some() || self.strict || other.strict {
            self == other
        } else if self.major != 0 && other.major != 0 {
            self.major == other.major
        } else {
            self.major == other.major && self.minor == other.minor
        }
    }
}

/// Implement an is function.
macro_rules! impl_is {
    ($(#[$attrs:meta])* $p:pat, $name:ident) => {
        paste::paste! {
            #[doc = concat!("Returns whether this is a ", stringify!($ty), ".")]
            $(#[$attrs])*
            #[allow(unused_variables)]
            pub fn [< is_ $name >](&self) -> bool {
                matches!(self, $p)
            }
        }
    };
}

use impl_is;

/// Implement an unwrap function.
macro_rules! impl_unwrap {
    ($(#[$attrs:meta])* $parent:ty, $t:ty, $ty:ty, $p:pat => $e:expr, $name:ident) => {
        paste::paste! {
            #[doc = concat!("Returns the ", stringify!($t), " value if it has one.")]
            $(#[$attrs])*
            #[doc = ""]
            #[doc = "# Panics"]
            #[doc = concat!("If this value is not a [`", stringify!($ty), "`], then this will panic")]
            #[allow(clippy::double_must_use, unused_variables)]
            #[must_use]
            pub fn [< unwrap_ $name >](self) -> $t {
                match self {
                    $p => {$e},
                    _ => panic!(concat!(
                        "`", stringify!($parent), "::unwrap_", stringify!($t), "` attempted on non-`", stringify!($t),"` value"
                    )),
                }
            }

            #[doc = concat!("Returns the ", stringify!($t), " value if it has one.")]
            $(#[$attrs])*
            #[doc = ""]
            #[doc = "# Errors"]
            #[doc = concat!("If this value is not a [`", stringify!($ty), "`], then this will return a
                             [`IRWrongValueError`](crate::errors::IRWrongValueError).")]
            #[allow(unused_variables)]
            pub fn [< ok_ $name >](self) -> Result<$t, $crate::errors::IRWrongValueError<'a>> {
                match self {
                    $p => Ok({$e}),
                    _ => Err($crate::errors::IRWrongValueError::IsntExpectedValue(
                        stringify!($parent), stringify!($t)
                    )),
                }
            }

            #[doc = concat!("Returns the ", stringify!($t), " value if it has one.")]
            $(#[$attrs])*
            #[doc = ""]
            #[doc = concat!("If this value is not a [`", stringify!($ty), "`], then this will return None.")]
            #[allow(unused_variables)]
            pub fn [< some_ $name >](self) -> Option<$t> {
                match self {
                    $p => Some({$e}),
                    _ => None,
                }
            }
        }
    };
    ($(#[$attrs:meta])* $parent:ty, $t:ty, $ty:path, $name:ident) => {
        impl_unwrap!($(#[$attrs])* $parent, $t, $ty, $ty (v) => v, $name);
        $crate::ir::impl_is!($(#[$attrs])* $ty (v), $name);
    };
}

use impl_unwrap;
