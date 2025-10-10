//! Error types.

use std::{
    error::Error,
    fmt::Display,
    marker::PhantomCovariantLifetime,
    num::{ParseFloatError, ParseIntError},
};

use crate::{
    Str,
    ir::{Location, parse::tokenizer::RawToken},
};

/// Define an error type.
macro_rules! define_error {
    {
        $($vis:vis enum $name:ident $(< $($gen:tt : $trait:ident),* $(,)? >)? {
            $(
                $(#[$meta:meta])*
                $variant_name:ident $(($($fmt_arg:ident: $fmt_type:ty),*))? = $variant_value:literal
            ),* $(,)?
        })*
    } => {
        $(
            #[doc = concat!("The ", stringify!($name), " error type.")]
            #[derive(Debug)]
            $vis enum $name <'a $(, $($gen),+)? >
                $(where $($gen: $trait),+)? {
                $(
                    $(#[$meta])*
                    $variant_name $(($($fmt_type),*))?
                ),+
                ,
                /// Makes the compiler not be angry when a lifetime is unused.
                _PhantomLifetime(PhantomCovariantLifetime<'a>)
            }

            impl <'a $(, $($gen : $trait),+)?> Display for $name <'a $(, $($gen),+)? > {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match &self {
                        $(
                            Self::$variant_name $(($($fmt_arg),*))? => f.write_fmt(
                                format_args!($variant_value)
                            )
                        ),*
                        , Self::_PhantomLifetime(_) => Ok(())
                    }
                }
            }

            impl <'a $(, $($gen : $trait),+)?> Error for $name <'a $(, $($gen),+)? > {}
        )*
    };
}

define_error! {
    pub enum ParseVersionError {
        /// Output when the `major` component is missing. Means that `minor` and `patch` are also missing.
        MissingComponentMajor = "missing components major, minor, and patch",
        /// Output when the `minor` component is missing. Means that `patch` is also missing.
        MissingComponentMinor = "missing components minor and patch",
        /// Output when the `patch` component is missing.
        MissingComponentPatch = "missing component patch",
        /// Output when there was an issue parsing the `major` component.
        ParsingComponentMajorError(err: std::num::ParseIntError) = "error parsing component major: {err}",
        /// Output when there was an issue parsing the `minor` component.
        ParsingComponentMinorError(err: std::num::ParseIntError) = "error parsing component minor: {err}",
        /// Output when there was an issue parsing the `patch` component.
        ParsingComponentPatchError(err: std::num::ParseIntError) = "error parsing component patch: {err}",
    }
    pub enum IRWrongValueError {
        /// Returned when the (value) isn't the expected type
        IsntExpectedValue(ty: &'static str, value: &'static str) = "{ty} isn't {value}",
    }

    pub enum IRTokenizerError<E: Error> {
        /// An error from the IO stream.
        IOError(err: E) = "IO error: {err}",
        /// An invalid character provided.
        InvalidCharacter(ch: char, loc: Location<'a>, note: &'static str) = "invalid character {ch} at {loc}: {note}"
    }
    pub enum IRParserError {
        /// An invalid token was encountered when attempting to parse something.
        UnexpectedToken(tok: RawToken<'a>, loc: Location<'a>, note: &'static str) = "unexpected token {tok:?} at {loc}: {note}",
        /// An error was encountered when parsing an integer literal.
        ParseIntError(err: ParseIntError, tok: RawToken<'a>, loc: Location<'a>) = "error parsing integer literal ({tok:?}) at {loc}: {err}",
        /// An error was encountered when parsing an float literal.
        ParseFloatError(err: ParseFloatError, tok: RawToken<'a>, loc: Location<'a>) = "error parsing float literal ({tok:?}) at {loc}: {err}",
        /// An unexpected metadata value was passed.
        UnexpectedMetaValue(note: Str<'a>) = "unexpected metadata value: {note}",
        /// An invalid value in the token was passed.
        UnexpectedValue(loc: Location<'a>, token_type: &'static str, note: Str<'a>) = "invalid {token_type} at {loc}: {note}",
        /// A generic std error.
        GenericError(loc: Location<'a>, token_type: &'static str, err: Box<dyn std::error::Error>) = "invalid {token_type} at {loc}: {err}",
        /// An unexpectedly early EOF.
        UnexpectedEOF(loc: Location<'a>, note: Str<'a>) = "unexpected EOF at {loc}: {note}"
    }
    pub enum TransformerError {
        /// A generic std error.
        GenericError(loc: Location<'a>, ty: &'static str, err: Box<dyn std::error::Error>) = "invalid {ty} at {loc}: {err}",
    }
    pub enum CodegenError {
        /// Returned if there are zero codegen units specified.
        ZeroCodegenUnits = "zero codegen units specified",
        /// A generic std error.
        GenericError(loc: Str<'a>, err: Box<dyn std::error::Error>) = "codegen error at {loc}: {err}",
        /// Returned if this codegen cannot compile to assembly.
        CannotCompileToAsm = "cannot compile to assembly"
    }
}

impl<E: Error> From<E> for IRTokenizerError<'_, E> {
    fn from(value: E) -> Self {
        IRTokenizerError::IOError(value)
    }
}
