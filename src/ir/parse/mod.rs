//! The parser for QIR. Includes tokenization and the actual parsing.

use super::{
    CallingConvention, ConstValue, ExtendedVarRef, ExternalFunctionSignature, FunctionAnnotation,
    FunctionCall, FunctionDef, FunctionHint, InternalFunctionSignature, ModuleAnnotation,
    Primitive, RetType, RuntimeCheck, Str, StructAnnotation, StructDef, Type, Value, VarRef,
    Version,
    code::{AssemblyOpt, AssemblyOption, Block, Cond, NoProdInstruction, ProdInstruction},
};
use crate::{errors::IRParserError, ir::ModuleItem};
use tokenizer::{RawToken, StrType, Token};

pub mod tokenizer;

/// Trait for parsing IR.
pub trait Parse<'a>: Sized {
    /// Metadata needed to properly parse.
    type Meta: Default;
    /// The default meta value.
    #[must_use]
    fn default_meta() -> Self::Meta {
        Default::default()
    }
    /// Actually do the parsing. Returns the parsed value and the number of tokens
    /// to remove from the front.
    ///
    /// # Errors
    /// Implementation-specific; should not panic.
    fn parse(meta: Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>>;

    /// Output debug info after parsing.
    ///
    /// # Errors
    /// Propogates errors from [`Parse::parse`].
    fn debug_output(
        meta: Self::Meta,
        input: &'a [Token],
    ) -> Result<(Self, usize), IRParserError<'a>>
    where
        Self: std::fmt::Debug,
    {
        let (val, len) = Self::parse(meta, input)?;
        eprintln!("{val:#?} (length {len})");
        Ok((val, len))
    }
}

/// Bail out of a parse function.
macro_rules! bail {
    (UnexpectedToken($tok:expr, $s:expr $(,)?)) => {{
        let tok: &Token = &($tok);
        return Err(IRParserError::UnexpectedToken(
            tok.raw.clone(),
            tok.loc.clone(),
            $s,
        ));
    }};
    (ParseIntError($tok:expr, $err:expr $(,)?)) => {{
        let tok: &Token = &($tok);
        return Err(IRParserError::ParseIntError(
            $err,
            tok.raw.clone(),
            tok.loc.clone(),
        ));
    }};
    (ParseFloatError($tok:expr, $err:expr $(,)?)) => {{
        let tok: &Token = &($tok);
        return Err(IRParserError::ParseFloatError(
            $err,
            tok.raw.clone(),
            tok.loc.clone(),
        ));
    }};
    (UnexpectedMetaValue($s:expr $(,)?)) => {
        return Err(IRParserError::UnexpectedMetaValue(($s).into()))
    };
    (UnexpectedValue($loc:expr, $ty:literal, $note:expr $(,)?)) => {
        return Err(IRParserError::UnexpectedValue($loc, $ty, ($note).into()))
    };
    (GenericError($loc:expr, $ty:literal, $err:expr $(,)?)) => {
        return Err(IRParserError::GenericError($loc, $ty, Box::new($err)))
    };
    (UnexpectedEOF($loc:expr, $note:expr $(,)?)) => {
        return Err(IRParserError::UnexpectedEOF($loc, ($note).into()))
    };
}

/// Expect a token to be a certain pattern.
macro_rules! expect_token {
    ($tok:expr, $ty:pat) => {
        if !matches!($tok.raw, $ty) {
            bail!(UnexpectedToken(
                $tok,
                concat!("expected a ", stringify!($ty))
            ));
        }
    };
}

/// Parse a comma separated pattern. The first token should be an `OpenParen`
/// and the last a `CloseParen`.
macro_rules! parse_separated {
    {
        $input:expr => $t:ty {
            $value_matcher:pat $(if $guard:expr)? => $item_name:literal
        }
    } => {
        parse_separated! {
            (RawToken::OpenParen, RawToken::Comma, RawToken::CloseParen): $input => $t {
                $value_matcher $(if $guard)? => $item_name
            }
        }
    };
    {
        ($start_tok:pat, $sep:pat, $end_tok:pat): $input:expr => $t:ty {
            $value_matcher:pat $(if $guard:expr)? => $item_name:literal
        }
    } => {
        'bl: {
            let input = $input;
            let mut expecting_comma = false;
            let mut out = Vec::new();
            let mut i = 1usize;
            if !matches!(input[0].raw, $start_tok) {
                break 'bl Err(IRParserError::UnexpectedToken(
                    input[0].raw.clone(),
                    input[0].loc.clone(),
                    concat!("expected a ", stringify!($start_tok)),
                ))
            }
            while !matches!(input[i].raw, $end_tok) {
                #[allow(unreachable_patterns, reason="user-provided patterns")]
                match input[i].raw {
                    RawToken::Newline => {},
                    $sep => {
                        if !expecting_comma {
                            break 'bl Err(IRParserError::UnexpectedToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a ", $item_name," next, got ", stringify!($sep)),
                            ));
                        }
                        expecting_comma = false;
                    },
                    $value_matcher $(if $guard)? => {
                        if expecting_comma {
                            break 'bl Err(IRParserError::UnexpectedToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a ", stringify!($sep), " next, got ", $item_name),
                            ));
                        }
                        let (val, l) = <$t>::parse(<$t>::default_meta(), &input[i..])?;
                        out.push(val);
                        i += l - 1;
                        expecting_comma = true;
                    },
                    _ => {
                        break 'bl Err(IRParserError::UnexpectedToken(
                            input[i].raw.clone(),
                            input[i].loc.clone(),
                            concat!("expected one of ", stringify!($sep), ", ", $item_name, ", ", stringify!($end_tok)),
                        ));
                    }
                }
                i += 1;
            }
            Ok((out, i+1))
        }
    };
    {
        $input:expr => $item_name:literal {
            $($value_matcher:pat $(if $guard:expr)? => $bl:expr),+
            $(,)?
        }
    } => {
        parse_separated! {
            (RawToken::OpenParen, RawToken::Comma, RawToken::CloseParen): $input => $item_name {
                $($value_matcher $(if $guard)? => $bl),+
            }
        }
    };
    {
        ($start_tok:pat, $sep:pat, $end_tok:pat): $input:expr => $item_name:literal {
            $($value_matcher:pat $(if $guard:expr)? => $bl:expr),+
            $(,)?
        }
    } => {
        'bl: {
            let input = $input;
            let mut expecting_comma = false;
            let mut i = 1usize;
            if !matches!(input[0].raw, $start_tok) {
                break 'bl Err(IRParserError::UnexpectedToken(
                    input[0].raw.clone(),
                    input[0].loc.clone(),
                    concat!("expected a ", stringify!($start_tok)),
                ))
            }
            while !matches!(input[i].raw, $end_tok) {
                #[allow(unreachable_patterns, reason="user-provided patterns")]
                match input[i].raw {
                    $sep => {
                        if !expecting_comma {
                            break 'bl Err(IRParserError::UnexpectedToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a ", $item_name," next, got ", stringify!($sep)),
                            ));
                        }
                        expecting_comma = false;
                    }
                    $($value_matcher $(if $guard)? => {
                        if expecting_comma {
                            break 'bl Err(IRParserError::UnexpectedToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a ", stringify!($sep), " next, got ", $item_name),
                            ));
                        }
                        let l: usize = match $bl(input, i) {
                            Ok(l) => l,
                            Err(e) => break 'bl Err(e),
                        };
                        i += l.saturating_sub(1);
                        expecting_comma = true;
                    }),+
                    _ => {
                        break 'bl Err(IRParserError::UnexpectedToken(
                            input[i].raw.clone(),
                            input[i].loc.clone(),
                            concat!("expected one of ", stringify!($sep), ", ", $item_name, ", ", stringify!($end_tok)),
                        ));
                    }
                }
                i += 1;
            }
            Ok(i+1)
        }
    };
}

/// Add a newline and semicolon to the length if there is one.
macro_rules! add_newline_to_len {
    ($len:expr, $input:expr) => {
        add_newline_to_len!(newline_only, add_newline_to_len!(;, $len, $input), $input)
    };
    (newline_only, $len:expr, $input:expr) => {
        $len + usize::from($input.get($len).map(|v| &v.raw) == Some(&RawToken::Newline))
    };
    (;, $len:expr, $input:expr) => {
        ($len + usize::from($input.get($len).map(|v| &v.raw) == Some(&RawToken::Semicolon)))
    };
}

impl<'a> Parse<'a> for ConstValue<'a> {
    type Meta = Option<Primitive>;
    fn parse(meta: Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let loc = input[0].loc.clone();
        let raw = input[0].raw.clone();
        match &raw {
            RawToken::BlockRefOrAnd if input.get(1).map(|v| &v.raw) == Some(&RawToken::Drop) => Ok(Self::DropAddress),
            RawToken::StrLiteral(ty, v) => Ok(match meta {
                None => match ty {
                    tokenizer::StrType::Normal => Self::String(v.clone()),
                    tokenizer::StrType::C => Self::CString(v.clone()),
                },
                Some(Primitive::CString) => Self::CString(v.clone()),
                Some(Primitive::String) => Self::String(v.clone()),
                meta => {
                    bail!(UnexpectedMetaValue(format!(
                        "expected one of None, Some(Primitive::CString) or Some(Primitive::String); got {meta:?}"
                    )));
                }
            }),
            RawToken::NumericLiteral(v) => match meta {
                Some(Primitive::U8) => Ok(Self::U8(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::U16) => Ok(Self::U16(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::U32) => Ok(Self::U32(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::U64) => Ok(Self::U64(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::U128) => Ok(Self::U128(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::Uptr) => Ok(Self::Uptr(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),

                Some(Primitive::I8) => Ok(Self::I8(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::I16) => Ok(Self::I16(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::I32) => Ok(Self::I32(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::I64) => Ok(Self::I64(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::I128) => Ok(Self::I128(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),
                Some(Primitive::Iptr) => Ok(Self::Iptr(
                    v.parse()
                        .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                )),

                Some(Primitive::F32) => Ok(Self::F32(
                    v.parse()
                        .map_err(|e| IRParserError::ParseFloatError(e, raw, loc))?,
                )),
                Some(Primitive::F64) => Ok(Self::F64(
                    v.parse()
                        .map_err(|e| IRParserError::ParseFloatError(e, raw, loc))?,
                )),

                None => Ok(if v.contains('.') {
                    Self::F64(
                        v.parse()
                            .map_err(|e| IRParserError::ParseFloatError(e, raw, loc))?,
                    )
                } else if v.contains('-') {
                    Self::I128(
                        v.parse()
                            .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                    )
                } else {
                    Self::U128(
                        v.parse()
                            .map_err(|e| IRParserError::ParseIntError(e, raw, loc))?,
                    )
                }),

                meta => Err(IRParserError::UnexpectedMetaValue(
                    format!("expected one of None or Some({{integer type}}); got {meta:?}").into(),
                )),
            },
            tok => Err(IRParserError::UnexpectedToken(
                tok.clone(),
                loc,
                "expected a strliteral or numericliteral",
            )),
        }.map(|v| {
            let len = if v == Self::DropAddress {
                2
            } else {
                1
            };
            (v, add_newline_to_len!(len, input))
        })
    }
}

impl<'a> Parse<'a> for Primitive {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let loc = input[0].loc.clone();
        if let RawToken::Ident(ty) = &input[0].raw {
            Ok((
                match ty.as_ref().to_lowercase().as_str() {
                    "u8" => Primitive::U8,
                    "u16" => Primitive::U16,
                    "u32" => Primitive::U32,
                    "u64" => Primitive::U64,
                    "u128" => Primitive::U128,
                    "uptr" => Primitive::Uptr,

                    "i8" => Primitive::I8,
                    "i16" => Primitive::I16,
                    "i32" => Primitive::I32,
                    "i64" => Primitive::I64,
                    "i128" => Primitive::I128,
                    "iptr" => Primitive::Iptr,

                    "f32" => Primitive::F32,
                    "f64" => Primitive::F64,

                    "c_str" => Primitive::CString,
                    "str" => Primitive::String,

                    _ => {
                        bail!(UnexpectedValue(
                            loc,
                            "ident",
                            "expected a primitive type name"
                        ));
                    }
                },
                add_newline_to_len!(1, input),
            ))
        } else {
            Err(IRParserError::UnexpectedToken(
                input[0].clone().raw,
                loc,
                "expected a Token::Ident",
            ))
        }
    }
}

impl<'a> Parse<'a> for Version<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let loc = input[0].loc.clone();
        if let RawToken::StrLiteral(ty, ver) = &input[0].raw {
            if *ty == StrType::Normal {
                ver.parse()
                    .map(|v| (v, add_newline_to_len!(1, input)))
                    .map_err(|v| IRParserError::GenericError(loc, "version", Box::new(v)))
            } else {
                Err(IRParserError::UnexpectedValue(
                    loc,
                    "version",
                    format!("expected a normal string, got a {ty:?}").into(),
                ))
            }
        } else {
            Err(IRParserError::UnexpectedToken(
                input[0].clone().raw,
                loc,
                "expected a string literal",
            ))
        }
    }
}

impl<'a> Parse<'a> for RuntimeCheck {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let loc = input[0].loc.clone();

        match
            Str::parse((), input)?.0
            .as_ref().to_lowercase().as_str() {
            "array_bounds" => Ok(Self::ArrayBounds),
            "overflow" => Ok(Self::Overflow),
            "underflow" => Ok(Self::Underflow),
            "null_ptr" => Ok(Self::NullPtr),
            "unaligned_ptr" => Ok(Self::UnalignedPtr),
            check => Err(IRParserError::UnexpectedValue(loc, "runtime check", format!(
                "expected one of array_bounds, overflow, underflow, null_ptr, or unaligned_ptr; got {check}"
            ).into()))
        }.map(|v| (v, add_newline_to_len!(1, input)))
    }
}

impl<'a> Parse<'a> for Str<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if let RawToken::Ident(val) = &input[0].raw {
            Ok((
                std::borrow::Cow::Borrowed(val),
                add_newline_to_len!(1, input),
            ))
        } else {
            Err(IRParserError::UnexpectedToken(
                input[0].clone().raw,
                input[0].loc.clone(),
                "expected an ident",
            ))
        }
    }
}

impl<'a> Parse<'a> for VarRef<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 3 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a variable".into(),
            ));
        }
        assert!(input.len() > 3); // unnecessary but clippy is unaware
        expect_token!(input[0], RawToken::Variable);
        let version = if let RawToken::NumericLiteral(version) = &input[3].raw {
            Some(version.parse().map_err(|e| {
                IRParserError::ParseIntError(e, input[3].raw.clone(), input[3].loc.clone())
            })?)
        } else if input[3].raw.is_drop() {
            None
        } else {
            bail!(UnexpectedToken(
                input[3].clone(),
                "expected a variable version or _",
            ))
        };

        Str::parse((), &input[1..]).map(|v| {
            (
                VarRef { name: v.0, version },
                add_newline_to_len!(v.1 + 3, input),
            )
        })
    }
}

impl<'a> Parse<'a> for Value<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 2 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a value".into(),
            ));
        }
        assert!(input.len() > 2); // unnecessary but clippy is unaware
        if input[0].raw == RawToken::Variable && input[2].raw == RawToken::Property {
            ExtendedVarRef::parse((), input).map(|v| (Self::Variable(v.0), v.1))
        } else if let Ok(val) = ConstValue::parse(None, input) {
            Ok((Value::Constant(val.0), add_newline_to_len!(val.1, input)))
        } else if let RawToken::Ident(name) = &input[0].raw
            && name == "undef"
        {
            Ok((Value::Undef, add_newline_to_len!(1, input)))
        } else {
            Err(IRParserError::UnexpectedToken(
                input[0].raw.clone(),
                input[0].loc.clone(),
                "expected one of undef, $variable_name.version, or constant",
            ))
        }
    }
}

impl<'a> Parse<'a> for FunctionCall<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let name = Str::parse((), input)?.0;
        let (vals, len) = parse_separated! {
            &input[1..] => Value {
                RawToken::Variable
                | RawToken::StrLiteral(_, _)
                | RawToken::NumericLiteral(_)
                | RawToken::Ident(_) => "value"
            }
        }?;
        Ok((
            FunctionCall {
                func: name,
                args: vals.into(),
            },
            add_newline_to_len!(len + 1, input),
        ))
    }
}

impl<'a> Parse<'a> for ModuleAnnotation<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 4 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a module annotation".into(),
            ));
        }
        assert!(input.len() > 4); // unnecessary but clippy is unaware
        expect_token!(input[0], RawToken::ModuleAnnotation);
        expect_token!(input[2], RawToken::OpenParen);
        if let RawToken::Ident(name) = &input[1].raw {
            let name = name.as_ref().to_lowercase();
            if name.as_str() == "quver" && input[4].raw != RawToken::CloseParen {
                Err(IRParserError::UnexpectedToken(
                    input[0].raw.clone(),
                    input[0].loc.clone(),
                    "expected a close paren after a single argument for quver module annotation",
                ))
            } else {
                match name.as_str() {
                    "quver" => Ok((
                        Self::QuVer(Version::parse((), &input[3..])?.0),
                        add_newline_to_len!(5, input),
                    )),
                    "runtime_checks" => {
                        let (checks, len) = parse_separated! {
                            &input[2..] => RuntimeCheck {
                                RawToken::Ident(_) => "check"
                            }
                        }?;
                        Ok((
                            Self::RuntimeChecks(checks.into()),
                            add_newline_to_len!(len + 2, input),
                        ))
                    }
                    "check_violated" => {
                        let (calls, len) = parse_separated! {
                            &input[2..] => FunctionCall {
                                RawToken::Ident(_) => "call"
                            }
                        }?;
                        Ok((
                            Self::CheckViolated(calls.into()),
                            add_newline_to_len!(len + 2, input),
                        ))
                    }
                    name => Err(IRParserError::UnexpectedValue(
                        input[1].loc.clone(),
                        "module annotation",
                        format!(
                            "expected one of quver, runtime_checks, check_violated; got {name}"
                        )
                        .into(),
                    )),
                }
            }
        } else {
            bail!(UnexpectedToken(input[0], "expected a module annotation"));
        }
    }
}

impl<'a> Parse<'a> for CallingConvention {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if let RawToken::Ident(name) = &input[0].raw {
            match name.as_ref().to_lowercase().as_str() {
                "c" => Ok((CallingConvention::C, add_newline_to_len!(1, input))),
                "qu" => Ok((CallingConvention::Qu, add_newline_to_len!(1, input))),
                _ => Err(IRParserError::UnexpectedValue(
                    input[0].clone().loc,
                    "calling convention",
                    "expected one of 'C' 'Qu'".into(),
                )),
            }
        } else {
            bail!(UnexpectedToken(input[0], "expected a calling convention"));
        }
    }
}

impl<'a> Parse<'a> for FunctionHint {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 2 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a function hint".into(),
            ));
        }
        assert!(input.len() > 2); // unnecessary but clippy is unaware
        if let RawToken::Ident(name) = &input[0].raw {
            match name.as_ref().to_lowercase().as_str() {
                "inline" => {
                    expect_token!(input[1], RawToken::Colon);
                    if let RawToken::Ident(name) = &input[2].raw {
                        match name.as_ref().to_lowercase().as_str() {
                            "always" => {
                                Ok((FunctionHint::InlineAlways, add_newline_to_len!(3, input)))
                            }
                            "never" => {
                                Ok((FunctionHint::InlineNever, add_newline_to_len!(3, input)))
                            }
                            _ => bail!(UnexpectedValue(
                                input[2].loc.clone(),
                                "inline specifier",
                                "expected one of 'always' 'never'"
                            )),
                        }
                    } else {
                        bail!(UnexpectedToken(input[2], "expected an inline specifier"));
                    }
                }
                _ => bail!(UnexpectedValue(
                    input[0].loc.clone(),
                    "function hint",
                    "expected a valid function hint"
                )),
            }
        } else {
            bail!(UnexpectedToken(input[0], "expected a function hint"));
        }
    }
}

impl<'a> Parse<'a> for FunctionAnnotation<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 3 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a function annotation".into(),
            ));
        }
        assert!(input.len() > 3); // unnecessary but clippy is unaware
        expect_token!(input[0], RawToken::ItemAnnotation);
        if let RawToken::Ident(name) = &input[1].raw {
            let name = name.as_ref().to_lowercase();
            match name.as_str() {
                "export" | "extern" => {
                    let mut calling_convention = None;
                    let mut exported_name = None;
                    let len = parse_separated! {
                        &input[2..] => "calling convention or name" {
                            RawToken::Ident(_) => |input: &'a [Token], i: usize| {
                                if calling_convention.is_none() {
                                    calling_convention = Some(CallingConvention::parse((), &input[i..])?.0);
                                    Ok(1)
                                } else {
                                    bail!(UnexpectedValue(input[i].loc.clone(), "ident", "expected calling convention to be passed once"))
                                }
                            },
                            RawToken::StrLiteral(_, _) => |input: &'a [Token], i: usize| {
                                if exported_name.is_none() {
                                    exported_name = input[i].raw.clone().some_str_lit();
                                    Ok(1)
                                } else {
                                    bail!(UnexpectedValue(input[i].loc.clone(), "str", "expected exported name to be passed once"))
                                }
                            },
                        }
                    }?;
                    if calling_convention.is_none() || exported_name.is_none() {
                        bail!(UnexpectedValue(
                            input[3].loc.clone(),
                            "export annotation",
                            "expected calling convention and exported name to be specified"
                        ));
                    }

                    Ok((
                        #[allow(clippy::unwrap_used, reason = "checked ^^^")]
                        if name == "export" {
                            Self::Export(calling_convention.unwrap(), exported_name.unwrap())
                        } else {
                            Self::Extern(calling_convention.unwrap(), exported_name.unwrap())
                        },
                        add_newline_to_len!(len + 2, input),
                    ))
                }
                "hint" => {
                    let (out, len) = parse_separated! {
                        &input[2..] => FunctionHint {
                            RawToken::Ident(_) => "function hint"
                        }
                    }?;

                    Ok((Self::Hint(out.into()), add_newline_to_len!(len + 2, input)))
                }
                _ => bail!(UnexpectedValue(
                    input[0].loc.clone(),
                    "function annotation",
                    "expected a valid function annotation"
                )),
            }
        } else {
            bail!(UnexpectedToken(input[0], "expected a function annotation"));
        }
    }
}

impl<'a> Parse<'a> for Type<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        match &input[0].raw {
            RawToken::Ident(name) => {
                // primitive or struct
                if let Ok((v, len)) = Primitive::parse((), input) {
                    return Ok((
                        Self::Primitive(v),
                        add_newline_to_len!(newline_only, len, input),
                    ));
                }
                Ok((
                    Self::Struct(name.clone()),
                    add_newline_to_len!(newline_only, 1, input),
                ))
            }
            RawToken::PointerOrMul => {
                let (ty, len) = Self::parse((), &input[1..])?;
                Ok((
                    Self::Pointer(Box::new(ty)),
                    add_newline_to_len!(newline_only, len + 1, input),
                ))
            }
            RawToken::OpenSquare => {
                let (ty, len) = Self::parse((), &input[1..])?;
                expect_token!(input[len], RawToken::Semicolon);
                let (arr_len, len2) = Value::parse((), &input[(len + 2)..])?;
                expect_token!(input[len + len2 + 2], RawToken::CloseSquare);

                Ok((
                    Self::List {
                        ty: Box::new(ty),
                        length: arr_len,
                    },
                    add_newline_to_len!(newline_only, len + len2 + 3, input),
                ))
            }
            _ => {
                bail!(UnexpectedToken(input[0], "expected a type"));
            }
        }
    }
}

impl<'a> Parse<'a> for RetType<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input[0].raw == RawToken::Bang {
            return Ok((Self::Never, add_newline_to_len!(1, input)));
        }
        Type::parse((), input).map(|v| (RetType::Normal(v.0), v.1))
    }
}

impl<'a> Parse<'a> for InternalFunctionSignature<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let mut i = 0usize;
        let mut annotations = Vec::new();
        while input[i].raw == RawToken::ItemAnnotation {
            let (annotation, len) = FunctionAnnotation::parse((), &input[i..])?;
            annotations.push(annotation);
            i += len;
        }

        expect_token!(input[i], RawToken::Function);
        i += 1;

        if let RawToken::Ident(name) = &input[i].raw {
            let name = name.as_ref();
            i += 1;

            let mut params = Vec::new();

            let len = parse_separated! {
                &input[i..] => "variable and type" {
                    RawToken::Variable => |input: &'a [Token], i: usize| {
                        if input.len()-i < 4 {
                            bail!(UnexpectedEOF(input[input.len()-1].loc.clone(), "expected a variable and type"));
                        }
                        expect_token!(input[i+2], RawToken::Colon);
                        let (ty, len) = Type::parse((), &input[(i+3)..])?;
                        let name = if let RawToken::Ident(name) = &input[i+1].raw {
                            name.clone()
                        } else {
                            bail!(UnexpectedToken(input[i+1], "expected a variable name"));
                        };
                        params.push((name, ty));
                        Ok(len+3)
                    },
                }
            }?;

            i += len;

            let result = if input[i].raw.is_returns() {
                i += 1;
                let (ty, len) = RetType::parse((), &input[i..])?;
                i += len;
                Some(ty)
            } else {
                None
            };

            Ok((
                InternalFunctionSignature {
                    annotations: annotations.into(),
                    name: name.into(),
                    params: params.into(),
                    result,
                },
                add_newline_to_len!(newline_only, i, input),
            ))
        } else {
            bail!(UnexpectedToken(input[i], "expected function name"));
        }
    }
}

impl<'a> Parse<'a> for ExternalFunctionSignature<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let mut i = 0usize;
        let mut annotations = Vec::new();
        while input[i].raw == RawToken::ItemAnnotation {
            let (annotation, len) = FunctionAnnotation::parse((), &input[i..])?;
            annotations.push(annotation);
            i += len;
        }

        expect_token!(input[i], RawToken::Function);
        i += 1;

        if let RawToken::Ident(name) = &input[i].raw {
            let name = name.as_ref();
            i += 1;

            let mut params = Vec::new();
            let mut params_continue = false;

            let len = parse_separated! {
                &input[i..] => "variable and type" {
                    _ if params_continue => |input: &'a [Token], i: usize| {
                        bail!(UnexpectedToken(input[i], "no tokens expected after a continues marker"));
                    },
                    RawToken::Ident(_) | RawToken::PointerOrMul | RawToken::OpenSquare => |input: &'a [Token], i: usize| {
                        let (ty, len) = Type::parse((), &input[i..])?;
                        params.push(ty);
                        Ok(len)
                    },
                    RawToken::Continues => |_, _| {
                        params_continue = true;
                        Ok(1)
                    },
                }
            }?;

            i += len;

            let result = if input[i].raw.is_returns() {
                i += 1;
                let (ty, len) = RetType::parse((), &input[i..])?;
                i += len;
                Some(ty)
            } else {
                None
            };

            Ok((
                ExternalFunctionSignature {
                    annotations: annotations.into(),
                    name: name.into(),
                    params: params.into(),
                    result,
                    params_continue,
                },
                add_newline_to_len!(i, input),
            ))
        } else {
            bail!(UnexpectedToken(input[i], "expected function name"));
        }
    }
}

impl<'a> Parse<'a> for StructAnnotation {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 3 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a struct annotation".into(),
            ));
        }
        assert!(input.len() > 3); // unnecessary but clippy is unaware
        expect_token!(input[0], RawToken::ItemAnnotation);
        if let RawToken::Ident(name) = &input[1].raw {
            let name = name.as_ref().to_lowercase();
            match name.as_str() {
                "repr" => {
                    let mut packed = false;
                    let mut calling_convention = None;
                    let len = parse_separated! {
                        &input[2..] => "calling convention" {
                            RawToken::Ident(_) => |input: &'a [Token], i: usize| {
                                if let Ok((conv, len)) = CallingConvention::parse((), &input[i..]) {
                                    if calling_convention.is_some() {
                                        bail!(UnexpectedValue(input[i].loc.clone(), "ident", "expected calling convention to be passed once"));
                                    }
                                    calling_convention = Some(conv);
                                    Ok(len)
                                } else if let RawToken::Ident(v) = &input[i].raw && v == "packed" {
                                    if packed {
                                        bail!(UnexpectedValue(input[i].loc.clone(), "ident", "`packed` can only be passed once"))
                                    }
                                    packed = true;
                                    Ok(1)
                                } else {
                                    bail!(UnexpectedValue(input[i].loc.clone(), "ident", "expected a valid calling convention or `packed`"))
                                }
                            },
                        }
                    }?;
                    if calling_convention.is_none() {
                        bail!(UnexpectedValue(
                            input[3].loc.clone(),
                            "repr annotation",
                            "expected calling convention to be specified"
                        ));
                    }

                    Ok((
                        Self::Repr {
                            #[allow(clippy::unwrap_used, reason = "checked above ^^^")]
                            cc: calling_convention.unwrap(),
                            packed,
                        },
                        add_newline_to_len!(len + 2, input),
                    ))
                }
                _ => bail!(UnexpectedValue(
                    input[0].loc.clone(),
                    "function annotation",
                    "expected a valid function annotation"
                )),
            }
        } else {
            bail!(UnexpectedToken(input[0], "expected a struct annotation"));
        }
    }
}

impl<'a> Parse<'a> for StructDef<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let mut i = 0usize;
        let mut annotations = Vec::new();
        while input[i].raw == RawToken::ItemAnnotation {
            let (annotation, len) = StructAnnotation::parse((), &input[i..])?;
            annotations.push(annotation);
            i += len;
        }

        expect_token!(input[i], RawToken::Struct);
        i += 1;
        if let RawToken::Ident(name) = &input[i].raw {
            i += 1;

            let mut fields = Vec::new();
            i += parse_separated! {
                (RawToken::OpenCurly, RawToken::Comma, RawToken::CloseCurly): &input[i..] => "struct name and type" {
                    RawToken::Variable => |input: &'a [Token], i: usize| {
                        if let RawToken::Ident(name) = &input[i+1].raw {
                            expect_token!(input[i+2], RawToken::Colon);
                            let (ty, ty_len) = Type::parse((), &input[(i+3)..])?;
                            fields.push((name.clone(), Box::new(ty)));
                            Ok(ty_len+3)
                        } else {
                            bail!(UnexpectedValue(
                                input[0].loc.clone(),
                                "struct field def",
                                "expected a field name"
                            ))
                        }
                    }
                }
            }?;

            Ok((
                StructDef {
                    annotations: annotations.into(),
                    name: name.clone(),
                    fields: fields.into(),
                },
                add_newline_to_len!(i, input),
            ))
        } else {
            bail!(UnexpectedToken(input[i], "expected struct name"));
        }
    }
}

impl<'a> Parse<'a> for ExtendedVarRef<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input[0].raw.is_drop() {
            return Ok((Self::Drop, 1));
        }
        if input[0].raw.is_variable() {
            let (vref, len) = VarRef::parse((), input)?;
            if input[len].raw.is_property() {
                expect_token!(input[len + 1], RawToken::Ident(_));
                Ok((
                    Self::Struct(vref, input[len + 1].raw.clone().unwrap_ident()),
                    len + 2,
                ))
            } else {
                Ok((Self::Real(vref), len))
            }
        } else {
            bail!(UnexpectedToken(input[0], "expected _ or variable"));
        }
    }
}

impl<'a> Parse<'a> for AssemblyOpt<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 2 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for an inline assembly option".into(),
            ));
        }
        assert!(input.len() > 2); // unnecessary but clippy is unaware
        match &input[0].raw {
            RawToken::Variable => {
                let (vref, len) = VarRef::parse((), input)?;
                expect_token!(input[len], RawToken::Colon);
                expect_token!(input[len + 1], RawToken::Ident(_));
                Ok((
                    Self::VarToReg(vref, input[len + 1].raw.clone().unwrap_ident()),
                    len + 2,
                ))
            }
            RawToken::Ident(v) => {
                expect_token!(input[1], RawToken::Colon);
                match v {
                    v if v.to_lowercase() == "_opt" => {
                        expect_token!(input[2], RawToken::Ident(_));
                        let opt_s = input[2].raw.clone().unwrap_ident().to_lowercase();
                        let opt = match opt_s.as_str() {
                            "noflags" => AssemblyOption::NoFlags,
                            "noreturn" => AssemblyOption::NoReturn,
                            "pure" => AssemblyOption::Pure,
                            "nomem" => AssemblyOption::NoMem,
                            "readonly" => AssemblyOption::ReadOnly,
                            "nostack" => AssemblyOption::NoStack,
                            _ => AssemblyOption::CodegenSpecific(opt_s.into()),
                        };
                        Ok((Self::Option(opt), 3))
                    }
                    reg => {
                        expect_token!(input[2], RawToken::Variable);
                        let (vref, len) = ExtendedVarRef::parse((), &input[2..])?;
                        Ok((Self::RegToVar(reg.clone(), vref), len + 2))
                    }
                }
            }
            _ => {
                bail!(UnexpectedToken(input[0], "expected variable or ident"));
            }
        }
    }
}

impl<'a> Parse<'a> for Cond {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let (s, _) = Str::parse((), input)?;
        let s = s.to_lowercase();
        Ok((
            match s.as_ref() {
                "gt" => Self::GreaterThan,
                "ge" => Self::GreaterEqual,
                "eq" => Self::Equal,
                "ne" => Self::NotEqual,
                "le" => Self::LessEqual,
                "lt" => Self::LessThan,
                _ => bail!(UnexpectedValue(
                    input[0].loc.clone(),
                    "condition",
                    format!("expected one of gt, ge, eq, ne, le, lt; got '{s}'")
                )),
            },
            1,
        ))
    }
}

impl<'a> Parse<'a> for ProdInstruction<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 1 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a producing instruction".into(),
            ));
        }
        assert!(input.len() > 1); // unnecessary but clippy is unaware
        let value = Value::parse((), input);
        match &input[0].raw {
            _ if value.is_ok() => {
                #[allow(clippy::unwrap_used, reason = "checked above ^^^")]
                let val = value.unwrap();
                Ok((Self::Value(val.0), val.1))
            }
            RawToken::Ident(v) => {
                let (params, len) = parse_separated! {
                    &input[1..] => Value {
                        RawToken::Variable | RawToken::Ident(_) | RawToken::StrLiteral(_, _) | RawToken::NumericLiteral(_) => "parameter"
                    }
                }?;
                Ok((
                    ProdInstruction::Call(FunctionCall {
                        func: v.clone(),
                        args: params.into(),
                    }),
                    len + 1,
                ))
            }
            RawToken::OpenParen => {
                if input[1].raw == RawToken::Ident("phi".into()) {
                    let (variables, len) = parse_separated! {
                        (_, RawToken::Comma, RawToken::CloseParen): &input[2..] => ExtendedVarRef {
                            RawToken::Variable => "variable reference"
                        }
                    }?;
                    return Ok((Self::Phi(variables.into()), len + 2));
                }
                let mut i = 2usize;
                let (v0, len) = Value::parse((), &input[i..])?;
                i += len;

                expect_token!(input[i], RawToken::Comma);
                i += 1;

                let (v1, len) = Value::parse((), &input[i..])?;
                i += len;

                expect_token!(input[i], RawToken::CloseParen);
                i += 1;

                Ok((
                    match &input[1].raw {
                        RawToken::Add => Self::Add(v0, v1),
                        RawToken::Sub => Self::Sub(v0, v1),
                        RawToken::PointerOrMul => Self::Mul(v0, v1),
                        RawToken::Div => Self::Div(v0, v1),
                        RawToken::Rem => Self::Rem(v0, v1),

                        RawToken::BlockRefOrAnd => Self::And(v0, v1),
                        RawToken::Or => Self::Or(v0, v1),
                        RawToken::Xor => Self::Xor(v0, v1),
                        _ => bail!(UnexpectedToken(
                            input[1].clone(),
                            "invalid built-in operation"
                        )),
                    },
                    i,
                ))
            }
            RawToken::PointerOrMul => {
                let (val, len) = Value::parse((), &input[1..])?;

                Ok((Self::DerefPtr(val), len + 1))
            }
            RawToken::BlockRefOrAnd => {
                let (var, len) = Str::parse((), &input[1..])?;

                Ok((Self::CreatePtr(var), len + 1))
            }
            _ => bail!(UnexpectedToken(
                input[0].clone(),
                "expected a producing instruction"
            )),
        }
    }
}

impl<'a> Parse<'a> for NoProdInstruction<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input.len() <= 4 {
            return Err(IRParserError::UnexpectedEOF(
                input
                    .last()
                    .map(|v| v.loc.clone() + v.len.clone())
                    .unwrap_or_default(),
                "expected tokens for a non-producing instruction".into(),
            ));
        }
        assert!(input.len() > 4); // unnecessary but clippy is unaware
        match &input[0].raw {
            RawToken::Variable => {
                if input[2].raw.is_property() {
                    // assigning
                    let (var_ref, len) = ExtendedVarRef::parse((), input)?;
                    expect_token!(input[len], RawToken::Assign);
                    let (value, len2) = ProdInstruction::parse((), &input[(len + 1)..])?;
                    Ok((Self::Assign(var_ref, value), len + 1 + len2))
                } else {
                    // definition
                    expect_token!(input[1], RawToken::Ident(_));
                    expect_token!(input[2], RawToken::Colon);
                    let (ty, len) = Type::parse((), &input[3..])?;
                    Ok((
                        Self::VarDef(input[1].raw.clone().unwrap_ident(), ty),
                        len + 3,
                    ))
                }
            }
            RawToken::Ident(v) => {
                if crate::TARGETS.contains(&v.as_ref())
                    && (input[1].raw.is_colon() || input[1].raw.is_open_square())
                {
                    // inline assembly
                    let target = v.clone();
                    let mut i = 1usize;
                    let target_opts = if input[i].raw.is_colon() {
                        i += 1;
                        expect_token!(input[i], RawToken::Ident(_));
                        i += 1;
                        Some(input[i - 1].raw.clone().unwrap_ident())
                    } else {
                        None
                    };
                    expect_token!(input[i], RawToken::OpenSquare);
                    i += 1;
                    expect_token!(input[i], RawToken::InlineAssemblyContents(_));
                    let asm = input[i].raw.clone().unwrap_inline_asm();
                    i += 1;
                    expect_token!(input[i], RawToken::CloseSquare);
                    i += 1;
                    let (opts, len) = parse_separated! {
                        &input[i..] => AssemblyOpt {
                            RawToken::Variable | RawToken::Ident(_) => "assembly option"
                        }
                    }?;
                    i += len;
                    return Ok((
                        NoProdInstruction::InlineAssembly {
                            target,
                            target_opts,
                            asm,
                            opts: opts.into(),
                        },
                        i,
                    ));
                } else if input[1].raw.is_open_paren() {
                    let (params, len) = parse_separated! {
                        &input[1..] => Value {
                            RawToken::Variable | RawToken::Ident(_) | RawToken::StrLiteral(_, _) | RawToken::NumericLiteral(_) => "parameter"
                        }
                    }?;
                    return Ok((
                        NoProdInstruction::Call(FunctionCall {
                            func: v.clone(),
                            args: params.into(),
                        }),
                        len + 1,
                    ));
                }
                bail!(UnexpectedToken(
                    input[0].clone(),
                    "got an ident, but surroundings do not match an inline assembly block or call"
                ));
            }
            RawToken::OpenParen => {
                expect_token!(input[1], RawToken::Ident(_));
                let name_s = input[1].raw.clone().unwrap_ident().to_lowercase();
                match name_s.as_str() {
                    "cmpbr" => {
                        let mut i = 2usize;
                        let (v0, len) = Value::parse((), &input[i..])?;
                        i += len;

                        expect_token!(input[i], RawToken::Comma);
                        i += 1;

                        let (cond, len) = Cond::parse((), &input[i..])?;
                        i += len;

                        expect_token!(input[i], RawToken::Comma);
                        i += 1;

                        let (v1, len) = Value::parse((), &input[i..])?;
                        i += len;

                        expect_token!(input[i], RawToken::Comma);
                        i += 1;

                        expect_token!(input[i], RawToken::BlockRefOrAnd);
                        i += 1;

                        expect_token!(input[i], RawToken::Ident(_));
                        let b_true = input[i].raw.clone().unwrap_ident();
                        i += 1;

                        expect_token!(input[i], RawToken::Comma);
                        i += 1;

                        expect_token!(input[i], RawToken::BlockRefOrAnd);
                        i += 1;

                        expect_token!(input[i], RawToken::Ident(_));
                        let b_false = input[i].raw.clone().unwrap_ident();
                        i += 1;

                        expect_token!(input[i], RawToken::CloseParen);
                        i += 1;

                        Ok((
                            Self::CmpBr {
                                v0,
                                cond,
                                v1,
                                b_true,
                                b_false,
                            },
                            i,
                        ))
                    }
                    "jmp" => {
                        expect_token!(input[2], RawToken::BlockRefOrAnd);
                        expect_token!(input[3], RawToken::Ident(_));
                        expect_token!(input[4], RawToken::CloseParen);
                        Ok((Self::Jmp(input[3].raw.clone().unwrap_ident()), 5))
                    }
                    "ret" => {
                        let val = if input[2].raw == RawToken::CloseParen {
                            (None, 1usize)
                        } else {
                            let (val, len) = Value::parse((), &input[2..])?;
                            (Some(val), len + 1)
                        };
                        Ok((Self::Return(val.0), val.1 + 1))
                    }
                    _ => {
                        bail!(UnexpectedValue(
                            input[1].loc.clone(),
                            "non-producing instruction",
                            "expected one of cmpbr, jmp, ret"
                        ));
                    }
                }
            }
            _ => bail!(UnexpectedToken(
                input[0].clone(),
                "expected a non-producing instruction"
            )),
        }
    }
}

impl<'a> Parse<'a> for FunctionDef<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let mut i = 0usize;
        let mut is_extern = false;
        while input[i].raw == RawToken::ItemAnnotation {
            let (annotation, len) = FunctionAnnotation::parse((), &input[i..])?;
            if matches!(annotation, FunctionAnnotation::Extern(_, _)) {
                is_extern = true;
                break;
            }
            i += len;
        }

        if is_extern {
            let (sig, len) = ExternalFunctionSignature::parse((), &input[i..])?;
            let extern_annotations = sig
                .annotations
                .iter()
                .filter(|v| matches!(*v, FunctionAnnotation::Extern(_, _)))
                .collect::<Vec<_>>();
            if extern_annotations.len() > 1 {
                bail!(UnexpectedValue(
                    input[i].loc.clone(),
                    "external function",
                    "expected a single extern annotation"
                ));
            }
            i += len;
            let (cc, name) = extern_annotations[0].clone().unwrap_extern();
            return Ok((
                Self::External {
                    calling_conv: cc,
                    sig,
                    external_name: name,
                },
                i,
            ));
        }

        let (sig, len) = InternalFunctionSignature::parse((), &input[i..])?;
        if !input[i].raw.is_block() {
            bail!(UnexpectedToken(input[i], "expected entry block"));
        }
        i += len;

        let RawToken::Ident(entry) = input[i].raw.clone() else {
            bail!(UnexpectedToken(input[i], "expected entry block"));
        };
        i += 1;

        let mut vars = Vec::new();
        let mut blocks = Vec::new();

        i += parse_separated! {
            (RawToken::OpenCurly, RawToken::Semicolon, RawToken::CloseCurly): &input[i..] => "function item" {
                RawToken::Variable => |input: &'a [Token], i: usize| {
                    if input.len() - i < 5 {
                        bail!(UnexpectedEOF(input[i].loc.clone() + input[i].len.clone(), "expected a variable definition"));
                    } else if let Some(RawToken::Ident(name)) = input.get(i+1).map(|v| v.raw.clone()) {
                        expect_token!(input[i+2], RawToken::Colon);
                        let (ty, len) = Type::parse((), &input[(i+3)..])?;
                        vars.push((name, ty));
                        Ok(len + 3)
                    } else {
                        bail!(UnexpectedToken(input[i+1], "expected a variable definition"));
                    }
                },
                RawToken::BlockRefOrAnd => |input: &'a [Token], i: usize| {
                    if input.len() - i < 4 {
                        bail!(UnexpectedEOF(input[i].loc.clone() + input[i].len.clone(), "expected a block definition"));
                    } else if let Some(RawToken::Ident(name)) = input.get(i+1).map(|v| v.raw.clone()) {
                        let (instructions, len) = parse_separated! {
                            (RawToken::OpenCurly, RawToken::Semicolon, RawToken::CloseCurly): &input[i..] => NoProdInstruction {
                                _ => "instructions"
                            }
                        }?;
                        blocks.push(Block {
                            name,
                            instructions: instructions.into()
                        });
                        Ok(len + 2)
                    } else {
                        bail!(UnexpectedToken(input[i+1], "expected a block definition"));
                    }
                },
            }
        }?;

        Ok((
            Self::Internal {
                sig,
                func_vars: vars.into(),
                code: blocks.into(),
                entry_block: entry,
            },
            i,
        ))
    }
}

impl<'a> Parse<'a> for ModuleItem<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        Ok(if let Ok(v) = ModuleAnnotation::parse((), input) {
            (Self::ModuleAnnotation(v.0), v.1)
        } else if let Ok(v) = StructDef::parse((), input) {
            (Self::StructDef(v.0), v.1)
        } else {
            let v = FunctionDef::parse((), input)?;
            (Self::Func(v.0), v.1)
        })
    }
}
