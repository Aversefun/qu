//! The parser for QIR. Includes tokenization and the actual parsing.

use super::{
    ConstValue, FunctionCall, ModuleAnnotation, Primitive, RuntimeCheck, Str, Value, VarRef,
    Version,
};
use crate::{
    errors::IRParserError,
    ir::{CallingConvention, FunctionAnnotation, FunctionHint, FunctionSignature},
};
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
    (InvalidToken($tok:expr, $s:expr)) => {
        return Err(IRParserError::InvalidToken(
            $tok.raw.clone(),
            $tok.loc.clone(),
            $s,
        ))
    };
    (ParseIntError($tok:expr, $err:expr)) => {
        return Err(IRParserError::ParseIntError(
            $err,
            $tok.raw.clone(),
            $tok.loc.clone(),
        ))
    };
    (ParseFloatError($tok:expr, $err:expr)) => {
        return Err(IRParserError::ParseFloatError(
            $err,
            $tok.raw.clone(),
            $tok.loc.clone(),
        ))
    };
    (UnexpectedMetaValue($s:expr)) => {
        return Err(IRParserError::UnexpectedMetaValue(($s).into()))
    };
    (InvalidValue($loc:expr, $ty:literal, $note:expr)) => {
        return Err(IRParserError::InvalidValue($loc, $ty, ($note).into()))
    };
    (GenericError($loc:expr, $ty:literal, $err:expr)) => {
        return Err(IRParserError::GenericError($loc, $ty, Box::new($err)))
    };
}

/// Expect a token to be a certain pattern.
macro_rules! expect_token {
    ($tok:expr, $ty:pat) => {
        if !matches!($tok.raw, $ty) {
            bail!(InvalidToken($tok, concat!("expected a ", stringify!($ty))));
        }
    };
}

/// Parse a comma separated pattern. The first token should be an `OpenParen`
/// and the last a `CloseParen`.
macro_rules! parse_comma_separated {
    {
        $input:expr => $t:ty {
            $value_matcher:pat => $item_name:literal
        }
    } => {
        'bl: {
            let input = $input;
            let mut expecting_comma = false;
            let mut out = Vec::new();
            let mut i = 1usize;
            if !matches!(input[0].raw, RawToken::OpenParen) {
                break 'bl Err(IRParserError::InvalidToken(
                    input[0].raw.clone(),
                    input[0].loc.clone(),
                    "expected a RawToken::OpenParen",
                ))
            }
            while input[i].raw != RawToken::CloseParen {
                match input[i].raw {
                    $value_matcher => {
                        if expecting_comma {
                            break 'bl Err(IRParserError::InvalidToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a comma next, got ", $item_name),
                            ));
                        }
                        let (val, l) = <$t>::parse(<$t>::default_meta(), &input[i..])?;
                        out.push(val);
                        i += l - 1;
                        expecting_comma = true;
                    }
                    RawToken::Comma => {
                        if !expecting_comma {
                            break 'bl Err(IRParserError::InvalidToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a ", $item_name," next, got comma"),
                            ));
                        }
                        expecting_comma = false;
                    }
                    _ => {
                        break 'bl Err(IRParserError::InvalidToken(
                            input[i].raw.clone(),
                            input[i].loc.clone(),
                            concat!("expected one of comma, ", $item_name, ", closeparen"),
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
            $($value_matcher:pat => $bl:expr),+
            $(,)?
        }
    } => {
        'bl: {
            let input = $input;
            let mut expecting_comma = false;
            let mut i = 1usize;
            if !matches!(input[0].raw, RawToken::OpenParen) {
                break 'bl Err(IRParserError::InvalidToken(
                    input[0].raw.clone(),
                    input[0].loc.clone(),
                    "expected a RawToken::OpenParen",
                ))
            }
            while input[i].raw != RawToken::CloseParen {
                match input[i].raw {
                    $($value_matcher => {
                        if expecting_comma {
                            break 'bl Err(IRParserError::InvalidToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a comma next, got ", $item_name),
                            ));
                        }
                        let l: usize = match $bl(input, i) {
                            Ok(l) => l,
                            Err(e) => break 'bl Err(e),
                        };
                        i += l.saturating_sub(1);
                        expecting_comma = true;
                    }),+
                    RawToken::Comma => {
                        if !expecting_comma {
                            break 'bl Err(IRParserError::InvalidToken(
                                input[i].raw.clone(),
                                input[i].loc.clone(),
                                concat!("expected a ", $item_name," next, got comma"),
                            ));
                        }
                        expecting_comma = false;
                    }
                    _ => {
                        break 'bl Err(IRParserError::InvalidToken(
                            input[i].raw.clone(),
                            input[i].loc.clone(),
                            concat!("expected one of comma, ", $item_name, ", closeparen"),
                        ));
                    }
                }
                i += 1;
            }
            Ok(i+1)
        }
    };
}

macro_rules! add_newline_to_len {
    ($len:expr, $input:expr) => {
        $len + usize::from(
            $input.get($len).map(|v| &v.raw) == Some(&RawToken::Newline),
        )
    };
}

impl<'a> Parse<'a> for ConstValue<'a> {
    type Meta = Option<Primitive>;
    fn parse(meta: Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        let loc = input[0].loc.clone();
        let raw = input[0].raw.clone();
        match &raw {
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
            tok => Err(IRParserError::InvalidToken(
                tok.clone(),
                loc,
                "expected a strliteral or numericliteral",
            )),
        }.map(|v| (v, 1))
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
                        bail!(InvalidValue(loc, "ident", "expected a primitive type name"));
                    }
                },
                1,
            ))
        } else {
            Err(IRParserError::InvalidToken(
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
                    .map(|v| (v, 1))
                    .map_err(|v| IRParserError::GenericError(loc, "version", Box::new(v)))
            } else {
                Err(IRParserError::InvalidValue(
                    loc,
                    "version",
                    format!("expected a normal string, got a {ty:?}").into(),
                ))
            }
        } else {
            Err(IRParserError::InvalidToken(
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
            check => Err(IRParserError::InvalidValue(loc, "runtime check", format!("expected one of array_bounds, overflow, underflow, null_ptr, or unaligned_ptr; got {check}").into()))
        }.map(|v| (v, 1))
    }
}

impl<'a> Parse<'a> for Str<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if let RawToken::Ident(val) = &input[0].raw {
            Ok((std::borrow::Cow::Borrowed(val), 1))
        } else {
            Err(IRParserError::InvalidToken(
                input[0].clone().raw,
                input[0].loc.clone(),
                "expected an ident",
            ))
        }
    }
}

impl<'a> Parse<'a> for Value<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if input[0].raw == RawToken::Variable
            && input[2].raw == RawToken::Property
            && let RawToken::NumericLiteral(version) = &input[3].raw
        {
            let version = version.parse().map_err(|e| {
                IRParserError::ParseIntError(e, input[3].raw.clone(), input[3].loc.clone())
            })?;

            Str::parse((), input).map(|v| (Value::Local(VarRef { name: v.0, version }), v.1 + 1))
        } else if let Ok(val) = ConstValue::parse(None, input) {
            Ok((Value::Constant(val.0), val.1))
        } else if let RawToken::Ident(name) = &input[0].raw
            && name == "undef"
        {
            Ok((Value::Undef, 1))
        } else {
            Err(IRParserError::InvalidToken(
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
        let (vals, len) = parse_comma_separated! {
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
            len + 1,
        ))
    }
}

impl<'a> Parse<'a> for ModuleAnnotation<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        expect_token!(input[0], RawToken::ModuleAnnotation);
        expect_token!(input[2], RawToken::OpenParen);
        if let RawToken::Ident(name) = &input[1].raw {
            let name = name.as_ref().to_lowercase();
            if name.as_str() == "quver" && input[4].raw != RawToken::CloseParen {
                Err(IRParserError::InvalidToken(
                    input[0].raw.clone(),
                    input[0].loc.clone(),
                    "expected a close paren after a single argument for quver module annotation",
                ))
            } else {
                match name.as_str() {
                    "quver" => {
                        let len = if input.get(5).map(|v| &v.raw) == Some(&RawToken::Newline) {
                            6
                        } else {
                            5
                        };
                        Ok((Self::QuVer(Version::parse((), &input[3..])?.0), len))
                    }
                    "runtime_checks" => {
                        let (checks, len) = parse_comma_separated! {
                            &input[2..] => RuntimeCheck {
                                RawToken::Ident(_) => "check"
                            }
                        }?;
                        Ok((
                            Self::RuntimeChecks(checks.into()),
                            add_newline_to_len!(len+2, input),
                        ))
                    }
                    "check_violated" => {
                        let (calls, len) = parse_comma_separated! {
                            &input[2..] => FunctionCall {
                                RawToken::Ident(_) => "call"
                            }
                        }?;
                        Ok((
                            Self::CheckViolated(calls.into()),
                            add_newline_to_len!(len+2, input),
                        ))
                    }
                    name => Err(IRParserError::InvalidValue(
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
            bail!(InvalidToken(input[0], "expected a module annotation"));
        }
    }
}

impl<'a> Parse<'a> for CallingConvention {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if let RawToken::Ident(name) = &input[0].raw {
            match name.as_ref().to_lowercase().as_str() {
                "c" => Ok((CallingConvention::C, 1)),
                "qu" => Ok((CallingConvention::Qu, 1)),
                _ => Err(IRParserError::InvalidValue(
                    input[0].clone().loc,
                    "calling convention",
                    "expected one of 'C' 'Qu'".into(),
                )),
            }
        } else {
            bail!(InvalidToken(input[0], "expected a calling convention"));
        }
    }
}

impl<'a> Parse<'a> for FunctionHint {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        if let RawToken::Ident(name) = &input[0].raw {
            match name.as_ref().to_lowercase().as_str() {
                "inline" => {
                    expect_token!(input[1], RawToken::Colon);
                    if let RawToken::Ident(name) = &input[2].raw {
                        match name.as_ref().to_lowercase().as_str() {
                            "always" => Ok((FunctionHint::InlineAlways, 3)),
                            "never" => Ok((FunctionHint::InlineNever, 3)),
                            _ => bail!(InvalidValue(
                                input[2].loc.clone(),
                                "inline specifier",
                                "expected one of 'always' 'never'"
                            )),
                        }
                    } else {
                        bail!(InvalidToken(input[2], "expected an inline specifier"));
                    }
                }
                _ => bail!(InvalidValue(
                    input[0].loc.clone(),
                    "function hint",
                    "expected a valid function hint"
                )),
            }
        } else {
            bail!(InvalidToken(input[0], "expected a function hint"));
        }
    }
}

impl<'a> Parse<'a> for FunctionAnnotation<'a> {
    type Meta = ();
    fn parse((): Self::Meta, input: &'a [Token]) -> Result<(Self, usize), IRParserError<'a>> {
        expect_token!(input[0], RawToken::ItemAnnotation);
        if let RawToken::Ident(name) = &input[1].raw {
            let name = name.as_ref().to_lowercase();
            match name.as_str() {
                "export" | "extern" => {
                    let mut calling_convention = None;
                    let mut exported_name = None;
                    let len = parse_comma_separated! {
                        &input[2..] => "calling convention or name" {
                            RawToken::Ident(_) => |input: &'a [Token], i: usize| {
                                if calling_convention.is_none() {
                                    calling_convention = Some(CallingConvention::parse((), &input[i..])?.0);
                                    Ok(1)
                                } else {
                                    bail!(InvalidValue(input[i].loc.clone(), "ident", "expected calling convention to be passed once"))
                                }
                            },
                            RawToken::StrLiteral(_, _) => |input: &'a [Token], i: usize| {
                                if exported_name.is_none() {
                                    exported_name = input[i].raw.clone().some_str_lit();
                                    Ok(1)
                                } else {
                                    bail!(InvalidValue(input[i].loc.clone(), "str", "expected exported name to be passed once"))
                                }
                            },
                        }
                    }?;
                    if calling_convention.is_none() || exported_name.is_none() {
                        bail!(InvalidValue(
                            input[3].loc.clone(),
                            "export annotation",
                            "expected calling convention and exported name to be specified"
                        ));
                    }

                    Ok((
                        if name == "export" {
                            Self::Export(calling_convention.unwrap(), exported_name.unwrap())
                        } else {
                            Self::Extern(calling_convention.unwrap(), exported_name.unwrap())
                        },
                        add_newline_to_len!(len+2, input),
                    ))
                }
                "hint" => {
                    let (out, len) = parse_comma_separated! {
                        &input[2..] => FunctionHint {
                            RawToken::Ident(_) => "function hint"
                        }
                    }?;

                    Ok((Self::Hint(out.into()), add_newline_to_len!(len+2, input)))
                }
                _ => bail!(InvalidValue(
                    input[0].loc.clone(),
                    "function annotation",
                    "expected a valid function annotation"
                )),
            }
        } else {
            bail!(InvalidToken(input[0], "expected a function annotation"));
        }
    }
}

impl<'a> Parse<'a> for FunctionSignature<'a> {
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
            let name = name.as_ref().to_lowercase();
            i += 1;

            let mut parameters = Vec::new();
            let len = parse_comma_separated! {
                &input[i..] => "type" {
                    RawToken::Ident(_) | RawToken::PointerOrMul | RawToken::OpenSquare => |input: &'a [Token], i: usize| {
                        let (ty, len) = Type::parse((), &input[i..])?;
                        parameters.push(ty);
                        Ok(len)
                    },
                    RawToken::Variable => |input: &'a [Token], i: usize| {
                        
                    }
                }
            };
            i += len;
        } else {
            bail!(InvalidToken(input[i], "expected function keyword"));
        }
    }
}
