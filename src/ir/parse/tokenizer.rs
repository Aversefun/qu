//! Tokenization of QIR.

use crate::{Str, errors::IRTokenizerError, ir::Location};

/// The type of a string literal.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StrType {
    /// A normal string.
    Normal,
    /// A C string.
    C,
}

/// A wrapped token.
#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    /// The raw token itself.
    pub raw: RawToken<'a>,
    /// The location of this token.
    pub loc: Location<'a>,
}

/// A single token.
#[derive(Clone, Debug, PartialEq)]
pub enum RawToken<'a> {
    /// @@
    ModuleAnnotation,
    /// Includes types and other stuff.
    Ident(Str<'a>),
    /// (
    OpenParen,
    /// Anything that matches the regex `/-?[0-9]+(\.[0-9]+)?/.`
    NumericLiteral(Str<'a>),
    /// ([`StrType`])`\"[^\"]*\"`
    StrLiteral(StrType, Str<'a>),
    /// )
    CloseParen,
    /// \n
    Newline,
    /// ,
    Comma,
    /// @
    ItemAnnotation,
    /// fn
    Function,
    /// ->
    Returns,
    /// !
    Bang,
    /// ...
    Continues,
    /// struct
    Struct,
    /// {
    OpenCurly,
    /// $
    Variable,
    /// :
    Colon,
    /// *
    PointerOrMul,
    /// [
    OpenSquare,
    /// ;
    Semicolon,
    /// ]
    CloseSquare,
    /// }
    CloseCurly,
    /// &
    Block,
    /// =
    Assign,
    /// Tokenizer does some special stuff to serialize everything after the start of an inline assembly block
    /// into this.
    InlineAssemblyContents(Str<'a>),
    /// +
    Plus,
    /// -
    Sub,
    /// /
    Div,
    /// %
    Mod,
    /// .
    Property,
    /// Any whitespace character. Should never be produced by the tokenizer.
    Whitespace,
}

/// A character stream used for turning into tokens
pub trait ReadChar {
    /// The error type.
    type Error: std::error::Error;
    /// Read a character. Returns Ok(None) if the end has been reached.
    ///
    /// # Errors
    /// If there was an error reading, then it should be propagated.
    /// This function should NEVER panic.
    fn read_char(&mut self) -> Result<Option<char>, Self::Error>;
    /// Read this stream to the end.
    ///
    /// # Errors
    /// Propogates errors from [`read_char`](ReadChar::read_char).
    fn read_all(&mut self) -> Result<Vec<char>, Self::Error> {
        let mut out = Vec::new();
        while let Some(ch) = self.read_char()? {
            out.push(ch);
        }
        Ok(out)
    }
}

// impl<T: Iterator<Item = char>> ReadChar for T {
//     type Error = Infallible;
//     fn read_char(&mut self) -> Result<Option<char>, Self::Error> {
//         Ok(self.next())
//     }
// }

/// Get the width of a UTF-8 character from the first byte.
///
/// Returns 1 for invalid characters.
#[allow(clippy::match_same_arms)]
fn utf8_char_width(b: u8) -> usize {
    match b {
        0x00..=0x7F => 1,
        0xC0..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF7 => 4,
        _ => 1, // Detected invalid by str::from_utf8
    }
}

impl<T: std::io::Read> ReadChar for T {
    type Error = std::io::Error;
    fn read_char(&mut self) -> Result<Option<char>, Self::Error> {
        let mut byte1 = [0u8];
        if self.read(&mut byte1)? == 0 {
            return Ok(None);
        }

        let mut out = [byte1[0], 0u8, 0u8, 0u8];

        let len = utf8_char_width(byte1[0]);

        if len > 1 {
            self.read_exact(&mut out[1..len])?;
        }

        let s = str::from_utf8(&out[..len])
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

        Ok(s.chars().next())
    }
}

/// Parse a number literal.
fn parse_number<'a, T: 'a + ReadChar>(
    chs: &[char],
    mut loc: Location<'a>,
) -> Result<(Option<RawToken<'a>>, usize), IRTokenizerError<'a, <T as ReadChar>::Error>> {
    let mut out = String::new();
    let mut has_decimal = false;
    let mut neg = false;
    let mut len = 0usize;
    for (i, ch) in chs.iter().enumerate() {
        match ch {
            '-' if i == 0 && !neg => {
                out.push('-');
                neg = true;
            }
            '+' if i == 0 => {}
            '-' => {
                return Err(IRTokenizerError::InvalidCharacter(
                    '-',
                    loc.add_column(i).clone(),
                    "negative symbol must be at the start",
                ));
            }
            '0'..='9' => out.push(*ch),
            '.' => {
                if has_decimal {
                    return Err(IRTokenizerError::InvalidCharacter(
                        '.',
                        loc.add_column(i).clone(),
                        "only one decimal point is expected per number",
                    ));
                }
                has_decimal = true;
                out.push(*ch);
            }
            '_' => {}
            _ => {
                len = i;
                break;
            }
        }
    }
    Ok((Some(RawToken::NumericLiteral(out.into())), len))
}

/// Parse a string literal.
fn parse_str_literal<'a>(chs: &[char], ty: StrType) -> (std::option::Option<RawToken<'a>>, usize) {
    let mut out = String::new();
    for ch in chs {
        match ch {
            '"' | '\n' => break,
            _ => out.push(*ch),
        }
    }
    let len = out.len();
    (
        Some(RawToken::StrLiteral(ty, out.into())),
        len + 2 + usize::from(ty == StrType::C),
    )
}

/// Using the provided char reader, read it to the end into tokens.
///
/// # Errors
/// Any reader errors are propogated and invalid characters return an error.
pub fn read_tokens<'a, T: 'a + ReadChar>(
    reader: &mut T,
    starting_loc: &Location<'a>,
) -> Result<Vec<Token<'a>>, IRTokenizerError<'a, <T as ReadChar>::Error>> {
    fn match_token<'a, T: 'a + ReadChar>(
        chs: &[char],
        hist: &[Token<'a>],
        loc: &Location<'a>,
    ) -> Result<(Option<RawToken<'a>>, usize), IRTokenizerError<'a, <T as ReadChar>::Error>> {
        match chs[0] {
            '@' => match chs[1] {
                '@' => Ok((Some(RawToken::ModuleAnnotation), 2)),
                _ => Ok((Some(RawToken::ItemAnnotation), 1)),
            },
            '(' => Ok((Some(RawToken::OpenParen), 1)),
            ')' => Ok((Some(RawToken::CloseParen), 1)),
            '\n' => Ok((Some(RawToken::Newline), 1)),
            ',' => Ok((Some(RawToken::Comma), 1)),
            'f' if chs[1] == 'n' && chs[2].is_whitespace() => Ok((Some(RawToken::Function), 2)),
            '-' if chs[1] == '>' => Ok((Some(RawToken::Returns), 2)),
            '!' => Ok((Some(RawToken::Bang), 1)),
            '.' if chs[1..=2] == ['.', '.'] => Ok((Some(RawToken::Continues), 3)),
            's' if chs[1..=5] == ['t', 'r', 'u', 'c', 't'] && chs[6].is_whitespace() => {
                Ok((Some(RawToken::Struct), 6))
            }
            '{' => Ok((Some(RawToken::OpenCurly), 1)),
            '$' => Ok((Some(RawToken::Variable), 1)),
            ':' => Ok((Some(RawToken::Colon), 1)),
            '*' => Ok((Some(RawToken::PointerOrMul), 1)),
            '[' => Ok((Some(RawToken::OpenSquare), 1)),
            ';' => Ok((Some(RawToken::Semicolon), 1)),
            ']' => Ok((Some(RawToken::CloseSquare), 1)),
            '}' => Ok((Some(RawToken::CloseCurly), 1)),
            '&' => Ok((Some(RawToken::Block), 1)),
            '=' => Ok((Some(RawToken::Assign), 1)),
            '+' => Ok((Some(RawToken::Plus), 1)),
            '/' if chs[1] == '/' => {
                let mut len = 0usize;
                for ch in chs {
                    len += 1;
                    if ch == &'\n' {
                        break;
                    }
                }
                Ok((None, len))
            }
            '/' => Ok((Some(RawToken::Div), 1)),
            '%' => Ok((Some(RawToken::Mod), 1)),
            '-' if chs[1].is_ascii_digit() => parse_number::<T>(chs, loc.clone()),
            '-' => Ok((Some(RawToken::Sub), 1)),
            '0'..='9' => parse_number::<T>(chs, loc.clone()),

            'c' if chs[1] == '"' => Ok(parse_str_literal(&chs[2..], StrType::Normal)),
            '"' => Ok(parse_str_literal(&chs[1..], StrType::Normal)),

            ch if ch.is_alphabetic() || ch == '_' => {
                let mut out = String::new();
                out.push(chs[0]);
                for ch in &chs[1..] {
                    match ch {
                        _ if ch.is_alphanumeric() => out.push(*ch),
                        '_' => out.push(*ch),
                        _ => break,
                    }
                }
                let asm_id_slice = if hist[hist.len().saturating_sub(3)].raw == RawToken::Colon {
                    &hist[hist.len().saturating_sub(4)..]
                } else {
                    &hist[hist.len().saturating_sub(2)..]
                };
                let is_asm = matches!(&asm_id_slice[0].raw, RawToken::Ident(v) if crate::TARGETS.contains(&v.as_ref()))
                    && asm_id_slice.last().unwrap().raw == RawToken::OpenSquare
                    && hist.len() >= 4;

                let len = out.len();

                Ok((
                    Some(if is_asm {
                        RawToken::InlineAssemblyContents(out.into())
                    } else {
                        RawToken::Ident(out.into())
                    }),
                    len,
                ))
            }
            '.' => Ok((Some(RawToken::Property), 1)),
            ch if ch.is_whitespace() => Ok((Some(RawToken::Whitespace), 1)),
            ch => Err(IRTokenizerError::InvalidCharacter(
                ch,
                loc.clone(),
                "unexpected character",
            )),
        }
    }
    let mut out = Vec::new();
    let mut loc = starting_loc.clone();

    let chs = reader.read_all()?;
    let mut i = 0usize;
    while i < chs.len() {
        let (token, add) = match_token::<T>(&chs[i..], &out, &loc)?;
        eprintln!("{loc}: {token:?}");
        let tok_loc = loc.clone();
        loc.add_column(add);
        i += add;
        if token == Some(RawToken::Newline) || token.is_none() {
            loc.add_line(1);
        }

        if let Some(token) = token
            && token != RawToken::Whitespace
            && (token != RawToken::Newline
                || (out.last().map(|v| &v.raw) != Some(&token)
                    && out.last().map(|v| &v.raw) != Some(&RawToken::Comma)))
        {
            out.push(Token {
                raw: token,
                loc: tok_loc,
            });
        }
    }

    Ok(out)
}
